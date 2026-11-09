# =============================================================================
#  MODULE 4 - EVALUATION
#  Benchmark, factors and risk-free rate; performance metrics; Newey-West
#  regressions; factor attribution; pairwise comparisons; bootstrap tests;
#  robustness and subperiods.
#  Strategies are a NAMED LIST of backtest outputs (columns mese_reso, r_lordo,
#  r_netto, turnover): no strategy name is hard-wired.
# =============================================================================
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(lubridate) })

# =============================================================================
#  1. BENCHMARK, FACTORS AND RISK-FREE RATE (all cached locally)
# =============================================================================
# Fama-French monthly file from Kenneth French's data library (percent -> decimal).
scarica_french <- function(zip, base = "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/") {
  tmp <- tempfile(fileext = ".zip")
  if (inherits(try(utils::download.file(paste0(base, zip), tmp, mode = "wb", quiet = TRUE),
                   silent = TRUE), "try-error")) return(NULL)
  cart <- tempfile(); dir.create(cart); utils::unzip(tmp, exdir = cart)
  righe <- readLines(list.files(cart, full.names = TRUE)[1], warn = FALSE)
  men <- grepl("^\\s*\\d{6}\\s*,", righe); if (!any(men)) return(NULL)
  ini <- which(men)[1]; fin <- ini
  while (fin <= length(righe) && men[fin]) fin <- fin + 1
  d <- read.csv(text = c(righe[ini - 1], righe[ini:(fin - 1)]), stringsAsFactors = FALSE)
  names(d)[1] <- "aaaamm"
  d <- d |> mutate(mese_reso = as.Date(paste0(trimws(aaaamm), "01"), "%Y%m%d")) |> select(-aaaamm) |>
    mutate(across(where(is.numeric), \(x) x / 100))
  names(d) <- gsub("[^A-Za-z0-9_]", "", names(d)); d
}

# Market, size, value, profitability, investment and momentum factors.
carica_fattori <- function(file_cache = "fattori_french.csv",
                           regione = c("US", "Europe", "Developed")) {
  regione <- match.arg(regione)
  if (file.exists(file_cache)) return(leggi(file_cache) |> mutate(mese_reso = as.Date(mese_reso)))
  zip <- switch(regione,
    US        = c(ff3 = "F-F_Research_Data_Factors_CSV.zip", mom = "F-F_Momentum_Factor_CSV.zip",
                  ff5 = "F-F_Research_Data_5_Factors_2x3_CSV.zip"),
    Europe    = c(ff3 = "Europe_3_Factors_CSV.zip", mom = "Europe_Mom_Factor_CSV.zip",
                  ff5 = "Europe_5_Factors_CSV.zip"),
    Developed = c(ff3 = "Developed_3_Factors_CSV.zip", mom = "Developed_Mom_Factor_CSV.zip",
                  ff5 = "Developed_5_Factors_CSV.zip"))
  ff3 <- scarica_french(zip[["ff3"]]); if (is.null(ff3)) stop("Factor download failed.")
  f <- ff3 |> rename(mkt_rf = MktRF, smb = SMB, hml = HML, rf = RF)
  mm <- scarica_french(zip[["mom"]])
  if (!is.null(mm)) f <- f |> left_join(mm |> select(mese_reso, mom = setdiff(names(mm), "mese_reso")[1]),
                                       by = "mese_reso")
  f5 <- scarica_french(zip[["ff5"]])
  if (!is.null(f5)) f <- f |> left_join(f5 |> select(mese_reso, rmw = RMW, cma = CMA), by = "mese_reso")
  f <- f |> arrange(mese_reso); write.csv(f, file_cache, row.names = FALSE); f
}

# Total-return benchmark from LSEG. It must be TOTAL RETURN because the
# strategy returns include dividends.
carica_benchmark_lseg <- function(ric, data_inizio, data_fine, file_cache) {
  if (file.exists(file_cache)) return(leggi(file_cache) |> mutate(mese_reso = as.Date(mese_reso)))
  br <- as.data.frame(Refinitiv::rd_GetHistory(universe = ric, fields = "TR.PriceClose",
          parameters = list("SDate" = data_inizio, "EDate" = data_fine, "Frq" = "M")))
  ch <- gsub("[^a-z]", "", tolower(names(br)))
  b <- data.frame(Data = as.Date(br[[which(grepl("date", ch))[1]]]),
                  liv = suppressWarnings(as.numeric(br[[which(grepl("priceclose", ch))[1]]]))) |>
    filter(!is.na(liv)) |> arrange(Data) |>
    mutate(mese_reso = floor_date(Data, "month"), r_bench = liv / lag(liv) - 1) |>
    select(mese_reso, r_bench)
  write.csv(b, file_cache, row.names = FALSE); b
}

# Euribor 1M from the ECB Data Portal (monthly average, % per year), with retries
# and a local cache. The API occasionally answers HTTP 503.
carica_euribor <- function(file_cache, inizio = "2003-12", tentativi = 5) {
  if (file.exists(file_cache)) return(leggi(file_cache))
  url <- paste0("https://data-api.ecb.europa.eu/service/data/FM/M.U2.EUR.RT.MM.EURIBOR1MD_.HSTA",
                "?format=csvdata&startPeriod=", inizio)
  for (k in seq_len(tentativi)) {
    ecb <- try(read.csv(url, stringsAsFactors = FALSE), silent = TRUE)
    if (!inherits(ecb, "try-error")) break
    cat("ECB download failed (attempt", k, "of", tentativi, "), retrying...\n"); Sys.sleep(5 * k)
  }
  if (inherits(ecb, "try-error"))
    stop("ECB download failed after ", tentativi, " attempts: ", conditionMessage(attr(ecb, "condition")))
  ecb <- ecb[, c("TIME_PERIOD", "OBS_VALUE")]
  write.csv(ecb, file_cache, row.names = FALSE); ecb
}

# Risk-free rate in euro: Euribor 1M monthly average / 100 / 12. The French
# factors are unchanged; the original US T-bill stays in the rf_french column.
aggiungi_rf_euro <- function(fatt, ecb) {
  rf_eur <- data.frame(mese_reso = as.Date(paste0(ecb$TIME_PERIOD, "-01")),
                       rf_eur = ecb$OBS_VALUE / 100 / 12)
  fatt |> left_join(rf_eur, by = "mese_reso") |>
    mutate(rf_french = rf, rf = rf_eur) |> select(-rf_eur)
}

# Wide table: one column <name>_lordo, <name>_netto, <name>_tn per strategy.
unisci_serie <- function(strategie, benchmark, fattori) {
  out <- NULL
  for (nm in names(strategie)) {
    s <- strategie[[nm]] |> transmute(mese_reso,
           !!paste0(nm, "_lordo") := r_lordo, !!paste0(nm, "_netto") := r_netto,
           !!paste0(nm, "_tn") := turnover)
    out <- if (is.null(out)) s else full_join(out, s, by = "mese_reso")
  }
  out |> arrange(mese_reso) |> left_join(benchmark, by = "mese_reso") |> left_join(fattori, by = "mese_reso")
}

# Reconstruction of the cap-weighted benchmark (cap at the end of t-1), to
# validate it against the official index: correlation, annual gap, tracking error.
valida_benchmark_ricostruito <- function(pannello, benchmark, data_backtest) {
  cap <- pannello |> filter(in_indice, !is.na(mktcap)) |>
    transmute(sec_id, pt = periodo %m+% months(1), cap = mktcap)
  ric <- pannello |> filter(!is.na(r)) |> inner_join(cap, by = c("sec_id", "periodo" = "pt")) |>
    group_by(periodo) |> summarise(r_bmk = sum(cap * r) / sum(cap), n = n(), .groups = "drop") |>
    rename(mese_reso = periodo)
  v <- ric |> inner_join(benchmark, by = "mese_reso") |>
    filter(!is.na(r_bench), mese_reso >= as.Date(data_backtest))
  n <- nrow(v); ann <- function(x) prod(1 + x)^(12 / n) - 1
  data.frame(mesi = n, correlazione = cor(v$r_bmk, v$r_bench), ann_ricostruito = ann(v$r_bmk),
             ann_ufficiale = ann(v$r_bench), scarto = ann(v$r_bmk) - ann(v$r_bench),
             tracking_error = sd(v$r_bmk - v$r_bench) * sqrt(12))
}

# =============================================================================
#  2. PERFORMANCE METRICS
# =============================================================================
# Sharpe: mean(r - rf) / sd(r - rf) x sqrt(12). Active return both arithmetic
# (the one in the IR) and geometric (the realised one). Turnover = half of the
# traded amount.
metriche <- function(r, r_bench = NULL, turnover = NULL, rf = NULL) {
  r <- as.numeric(r); ok <- !is.na(r); if (sum(ok) < 3) return(NULL)
  rr <- r[ok]; n <- length(rr); cum <- cumprod(1 + rr)
  rfm <- if (is.null(rf)) rep(0, n) else { x <- as.numeric(rf)[ok]; x[is.na(x)] <- 0; x }
  ecc <- rr - rfm
  out <- data.frame(n_mesi = n, rend_annuo = prod(1 + rr)^(12 / n) - 1, volatilita = sd(rr) * sqrt(12),
                    rf_annuo = prod(1 + rfm)^(12 / n) - 1, sharpe = mean(ecc) / sd(ecc) * sqrt(12),
                    max_dd = min(cum / cummax(cum) - 1))
  if (!is.null(turnover)) out$turnover_medio <- mean(as.numeric(turnover)[ok], na.rm = TRUE)
  if (!is.null(r_bench)) {
    rb <- as.numeric(r_bench); o2 <- !is.na(r) & !is.na(rb)
    if (sum(o2) > 3) {
      att <- r[o2] - rb[o2]; n2 <- sum(o2); te <- sd(att) * sqrt(12)
      out$active_aritm <- mean(att) * 12
      out$active_geom  <- prod(1 + r[o2])^(12 / n2) - prod(1 + rb[o2])^(12 / n2)
      out$tracking_err <- te; out$info_ratio <- mean(att) * 12 / te
    }
  }
  out
}

# Metrics of all strategies, gross and net, plus the benchmark.
tabella_metriche <- function(sb, nomi, versioni = c("lordo", "netto")) {
  righe <- list()
  for (nm in nomi) for (v in versioni)
    righe[[length(righe) + 1]] <- cbind(strategia = paste(nm, v),
      metriche(sb[[paste0(nm, "_", v)]], sb$r_bench, sb[[paste0(nm, "_tn")]], sb$rf))
  bind_rows(righe, cbind(strategia = "Benchmark", metriche(sb$r_bench, NULL, NULL, sb$rf)))
}

# =============================================================================
#  3. NEWEY-WEST REGRESSIONS
# =============================================================================
regressione_nw <- function(y, X = NULL, lag = NULL, nomi = NULL) {
  y <- as.numeric(y)
  Xm <- if (is.null(X)) matrix(1, length(y), 1) else cbind(1, as.matrix(X))
  colnames(Xm) <- c("alpha", if (!is.null(X)) { if (is.null(nomi)) paste0("b", seq_len(ncol(Xm) - 1)) else nomi })
  ok <- stats::complete.cases(cbind(y, Xm)); y <- y[ok]; Xm <- Xm[ok, , drop = FALSE]
  n <- length(y); k <- ncol(Xm); if (n <= k + 2) return(NULL)
  XtX <- solve(crossprod(Xm)); beta <- XtX %*% crossprod(Xm, y); res <- as.numeric(y - Xm %*% beta)
  if (is.null(lag)) lag <- floor(4 * (n / 100)^(2 / 9))
  u <- Xm * res; S <- crossprod(u) / n
  if (lag > 0) for (l in seq_len(lag)) {
    G <- crossprod(u[(l + 1):n, , drop = FALSE], u[1:(n - l), , drop = FALSE]) / n
    S <- S + (1 - l / (lag + 1)) * (G + t(G))           # Bartlett kernel
  }
  se <- sqrt(diag(XtX %*% (n * S) %*% XtX)); tt <- as.numeric(beta) / se
  data.frame(termine = colnames(Xm), stima = as.numeric(beta), se_nw = se, t = tt,
             p = 2 * stats::pt(-abs(tt), df = n - k), lag_nw = lag, n_oss = n,
             r2 = 1 - sum(res^2) / sum((y - mean(y))^2), stringsAsFactors = FALSE)
}

stelle <- function(p) ifelse(is.na(p), "", ifelse(p < .01, "***", ifelse(p < .05, "**", ifelse(p < .10, "*", ""))))

# Estimates and errors annualised with the SAME factor (x12) for alpha only;
# betas and factor coefficients are not annualised; 95% interval.
annualizza <- function(d) d |> mutate(
  fattore = ifelse(termine == "alpha", 12, 1),
  stima_a = stima * fattore, se_a = se_nw * fattore,
  ic_basso = stima_a - 1.96 * se_a, ic_alto = stima_a + 1.96 * se_a, sig = stelle(p))

# (a) mean active return, (b) alpha and beta of the market model.
test_vs_benchmark <- function(sb, nomi, versione = "netto") {
  bind_rows(lapply(nomi, function(nm) {
    r <- sb[[paste0(nm, "_", versione)]]
    t1 <- regressione_nw(r - sb$r_bench)
    t2 <- regressione_nw(r - sb$rf, X = sb$r_bench - sb$rf, nomi = "beta")
    bind_rows(cbind(strategia = paste(nm, versione), test = "Active return medio", t1[t1$termine == "alpha", ]),
              cbind(strategia = paste(nm, versione), test = "Alpha (mercato)",   t2[t2$termine == "alpha", ]),
              cbind(strategia = paste(nm, versione), test = "Beta di mercato",   t2[t2$termine == "beta", ]))
  })) |> annualizza()
}

MODELLI_FATTORIALI <- list(
  "Mercato"           = c("mkt_rf"),
  "Carhart 4 fattori" = c("mkt_rf", "smb", "hml", "mom"),
  "FF5 + momentum"    = c("mkt_rf", "smb", "hml", "rmw", "cma", "mom"))
NOMI_FATTORI <- c(mkt_rf = "mkt", smb = "smb", hml = "hml", mom = "mom", rmw = "rmw", cma = "cma")

# Factor attribution of a NAMED LIST of return series `ys` (already in excess of
# the right reference: rf for levels, the benchmark for relative attribution).
# The study uses it on the difference strategy - benchmark: the French factors
# are in USD and cover a wider region than the euro-area index, so absolute
# alphas would be artefacts.
fattoriale <- function(ys, modelli, sb) {
  out <- list()
  for (nm in names(ys)) for (mo in names(modelli)) {
    f <- modelli[[mo]]
    reg <- regressione_nw(ys[[nm]], X = sb[, f, drop = FALSE], nomi = unname(NOMI_FATTORI[f]))
    if (!is.null(reg)) out[[length(out) + 1]] <- cbind(serie = nm, modello = mo, reg)
  }
  bind_rows(out) |> annualizza()
}

# Monthly differences between pairs of strategies (same selection: isolates the
# weighting; optimised vs simple: isolates the construction).
confronti_coppie <- function(sb, coppie, versione = "netto") {
  bind_rows(lapply(coppie, function(cp) {
    reg <- regressione_nw(sb[[paste0(cp[1], "_", versione)]] - sb[[paste0(cp[2], "_", versione)]])
    cbind(confronto = paste(cp[1], "-", cp[2]), versione = versione, reg[reg$termine == "alpha", ])
  })) |> annualizza()
}

# =============================================================================
#  4. CIRCULAR BLOCK BOOTSTRAP TESTS
# =============================================================================
# Resampling indices: blocks of length `blocco` drawn with replacement on the
# circle of n observations, truncated to n. Preserves serial dependence and the
# cross-correlation of the series (the same indices are applied to every series).
indici_blocchi <- function(n, blocco) {
  nb <- ceiling(n / blocco); st <- sample.int(n, nb, replace = TRUE)
  ((rep(st, each = blocco) + rep(0:(blocco - 1), nb) - 1) %% n + 1)[seq_len(n)]
}

sharpe_ecc <- function(r, rf) { e <- r - rf; mean(e) / sd(e) * sqrt(12) }

# Difference between two Sharpe ratios (excess over rf).
boot_sharpe_diff <- function(a, b, rf, B = 5000, blocco = 6) {
  n <- length(a)
  d0 <- sharpe_ecc(a, rf) - sharpe_ecc(b, rf)
  dstar <- replicate(B, {
    idx <- indici_blocchi(n, blocco)
    sharpe_ecc(a[idx], rf[idx]) - sharpe_ecc(b[idx], rf[idx])
  })
  se <- sd(dstar)
  c(diff = d0, se = se, p = mean(abs(dstar - d0) >= abs(d0)), lo = d0 - 1.96 * se, hi = d0 + 1.96 * se)
}

# Ratio of two annual volatilities (< 1 = lower volatility), on the log scale.
boot_rapporto_vol <- function(a, b, B = 5000, blocco = 6) {
  n <- length(a)
  d0 <- log(sd(a) / sd(b))
  dstar <- replicate(B, {
    idx <- indici_blocchi(n, blocco)
    log(sd(a[idx]) / sd(b[idx]))
  })
  se <- sd(dstar)
  c(rapporto = exp(d0), se_log = se, p = mean(abs(dstar - d0) >= abs(d0)),
    lo = exp(d0 - 1.96 * se), hi = exp(d0 + 1.96 * se))
}

# Net return series by name; "Bench" is the benchmark.
serie_netta <- function(sb, nm) if (nm == "Bench") sb$r_bench else sb[[paste0(nm, "_netto")]]

# One family of tests (Holm adjustment within the family).
# tipo = "sharpe" or "vol"; cop = list of c(name_1, name_2).
famiglia_bootstrap <- function(sb, cop, famiglia, tipo = c("sharpe", "vol")) {
  tipo <- match.arg(tipo)
  d <- bind_rows(lapply(cop, function(cp) {
    a <- serie_netta(sb, cp[1]); b <- serie_netta(sb, cp[2])
    if (tipo == "sharpe") data.frame(famiglia = famiglia, confronto = paste(cp[1], "-", cp[2]),
                                     t(boot_sharpe_diff(a, b, sb$rf)))
    else data.frame(famiglia = famiglia, confronto = paste(cp[1], "/", cp[2]),
                    t(boot_rapporto_vol(a, b)))
  }))
  d$p_holm <- p.adjust(d$p, method = "holm"); d
}

# =============================================================================
#  5. ROBUSTNESS AND SUBPERIODS
# =============================================================================
riassumi <- function(serie, etichetta, benchmark, fattori) {
  s <- serie |> left_join(benchmark, by = "mese_reso") |> left_join(fattori |> select(mese_reso, rf), by = "mese_reso")
  m <- metriche(s$r_netto, s$r_bench, s$turnover, s$rf); if (is.null(m)) NULL else cbind(variante = etichetta, m)
}

# varianti: named list of zero-argument functions returning a backtest output,
# so that any parameter can be varied.
robustezza <- function(varianti, benchmark, fattori) {
  bind_rows(lapply(names(varianti), function(nm) {
    cat("Variant", nm, "...\n"); riassumi(varianti[[nm]](), nm, benchmark, fattori)
  }))
}

sottoperiodi <- function(sb, nomi, periodi, versione = "netto") {
  bind_rows(lapply(names(periodi), function(pn) {
    d <- as.Date(periodi[[pn]]); s <- sb |> filter(mese_reso >= d[1], mese_reso <= d[2])
    if (nrow(s) < 6) return(NULL)
    bind_rows(lapply(nomi, function(nm) cbind(periodo = pn, strategia = paste(nm, versione),
                metriche(s[[paste0(nm, "_", versione)]], s$r_bench, s[[paste0(nm, "_tn")]], s$rf))),
              cbind(periodo = pn, strategia = "Benchmark", metriche(s$r_bench, NULL, NULL, s$rf)))
  }))
}
