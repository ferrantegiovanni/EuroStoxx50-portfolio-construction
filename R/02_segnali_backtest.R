# =============================================================================
#  MODULE 2 - ELIGIBILITY SIGNAL, TIMELINE, DELISTING, BACKTEST ENGINE
#  A SIGNAL is a function f(panel) returning sec_id, periodo, segnale, using
#  only data up to the end of the period. A CONSTRUCTOR maps the candidates at
#  a formation date to target weights; the engine is the same for every strategy.
# =============================================================================
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(lubridate) })

# =============================================================================
#  1. ELIGIBILITY SIGNAL
# =============================================================================
# A security is a candidate at date t if its return is non-missing in each of
# the last `finestra` calendar months (signal = 1, otherwise NA). The window is
# anchored to the calendar: a single gap makes the security ineligible until
# `finestra` complete months have accumulated again.
segnale_storia_completa <- function(finestra = 60) {
  function(df) {
    base <- df |> select(sec_id, periodo, r) |> distinct(sec_id, periodo, .keep_all = TRUE)
    cal <- seq(min(base$periodo), max(base$periodo), by = "month")
    griglia <- expand_grid(sec_id = unique(base$sec_id), periodo = cal) |>
      left_join(base, by = c("sec_id", "periodo")) |>
      arrange(sec_id, periodo) |> group_by(sec_id) |>
      mutate(ok = as.integer(!is.na(r)), cum = cumsum(ok),
             n_fin = cum - lag(cum, finestra, default = 0L),
             completi = row_number() >= finestra & n_fin == finestra) |>
      ungroup()
    base |> select(sec_id, periodo) |>
      left_join(griglia |> select(sec_id, periodo, completi), by = c("sec_id", "periodo")) |>
      transmute(sec_id, periodo, segnale = ifelse(completi %in% TRUE, 1, NA_real_))
  }
}

# =============================================================================
#  2. TIMELINE
# =============================================================================
# Information up to the END of month t -> formation at the end of t -> return in
# t+1. r_next is joined on the calendar (never lead()); eligibility is
# evaluated at the formation date, not at download time.
prepara_timeline <- function(pannello, f_segnale, data_backtest, data_fine) {
  iv_key <- attr(pannello, "iv_key")
  bt <- pannello |> left_join(f_segnale(pannello), by = c("sec_id", "periodo")) |>
    mutate(data_formazione = ceiling_date(periodo, "month") - days(1),
           ritardo_giorni  = as.integer(data_formazione - ultima_oss))
  avanti <- pannello |> transmute(sec_id, pp = periodo %m-% months(1), r_next = r)
  bt <- bt |> left_join(avanti, by = c("sec_id", "periodo" = "pp"))
  bt <- marca_membership(bt, iv_key, data_fine, col_data = "data_formazione") |>
    rename(eleggibile = dentro) |>
    mutate(nel_backtest = data_formazione >= as.Date(data_backtest))
  cat("Eligible rows in the backtest:", sum(bt$eleggibile & bt$nel_backtest), "\n")
  attr(bt, "iv_key") <- iv_key
  bt
}

# Recomputes the signal on a panel TRUNCATED at the end of t: the value at t
# must not change. Valid for any f_segnale.
test_look_ahead <- function(pannello, f_segnale, n_prove = 20, seme = 1, toll = 1e-10) {
  validi <- f_segnale(pannello) |> filter(!is.na(segnale))
  if (!nrow(validi)) { cat("No signal to test.\n"); return(invisible(NA)) }
  set.seed(seme); prove <- validi[sample(nrow(validi), min(n_prove, nrow(validi))), ]
  disc <- 0L
  for (i in seq_len(nrow(prove))) {
    r <- f_segnale(pannello |> filter(periodo <= prove$periodo[i])) |>
      filter(sec_id == prove$sec_id[i], periodo == prove$periodo[i])
    if (!nrow(r) || is.na(r$segnale[1]) || abs(r$segnale[1] - prove$segnale[i]) > toll) disc <- disc + 1L
  }
  cat("Look-ahead test:", nrow(prove) - disc, "/", nrow(prove), "match.",
      if (disc == 0) "No future information is used.\n" else "!! THE SIGNAL LOOKS INTO THE FUTURE.\n")
  invisible(disc)
}

# =============================================================================
#  3. MISSING RETURNS AT t+1 AND DELISTING
# =============================================================================
# Classifies missing next-month returns as: end-of-sample censoring /
# internal gap / permanent exit.
# solo_eleggibili = TRUE  -> diagnostics (which exits touch the universe);
# solo_eleggibili = FALSE -> all rows (used by applica_delisting).
classifica_mancanti <- function(bt, solo_eleggibili = TRUE, stampa = TRUE) {
  fine <- max(bt$periodo)
  ultimo <- bt |> filter(!is.na(r)) |> group_by(sec_id) |>
    summarise(ultimo_con_r = max(periodo), .groups = "drop")
  casi <- bt |> filter(is.na(r_next), if (solo_eleggibili) eleggibile & nel_backtest else TRUE) |>
    left_join(ultimo, by = "sec_id") |>
    mutate(tipo = case_when(periodo >= fine %m-% months(1) ~ "censura fine campione",
                            !is.na(ultimo_con_r) & ultimo_con_r > periodo ~ "buco interno",
                            TRUE ~ "uscita definitiva"))
  if (stampa) print(as.data.frame(casi |> count(tipo, name = "casi")), row.names = FALSE)
  casi
}

# regola: "esclusione" | "conservativa" (r_cons at the last price) |
#         "effettiva" (documented return where available, conservative elsewhere)
# `effettivi` is a data frame (sec_id, periodo, r_term) with documented terminal
# returns. The rule applies to ALL permanent exits, eligible or not.
applica_delisting <- function(bt, regola = "effettiva", effettivi = NULL, r_cons = 0) {
  usc <- classifica_mancanti(bt, solo_eleggibili = FALSE, stampa = FALSE) |>
    filter(tipo == "uscita definitiva") |> distinct(sec_id, periodo) |> mutate(uscita_definitiva = TRUE)
  bt <- bt |> select(-any_of("uscita_definitiva")) |>
    left_join(usc, by = c("sec_id", "periodo")) |>
    mutate(uscita_definitiva = coalesce(uscita_definitiva, FALSE))
  if (regola == "esclusione") return(bt)
  d <- bt |> mutate(r_term = NA_real_)
  if (regola == "effettiva" && !is.null(effettivi) && nrow(effettivi))
    d <- d |> left_join(effettivi |> rename(r_e = r_term), by = c("sec_id", "periodo")) |>
      mutate(r_term = coalesce(r_term, r_e)) |> select(-r_e)
  d |> mutate(r_term = ifelse(uscita_definitiva & is.na(r_term), r_cons, r_term),
              r_next = ifelse(is.na(r_next) & !is.na(r_term), r_term, r_next)) |> select(-r_term)
}

# =============================================================================
#  4. WEIGHTING RULES AND CONSTRUCTORS
# =============================================================================
# Weights: (selection, ctx) -> weights summing to 1.
pesi_ew <- function() function(sel, ctx = list()) rep(1 / nrow(sel), nrow(sel))
pesi_vw <- function() function(sel, ctx = list()) {
  m <- ifelse(is.na(sel$mktcap), 0, sel$mktcap)        # missing cap: weight zero, renormalised
  if (sum(m) > 0) m / sum(m) else rep(1 / nrow(sel), nrow(sel))
}
sel_tutti <- function() function(cand, ctx = list()) cand

# CONSTRUCTOR = the construction rule of a strategy: (cand, ctx) -> named vector
# of target weights (by sec_id), or NULL if the portfolio cannot be built at
# that date. It may attach attr(w, "diag"): a one-row data frame of diagnostics
# that the engine reports in its output.
costruttore_selezione <- function(selettore, pesatore) function(cand, ctx = list()) {
  sel <- selettore(cand, ctx); if (!nrow(sel)) return(NULL)
  w <- pesatore(sel, ctx); names(w) <- sel$sec_id; w
}

# =============================================================================
#  5. IMPLEMENTATION AND BACKTEST ENGINE
# =============================================================================
# frequenza : months between two rebalancings (1 monthly, 3 quarterly, ...)
# holding   : months each formed portfolio is held; multiple of frequenza.
#             holding > frequenza = overlapping portfolios (Jegadeesh-Titman):
#             holding/frequenza sleeves, one renewed at each rebalancing.
# ritardo   : months between the information date (formation) and execution
# cassa     : cash from securities that exit between two rebalancings:
#             "reinvesti" pro rata in the sleeve, "cassa" at zero return
# The study uses the base case (monthly, no lag); lag and quarterly frequency
# appear in the robustness checks.
parametri_impl <- function(frequenza = 1, holding = frequenza, ritardo = 0,
                           cassa = c("reinvesti", "cassa")) {
  cassa <- match.arg(cassa)
  stopifnot(frequenza >= 1, holding >= frequenza, holding %% frequenza == 0, ritardo >= 0)
  list(frequenza = as.integer(frequenza), holding = as.integer(holding),
       ritardo = as.integer(ritardo), cassa = cassa)
}

aggrega_maniche <- function(m) {
  v <- unlist(unname(m)); if (!length(v)) return(numeric(0))
  s <- tapply(v, names(v), sum); setNames(as.numeric(s), names(s))
}
distanza_l1 <- function(a, b) {
  n <- union(names(a), names(b)); x <- setNames(rep(0, length(n)), n); y <- x
  x[names(a)] <- a; y[names(b)] <- b; sum(abs(x - y))
}

# Single engine for ANY strategy. Positions are held in value, per sleeve.
# At each month-end d, in this order:
#  (a) securities with no row at d have exited: their value becomes cash;
#  (b) in months without rebalancing, cash is reinvested (or stays cash);
#  (c) if d is an execution date: universe and weights come from the formation
#      date f = d - ritardo; securities no longer listed at d cannot be traded
#      and the weights are renormalised; one sleeve is renewed; the traded
#      amount is measured on the whole portfolio (drifted positions first, new
#      ones after);
#  (d) the returns of month d+1 are applied (r_next of the row at d).
# The cost (traded amount x cost per side) is subtracted from the return of the
# following month. A held security with no return that resumes later (internal
# gap) stops the execution: it must be examined.
esegui_strategia <- function(bt, costruttore, impl = parametri_impl(), costo_unitario = 0.0010) {
  fine   <- suppressWarnings(max(bt$periodo[!is.na(bt$r_next)]))
  inizio <- min(bt$periodo[bt$nel_backtest])
  per    <- split(bt, bt$periodo)
  ok_r   <- !is.na(bt$r)
  ultimo_r <- tapply(as.numeric(bt$periodo[ok_r]), bt$sec_id[ok_r], max)
  formazioni <- seq(inizio, fine, by = paste(impl$frequenza, "months"))
  esecuzioni <- formazioni %m+% months(impl$ritardo); esecuzioni <- esecuzioni[esecuzioni <= fine]
  if (!length(esecuzioni)) return(data.frame())
  n_man <- impl$holding %/% impl$frequenza
  maniche <- replicate(n_man, numeric(0), simplify = FALSE)
  mesi <- sort(as.Date(names(per))); mesi <- mesi[mesi >= esecuzioni[1] & mesi <= fine]
  cassa_di <- function(h) if (".CASSA" %in% names(h)) h[[".CASSA"]] else 0
  k <- 0L; avviato <- FALSE; righe <- vector("list", length(mesi))

  for (ii in seq_along(mesi)) {
    d <- mesi[ii]; td <- per[[as.character(d)]]; presenti <- td$sec_id
    # (a) exits -> cash
    for (j in seq_len(n_man)) {
      h <- maniche[[j]]; out <- setdiff(names(h), c(".CASSA", presenti))
      if (length(out)) { c0 <- cassa_di(h) + sum(h[out]); h <- h[!names(h) %in% out]; h[".CASSA"] <- c0 }
      maniche[[j]] <- h
    }
    esegui <- d %in% esecuzioni
    # (b) reinvestment in months without rebalancing
    if (!esegui && impl$cassa == "reinvesti") for (j in seq_len(n_man)) {
      h <- maniche[[j]]; c0 <- cassa_di(h); tit <- setdiff(names(h), ".CASSA")
      if (c0 > 0 && length(tit) && sum(h[tit]) > 0) {
        h[tit] <- h[tit] + c0 * h[tit] / sum(h[tit]); maniche[[j]] <- h[tit]
      }
    }
    # (c) execution
    sc <- 0; f <- as.Date(NA); diag <- NULL; n_univ <- NA_integer_; non_es <- NA_integer_; saltato <- FALSE
    if (esegui) {
      f <- d %m-% months(impl$ritardo); tf <- per[[as.character(f)]]
      cand <- if (is.null(tf)) tf else tf[tf$eleggibile & !is.na(tf$segnale), , drop = FALSE]
      w <- if (!is.null(cand) && nrow(cand)) costruttore(cand, list(t = f, dati = bt)) else NULL
      if (is.null(w) || !length(w)) saltato <- TRUE else {
        diag <- attr(w, "diag"); n_univ <- nrow(cand)
        es <- names(w) %in% presenti; non_es <- sum(!es); w <- w[es]; w <- w / sum(w)
        rn <- setNames(td$r_next, td$sec_id)[names(w)]
        if (any(is.na(rn)))
          stop("Missing next-month return for ", sum(is.na(rn)),
               " securities bought at ", format(d), ": case to be examined, not to be absorbed.")
        prima <- aggrega_maniche(maniche)
        V <- if (avviato) sum(prima) else 1
        if (!avviato) { maniche <- replicate(n_man, w / n_man, simplify = FALSE); avviato <- TRUE }
        else { j <- (k %% n_man) + 1L; maniche[[j]] <- w * sum(maniche[[j]]) }
        sc <- distanza_l1(aggrega_maniche(maniche), prima) / V
        k <- k + 1L
      }
    }
    if (!avviato) next
    # (d) returns of month d+1
    rn_all <- setNames(td$r_next, td$sec_id); V0 <- sum(aggrega_maniche(maniche))
    for (j in seq_len(n_man)) {
      h <- maniche[[j]]; tit <- setdiff(names(h), ".CASSA"); if (!length(tit)) next
      r <- rn_all[tit]
      for (m in tit[is.na(r)]) {
        if (!is.na(ultimo_r[m]) && ultimo_r[m] > as.numeric(d))
          stop("Internal gap in the series of ", m, " held at ", format(d), ": case to be examined.")
        r[m] <- 0                                         # exit without terminal return: last price
      }
      h[tit] <- h[tit] * (1 + r); maniche[[j]] <- h
    }
    tot <- aggrega_maniche(maniche); rl <- sum(tot) / V0 - 1
    riga <- data.frame(formazione = f, mese_reso = d %m+% months(1), ribilancio = esegui && !saltato,
                       saltato = saltato, n_universo = n_univ,
                       n_titoli = sum(tot[names(tot) != ".CASSA"] > 1e-6 * sum(tot)),
                       r_lordo = rl, costo = sc * costo_unitario, r_netto = rl - sc * costo_unitario,
                       turnover = sc / 2, non_eseguibili = non_es)
    righe[[ii]] <- if (is.null(diag)) riga else cbind(riga, diag)
  }
  bind_rows(righe)
}
