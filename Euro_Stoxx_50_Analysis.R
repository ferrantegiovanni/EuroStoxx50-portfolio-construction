# =============================================================================
#  RISK-BASED PORTFOLIO CONSTRUCTION ON THE EURO STOXX 50 (2004-2026)
#
#  Seven long-only strategies (equal weight, cap-weighted, inverse volatility,
#  minimum variance, equal risk contribution, most diversified portfolio,
#  hierarchical risk parity) built from the point-in-time constituents of the
#  EURO STOXX 50 and tested against the EURO STOXX 50 Gross Return index.
#
#  HOW TO RUN
#    Open R in the project root (the folder that contains R/ and this file) and
#    run the script from top to bottom. Every data step saves its output in
#    dati/ and is skipped on later runs if the file exists; delete a file to
#    force its step to run again. LSEG Workspace (with LSEG_APP_KEY set in
#    ~/.Renviron) is needed only for the steps whose cache file is missing.
#
#  INPUTS (dati/, never published - LSEG licence)
#    SX5E Leavers & Joiners Constituents.xlsx, SX5E Current Constituents.xlsx
#  OUTPUTS
#    tabelle/   result tables (CSV + tables.xlsx)
#    grafici/, grafici_tutte/   figures (four / seven strategies)
#
#  LANGUAGE NOTE
#    Folder names and internal identifiers are in Italian (dati = data,
#    tabelle = tables, grafici = figures, periodo = month, pannello = panel);
#    comments, console messages, tables and figures are in English.
#
#  METHODOLOGICAL DECISIONS (R1-R8) are documented in the report (Appendix A).
# =============================================================================

# =============================================================================
#  0. SETUP
# =============================================================================
stopifnot("Run the script from the project root: folder R/ not found." = dir.exists("R"))

pacchetti <- c("dplyr", "tidyr", "lubridate", "ggplot2", "scales", "readxl", "writexl", "quadprog")
mancanti <- pacchetti[!vapply(pacchetti, requireNamespace, logical(1), quietly = TRUE)]
if (length(mancanti)) stop("Missing packages: ", paste(mancanti, collapse = ", "),
                           ". Install them with install.packages().")

for (d in c("dati", "tabelle", "grafici", "grafici_tutte")) dir.create(d, showWarnings = FALSE)
for (f in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) source(f, encoding = "UTF-8")
options(width = 200)

# ---- Study parameters --------------------------------------------------------
data_inizio   <- as.Date("1999-01-01")   # start of the series (5 years of warm-up)
data_backtest <- as.Date("2004-01-31")   # first formation date
data_fine     <- as.Date("2026-06-30")   # end of the sample
FINESTRA_STORIA <- 60                    # months of complete returns required for eligibility
COSTO_PER_LATO  <- 0.0010                # 10 bp per side on the traded amount
bench_inizio    <- "2003-12-31"          # benchmark level needed for the first return

# Opens the LSEG connection once, only if a step needs it.
connetti_se_serve <- local({ fatto <- FALSE
function() { if (!fatto) { connetti_lseg(); fatto <<- TRUE }; invisible(NULL) } })

# =============================================================================
#  1. POINT-IN-TIME INDEX MEMBERSHIP
# =============================================================================
f_eventi   <- "dati/SX5E Leavers & Joiners Constituents.xlsx"
f_correnti <- "dati/SX5E Current Constituents.xlsx"
stopifnot(file.exists(f_eventi), file.exists(f_correnti))

eventi   <- leggi_eventi_lseg(f_eventi, skip = 4)
correnti <- leggi_correnti_lseg(f_correnti)
cat("Events:", nrow(eventi), "|", format(min(eventi$date)), "->", format(max(eventi$date)),
    "| current constituents:", nrow(correnti), "\n")

# R2 - Frankfurt codes (.F) in the events file are aligned to Xetra (.DE): the
# same securities, listed under a different venue code (assumption checked on
# the price series in the crosswalk). Only these 11 codes are mapped.
mappa_codici <- data.frame(
  code_file  = c("ALVG.F", "BAYGn.F", "CECG.F", "DBKGn.F", "DTEGn.F", "EONGn.F",
                 "LHAG.F", "MBGn.F", "RWEG.F", "SIEGn.F", "VDFGn.F^H02 (expired)"),
  code_xetra = c("ALVG.DE", "BAYGn.DE", "CECG.DE", "DBKGn.DE", "DTEGn.DE", "EONGn.DE",
                 "LHAG.DE", "MBGn.DE", "RWEG.DE", "SIEGn.DE", "VDFGn.DE^H02 (expired)"),
  stringsAsFactors = FALSE)
write.csv(mappa_codici, "dati/mappa_codici_eventi.csv", row.names = FALSE)
eventi <- eventi |>
  mutate(code_originale = code,
         code = ifelse(code %in% mappa_codici$code_file,
                       mappa_codici$code_xetra[match(code, mappa_codici$code_file)], code))

# R3 - Volkswagen Pref leaves the index on 2026-09-21, an event missing from the
# LSEG report. Source: STOXX, Components Changes STOXX Blue Chip Indices,
# 01/09/2026 (deleted VOLKSWAGEN PREF, DE0007664039, effective at the open).
eventi <- bind_rows(eventi,
                    data.frame(status = "Leaver", issuer = "Volkswagen", code = "VOWG_p.DE",
                               date = as.Date("2026-09-21"), code_originale = "VOWG_p.DE"))

# Baseline = date of the first event (initial composition in the source).
baseline <- min(eventi$date)
mb <- ricostruisci_membership(eventi, correnti, baseline, Sys.Date())

cat("Anomalies:", nrow(mb$anomalie), "| months with other than 50 constituents:",
    sum(mb$conteggi$n_constituents != 50), "\n")
cat("Members at the first formation date:",
    sum(mb$iv$date_in <= data_backtest & (is.na(mb$iv$date_out) | mb$iv$date_out > data_backtest)), "\n")

write.csv(mb$iv,       "dati/membership_intervals.csv", row.names = FALSE, na = "")
write.csv(mb$anomalie, "dati/membership_anomalie.csv",  row.names = FALSE, na = "")
write.csv(mb$conteggi, "dati/membership_conteggi.csv",  row.names = FALSE)

# Intervals overlapping the sample, and the RICs to extract.
iv_p <- mb$iv |> filter(is.na(date_out) | date_out >= data_inizio, date_in <= data_fine)
rics <- sort(unique(trimws(sub(" \\(expired\\)", "", iv_p$code))))
cat("Intervals in the period:", nrow(iv_p), "| RICs to extract:", length(rics), "\n")

# =============================================================================
#  2. SECURITY IDENTIFIERS (CROSSWALK RIC -> SECURITY)
#  Key = organisation PermID + ShareClass (never the RIC, which can be
#  reassigned, nor the ISIN, which changes with the domicile). Groups with more
#  than one RIC per key are disambiguated with a price guard (1% threshold on
#  overlapping months) and the result is FROZEN in dati/crosswalk.csv.
# =============================================================================
f_crosswalk <- "dati/crosswalk.csv"
if (da_rigenerare(f_crosswalk)) {
  connetti_se_serve()
  cw <- estrai_a_lotti(rics, lseg_identificativi(), lotto = 50,
                       completo = function(d) !is.na(d$PermID)) |>
    distinct(RIC, .keep_all = TRUE) |>
    costruisci_chiave()
  write.csv(cw, "dati/crosswalk_grezzo.csv", row.names = FALSE, na = "")
  cat("RICs requested:", length(rics), "| returned:", nrow(cw),
      "| not recovered:", length(attr(cw, "non_recuperati")),
      "| missing PermID:", sum(is.na(cw$PermID)), "\n")
  
  f_px <- function(r) lseg_serie_mensili(format(data_inizio), format(data_fine), "TR.PriceClose")(r) |>
    transmute(RIC, Data, p = prezzo) |> filter(!is.na(p))
  dz <- disambigua_crosswalk(cw, f_px)
  print(dz$esiti, right = FALSE)
  
  # R4 - Unilever (UNc.AS^G19 and UNA.AS^K20) is ONE security although the
  # guard flags it: same issuer, continuous prices (median gap 0.3%, no jump on
  # 01/07/2019), ISIN change with a trading halt 1-5/07/2019. Volkswagen and
  # Telecom Italia are confirmed as distinct securities.
  stessa_decisa <- c("UNA.AS^K20", "UNc.AS^G19")
  cw_f <- dz$crosswalk |>
    mutate(sec_id       = ifelse(RIC %in% stessa_decisa, sec_id_base, sec_id),
           disambiguato = ifelse(RIC %in% stessa_decisa, FALSE, disambiguato))
  print(as.data.frame(cw_f |> filter(disambiguato) |> select(RIC, sec_id_base, sec_id)), right = FALSE)
  
  write.csv(cw_f |> mutate(data_estrazione = Sys.Date()), f_crosswalk, row.names = FALSE, na = "")
  write.csv(dz$disambiguazioni, "dati/crosswalk_disambiguazioni.csv", row.names = FALSE, na = "")
}
cw_f  <- leggi(f_crosswalk)
ponte <- cw_f |> come_testo(c("RIC", "PermID", "ShareClass", "ISIN", "Nome", "sec_id")) |>
  filter(!is.na(sec_id)) |> distinct(RIC, .keep_all = TRUE)
cat("Crosswalk:", nrow(ponte), "RICs |", n_distinct(ponte$sec_id), "securities\n")

# =============================================================================
#  3. MONTHLY MARKET DATA (total return, price, market cap)
# =============================================================================
f_prezzi <- "dati/prezzi_mensili.csv"
if (da_rigenerare(f_prezzi)) {
  connetti_se_serve()
  px <- estrai_a_lotti(unique(ponte$RIC),
                       lseg_serie_mensili(format(data_inizio), format(data_fine)),
                       lotto = 25,
                       completo = function(d) !is.na(d$Data) & !(is.na(d$ret) & is.na(d$prezzo)),
                       file_parziale = "dati/prezzi_parziali.csv")
  write.csv(px, f_prezzi, row.names = FALSE, na = "")
  diagnostica_copertura(px, iv_p, data_inizio)                 # series that start late / end early
  diagnostica_valute(lseg_valute(unique(ponte$RIC)))           # price and cap must be in EUR
}

# =============================================================================
#  4. CANONICAL PANEL (security x month)
# =============================================================================
iv      <- leggi("dati/membership_intervals.csv") |>
  mutate(date_in = as.Date(date_in), date_out = as.Date(date_out))
mensili <- leggi(f_prezzi) |> mutate(RIC = trimws(RIC), Data = as.Date(Data))

pannello <- costruisci_pannello(mensili, ponte, iv, data_fine)
diagnostica_capitalizzazione(pannello)
cat("Securities in the panel:", n_distinct(pannello$sec_id),
    "| range:", format(min(pannello$periodo)), "->", format(max(pannello$periodo)), "\n")

# =============================================================================
#  5. ELIGIBILITY AND TIMELINE
#  A security is a candidate at a formation date if it is an index member on that
#  date and has FINESTRA_STORIA complete months of returns (a single gap makes
#  it ineligible until the history is complete again).
# =============================================================================
segnale <- segnale_storia_completa(FINESTRA_STORIA)
stopifnot(test_look_ahead(pannello, segnale) == 0)
bt <- prepara_timeline(pannello, segnale, data_backtest, data_fine)

el <- bt |> filter(nel_backtest, eleggibile) |> group_by(periodo) |>
  summarise(membri = n(), candidati = sum(!is.na(segnale)), .groups = "drop")
cat("Formations:", nrow(el), "| candidates per formation: min", min(el$candidati),
    "| median", median(el$candidati), "| max", max(el$candidati), "\n")

# =============================================================================
#  6. MISSING RETURNS AT t+1 AND DELISTING
#  Missing next-month returns are end-of-sample censoring, internal gaps, or
#  permanent exits. Every permanent exit books a terminal return of 0% in its
#  last month (conservative rule).
# =============================================================================
casi <- classifica_mancanti(bt)
nomi <- ponte |> group_by(sec_id) |> summarise(Nome = first(Nome), .groups = "drop")
usc  <- casi |> filter(tipo == "uscita definitiva") |> left_join(nomi, by = "sec_id") |>
  arrange(periodo) |> transmute(Nome, RIC, ultimo_mese_con_rend = format(periodo), ultimo_prezzo = prezzo)
cat("\nPermanent exits among the candidates:", nrow(usc), "\n")
print(as.data.frame(usc), right = FALSE)
cat("Internal gaps among the candidates:", sum(casi$tipo == "buco interno"), "\n")
diagnostica_delisting(bt, casi)

# R6 - Sanpaolo IMI merged into Banca Intesa (exchange ratio 3.115 Intesa shares
# per Sanpaolo IMI share, Borsa Italiana). Terminal return set to 0%: a price-level
# comparison is unreliable (adjusted Intesa series, last Sanpaolo IMI price not
# verifiable). Same rule as the other permanent exits.
sec_spi <- ponte$sec_id[ponte$RIC == "SPI.MI^A07"]
eff <- data.frame(sec_id = sec_spi, periodo = as.Date("2006-12-01"), r_term = 0)

n_usc <- sum(classifica_mancanti(bt, solo_eleggibili = FALSE, stampa = FALSE)$tipo == "uscita definitiva")
bt_reg <- applica_delisting(bt, "effettiva", eff, r_cons = 0)
cat("Permanent exits in the panel:", n_usc, "| documented terminal return:", nrow(eff),
    "| conservative terminal return:", n_usc - nrow(eff), "\n")
cat("Residual missing returns among the candidates:\n")
invisible(classifica_mancanti(bt_reg))

# =============================================================================
#  7. STRATEGIES
#  Same engine and implementation for all seven: monthly rebalancing, no
#  execution lag, 10 bp per side. R5: in the cap-weighted portfolio a missing
#  market cap gets zero weight and the remaining weights are renormalised.
# =============================================================================
impl_base <- parametri_impl(frequenza = 1, holding = 1, ritardo = 0)

# Risk parameters: 60-month covariance, at least 20 securities with a complete
# window; Ledoit-Wolf shrinkage except for inverse volatility (only variances
# matter); 5% cap for minimum variance and most diversified.
p_rischio <- list(
  IV  = parametri_rischio(fin_cov = 60, min_titoli = 20, shrinkage = FALSE),
  GMV = parametri_rischio(fin_cov = 60, min_titoli = 20, peso_max = 0.05, shrinkage = TRUE),
  ERC = parametri_rischio(fin_cov = 60, min_titoli = 20, shrinkage = TRUE),
  MDP = parametri_rischio(fin_cov = 60, min_titoli = 20, peso_max = 0.05, shrinkage = TRUE),
  HRP = parametri_rischio(fin_cov = 60, min_titoli = 20, shrinkage = TRUE))

costruttori <- list(
  EW  = costruttore_selezione(sel_tutti(), pesi_ew()),
  VW  = costruttore_selezione(sel_tutti(), pesi_vw()),
  IV  = costruttore_rischio("iv",  p_rischio$IV),
  GMV = costruttore_rischio("gmv", p_rischio$GMV),
  ERC = costruttore_rischio("erc", p_rischio$ERC),
  MDP = costruttore_rischio("mdp", p_rischio$MDP),
  HRP = costruttore_rischio("hrp", p_rischio$HRP))

stopifnot("mktcap" %in% names(bt_reg))
risultati <- setNames(lapply(names(costruttori), function(nm) {
  res <- esegui_strategia(bt_reg, costruttori[[nm]], impl = impl_base, costo_unitario = COSTO_PER_LATO)
  riepilogo_strategia(res, nm)
  res
}), names(costruttori))
stopifnot(length(unique(lapply(risultati, function(x) x$mese_reso))) == 1)   # same months for all

cat("\nEx-ante diagnostics of the risk-based strategies (means over formations):\n")
cols_diag <- c("n_cov", "shrink", "vol_ex_ante", "peso_max_eff", "n_effettivo", "rapporto_div", "rc_max", "rc_min")
print(round(t(sapply(risultati[c("IV", "GMV", "ERC", "MDP", "HRP")],
                     function(x) colMeans(x[, cols_diag], na.rm = TRUE))), 4))
cat("Months with the 5% cap active (max weight >= 4.99%) | GMV:",
    sum(risultati$GMV$peso_max_eff >= 0.0499), "| MDP:", sum(risultati$MDP$peso_max_eff >= 0.0499),
    "of", nrow(risultati$GMV), "\n")

saveRDS(risultati, "dati/risultati_strategie_base.rds")   # LSEG-derived: stays local

# =============================================================================
#  8. BENCHMARK, FACTORS AND RISK-FREE RATE
#  R7 - Benchmark: .SX5GT (EURO STOXX 50 EUR Gross Return), consistent with the
#       gross total returns of the LSEG securities. A price index would credit
#       the strategies with the dividend yield as spurious alpha.
#  R8 - Risk-free rate: Euribor 1M (ECB), monthly average / 100 / 12, so that
#       the Sharpe ratio and the factor models use a euro rate. The US T-bill of
#       the French files stays in rf_french.
#       Factors: Fama-French Europe (USD) plus momentum. They cover a wider
#       region and are in dollars, so attribution is run RELATIVE to the benchmark.
# =============================================================================
f_bench <- "dati/benchmark_SX5GT_grezzo.csv"
if (!file.exists(f_bench)) connetti_se_serve()
bench <- carica_benchmark_lseg(".SX5GT", bench_inizio, format(data_fine), f_bench) |>
  filter(!is.na(r_bench)) |> arrange(mese_reso)

fatt    <- carica_fattori("dati/fattori_french_europe.csv", regione = "Europe")
ecb     <- carica_euribor("dati/euribor_1m_ecb.csv")
fatt_eu <- aggiungi_rf_euro(fatt, ecb)

mesi <- risultati$EW$mese_reso
cat("Strategy months:", length(mesi), "| missing in benchmark:", sum(!mesi %in% bench$mese_reso),
    "| in factors:", sum(!mesi %in% fatt_eu$mese_reso), "| NA in rf:",
    sum(is.na(fatt_eu$rf[match(mesi, fatt_eu$mese_reso)])), "\n")
cat("Mean annualised rate | Euribor 1M:", round(12 * mean(fatt_eu$rf[match(mesi, fatt_eu$mese_reso)]), 4),
    "| US T-bill (French):", round(12 * mean(fatt_eu$rf_french[match(mesi, fatt_eu$mese_reso)]), 4), "\n")

# =============================================================================
#  9. EVALUATION
# =============================================================================
strategie <- risultati                       # EW, VW, IV, GMV, ERC, MDP, HRP
nomi_str  <- names(strategie)
sb <- unisci_serie(strategie, bench, fatt_eu)

# ---- 9a. Performance metrics (gross and net) ---------------------------------
tm <- tabella_metriche(sb, nomi_str)
tm_p <- tm[, intersect(c("strategia", "rend_annuo", "volatilita", "sharpe", "max_dd", "turnover_medio",
                         "active_geom", "tracking_err", "info_ratio"), names(tm))]
tm_p[, -1] <- round(tm_p[, -1], 3)
print(tm_p, row.names = FALSE)

# ---- 9b. Tests versus the benchmark (Newey-West, net of costs) ---------------
dg <- diagnostica_fattori(sb)                # the factor timing must be aligned
tb <- test_vs_benchmark(sb, nomi_str, versione = "netto")

# ---- 9c. Factor attribution RELATIVE to the benchmark ------------------------
# "Benchmark" row: excess return of .SX5GT on the factors (the "background"
# alpha of the mismatch). "X - Bench" rows: net strategy minus benchmark on the
# factors: relative alpha and style tilts versus the index.
ys <- c(list("Benchmark" = sb$r_bench - sb$rf),
        setNames(lapply(nomi_str, function(nm) sb[[paste0(nm, "_netto")]] - sb$r_bench),
                 paste0(nomi_str, " - Bench")))
rel <- fattoriale(ys, MODELLI_FATTORIALI[c("Carhart 4 fattori", "FF5 + momentum")], sb)

# ---- 9d. Pairwise differences (Newey-West), Holm within each version ---------
coppie <- list(c("VW", "EW"), c("IV", "EW"), c("ERC", "EW"), c("GMV", "EW"),
               c("MDP", "EW"), c("HRP", "EW"), c("GMV", "VW"))
cp <- bind_rows(confronti_coppie(sb, coppie, "lordo"), confronti_coppie(sb, coppie, "netto")) |>
  group_by(versione) |> mutate(p_holm = p.adjust(p, method = "holm")) |> ungroup()

# ---- 9e. Bootstrap tests: Sharpe differences and volatility ratios -----------
# Circular block bootstrap (blocks of 6 months, 5000 replications), seeds fixed.
# Two families per test, Holm within each: (a) pairs / strategies against EW,
# (b) every strategy against the benchmark.
coppie_b <- lapply(nomi_str, function(nm) c(nm, "Bench"))
set.seed(2026)
sh <- bind_rows(famiglia_bootstrap(sb, coppie,   "(a) coppie",        "sharpe"),
                famiglia_bootstrap(sb, coppie_b, "(b) vs benchmark",  "sharpe"))
set.seed(2027)
cop_a <- lapply(setdiff(nomi_str, "EW"), function(nm) c(nm, "EW"))
vv <- bind_rows(famiglia_bootstrap(sb, cop_a,    "(a) vs EW",         "vol"),
                famiglia_bootstrap(sb, coppie_b, "(b) vs benchmark",  "vol"))

# ---- 9f. Subperiods (fixed ex ante) ------------------------------------------
periodi <- list("2004-2007 Pre-crisis"                = c("2004-01-01", "2007-12-31"),
                "2008-2009 Financial crisis"          = c("2008-01-01", "2009-12-31"),
                "2010-2019 Euro crisis and expansion" = c("2010-01-01", "2019-12-31"),
                "2020-2026 Post-COVID"                = c("2020-01-01", "2026-12-31"))
sp <- sottoperiodi(sb, nomi_str, periodi, versione = "netto")

# ---- 9g. Robustness: one variant at a time versus the GMV and HRP bases ------
# Verification, not selection: no variant replaces the base case. Cost variants
# are recomputed from gross returns and turnover (cost = 2 x turnover x c); the
# others rerun the engine.
pg <- function(fin = 60, cap = 0.05, shr = TRUE)
  parametri_rischio(fin_cov = fin, min_titoli = 20, peso_max = cap, shrinkage = shr)
run_gmv <- function(p, impl = impl_base) function()
  esegui_strategia(bt_reg, costruttore_rischio("gmv", p), impl = impl, costo_unitario = COSTO_PER_LATO)
run_hrp <- function(legame) function()
  esegui_strategia(bt_reg, costruttore_rischio("hrp", p_rischio$HRP, legame), impl = impl_base,
                   costo_unitario = COSTO_PER_LATO)
costo_var <- function(c) function() { d <- risultati$GMV; d$r_netto <- d$r_lordo - 2 * d$turnover * c; d }

varianti <- list(
  "GMV base"                = function() risultati$GMV,
  "GMV window 36m"          = run_gmv(pg(fin = 36)),
  "GMV sample covariance"   = run_gmv(pg(shr = FALSE)),
  "GMV cap 3%"              = run_gmv(pg(cap = 0.03)),
  "GMV cap 10%"             = run_gmv(pg(cap = 0.10)),
  "GMV no cap"              = run_gmv(pg(cap = 1)),
  "GMV quarterly"           = run_gmv(pg(), parametri_impl(frequenza = 3, holding = 3)),
  "GMV lag 1 month"         = run_gmv(pg(), parametri_impl(frequenza = 1, holding = 1, ritardo = 1)),
  "GMV costs 5 bp"          = costo_var(0.0005),
  "GMV costs 25 bp"         = costo_var(0.0025),
  "GMV costs 50 bp"         = costo_var(0.0050),
  "HRP single (base)"       = function() risultati$HRP,
  "HRP average linkage"     = run_hrp("average"),
  "HRP Ward linkage"        = run_hrp("ward.D2"))
gruppo_rob <- c("Base", "Estimation window", "Covariance estimator", "Weight cap", "Weight cap", "Weight cap",
                "Rebalancing frequency", "Execution lag", "Transaction costs", "Transaction costs",
                "Transaction costs", "HRP base", "HRP linkage", "HRP linkage")
stopifnot(length(gruppo_rob) == length(varianti))

rob  <- robustezza(varianti, bench, fatt_eu)
rob2 <- rob |> mutate(gruppo = gruppo_rob)

# ---- 9h. Final checks and benchmark reconstruction ---------------------------
cat("\n--- Checks on the strategies ---\n")
controlli_finali(pannello, strategie)
cat("\n--- Cap-weighted benchmark rebuilt from the panel vs .SX5GT ---\n")
vb <- valida_benchmark_ricostruito(pannello, bench, data_backtest)
vb[] <- lapply(vb, function(x) if (is.numeric(x)) round(x, 4) else x)
print(vb, row.names = FALSE)

# =============================================================================
#  10. TABLES AND FIGURES (English)
#  Internal labels stay in Italian; translation happens only here, via a glossary.
# =============================================================================
etichette <- c(EW = "Equal weight", VW = "Cap-weighted", IV = "Inverse volatility",
               GMV = "Minimum variance", ERC = "Equal risk contribution",
               MDP = "Most diversified", HRP = "Hierarchical risk parity")
etic2 <- c(etichette, Bench = "Benchmark")
gl <- crea_glossario(etichette, "Benchmark (EURO STOXX 50 Gross Return)",
                     extra = setNames(paste(etichette, "minus benchmark"), paste(names(etichette), "- Bench")))
rel_attr <- rel |> rename(strategia = serie)

tab <- costruisci_tabelle(gl, metr = tm, test = tb, confronti = cp, attr = rel_attr,
                          sottop = sp, rob = rob2, modello_attr = "Carhart 4 fattori")
names(tab)[names(tab) == "Factor attribution"] <- "Factor attribution (Carhart, relative)"
tab$`Factor attribution (FF5+MOM, relative)` <-
  costruisci_tabelle(gl, attr = rel_attr, modello_attr = "FF5 + momentum")$`Factor attribution`
tab$`Sharpe tests`     <- tabella_test_sharpe(sh, etic2)
tab$`Volatility tests` <- tabella_test_vol(vv, etic2)

scrivi_tabelle(tab, "tabelle", "tables.xlsx")
print(as.data.frame(tab$Performance), row.names = FALSE)

# Figures: four strategies in the time-series charts, all seven in the second set.
didascalia <- paste(strwrap(paste(
  "Own calculations on LSEG data. Benchmark: EURO STOXX 50 Gross Return. Monthly rebalancing, 10 bp per side,",
  "point-in-time index members with 60 months of history; 60-month Ledoit-Wolf covariance;",
  "5% weight cap for minimum variance."), width = 110), collapse = "\n")

costruisci_grafici(sb, etichette[c("EW", "VW", "GMV", "HRP")], "Benchmark", cartella = "grafici",
                   didascalia = didascalia, palette = PALETTE_SUB)
costruisci_grafici(sb, etichette, "Benchmark", cartella = "grafici_tutte",
                   didascalia = didascalia, palette = PALETTE_TUTTE)
cat("Done.\n")
