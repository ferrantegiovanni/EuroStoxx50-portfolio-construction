# =============================================================================
#  MODULE 6 - DIAGNOSTICS
#  Checks that established the methodological decisions: run them on every new
#  dataset BEFORE reading the results. Console output only; nothing here
#  changes the data or the results.
# =============================================================================
suppressPackageStartupMessages({ library(dplyr); library(lubridate) })

# Are the exits concentrated in the top tail of the signal? Binomial test on
# the DEFINITIVE exits only (including end-of-sample censoring would dilute it).
diagnostica_delisting <- function(bt, casi, quota = 0.20) {
  usc <- casi |> filter(tipo == "uscita definitiva", !is.na(segnale))
  if (!nrow(usc)) { cat("No permanent exit with a signal.\n"); return(invisible(NULL)) }
  rango <- bt |> filter(eleggibile, nel_backtest, !is.na(segnale)) |> group_by(periodo) |>
    mutate(pos = (rank(-segnale, ties.method = "average") - 1) / pmax(n() - 1, 1)) |> ungroup() |>
    select(sec_id, periodo, pos)
  u <- usc |> left_join(rango, by = c("sec_id", "periodo"))
  k <- sum(u$pos <= quota, na.rm = TRUE); n <- sum(!is.na(u$pos))
  bt1 <- binom.test(k, n, quota, alternative = "greater"); bt2 <- binom.test(k, n, quota)
  cat(sprintf("Permanent exits: %d | in the top quantile: %d (%.1f%% vs %.0f%% expected)\n", n, k, 100 * k / n, 100 * quota))
  cat(sprintf("one-sided p %.4f | two-sided p %.4f | 95%% CI [%.1f%%; %.1f%%] | median position %.3f\n",
              bt1$p.value, bt2$p.value, 100 * bt2$conf.int[1], 100 * bt2$conf.int[2], median(u$pos, na.rm = TRUE)))
  invisible(u |> arrange(pos))
}

# Market-cap coverage: counts the SHARE of missing cap, not the number of
# securities (five small names out of five hundred weigh little).
diagnostica_capitalizzazione <- function(pannello) {
  m <- pannello |> filter(in_indice) |> group_by(sec_id) |> arrange(periodo) |>
    mutate(cap_prec = lag(mktcap)) |> ungroup() |> group_by(periodo) |>
    summarise(n = n(), mancanti = sum(is.na(mktcap)),
              quota_mancante = sum(cap_prec[is.na(mktcap)], na.rm = TRUE) / sum(cap_prec, na.rm = TRUE),
              .groups = "drop")
  cat(sprintf("Cap coverage: %.1f%% | missing per month median %g max %g | share of cap missing, max %.3f%%\n",
              100 * mean(!is.na(pannello$mktcap)), median(m$mancanti), max(m$mancanti),
              100 * max(m$quota_mancante, na.rm = TRUE)))
  invisible(m)
}

# Currency check: price and market cap must both be in EUR. `val` comes from
# lseg_valute(); lists the RICs that are not.
diagnostica_valute <- function(val) {
  print(table(price = val$valuta_prezzo, cap = val$valuta_cap, useNA = "ifany"))
  cat("RICs with a currency other than EUR or missing:\n")
  print(val |> filter(is.na(valuta_prezzo) | is.na(valuta_cap) |
                        valuta_prezzo != "EUR" | valuta_cap != "EUR"), right = FALSE)
}

# Coverage of the price series against the index-membership periods: series
# that start late, end early, or are missing altogether.
diagnostica_copertura <- function(px, iv_p, data_inizio) {
  cop <- px |> filter(!is.na(Data), !(is.na(ret) & is.na(prezzo))) |> group_by(RIC) |>
    summarise(da = min(Data), a = max(Data), mesi = n(), cap_NA = sum(is.na(mktcap)), .groups = "drop")
  iv_ric <- iv_p |> mutate(RIC = trimws(sub(" \\(expired\\)", "", code))) |> group_by(RIC) |>
    summarise(prima_entrata = min(date_in),
              ultima_uscita = if (any(is.na(date_out))) as.Date(NA) else max(date_out), .groups = "drop")
  chk <- iv_ric |> left_join(cop, by = "RIC") |>
    mutate(senza_dati      = is.na(da),
           inizio_tardivo  = !is.na(da) & da > pmax(prima_entrata, as.Date(data_inizio)) %m+% months(1),
           fine_anticipata = !is.na(a) & !is.na(ultima_uscita) & a < ultima_uscita %m-% months(2))
  cat("No data:", sum(chk$senza_dati), "| late start:", sum(chk$inizio_tardivo),
      "| early end:", sum(chk$fine_anticipata), "\n")
  print(as.data.frame(chk |> filter(senza_dati | inizio_tardivo | fine_anticipata)), right = FALSE)
  invisible(chk)
}

# Time alignment of the factors: the benchmark excess return vs mkt_rf must
# correlate ~0.99 with no lag and collapse with +/-1 month.
diagnostica_fattori <- function(sb) {
  a <- sb |> filter(!is.na(r_bench), !is.na(mkt_rf), !is.na(rf)) |> transmute(e = r_bench - rf, mkt_rf)
  n <- nrow(a); c0 <- cor(a$e, a$mkt_rf)
  ca <- cor(a$e[-n], a$mkt_rf[-1]); ci <- cor(a$e[-1], a$mkt_rf[-n])
  cat(sprintf("Factor alignment: corr %.4f | forward %.4f | backward %.4f -> %s\n", c0, ca, ci,
              if (c0 > 0.9 && c0 > max(ca, ci) + 0.5) "CORRECT" else "!! TO BE CHECKED"))
  cat("Missing in the period:", paste(names(sb)[names(sb) %in% c("rf", "mkt_rf", "smb", "hml", "mom", "rmw", "cma")],
      sapply(sb[intersect(names(sb), c("rf", "mkt_rf", "smb", "hml", "mom", "rmw", "cma"))], function(x) sum(is.na(x))),
      sep = "=", collapse = " "), "\n")
  invisible(c(corrente = c0, avanti = ca, indietro = ci))
}

# One-line summary of a strategy: months, skipped months, holdings, turnover,
# annualised return (gross and net) and volatility.
riepilogo_strategia <- function(res, nome) {
  ann_ret <- function(r) prod(1 + r)^(12 / length(r)) - 1
  cat(sprintf("%-4s months %d | skipped %d | holdings min/median/max %d/%g/%d | turnover %.2f%% | return gross %.2f%% net %.2f%% | vol %.2f%%\n",
              nome, nrow(res), sum(res$saltato), min(res$n_titoli), median(res$n_titoli), max(res$n_titoli),
              100 * mean(res$turnover), 100 * ann_ret(res$r_lordo), 100 * ann_ret(res$r_netto),
              100 * sd(res$r_netto) * sqrt(12)))
}

# Summary checks on the dataset and on the strategies: same sample of months,
# effective rebalancings, skipped months, non-executable securities.
controlli_finali <- function(pannello, strategie) {
  amp <- pannello |> filter(in_indice) |> count(periodo)
  cat("Securities:", n_distinct(pannello$sec_id), "| panel rows:", nrow(pannello),
      "| members/month:", min(amp$n), "-", median(amp$n), "-", max(amp$n), "\n")
  for (nm in names(strategie)) {
    s <- strategie[[nm]]; rb <- s[s$ribilancio, ]
    cat(sprintf("%-8s months %d | %s -> %s | rebalancings %d | skipped %d | non-executable %d | holdings median %g | turnover per rebalancing %.1f%%\n",
                nm, nrow(s), format(min(s$mese_reso)), format(max(s$mese_reso)), nrow(rb), sum(s$saltato),
                sum(s$non_eseguibili, na.rm = TRUE), median(s$n_titoli), 100 * mean(rb$turnover)))
  }
  mesi <- vapply(strategie, function(s) paste(range(s$mese_reso), collapse = "/"), character(1))
  if (length(unique(mesi)) > 1) cat("!! The strategies do not cover the same sample of months: comparison not homogeneous.\n")
}
