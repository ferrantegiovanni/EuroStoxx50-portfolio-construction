# =============================================================================
#  MODULE 5 - TABLES AND FIGURES
#  Calculations use internal labels (Italian); translation happens ONLY here,
#  through a glossary: no upstream logic depends on the language.
#  Labels missing from the glossary stay visible, they do not disappear.
# =============================================================================
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(lubridate)
  library(ggplot2); library(scales); library(writexl) })

pct <- function(x, d = 2) ifelse(is.na(x), "", paste0(formatC(100 * x, format = "f", digits = d), "%"))
num <- function(x, d = 2) ifelse(is.na(x), "", formatC(x, format = "f", digits = d))

# =============================================================================
#  1. TABLES
# =============================================================================
# etichette: internal strategy name -> presented name.
# Example: c(EW = "Equal weight", GMV = "Minimum variance")
crea_glossario <- function(etichette, nome_benchmark = "Benchmark", extra = NULL) {
  g <- c("Benchmark" = nome_benchmark,
         "Active return medio" = "Mean active return", "Alpha (mercato)" = "Market-model alpha",
         "Beta di mercato" = "Market beta", "lordo" = "Gross of costs", "netto" = "Net of costs",
         "alpha" = "Alpha", "mkt" = "MKT-RF", "smb" = "SMB", "hml" = "HML", "mom" = "MOM",
         "rmw" = "RMW", "cma" = "CMA", "beta" = "Market beta")
  for (k in names(etichette)) {
    g[paste(k, "lordo")] <- paste0(etichette[[k]], ", gross")
    g[paste(k, "netto")] <- paste0(etichette[[k]], ", net")
  }
  for (a in names(etichette)) for (b in names(etichette))
    if (a != b) g[paste(a, "-", b)] <- paste(etichette[[a]], "minus", etichette[[b]])
  if (!is.null(extra)) g[names(extra)] <- extra
  g
}
traduci <- function(x, gl) { x <- as.character(x); ifelse(x %in% names(gl), unname(gl[x]), x) }

# Builds the standard tables. Every argument can be NULL.
costruisci_tabelle <- function(gl, metr = NULL, test = NULL, confronti = NULL, attr = NULL,
                               sottop = NULL, rob = NULL, modello_attr = "Carhart 4 fattori") {
  tab <- list()
  if (!is.null(metr)) tab$Performance <- metr |> transmute(
    Strategy = traduci(strategia, gl), Months = n_mesi,
    `Annualized return` = pct(rend_annuo), `Annualized volatility` = pct(volatilita),
    `Sharpe ratio` = num(sharpe), `Maximum drawdown` = pct(max_dd),
    `Average monthly turnover` = pct(turnover_medio),
    `Active return (arithmetic)` = pct(active_aritm), `Active return (geometric)` = pct(active_geom),
    `Tracking error` = pct(tracking_err), `Information ratio` = num(info_ratio))
  fmt_reg <- function(d, gruppo, item) d |> transmute(Group = gruppo, Item = item,
    Estimate = ifelse(termine == "alpha", pct(stima_a), num(stima_a, 3)),
    `Std. error (Newey-West)` = ifelse(termine == "alpha", pct(se_a), num(se_a, 3)),
    `t-stat` = num(t, 2), `p-value` = num(p, 3), Sig. = sig)
  sig <- list()
  if (!is.null(test)) sig[[1]] <- fmt_reg(test, "Versus benchmark",
                                          paste0(traduci(test$strategia, gl), ": ", traduci(test$test, gl)))
  if (!is.null(confronti)) sig[[2]] <- fmt_reg(confronti, "Pairwise differences",
                                               paste0(traduci(confronti$confronto, gl), " (", traduci(confronti$versione, gl), ")"))
  if (length(sig)) tab$Significance <- bind_rows(sig)
  if (!is.null(attr)) tab$`Factor attribution` <- attr |> filter(modello == modello_attr) |> transmute(
    Strategy = traduci(strategia, gl), Term = traduci(termine, gl),
    Estimate = ifelse(termine == "alpha", pct(stima_a), num(stima_a, 3)),
    `Std. error (Newey-West)` = ifelse(termine == "alpha", pct(se_a), num(se_a, 3)),
    `t-stat` = num(t, 2), `p-value` = num(p, 3), Sig. = sig, `R-squared` = num(r2, 3))
  if (!is.null(sottop)) tab$Subperiods <- sottop |> transmute(
    Subperiod = traduci(periodo, gl), Strategy = traduci(strategia, gl), Months = n_mesi,
    `Annualized return` = pct(rend_annuo), `Annualized volatility` = pct(volatilita),
    `Sharpe ratio` = num(sharpe), `Maximum drawdown` = pct(max_dd),
    `Active return (geometric)` = pct(active_geom))
  if (!is.null(rob)) tab$Robustness <- rob |> transmute(
    Group = if ("gruppo" %in% names(rob)) traduci(gruppo, gl) else "", Variant = traduci(variante, gl),
    `Annualized return` = pct(rend_annuo), `Annualized volatility` = pct(volatilita),
    `Sharpe ratio` = num(sharpe), `Active return (geometric)` = pct(active_geom),
    `Average monthly turnover` = pct(turnover_medio))
  tab
}

# Bootstrap tests (Sharpe differences, volatility ratios).
# etic: internal name -> presented name, including "Bench".
# Pair labels such as "GMV - EW" or "GMV / Bench" are translated through etic.
traduci_coppia <- function(x, etic, verbo)
  vapply(strsplit(x, " [-/] "), function(p) paste(etic[[p[1]]], verbo, etic[[p[2]]]), character(1))

ic_testo <- function(lo, hi, d = 3) paste0("[", num(lo, d), ", ", num(hi, d), "]")

tabella_test_sharpe <- function(sh, etic) sh |> transmute(
  Family = traduci(famiglia, c("(a) coppie" = "(a) pairs")),
  Comparison = traduci_coppia(confronto, etic, "minus"),
  `Sharpe difference` = num(diff, 3), `Bootstrap s.e.` = num(se, 3), `95% CI` = ic_testo(lo, hi),
  `p-value` = num(p, 3), `p-value (Holm)` = num(p_holm, 3))

tabella_test_vol <- function(vv, etic) vv |> transmute(
  Family = famiglia, Comparison = traduci_coppia(confronto, etic, "over"),
  `Volatility ratio` = num(rapporto, 3), `95% CI` = ic_testo(lo, hi), `p-value` = num(p, 3),
  `p-value (Holm)` = num(p_holm, 3))

scrivi_tabelle <- function(tab, cartella = "tabelle", file_xlsx = "tables.xlsx") {
  dir.create(cartella, showWarnings = FALSE)
  for (i in seq_along(tab)) write.csv(tab[[i]], file.path(cartella,
    sprintf("table_%d_%s.csv", i, gsub("[^a-z]+", "_", tolower(names(tab)[i])))), row.names = FALSE, na = "")
  write_xlsx(tab, file.path(cartella, file_xlsx))
  cat("Written", length(tab), "tables in", cartella, "\n")
}

# =============================================================================
#  2. FIGURES
# =============================================================================
tema_tesi <- function() theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(colour = "grey30", size = 9),
        plot.caption = element_text(colour = "grey45", size = 8, hjust = 0),
        legend.position = "top", legend.title = element_blank(),
        axis.title = element_text(size = 9, colour = "grey30"),
        plot.margin = margin(8, 26, 8, 8))

PALETTE_TUTTE <- c("#1F4E79", "#C55A11", "#2E7D32", "#6A3D9A", "#B8860B", "#008B8B", "#C2185B")
PALETTE_SUB   <- PALETTE_TUTTE[c(1, 2, 4, 7)]

# sb: wide table from unisci_serie; etichette as in crea_glossario.
# All series are net of costs; the benchmark is grey and always last.
# The initial purchase (first month) is excluded from the turnover chart.
costruisci_grafici <- function(sb, etichette, nome_benchmark, cartella = "grafici",
                               didascalia = NULL, finestra_mobile = 36, dpi = 300,
                               palette = PALETTE_TUTTE) {
  dir.create(cartella, showWarnings = FALSE)
  nomi <- names(etichette); eti <- c(unname(etichette), nome_benchmark)
  col <- setNames(c(palette[seq_along(nomi)], "#7F7F7F"), eti)
  g <- sb |> filter(if_all(all_of(c(paste0(nomi, "_netto"), "r_bench")), ~ !is.na(.x))) |> arrange(mese_reso)
  mese_en <- function(d) paste(month.name[month(d)], year(d))
  per <- paste(mese_en(min(g$mese_reso)), "to", mese_en(max(g$mese_reso)))
  serie <- setNames(lapply(c(paste0(nomi, "_netto"), "r_bench"), function(cn) g[[cn]]), eti)
  lunga <- bind_rows(lapply(eti, function(e) data.frame(mese_reso = c(min(g$mese_reso) %m-% months(1), g$mese_reso),
                       serie = e, valore = 100 * c(1, cumprod(1 + serie[[e]]))))) |>
    mutate(serie = factor(serie, levels = eti))
  salva <- function(nome, p, w = 9, h = 5) ggsave(file.path(cartella, nome), p, width = w, height = h, dpi = dpi)

  # Figure 1 - growth of 100
  salva("figure_1_wealth.png", ggplot(lunga, aes(mese_reso, valore, colour = serie)) + geom_line(linewidth = .7) +
    scale_colour_manual(values = col) + scale_y_log10(labels = label_number(accuracy = 1)) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    labs(title = "Growth of 100 invested", subtitle = paste0("Net of transaction costs, log scale, ", per),
         x = NULL, y = "Value (base 100)", caption = didascalia) + tema_tesi())

  # Figure 2 - drawdown
  dd <- lunga |> group_by(serie) |> arrange(mese_reso) |> mutate(dd = valore / cummax(valore) - 1) |> ungroup()
  salva("figure_2_drawdown.png", ggplot(dd, aes(mese_reso, dd, colour = serie)) +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = .3) + geom_line(linewidth = .6) +
    scale_colour_manual(values = col) + scale_y_continuous(labels = percent_format(accuracy = 1)) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    labs(title = "Drawdown from previous peak", subtitle = paste0("Net of transaction costs, ", per), x = NULL, y = NULL) +
    tema_tesi(), h = 4.5)

  # Figure 3 - calendar-year returns
  ann <- bind_rows(lapply(eti, function(e) data.frame(anno = year(g$mese_reso), r = serie[[e]], serie = e))) |>
    group_by(serie, anno) |> summarise(r = prod(1 + r) - 1, n = n(), .groups = "drop") |>
    filter(n == 12) |> mutate(serie = factor(serie, levels = eti))
  salva("figure_3_calendar_year_returns.png", ggplot(ann, aes(factor(anno), r, fill = serie)) +
    geom_col(position = position_dodge(width = .8), width = .75) +
    geom_hline(yintercept = 0, colour = "grey40", linewidth = .3) + scale_fill_manual(values = col) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(title = "Calendar-year returns", subtitle = "Full calendar years only, net of transaction costs", x = NULL, y = NULL) +
    tema_tesi() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)), w = 11)

  # Figure 4 - risk and return
  pt <- bind_rows(lapply(eti, function(e) { m <- metriche(serie[[e]])
    data.frame(serie = e, vol = m$volatilita, ret = m$rend_annuo) })) |> mutate(serie = factor(serie, levels = eti))
  salva("figure_4_risk_return.png", ggplot(pt, aes(vol, ret, colour = serie)) + geom_point(size = 4) +
    geom_text(aes(label = serie), vjust = -1.2, size = 3.4, show.legend = FALSE) + scale_colour_manual(values = col) +
    scale_x_continuous(labels = percent_format(accuracy = .1), expand = expansion(mult = .15)) +
    scale_y_continuous(labels = percent_format(accuracy = .1), expand = expansion(mult = .15)) +
    labs(title = "Risk and return", subtitle = paste0("Net of transaction costs, ", per),
         x = "Annualized volatility", y = "Annualized return") + tema_tesi() + theme(legend.position = "none"), w = 7.5, h = 5.5)

  # Figure 5 - rolling active return
  ra <- function(x, n) { o <- rep(NA_real_, length(x))
    if (length(x) >= n) for (i in n:length(x)) o[i] <- prod(1 + x[(i - n + 1):i])^(12 / n) - 1; o }
  roll <- bind_rows(lapply(unname(etichette), function(e) data.frame(mese_reso = g$mese_reso, serie = e,
            active = ra(serie[[e]], finestra_mobile) - ra(g$r_bench, finestra_mobile)))) |>
    filter(!is.na(active)) |> mutate(serie = factor(serie, levels = eti))
  salva("figure_5_rolling_active_return.png", ggplot(roll, aes(mese_reso, active, colour = serie)) +
    geom_hline(yintercept = 0, colour = "grey40", linewidth = .4) + geom_line(linewidth = .7) +
    scale_colour_manual(values = col) + scale_y_continuous(labels = percent_format(accuracy = 1)) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    labs(title = sprintf("Rolling %d-month active return", finestra_mobile),
         subtitle = paste("Annualized difference versus", nome_benchmark, "- net of transaction costs"), x = NULL, y = NULL) +
    tema_tesi(), h = 4.5)

  # Figure 6 - monthly turnover (initial purchase excluded)
  sb_tn <- sb; sb_tn[1, grep("_tn$", names(sb_tn))] <- NA
  tn <- bind_rows(lapply(nomi, function(k) data.frame(mese_reso = sb_tn$mese_reso, misura = etichette[[k]],
          valore = sb_tn[[paste0(k, "_tn")]]))) |> filter(!is.na(valore)) |>
    mutate(misura = factor(misura, levels = unname(etichette)))
  salva("figure_6_turnover.png", ggplot(tn, aes(mese_reso, valore)) + geom_line(colour = "#1F4E79", linewidth = .5) +
    facet_wrap(~misura, ncol = 1) + scale_y_continuous(labels = percent_format(accuracy = 1)) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    labs(title = "Monthly portfolio turnover", subtitle = "Fraction of the portfolio renewed at each rebalancing",
         x = NULL, y = NULL) + tema_tesi() + theme(strip.text = element_text(face = "bold", size = 9, hjust = 0)),
    h = 2 + 1.6 * length(nomi))
  cat("Created 6 figures in", cartella, "\n")
}
