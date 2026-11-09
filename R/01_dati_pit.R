# =============================================================================
#  MODULE 1 - POINT-IN-TIME DATA
#  Index membership, security key, canonical panel, LSEG adapters.
#  The core functions work on plain data frames with documented columns;
#  the LSEG-specific code is at the bottom of the file.
#  (Identifiers are in Italian: periodo = month, pannello = panel,
#   eleggibile = index member at the formation date.)
# =============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(lubridate)
})

# ---- Utilities ---------------------------------------------------------------
# write.csv(na = "") writes missing values as empty strings; without na.strings
# read.csv would not read them back as NA and the missing-data checks would miss them.
leggi <- function(f) read.csv(f, stringsAsFactors = FALSE, na.strings = c("", "NA"))

# Identifiers are codes, not quantities: always text.
come_testo <- function(df, colonne) {
  for (cc in intersect(colonne, names(df))) {
    x <- trimws(as.character(df[[cc]])); x[x %in% c("", "NA")] <- NA_character_
    df[[cc]] <- x
  }
  df
}

# A step runs if its output file is missing or if it is forced.
da_rigenerare <- function(file, forza = FALSE) forza || !file.exists(file)

# =============================================================================
#  1. POINT-IN-TIME MEMBERSHIP
# =============================================================================
# eventi   : data frame with status ("Joiner"/"Leaver"), issuer, code, date
# correnti : data frame with code, name (constituents at the end date)
# baseline : date at which the source database was first populated
# Returns a list: iv (intervals [date_in, date_out)), anomalie, conteggi.
ricostruisci_membership <- function(eventi, correnti, baseline, end_date) {
  baseline <- as.Date(baseline); end_date <- as.Date(end_date)

  lj <- eventi |>
    mutate(date = as.Date(date), code = trimws(code)) |>
    filter(!is.na(date), status %in% c("Joiner", "Leaver"))

  # Leaver + Joiner pairs on the same code and day are corporate-action
  # artefacts: min(nJoiner, nLeaver) events of each type are removed.
  lj <- lj |>
    group_by(code, date, status) |> mutate(rk = row_number()) |>
    group_by(code, date) |>
    mutate(n_pairs = min(sum(status == "Joiner"), sum(status == "Leaver"))) |>
    ungroup() |> filter(rk > n_pairs) |> select(-rk, -n_pairs) |>
    arrange(date, desc(status))          # on the same date, Leaver comes first

  current_set <- trimws(correnti$code)
  cur_names   <- setNames(correnti$name, current_set)

  open_start <- list(); open_issuer <- list()
  intervals <- list(); anomalies <- list()
  aggiungi_an <- function(tipo, cd, is_, dt)
    anomalies[[length(anomalies) + 1]] <<-
      data.frame(tipo = tipo, code = cd, issuer = is_, date = dt)

  for (i in seq_len(nrow(lj))) {
    st <- lj$status[i]; cd <- lj$code[i]; is_ <- lj$issuer[i]; dt <- lj$date[i]
    if (st == "Joiner") {
      if (!is.null(open_start[[cd]])) aggiungi_an("joiner_while_member(ignored)", cd, is_, dt)
      else { open_start[[cd]] <- dt; open_issuer[[cd]] <- is_ }
    } else {
      if (!is.null(open_start[[cd]])) {
        intervals[[length(intervals) + 1]] <- data.frame(
          code = cd, issuer = open_issuer[[cd]], date_in = open_start[[cd]], date_out = dt)
        open_start[[cd]] <- NULL; open_issuer[[cd]] <- NULL
      } else {
        aggiungi_an("leaver_without_joiner(repaired_from_baseline)", cd, is_, dt)
        intervals[[length(intervals) + 1]] <- data.frame(
          code = cd, issuer = is_, date_in = baseline, date_out = dt)
      }
    }
  }
  for (cd in names(open_start))
    intervals[[length(intervals) + 1]] <- data.frame(
      code = cd, issuer = open_issuer[[cd]], date_in = open_start[[cd]], date_out = as.Date(NA))

  for (cd in setdiff(current_set, names(open_start))) {
    intervals[[length(intervals) + 1]] <- data.frame(
      code = cd, issuer = unname(cur_names[cd]), date_in = baseline, date_out = as.Date(NA))
    aggiungi_an("current_without_events(repaired_from_baseline)", cd, unname(cur_names[cd]), baseline)
  }

  iv <- bind_rows(intervals) |>
    arrange(code, date_in) |> group_by(code) |>
    mutate(prev_out  = lag(date_out),
           new_block = (is.na(prev_out) & row_number() > 1) |
                       (!is.na(prev_out) & date_in > prev_out),
           block = cumsum(as.integer(row_number() == 1 | new_block))) |>
    group_by(code, block) |>
    summarise(issuer = first(issuer), date_in = min(date_in),
              date_out = if (any(is.na(date_out))) as.Date(NA) else max(date_out),
              .groups = "drop") |>
    select(-block) |>
    mutate(base_ric = trimws(sub("\\^.*$", "", code)),
           still_member = is.na(date_out)) |>
    arrange(date_in, code)

  # Validation: the final members MUST coincide with the current constituents.
  finali <- iv$code[iv$still_member]
  if (!setequal(finali, current_set))
    stop("Final members differ from the current constituents: ",
         length(setdiff(finali, current_set)), " extra, ",
         length(setdiff(current_set, finali)), " missing.")

  firsts <- seq(ceiling_date(baseline, "month"), ceiling_date(end_date, "month"), by = "month")
  fine_mesi <- sort(unique(c(baseline, firsts - days(1), end_date)))
  fine_mesi <- fine_mesi[fine_mesi >= baseline & fine_mesi <= end_date]
  conteggi <- data.frame(date = fine_mesi, n_constituents = vapply(fine_mesi, function(d)
    sum(iv$date_in <= d & (is.na(iv$date_out) | iv$date_out > d)), integer(1)))

  an <- if (length(anomalies)) bind_rows(anomalies) else
    data.frame(tipo = character(), code = character(), issuer = character(), date = as.Date(character()))
  cat("Membership:", nrow(iv), "intervals | constituents per month min/max:",
      min(conteggi$n_constituents), "/", max(conteggi$n_constituents), "\n")
  list(iv = iv, anomalie = an, conteggi = conteggi, eventi_puliti = lj)
}

# =============================================================================
#  2. SECURITY KEY AND GROUP GUARD
# =============================================================================
# The join key is never the RIC (reassignable) nor the ISIN (changes with the
# domicile). Default: organisation PermID + ShareClass.
costruisci_chiave <- function(cw, col_entita = "PermID", col_classe = "ShareClass",
                              classe_unica = "UNICA") {
  cw |> mutate(classe = ifelse(is.na(.data[[col_classe]]), classe_unica, .data[[col_classe]]),
               sec_id = ifelse(is.na(.data[[col_entita]]), NA_character_,
                               paste(.data[[col_entita]], classe, sep = "_")))
}

# Keys associated with more than one code: not errors in themselves.
gruppi_multipli <- function(cw, col_codice = "RIC") {
  cw |> filter(!is.na(sec_id)) |> group_by(sec_id) |>
    summarise(n_ric = n(), RIC_associati = paste(sort(.data[[col_codice]]), collapse = " + "),
              .groups = "drop") |> filter(n_ric > 1)
}

# Are two codes the same security? A criterion independent of the labels:
#   no overlap                  -> succession (same security)
#   overlapping, equal prices   -> same security on several venues
#   overlapping, different prices -> DISTINCT securities
# px: long data frame with RIC, Data, p.
stessa_security <- function(px, a, b, soglia = 0.01) {
  ov <- inner_join(px |> filter(RIC == a) |> distinct(Data, .keep_all = TRUE) |> select(Data, pa = p),
                   px |> filter(RIC == b) |> distinct(Data, .keep_all = TRUE) |> select(Data, pb = p),
                   by = "Data")
  if (nrow(ov) == 0) return(TRUE)
  max(abs(ov$pa - ov$pb) / pmax(abs(ov$pa), abs(ov$pb)), na.rm = TRUE) < soglia
}

# Connected components; numbering is stable by date of first observation.
raggruppa_codici <- function(rics, px, soglia = 0.01) {
  g <- setNames(seq_along(rics), rics)
  for (i in seq_along(rics)) for (j in seq_along(rics)) {
    if (j <= i) next
    if (stessa_security(px, rics[i], rics[j], soglia)) g[g == g[[rics[j]]]] <- g[[rics[i]]]
  }
  prime <- px |> group_by(RIC) |> summarise(prima = min(Data), .groups = "drop")
  ordine <- sapply(unique(g), function(k) {
    r <- names(g)[g == k]; x <- prime$prima[prime$RIC %in% r]
    if (length(x)) min(x) else as.Date("2999-12-31")
  })
  rango <- rank(ordine, ties.method = "first")
  setNames(rango[match(g, unique(g))], names(g))
}

# Disambiguates multi-code groups; f_prezzi(rics) must return RIC, Data, p.
# The assignment must be FROZEN in the crosswalk (it is not a per-row rule).
disambigua_crosswalk <- function(cw, f_prezzi, soglia = 0.01) {
  multi <- gruppi_multipli(cw)
  mappa <- list(); esiti <- list()
  for (i in seq_len(nrow(multi))) {
    sid <- multi$sec_id[i]; rics <- strsplit(multi$RIC_associati[i], " \\+ ")[[1]]
    px <- f_prezzi(rics)
    if (is.null(px) || nrow(px) == 0) {
      esiti[[i]] <- data.frame(sec_id = sid, esito = "extraction failed"); next
    }
    g <- raggruppa_codici(rics, px, soglia)
    esiti[[i]] <- data.frame(sec_id = sid, RIC_associati = multi$RIC_associati[i],
                             esito = if (length(unique(g)) > 1) "DISTINCT SECURITIES" else "same security")
    if (length(unique(g)) > 1)
      mappa[[length(mappa) + 1]] <- data.frame(RIC = rics, sec_id_vecchio = sid,
                                              sec_id_nuovo = sprintf("%s_s%02d", sid, g[rics]))
  }
  mappa <- bind_rows(mappa)
  cw_f <- cw |> mutate(sec_id_base = sec_id, disambiguato = FALSE)
  if (nrow(mappa)) cw_f <- cw_f |>
    left_join(mappa |> select(RIC, sec_id_nuovo), by = "RIC") |>
    mutate(disambiguato = !is.na(sec_id_nuovo),
           sec_id = ifelse(disambiguato, sec_id_nuovo, sec_id)) |> select(-sec_id_nuovo)
  cat("Crosswalk:", nrow(cw_f), "codes |", n_distinct(cw_f$sec_id, na.rm = TRUE),
      "securities | disambiguated:", sum(cw_f$disambiguato), "\n")
  list(crosswalk = cw_f, disambiguazioni = mappa, esiti = bind_rows(esiti))
}

# =============================================================================
#  3. MEMBERSHIP FLAG AND CANONICAL PANEL
# =============================================================================
# Single convention: [date_in, date_out). Exception: the end-of-sample cap (open
# interval) is inclusive, otherwise the last month would be outside the index
# for every security.
marca_membership <- function(df, iv_key, data_fine, col_data = "Data") {
  ii <- iv_key |> mutate(aperto = is.na(date_out),
                         date_out_eff = coalesce(date_out, as.Date(data_fine))) |>
    select(sec_id, date_in, date_out_eff, aperto)
  d_vec <- as.Date(df[[col_data]])
  marcate <- data.frame(sec_id = df$sec_id, .d = d_vec, stringsAsFactors = FALSE) |>
    distinct() |> left_join(ii, by = "sec_id", relationship = "many-to-many") |>
    group_by(sec_id, .d) |>
    summarise(dentro = any(.d >= date_in & ifelse(aperto, .d <= date_out_eff, .d < date_out_eff),
                           na.rm = TRUE), .groups = "drop")
  df$.d <- d_vec
  df <- df |> left_join(marcate, by = c("sec_id", ".d")) |> mutate(dentro = coalesce(dentro, FALSE))
  df$.d <- NULL
  df
}

# mensili : RIC, Data, ret (decimal), prezzo, mktcap  (FULL history)
# ponte   : RIC -> sec_id (+ descriptive columns)
# iv      : intervals with a code column (or already sec_id)
# Rules: placeholder rows are dropped; missing returns STAY NA (never 0);
# one security - one month - one row, last non-missing value.
costruisci_pannello <- function(mensili, ponte, iv, data_fine) {
  vuote <- is.na(mensili$Data) | (is.na(mensili$ret) & is.na(mensili$prezzo))
  if (any(vuote)) cat("Placeholder rows removed:", sum(vuote), "\n")
  m <- mensili[!vuote, ] |> mutate(Data = as.Date(Data))

  if (!"sec_id" %in% names(m))
    m <- m |> left_join(ponte |> select(RIC, sec_id), by = "RIC") |> filter(!is.na(sec_id))
  iv_key <- if ("sec_id" %in% names(iv)) iv else
    iv |> mutate(code_pulito = trimws(sub(" \\(expired\\)", "", code))) |>
      left_join(ponte |> select(RIC, sec_id), by = c("code_pulito" = "RIC")) |>
      filter(!is.na(sec_id))
  iv_key <- iv_key |> mutate(date_in = as.Date(date_in), date_out = as.Date(date_out))

  m <- marca_membership(m, iv_key, data_fine) |> rename(in_indice = dentro) |>
    mutate(periodo = floor_date(Data, "month"))

  ultimo_nm <- function(x) if (all(is.na(x))) x[1] else x[max(which(!is.na(x)))]
  doppi <- m |> count(sec_id, periodo) |> filter(n > 1)
  cat("One-security-one-month-one-row violations before consolidation:", nrow(doppi), "\n")
  base_cols <- function(d) d |> transmute(sec_id, periodo, RIC, ultima_oss = Data,
                                          r = ret, prezzo, mktcap, in_indice)
  pann <- if (nrow(doppi)) bind_rows(
    m |> anti_join(doppi, by = c("sec_id", "periodo")) |> base_cols(),
    m |> semi_join(doppi, by = c("sec_id", "periodo")) |> arrange(sec_id, periodo, Data) |>
      group_by(sec_id, periodo) |>
      summarise(RIC = last(RIC), ultima_oss = max(Data[!is.na(prezzo)], Data[1]),
                r = ultimo_nm(ret), prezzo = ultimo_nm(prezzo), mktcap = ultimo_nm(mktcap),
                in_indice = ultimo_nm(in_indice), .groups = "drop")) else base_cols(m)
  pann <- pann |> arrange(sec_id, periodo)
  stopifnot(!any(duplicated(pann[, c("sec_id", "periodo")])))
  amp <- pann |> filter(in_indice) |> count(periodo)
  cat("Panel:", nrow(pann), "rows |", n_distinct(pann$sec_id), "securities | members per month, median",
      median(amp$n), "\n")
  attr(pann, "iv_key") <- iv_key
  pann
}

# =============================================================================
#  4. LSEG ADAPTERS (Refinitiv / RefinitivR)
# =============================================================================
# The credential ALWAYS comes from an environment variable (~/.Renviron: LSEG_APP_KEY=...).
connetti_lseg <- function() {
  if (!requireNamespace("Refinitiv", quietly = TRUE)) stop("Package Refinitiv is not installed.")
  key <- Sys.getenv("LSEG_APP_KEY")
  if (!nzchar(key)) stop("LSEG_APP_KEY is not set (usethis::edit_r_environ()).")
  Refinitiv::RDConnect(application_id = key, PythonModule = "JSON")
}

# Recognises columns by content, not by position.
rinomina_per_contenuto <- function(df, mappa) {
  df <- as.data.frame(df); ch <- gsub("[^a-z]", "", tolower(names(df)))
  for (nuovo in names(mappa)) {
    i <- which(grepl(mappa[[nuovo]], ch))[1]
    if (!is.na(i)) names(df)[i] <- nuovo
  }
  for (a in names(mappa)) if (!a %in% names(df)) df[[a]] <- NA
  df[, names(mappa), drop = FALSE]
}

# Batched extraction with retries, incremental saving and a repair loop PER
# CODE: a batch can fail partially and silently.
# f_lotto(codici) -> data frame with a RIC column (or NULL).
# completo(df)    -> logical vector: rows considered valid.
estrai_a_lotti <- function(codici, f_lotto, lotto = 25, tentativi = 3,
                           giri_riparo = 3, lotto_riparo = 5, completo = function(d) rep(TRUE, nrow(d)),
                           file_parziale = NULL) {
  prova <- function(x, k_max) {
    for (k in seq_len(k_max)) {
      r <- try(f_lotto(x), silent = TRUE)
      if (!inherits(r, "try-error") && !is.null(r) && nrow(r) > 0) return(r)
      Sys.sleep(2 * k)
    }
    NULL
  }
  lotti <- split(codici, ceiling(seq_along(codici) / lotto)); racc <- list()
  for (i in seq_along(lotti)) {
    o <- prova(lotti[[i]], tentativi); if (!is.null(o)) racc[[length(racc) + 1]] <- o
    cat("  batch", i, "/", length(lotti), if (is.null(o)) "FAILED" else paste(nrow(o), "rows"), "\n")
    if (!is.null(file_parziale) && (i %% 5 == 0 || i == length(lotti)))
      write.csv(bind_rows(racc), file_parziale, row.names = FALSE, na = "")
  }
  tutto <- bind_rows(racc)
  ok <- if (nrow(tutto)) unique(tutto$RIC[completo(tutto)]) else character(0)
  resta <- setdiff(codici, ok)
  for (giro in seq_len(giri_riparo)) {
    if (!length(resta)) break
    cat("Repair round", giro, "- missing:", length(resta), "\n")
    nuovi <- bind_rows(lapply(split(resta, ceiling(seq_along(resta) / lotto_riparo)),
                              function(x) prova(x, 2)))
    if (!nrow(nuovi)) break
    nuovi <- nuovi[completo(nuovi), , drop = FALSE]
    if (!nrow(nuovi)) break
    tutto <- bind_rows(tutto |> filter(!RIC %in% nuovi$RIC), nuovi)
    resta <- setdiff(resta, nuovi$RIC)
  }
  attr(tutto, "non_recuperati") <- resta
  cat("Extraction: requested", length(codici), "| not recovered", length(resta), "\n")
  tutto
}

# f_lotto for the monthly series (total return, price, market cap).
lseg_serie_mensili <- function(data_inizio, data_fine,
                               campi = c("TR.TotalReturn", "TR.PriceClose", "TR.CompanyMarketCap")) {
  function(rics) {
    res <- Refinitiv::rd_GetHistory(universe = rics, fields = campi,
      parameters = list("SDate" = data_inizio, "EDate" = data_fine, "Frq" = "M"))
    d <- rinomina_per_contenuto(res, list(RIC = "^instrument$|^ric$", Data = "date",
           ret = "totalreturn", prezzo = "priceclose|closeprice", mktcap = "marketcap"))
    num <- function(x) suppressWarnings(as.numeric(gsub(",", "", as.character(x))))
    d |> mutate(RIC = trimws(as.character(RIC)), Data = as.Date(Data),
                ret = num(ret) / 100, prezzo = num(prezzo), mktcap = num(mktcap))
  }
}

# f_lotto for the identifiers.
lseg_identificativi <- function(campi = c("TR.OrganizationID", "TR.QuoteId", "TR.ShareClass",
                                          "TR.ISIN", "TR.CommonName")) {
  function(rics) {
    res <- Refinitiv::rd_GetData(rics = rics, Eikonformulas = campi)
    rinomina_per_contenuto(res, list(RIC = "^instrument$|^ric$", PermID = "organi",
      QuoteId = "quoteid", ShareClass = "shareclass", ISIN = "isin", Nome = "commonname|company")) |>
      come_testo(c("RIC", "PermID", "QuoteId", "ShareClass", "ISIN", "Nome")) |>
      mutate(across(c(PermID, ShareClass, ISIN, Nome),
                    ~ ifelse(.x %in% "No best match available", NA_character_, .x)))
  }
}

# Price and market-cap currency per RIC (the study requires EUR for both).
lseg_valute <- function(rics) {
  val <- as.data.frame(Refinitiv::rd_GetData(rics = rics,
           Eikonformulas = c("TR.PriceClose.currency", "TR.CompanyMarketCap.currency")))
  names(val) <- c("RIC", "valuta_prezzo", "valuta_cap")[seq_len(ncol(val))]
  val
}

# Readers for the LSEG report files (Excel exports).
# Joiners/leavers: header rows are skipped; dates arrive as Excel serial numbers.
leggi_eventi_lseg <- function(file, skip = 4) {
  readxl::read_excel(file, skip = skip, col_names = c("status", "issuer", "code", "date")) |>
    mutate(date = if (is.numeric(date)) as.Date(date, origin = "1899-12-30") else as.Date(date),
           code = trimws(code))
}
# Current constituents: the export has 15 columns, only RIC and name are needed.
leggi_correnti_lseg <- function(file, escludi = "^Identifier|^Totals|^\\.") {
  readxl::read_excel(file, col_names = FALSE, .name_repair = "minimal")[, 1:2] |>
    setNames(c("code", "name")) |>
    mutate(code = trimws(code)) |>
    filter(!is.na(code), !grepl(escludi, code))
}
