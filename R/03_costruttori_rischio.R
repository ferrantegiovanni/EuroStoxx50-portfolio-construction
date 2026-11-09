# =============================================================================
#  MODULE 3 - RISK-BASED PORTFOLIO CONSTRUCTORS
#  Covariance estimation (sample or Ledoit-Wolf), the five risk-based weighting
#  rules (IV, GMV, ERC, MDP, HRP) and their diagnostics. Every rule is a
#  function of the covariance matrix only; no return forecast is used.
#  All rules are long-only and fully invested.
# =============================================================================
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(lubridate); library(quadprog) })

`%||%` <- function(a, b) if (is.null(a)) b else a

# fin_cov    : months of returns used to estimate the covariance
# min_titoli : minimum number of securities with a complete window
# peso_max   : per-security weight cap (GMV and MDP only)
# shrinkage  : Ledoit-Wolf (TRUE) or sample covariance (FALSE)
parametri_rischio <- function(fin_cov = 60, min_titoli = 20, peso_max = 0.10, shrinkage = TRUE) {
  stopifnot(fin_cov >= 12, min_titoli >= 2, peso_max > 0, peso_max <= 1)
  list(fin_cov = as.integer(fin_cov), min_titoli = as.integer(min_titoli),
       peso_max = peso_max, shrinkage = shrinkage)
}

# =============================================================================
#  1. COVARIANCE ESTIMATION
# =============================================================================
# Ledoit-Wolf (2004) shrinkage towards a rescaled identity, closed-form intensity.
shrink_lw <- function(R) {
  R <- as.matrix(R); T_ <- nrow(R); N <- ncol(R)
  X <- scale(R, center = TRUE, scale = FALSE); S <- crossprod(X) / T_
  m <- sum(diag(S)) / N; F_ <- diag(m, N); d2 <- sum((S - F_)^2) / N
  b2 <- (sum(rowSums(X^2)^2) - 2 * sum((X %*% S) * X) + T_ * sum(S^2)) / (N * T_^2)
  b2 <- min(b2, d2); delta <- if (d2 > 0) b2 / d2 else 0
  Sig <- delta * F_ + (1 - delta) * S; attr(Sig, "shrinkage") <- delta; Sig
}

# Window (t - fin_cov, t]: the return of month t is known at formation. Only
# securities with a complete history in the window enter, so that all
# covariances are estimated on the same set of months.
stima_covarianza <- function(dati, t, univ, p) {
  M <- dati |> filter(sec_id %in% univ, periodo <= t, periodo > t %m-% months(p$fin_cov), !is.na(r)) |>
    select(sec_id, periodo, r) |> distinct(sec_id, periodo, .keep_all = TRUE) |>
    pivot_wider(names_from = sec_id, values_from = r) |> arrange(periodo) |>
    select(-periodo) |> as.matrix()
  if (!length(M) || nrow(M) < p$fin_cov) return(NULL)
  M <- M[, colSums(is.na(M)) == 0, drop = FALSE]
  if (ncol(M) < p$min_titoli) return(NULL)
  if (p$shrinkage) Sig <- shrink_lw(M) else {
    Sig <- stats::cov(M) * (nrow(M) - 1) / nrow(M); attr(Sig, "shrinkage") <- 0
  }
  dimnames(Sig) <- list(colnames(M), colnames(M))
  Sig
}

# =============================================================================
#  2. WEIGHTING RULES
# =============================================================================
# IV - inverse volatility: w_i proportional to 1 / sigma_i.
pesi_iv <- function(Sigma) { w <- 1 / sqrt(diag(Sigma)); setNames(w / sum(w), colnames(Sigma)) }

# GMV - global minimum variance: min w'Sw  s.t. 1'w = 1, 0 <= w <= peso_max.
pesi_gmv <- function(Sigma, peso_max = 1) {
  n <- ncol(Sigma); if (peso_max * n < 1) stop("Cap not attainable: peso_max x n < 1.")
  D <- 2 * Sigma; diag(D) <- diag(D) + 1e-10
  A <- cbind(rep(1, n), diag(n), if (peso_max < 1) -diag(n))
  b <- c(1, rep(0, n), if (peso_max < 1) rep(-peso_max, n))
  w <- solve.QP(D, rep(0, n), A, b, meq = 1)$solution
  w <- pmax(w, 0); setNames(w / sum(w), colnames(Sigma))
}

# ERC - equal risk contribution: cyclical coordinate descent
# (Griveau-Billion, Richard and Roncalli, 2013). No cap.
pesi_erc <- function(Sigma, budget = NULL, toll = 1e-10, iter_max = 1000) {
  n <- ncol(Sigma); b <- if (is.null(budget)) rep(1 / n, n) else budget / sum(budget)
  y <- 1 / sqrt(diag(Sigma)); y <- y / sum(y)
  for (k in seq_len(iter_max)) {
    y_prec <- y
    for (i in seq_len(n)) {
      a <- Sigma[i, i]; c_ <- sum(Sigma[i, -i] * y[-i])
      y[i] <- (-c_ + sqrt(c_^2 + 4 * a * b[i])) / (2 * a)
    }
    if (max(abs(y - y_prec)) < toll) break
  }
  w <- y / sum(y); attr(w, "iterazioni") <- k
  setNames(w, colnames(Sigma))
}

# MDP - most diversified portfolio: max w'sigma / sqrt(w'Sw).
# Convex form: min y'Sy  s.t. sigma'y = 1, y >= 0, w = y / 1'y.
# The cap w_i <= peso_max becomes peso_max * 1'y - y_i >= 0 in y.
pesi_mdp <- function(Sigma, peso_max = 1) {
  n <- ncol(Sigma); s <- sqrt(diag(Sigma))
  if (peso_max * n < 1) stop("Cap not attainable: peso_max x n < 1.")
  D <- 2 * Sigma; diag(D) <- diag(D) + 1e-10
  A <- cbind(s, diag(n)); b <- c(1, rep(0, n))
  if (peso_max < 1) { A <- cbind(A, peso_max - diag(n)); b <- c(b, rep(0, n)) }
  y <- pmax(solve.QP(D, rep(0, n), A, b, meq = 1)$solution, 0)
  setNames(y / sum(y), colnames(Sigma))
}

# HRP - hierarchical risk parity (Lopez de Prado, 2016). Distance
# d_ij = sqrt((1 - rho_ij) / 2); hierarchical clustering (single linkage by
# default); quasi-diagonalisation; recursive bisection splitting the weight
# between the two halves in inverse proportion to their inverse-variance
# cluster variance. No cap.
pesi_hrp <- function(Sigma, legame = "single") {
  n <- ncol(Sigma); nomi <- colnames(Sigma)
  rho <- stats::cov2cor(Sigma); d <- sqrt(pmax((1 - rho) / 2, 0))
  ordine <- stats::hclust(stats::dist(d), method = legame)$order
  var_cluster <- function(idx) {
    S <- Sigma[idx, idx, drop = FALSE]; iv <- 1 / diag(S); iv <- iv / sum(iv)
    as.numeric(t(iv) %*% S %*% iv)
  }
  w <- rep(1, n); gruppi <- list(ordine)
  while (length(gruppi)) {
    nuovi <- list()
    for (g in gruppi) {
      if (length(g) < 2) next
      h <- length(g) %/% 2; s1 <- g[seq_len(h)]; s2 <- g[(h + 1):length(g)]
      v1 <- var_cluster(s1); v2 <- var_cluster(s2); a <- 1 - v1 / (v1 + v2)
      w[s1] <- w[s1] * a; w[s2] <- w[s2] * (1 - a)
      nuovi <- c(nuovi, list(s1), list(s2))
    }
    gruppi <- nuovi
  }
  setNames(w / sum(w), nomi)
}

# =============================================================================
#  3. CONSTRUCTOR AND EX-ANTE DIAGNOSTICS
# =============================================================================
contributi_rischio <- function(w, Sigma) { m <- as.numeric(Sigma %*% w); w * m / sum(w * m) }

diagnostiche_rischio <- function(w, Sigma) {
  s <- sqrt(diag(Sigma)); v <- as.numeric(t(w) %*% Sigma %*% w); rc <- contributi_rischio(w, Sigma)
  data.frame(n_cov = ncol(Sigma), shrink = attr(Sigma, "shrinkage") %||% NA_real_,
             vol_ex_ante = sqrt(v * 12), peso_max_eff = max(w),
             n_effettivo = 1 / sum(w^2), rapporto_div = sum(w * s) / sqrt(v),
             rc_max = max(rc), rc_min = min(rc))
}

# Returns a constructor usable by esegui_strategia(): (cand, ctx) -> weights.
# `legame` is the HRP linkage (used only when metodo = "hrp").
costruttore_rischio <- function(metodo = c("gmv", "iv", "erc", "mdp", "hrp"),
                                p = parametri_rischio(), legame = "single") {
  metodo <- match.arg(metodo)
  function(cand, ctx) {
    if (nrow(cand) < p$min_titoli) return(NULL)
    Sigma <- stima_covarianza(ctx$dati, ctx$t, cand$sec_id, p)
    if (is.null(Sigma)) return(NULL)
    w <- switch(metodo,
                gmv = pesi_gmv(Sigma, p$peso_max), iv = pesi_iv(Sigma), erc = pesi_erc(Sigma),
                mdp = pesi_mdp(Sigma, p$peso_max), hrp = pesi_hrp(Sigma, legame))
    w <- w[w > 1e-8]; w <- w / sum(w)
    Sig_w <- Sigma[names(w), names(w), drop = FALSE]; attr(Sig_w, "shrinkage") <- attr(Sigma, "shrinkage")
    attr(w, "diag") <- diagnostiche_rischio(w, Sig_w) |> mutate(n_cov = ncol(Sigma))
    w
  }
}
