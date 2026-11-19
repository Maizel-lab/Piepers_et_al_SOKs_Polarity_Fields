## common_axial_stats.R
##
## Shared axial/circular statistics functions used by all four R scripts in
## this folder. Mirrors the Python functions in
## circular_correlation_sok_vs_growth.py and make_all_pdfs.py exactly, so
## numbers should match to within permutation-seed noise.
##
## Core idea: SOK polarity axis and PDG axis are AXES (undirected), not
## directions -- theta and theta+180 are the same measurement. Circular
## statistics for genuine directions (mod 360) scramble this unless the
## angle is first "doubled" (theta -> 2*theta mod 360), which maps the
## 180-degree-periodic axial space onto an honest 360-degree circular
## space. Un-double (divide by 2) only at the very end, for display.

doubled <- function(theta_deg) {
  (2 * theta_deg) %% 360
}

undoubled <- function(theta2_deg) {
  (theta2_deg %% 360) / 2
}

#' Axial mean axis + mean resultant length R, in [0,1].
circular_mean_axial <- function(theta_deg) {
  th2 <- doubled(theta_deg) * pi / 180
  C <- mean(cos(th2)); S <- mean(sin(th2))
  R <- sqrt(C^2 + S^2)
  mean_axis_deg <- undoubled((atan2(S, C) * 180 / pi) %% 360)
  list(mean_axis_deg = mean_axis_deg, R = R)
}

#' Per-pair axial angular separation, folded into [0,90] deg.
#' 0 = parallel, 90 = perpendicular.
axial_separation_deg <- function(theta1_deg, theta2_deg) {
  t1 <- doubled(theta1_deg)
  t2 <- doubled(theta2_deg)
  diff2 <- (t1 - t2) %% 360
  diff2 <- pmin(diff2, 360 - diff2)
  diff2 / 2
}

#' Permutation null for the MEAN axial separation statistic: shuffle the
#' pairing between `a` and `b` (preserving each variable's own marginal),
#' recompute mean separation each time. Two-sided p-value.
permutation_test_mean_separation <- function(a, b, n_perm = 10000, seed = 0) {
  set.seed(seed)
  obs <- mean(axial_separation_deg(a, b))
  n <- length(b)
  null_means <- numeric(n_perm)
  for (i in seq_len(n_perm)) {
    perm <- sample.int(n)
    null_means[i] <- mean(axial_separation_deg(a, b[perm]))
  }
  null_center <- mean(null_means)
  p <- (sum(abs(null_means - null_center) >= abs(obs - null_center)) + 1) / (n_perm + 1)
  list(obs = obs, null_center = null_center, p = p, null_means = null_means)
}

#' Permutation null for the per-bin HISTOGRAM counts of axial separation
#' (used to draw the 95% band in the polar plots).
permutation_null_histogram <- function(a, b, edges_deg, n_perm = 10000, seed = 0) {
  set.seed(seed)
  n <- length(b)
  n_bins <- length(edges_deg) - 1
  null_counts <- matrix(NA_real_, nrow = n_perm, ncol = n_bins)
  null_means <- numeric(n_perm)
  for (i in seq_len(n_perm)) {
    perm <- sample.int(n)
    sep <- axial_separation_deg(a, b[perm])
    null_counts[i, ] <- as.numeric(table(cut(sep, breaks = edges_deg, include.lowest = TRUE)))
    null_means[i] <- mean(sep)
  }
  list(null_counts = null_counts, null_means = null_means)
}

#' Quality filter: drop the lower `pct`-th percentile of a max/min signal
#' ratio, computed SEPARATELY within each `window` group (the adopted
#' convention throughout this project).
filter_within_window <- function(df, ratio_col, pct = 25) {
  keep <- logical(nrow(df))
  for (w in unique(df$window)) {
    idx <- which(df$window == w)
    vals <- df[[ratio_col]][idx]
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0) next
    thresh <- quantile(vals, probs = pct / 100, na.rm = TRUE, names = FALSE)
    keep[idx] <- !is.na(df[[ratio_col]][idx]) & df[[ratio_col]][idx] >= thresh
  }
  df[keep, ]
}

#' Mardia (1976) circular-linear correlation: R^2 + parametric F-test p,
#' computed on the axially-correct DOUBLED angle. `y` is an ordinary linear
#' variable (e.g. log anisotropy).
mardia_circ_lin <- function(theta_deg, y) {
  ok <- complete.cases(theta_deg, y)
  theta_deg <- theta_deg[ok]; y <- y[ok]
  th <- doubled(theta_deg) * pi / 180
  rxc <- cor(y, cos(th)); rxs <- cor(y, sin(th)); rcs <- cor(cos(th), sin(th))
  r2 <- (rxc^2 + rxs^2 - 2 * rxc * rxs * rcs) / (1 - rcs^2)
  n <- length(y)
  fstat <- (n - 3) / 2 * r2 / (1 - r2)
  p <- pf(fstat, 2, n - 3, lower.tail = FALSE)
  list(n = n, r2 = r2, p = p)
}
