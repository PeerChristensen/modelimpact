# Internal bootstrap helpers shared by bootstrap_profit() and the optional
# confidence bands (`ci = TRUE`) on the ranking-based curve functions.
#
# Each ranking curve returns exactly one row per observation (ranked by
# descending predicted probability), so a bootstrap resample of the same size
# yields a curve of the same length. That means the k-th ranked position is a
# common grid across resamples and no interpolation is needed.

# Resample the rows of `x` `n_boot` times, apply `fun` (which must return a
# numeric vector of length nrow(x)) to each resample, and return the requested
# pointwise quantiles as a `length(probs)` x `nrow(x)` matrix.
.bootstrap_curve <- function(x, n_boot, probs, fun) {
  n <- nrow(x)
  mat <- vapply(seq_len(n_boot), function(b) {
    fun(x[sample(n, replace = TRUE), , drop = FALSE])
  }, numeric(n))
  apply(mat, 1, stats::quantile, probs = probs, names = FALSE)
}

# Convenience wrapper returning a lower/upper band for a symmetric confidence
# level (e.g. conf_level = 0.95 -> 2.5% and 97.5% quantiles).
.ci_band <- function(x, n_boot, conf_level, fun) {
  a <- (1 - conf_level) / 2
  q <- .bootstrap_curve(x, n_boot, c(a, 1 - a), fun)
  list(lower = q[1, ], upper = q[2, ])
}
