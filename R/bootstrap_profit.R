#' Bootstrap confidence bands for the profit curve
#'
#' Resamples the observations with replacement a number of times, recomputes the
#' cumulative profit curve for each resample, and returns pointwise quantile
#' bands. This turns the single profit curve from [profit()] into a range,
#' making it clear how much of the curve's shape is signal versus sampling noise.
#'
#' @param x A data frame containing predicted probabilities of a target event and the actual outcome/class.
#' @param fixed_cost Fixed cost (e.g. of a campaign).
#' @param var_cost Variable cost (e.g. discount offered) per targeted customer.
#'   Either a single value or an unquoted column name (or vector) giving a
#'   per-observation cost.
#' @param tp_val The value of a True Positive. Either a single value or an
#'   unquoted column name (or vector) giving a per-observation value.
#' @param prob_col The unquoted name of the column with probabilities of the event of interest.
#' @param truth_col The unquoted name of the column with the actual outcome/class.
#' @param positive The value in `truth_col` that identifies the event of interest. Defaults to 'Yes'.
#' @param n_boot Number of bootstrap resamples. Defaults to 200.
#' @param probs Lower, middle and upper quantiles for the bands. Defaults to `c(0.05, 0.5, 0.95)`.
#'
#' @return
#' A data frame with the following columns:
#'
#' row      = row numbers \cr
#' prop_pop = proportion of the population targeted (row / n) \cr
#' lower    = lower quantile of profit across resamples \cr
#' median   = median profit across resamples \cr
#' upper    = upper quantile of profit across resamples
#'
#' @export
#' @examples
#' bootstrap_profit(predictions,
#'    fixed_cost = 1000,
#'    var_cost   = 100,
#'    tp_val     = 2000,
#'    prob_col   = Yes,
#'    truth_col  = Churn,
#'    n_boot     = 100)

bootstrap_profit <- function(x,
                             fixed_cost = 0,
                             var_cost   = 0,
                             tp_val     = 0,
                             prob_col   = NA,
                             truth_col  = NA,
                             positive   = "Yes",
                             n_boot     = 200,
                             probs      = c(0.05, 0.5, 0.95)) {

  n <- nrow(x)

  mat <- vapply(seq_len(n_boot), function(b) {
    xb <- x[sample(n, replace = TRUE), , drop = FALSE]
    profit(xb,
           fixed_cost = fixed_cost,
           var_cost   = {{ var_cost }},
           tp_val     = {{ tp_val }},
           prob_col   = {{ prob_col }},
           truth_col  = {{ truth_col }},
           positive   = positive)$profit
  }, numeric(n))

  bands <- apply(mat, 1, stats::quantile, probs = probs, names = FALSE)

  dplyr::tibble(row      = seq_len(n),
                prop_pop = seq_len(n) / n,
                lower    = bands[1, ],
                median   = bands[2, ],
                upper    = bands[3, ]) %>%
    structure(class = c("mi_bootstrap", "tbl_df", "tbl", "data.frame"))
}
