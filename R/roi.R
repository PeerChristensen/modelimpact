#' Calculate Return on investment (ROI)
#'
#' Calculates ROI after sorting observations with ROI defined as (Current Value - Start Value) / Start Value
#' @param x A data frame containing predicted probabilities of a target event and the actual outcome/class.
#' @param fixed_cost Fixed cost (e.g. of a campaign)
#' @param var_cost Variable cost (e.g. discount offered). Either a single value
#'   applied to every case, or an unquoted column name (or vector) giving a
#'   per-observation cost.
#' @param tp_val The value of a True Positive. Either a single value applied to
#'   every case, or an unquoted column name (or vector) giving a
#'   per-observation value.
#' @param prob_accept Probability of the offer being accepted. Variable cost is only incurred when accepted. Defaults to 1.
#' @param prob_col The unquoted name of the column with probabilities of the event of interest.
#' @param truth_col The unquoted name of the column with the actual outcome/class.
#' @param positive The value in `truth_col` that identifies the event of interest. Defaults to 'Yes'.
#' @param ci Add bootstrap confidence bands? When `TRUE`, the returned data
#'   frame gains `.lower` and `.upper` columns and `autoplot()` draws a ribbon.
#'   Defaults to `FALSE`.
#' @param n_boot Number of bootstrap resamples used when `ci = TRUE`. Defaults to 1000.
#' @param conf_level Width of the confidence band when `ci = TRUE`. Defaults to 0.95.
#'
#' @return
#' A data frame with the following columns:
#'
#' row       = row numbers \cr
#' pct       = percentiles \cr
#' cum_rev   = cumulated revenue \cr
#' cost_sum  = cumulated costs \cr
#' roi       = return on investment
#'
#' @export
#' @examples
#' roi(predictions,
#'    fixed_cost = 1000,
#'    var_cost   = 100,
#'    tp_val     = 2000,
#'    prob_col   = Yes,
#'    truth_col  = Churn)

roi <- function(x,
                fixed_cost = 0,
                var_cost   = 0,
                tp_val     = 0,
                prob_accept = 1,
                prob_col   = NA,
                truth_col  = NA,
                positive   = "Yes",
                ci         = FALSE,
                n_boot     = 1000,
                conf_level = 0.95) {

  out <- x %>%
    dplyr::arrange(dplyr::desc({{ prob_col }})) %>%
    dplyr::mutate(row = dplyr::row_number()) %>%
    dplyr::mutate(pct = dplyr::ntile(row,100)) %>%
    dplyr::mutate(cost = ({{ var_cost }}) * prob_accept) %>%
    dplyr::mutate(cost_sum = cumsum(cost) + fixed_cost) %>%
    dplyr::mutate(rev = dplyr::if_else({{ truth_col }} == positive, ({{ tp_val }}) + 0, 0)) %>%
    dplyr::mutate(cum_rev = cumsum(rev)) %>%
    dplyr::mutate(roi = (cum_rev - cost_sum) /cost_sum ) %>%
    dplyr::select(row,pct,cum_rev,cost_sum,roi) %>%
    structure(class = c("mi_roi", "tbl_df", "tbl", "data.frame"))

  if (isTRUE(ci)) {
    band <- .ci_band(x, n_boot, conf_level, fun = function(d) {
      roi(d, fixed_cost, {{ var_cost }}, {{ tp_val }}, prob_accept = prob_accept,
          prob_col = {{ prob_col }}, truth_col = {{ truth_col }},
          positive = positive)$roi
    })
    out$.lower <- band$lower
    out$.upper <- band$upper
  }

  out
}

