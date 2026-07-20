#' Cumulative gains
#'
#' Calculates a cumulative gains curve after sorting observations by descending
#' predicted probability. The gain at a given point is the proportion of all
#' actual positives (events) that have been captured by targeting the top rows.
#'
#' @param x A data frame containing predicted probabilities of a target event and the actual outcome/class.
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
#' row        = row numbers \cr
#' pct        = percentiles \cr
#' prop_pop   = proportion of the population targeted (row / n) \cr
#' cum_events = cumulated number of actual positives captured \cr
#' gain       = proportion of all positives captured (cum_events / total positives) \cr
#' baseline   = expected gain from random targeting (equal to prop_pop)
#'
#' @export
#' @examples
#' cumulative_gains(predictions,
#'    prob_col  = Yes,
#'    truth_col = Churn)

cumulative_gains <- function(x,
                             prob_col  = NA,
                             truth_col = NA,
                             positive  = "Yes",
                             ci        = FALSE,
                             n_boot    = 1000,
                             conf_level = 0.95) {

  out <- x %>%
    dplyr::arrange(dplyr::desc({{ prob_col }})) %>%
    dplyr::mutate(row = dplyr::row_number()) %>%
    dplyr::mutate(pct = dplyr::ntile(row, 100)) %>%
    dplyr::mutate(prop_pop = row / dplyr::n()) %>%
    dplyr::mutate(event = dplyr::if_else({{ truth_col }} == positive, 1, 0)) %>%
    dplyr::mutate(cum_events = cumsum(event)) %>%
    dplyr::mutate(gain = cum_events / sum(event)) %>%
    dplyr::mutate(baseline = prop_pop) %>%
    dplyr::select(row, pct, prop_pop, cum_events, gain, baseline) %>%
    structure(class = c("mi_gains", "tbl_df", "tbl", "data.frame"))

  if (isTRUE(ci)) {
    band <- .ci_band(x, n_boot, conf_level, fun = function(d) {
      cumulative_gains(d, prob_col = {{ prob_col }}, truth_col = {{ truth_col }},
                       positive = positive)$gain
    })
    out$.lower <- band$lower
    out$.upper <- band$upper
  }

  out
}
