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
                             positive  = "Yes") {

  x %>%
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
}
