#' Lift
#'
#' Calculates a cumulative lift curve after sorting observations by descending
#' predicted probability. Lift is the ratio between the proportion of positives
#' captured by the model and the proportion that would be expected from random
#' targeting. A lift of 1 is no better than random. Named `lift_curve()` to avoid
#' clashing with `purrr::lift()`.
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
#' gain       = proportion of all positives captured \cr
#' lift       = gain / prop_pop
#'
#' @export
#' @examples
#' lift_curve(predictions,
#'    prob_col  = Yes,
#'    truth_col = Churn)

lift_curve <- function(x,
                 prob_col  = NA,
                 truth_col = NA,
                 positive  = "Yes") {

  x %>%
    dplyr::arrange(dplyr::desc({{ prob_col }})) %>%
    dplyr::mutate(row = dplyr::row_number()) %>%
    dplyr::mutate(pct = dplyr::ntile(row, 100)) %>%
    dplyr::mutate(prop_pop = row / dplyr::n()) %>%
    dplyr::mutate(event = dplyr::if_else({{ truth_col }} == positive, 1, 0)) %>%
    dplyr::mutate(gain = cumsum(event) / sum(event)) %>%
    dplyr::mutate(lift = gain / prop_pop) %>%
    dplyr::select(row, pct, prop_pop, gain, lift) %>%
    structure(class = c("mi_lift", "tbl_df", "tbl", "data.frame"))
}
