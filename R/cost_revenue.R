#' Calculate cost and revenue
#'
#' Calculates cost and revenue after sorting observations.
#'
#' @param x A data frame containing predicted probabilities of a target event and the actual outcome/class.
#' @param fixed_cost Fixed cost (e.g. of a campaign)
#' @param var_cost Variable cost (e.g. discount offered)
#' @param tp_val The average value of a True Positive
#' @param prob_col The unquoted name of the column with probabilities of the event of interest.
#' @param truth_col The unquoted name of the column with the actual outcome/class. Possible values are 'Yes' and 'No'.
#' @return
#' A data frame with the following columns:
#'
#' row       = row numbers \cr
#' pct       = percentiles \cr
#' cost_sum  = cumulated costs \cr
#' cum_rev   = cumulated revenue
#'
#' @export
#' @examples
#' cost_revenue(predictions,
#'    fixed_cost = 1000,
#'    var_cost   = 100,
#'    tp_val     = 2000,
#'    prob_col   = Yes,
#'    truth_col  = Churn)

cost_revenue <- function(x,
                         fixed_cost = 0,
                         var_cost   = 0,
                         tp_val     = 0,
                         prob_col   = NA,
                         truth_col  = NA ) {

  x %>%
    dplyr::arrange(dplyr::desc({{ prob_col }})) %>%
    dplyr::mutate(row = dplyr::row_number()) %>%
    dplyr::mutate(pct = dplyr::ntile(row,100)) %>%
    dplyr::mutate(cost = var_cost) %>%
    dplyr::mutate(cost_sum = cumsum(cost) + fixed_cost) %>%
    dplyr::mutate(rev = dplyr::if_else({{ truth_col }} == "Yes",tp_val,0)) %>%
    dplyr::mutate(cum_rev = cumsum(rev)) %>%
    dplyr::select(row,pct,cost_sum,cum_rev)
}
