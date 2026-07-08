#' Marginal profit per bin
#'
#' Splits observations into equally sized bins (deciles by default) after sorting
#' by descending predicted probability, and calculates the profit contributed by
#' each bin. This makes it easy to see where targeting additional customers stops
#' paying off: the first bin whose `marginal_profit` turns negative marks the
#' point of diminishing returns. The cumulative sum of `marginal_profit`
#' reconciles with the cumulative profit reported by [profit()].
#'
#' @param x A data frame containing predicted probabilities of a target event and the actual outcome/class.
#' @param fixed_cost Fixed cost (e.g. of a campaign). Charged once, to the first bin.
#' @param var_cost Variable cost (e.g. discount offered) per targeted customer.
#' @param tp_val The average value of a True Positive.
#' @param prob_accept Probability of the offer being accepted. Variable cost is only incurred when accepted. Defaults to 1.
#' @param bins Number of equally sized bins. Defaults to 10 (deciles).
#' @param prob_col The unquoted name of the column with probabilities of the event of interest.
#' @param truth_col The unquoted name of the column with the actual outcome/class.
#' @param positive The value in `truth_col` that identifies the event of interest. Defaults to 'Yes'.
#'
#' @return
#' A data frame with the following columns:
#'
#' bin            = bin number (1 = highest probabilities) \cr
#' n              = number of observations in the bin \cr
#' events         = number of actual positives in the bin \cr
#' cost           = cost incurred in the bin \cr
#' revenue        = revenue generated in the bin \cr
#' marginal_profit = revenue - cost for the bin \cr
#' cum_profit     = cumulative profit up to and including the bin
#'
#' @export
#' @examples
#' marginal_profit(predictions,
#'    fixed_cost = 1000,
#'    var_cost   = 100,
#'    tp_val     = 2000,
#'    bins       = 10,
#'    prob_col   = Yes,
#'    truth_col  = Churn)

marginal_profit <- function(x,
                            fixed_cost = 0,
                            var_cost   = 0,
                            tp_val     = 0,
                            prob_accept = 1,
                            bins       = 10,
                            prob_col   = NA,
                            truth_col  = NA,
                            positive   = "Yes") {

  x %>%
    dplyr::arrange(dplyr::desc({{ prob_col }})) %>%
    dplyr::mutate(row = dplyr::row_number()) %>%
    dplyr::mutate(bin = dplyr::ntile(row, bins)) %>%
    dplyr::mutate(event = dplyr::if_else({{ truth_col }} == positive, 1, 0)) %>%
    dplyr::group_by(bin) %>%
    dplyr::summarise(n       = dplyr::n(),
                     events  = sum(event),
                     .groups = "drop") %>%
    dplyr::mutate(cost = (n * var_cost * prob_accept) + dplyr::if_else(bin == 1, fixed_cost, 0)) %>%
    dplyr::mutate(revenue = events * tp_val) %>%
    dplyr::mutate(marginal_profit = revenue - cost) %>%
    dplyr::mutate(cum_profit = cumsum(marginal_profit)) %>%
    dplyr::select(bin, n, events, cost, revenue, marginal_profit, cum_profit) %>%
    structure(class = c("mi_marginal", "tbl_df", "tbl", "data.frame"))
}
