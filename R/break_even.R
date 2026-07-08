#' Break-even and optimal targeting point
#'
#' Summarises the two key operating points of the cumulative profit curve
#' produced by [profit()]: the point of maximum profit (the recommended share of
#' customers to target) and the break-even point (the largest share that can be
#' targeted while still turning a profit).
#'
#' @param x A data frame containing predicted probabilities of a target event and the actual outcome/class.
#' @param fixed_cost Fixed cost (e.g. of a campaign).
#' @param var_cost Variable cost (e.g. discount offered) per targeted customer.
#' @param tp_val The average value of a True Positive.
#' @param prob_accept Probability of the offer being accepted. Variable cost is only incurred when accepted. Defaults to 1.
#' @param prob_col The unquoted name of the column with probabilities of the event of interest.
#' @param truth_col The unquoted name of the column with the actual outcome/class.
#' @param positive The value in `truth_col` that identifies the event of interest. Defaults to 'Yes'.
#'
#' @return
#' A one-row data frame with the following columns:
#'
#' optimal_row   = row at which profit is maximised \cr
#' optimal_pct   = share of the population targeted at maximum profit \cr
#' max_profit    = the maximum profit \cr
#' breakeven_row = last row at which cumulative profit is still non-negative \cr
#' breakeven_pct = share of the population targeted at the break-even point
#'
#' @export
#' @examples
#' break_even(predictions,
#'    fixed_cost = 1000,
#'    var_cost   = 100,
#'    tp_val     = 2000,
#'    prob_col   = Yes,
#'    truth_col  = Churn)

break_even <- function(x,
                       fixed_cost = 0,
                       var_cost   = 0,
                       tp_val     = 0,
                       prob_accept = 1,
                       prob_col   = NA,
                       truth_col  = NA,
                       positive   = "Yes") {

  p <- profit(x,
              fixed_cost = fixed_cost,
              var_cost   = var_cost,
              tp_val     = tp_val,
              prob_accept = prob_accept,
              prob_col   = {{ prob_col }},
              truth_col  = {{ truth_col }},
              positive   = positive)

  n <- nrow(p)

  opt <- p %>%
    dplyr::filter(profit == max(profit)) %>%
    dplyr::slice(1)

  be <- p %>% dplyr::filter(profit >= 0)
  be_row <- if (nrow(be) == 0) NA_integer_ else max(be$row)

  dplyr::tibble(
    optimal_row   = opt$row,
    optimal_pct   = opt$row / n * 100,
    max_profit    = opt$profit,
    breakeven_row = be_row,
    breakeven_pct = be_row / n * 100
  )
}
