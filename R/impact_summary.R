#' One-line business-impact summary
#'
#' Rolls the ranking-based views (profit, ROI, gains and break-even) up into a
#' single row of headline numbers for reporting. It answers: what share of
#' customers should we target, how much profit does that make, what return does
#' it represent, how many churners do we catch, how far can we go before losing
#' money, and what happens if we simply target everyone.
#'
#' @param x A data frame containing predicted probabilities of a target event and the actual outcome/class.
#' @param fixed_cost Fixed cost (e.g. of a campaign).
#' @param var_cost Variable cost (e.g. discount offered) per targeted customer.
#'   Either a single value or an unquoted column name (or vector) giving a
#'   per-observation cost.
#' @param tp_val The value of a True Positive. Either a single value or an
#'   unquoted column name (or vector) giving a per-observation value.
#' @param prob_accept Probability of the offer being accepted. Variable cost is only incurred when accepted. Defaults to 1.
#' @param prob_col The unquoted name of the column with probabilities of the event of interest.
#' @param truth_col The unquoted name of the column with the actual outcome/class.
#' @param positive The value in `truth_col` that identifies the event of interest. Defaults to 'Yes'.
#'
#' @return
#' A one-row data frame with the following columns:
#'
#' optimal_pct        = share of the population targeted at maximum profit \cr
#' max_profit         = the maximum profit \cr
#' roi_at_optimum     = ROI at the maximum-profit point \cr
#' capture_at_optimum = proportion of all positives captured at that point \cr
#' breakeven_pct      = largest share that can be targeted while staying profitable \cr
#' profit_target_all  = profit if every customer is targeted
#'
#' @export
#' @examples
#' impact_summary(predictions,
#'    fixed_cost = 1000,
#'    var_cost   = 100,
#'    tp_val     = 2000,
#'    prob_col   = Yes,
#'    truth_col  = Churn)

impact_summary <- function(x,
                           fixed_cost = 0,
                           var_cost   = 0,
                           tp_val     = 0,
                           prob_accept = 1,
                           prob_col   = NA,
                           truth_col  = NA,
                           positive   = "Yes") {

  pf <- profit(x, fixed_cost, {{ var_cost }}, {{ tp_val }}, prob_accept = prob_accept,
               prob_col = {{ prob_col }}, truth_col = {{ truth_col }}, positive = positive)
  ro <- roi(x, fixed_cost, {{ var_cost }}, {{ tp_val }}, prob_accept = prob_accept,
            prob_col = {{ prob_col }}, truth_col = {{ truth_col }}, positive = positive)
  g  <- cumulative_gains(x,
                         prob_col = {{ prob_col }}, truth_col = {{ truth_col }}, positive = positive)
  be <- break_even(x, fixed_cost, {{ var_cost }}, {{ tp_val }}, prob_accept = prob_accept,
                   prob_col = {{ prob_col }}, truth_col = {{ truth_col }}, positive = positive)

  opt_row <- be$optimal_row

  dplyr::tibble(
    optimal_pct        = be$optimal_pct,
    max_profit         = be$max_profit,
    roi_at_optimum     = ro$roi[opt_row],
    capture_at_optimum = g$gain[opt_row],
    breakeven_pct      = be$breakeven_pct,
    profit_target_all  = pf$profit[nrow(pf)]
  )
}
