#' Tornado sensitivity analysis of maximum profit
#'
#' Varies each cost/value assumption up and down by a fixed fraction, one at a
#' time, and records how the maximum achievable profit (the peak of the
#' [profit()] curve) responds. The result feeds a tornado plot, ordering the
#' assumptions by how much they move the bottom line.
#'
#' @param x A data frame containing predicted probabilities of a target event and the actual outcome/class.
#' @param fixed_cost Baseline fixed cost.
#' @param var_cost Baseline variable cost per targeted customer.
#' @param tp_val Baseline average value of a True Positive.
#' @param prob_col The unquoted name of the column with probabilities of the event of interest.
#' @param truth_col The unquoted name of the column with the actual outcome/class.
#' @param positive The value in `truth_col` that identifies the event of interest. Defaults to 'Yes'.
#' @param variation Fraction by which each parameter is moved up and down. Defaults to 0.2 (+/- 20 percent).
#'
#' @return
#' A data frame, one row per parameter, ordered by descending `swing`:
#'
#' parameter    = the assumption varied \cr
#' low_value    = parameter value on the low side \cr
#' high_value   = parameter value on the high side \cr
#' low_profit   = maximum profit at the low value \cr
#' high_profit  = maximum profit at the high value \cr
#' base_profit  = maximum profit at the baseline values \cr
#' swing        = absolute difference between low_profit and high_profit
#'
#' @export
#' @examples
#' tornado(predictions,
#'    fixed_cost = 1000,
#'    var_cost   = 100,
#'    tp_val     = 2000,
#'    prob_col   = Yes,
#'    truth_col  = Churn)

tornado <- function(x,
                    fixed_cost = 0,
                    var_cost   = 0,
                    tp_val     = 0,
                    prob_col   = NA,
                    truth_col  = NA,
                    positive   = "Yes",
                    variation  = 0.2) {

  max_profit <- function(fc, vc, tv) {
    max(profit(x,
               fixed_cost = fc,
               var_cost   = vc,
               tp_val     = tv,
               prob_col   = {{ prob_col }},
               truth_col  = {{ truth_col }},
               positive   = positive)$profit)
  }

  base <- max_profit(fixed_cost, var_cost, tp_val)

  spec <- list(
    fixed_cost = c(fixed_cost * (1 - variation), fixed_cost * (1 + variation)),
    var_cost   = c(var_cost   * (1 - variation), var_cost   * (1 + variation)),
    tp_val     = c(tp_val     * (1 - variation), tp_val     * (1 + variation))
  )

  rows <- lapply(names(spec), function(p) {
    lo <- spec[[p]][1]
    hi <- spec[[p]][2]
    low_profit  <- switch(p,
                          fixed_cost = max_profit(lo, var_cost, tp_val),
                          var_cost   = max_profit(fixed_cost, lo, tp_val),
                          tp_val     = max_profit(fixed_cost, var_cost, lo))
    high_profit <- switch(p,
                          fixed_cost = max_profit(hi, var_cost, tp_val),
                          var_cost   = max_profit(fixed_cost, hi, tp_val),
                          tp_val     = max_profit(fixed_cost, var_cost, hi))
    dplyr::tibble(parameter   = p,
                  low_value   = lo,
                  high_value  = hi,
                  low_profit  = low_profit,
                  high_profit = high_profit,
                  base_profit = base,
                  swing       = abs(high_profit - low_profit))
  })

  dplyr::bind_rows(rows) %>%
    dplyr::arrange(dplyr::desc(swing))
}
