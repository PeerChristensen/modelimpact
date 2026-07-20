#' Budget-constrained targeting
#'
#' Real campaigns rarely get to target the profit-maximising share of the
#' population; they get a fixed pot of money. `budget_profit()` answers *"given
#' this budget, how many cases can we action, and what is the best profit we can
#' make?"* for a single budget, while [budget_frontier()] sweeps a range of
#' budgets to show how achievable profit grows as the budget increases.
#'
#' Cases are ranked from most to least likely to be a positive, then targeted
#' from the top down until the budget is exhausted. Because doing nothing is
#' always an option, the reported profit is never negative: if no affordable
#' campaign is profitable, the functions report targeting no one (a profit of
#' zero).
#'
#' @param x A data frame containing predicted probabilities of a target event and the actual outcome/class.
#' @param budget The total amount available to spend (fixed plus variable costs).
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
#' `budget_profit()` returns a one-row data frame; [budget_frontier()] returns
#' one row per budget (class `mi_budget`). Both have the columns:
#'
#' budget     = the budget considered \cr
#' n_targeted = number of cases targeted at the best affordable operating point \cr
#' prop_pop   = share of the population targeted (n_targeted / n) \cr
#' cost       = amount actually spent \cr
#' profit     = best profit achievable within the budget \cr
#' roi        = return on investment at that point \cr
#' capture    = proportion of all positives captured at that point
#'
#' @export
#' @examples
#' budget_profit(predictions,
#'    budget     = 50000,
#'    fixed_cost = 1000,
#'    var_cost   = 100,
#'    tp_val     = 2000,
#'    prob_col   = Yes,
#'    truth_col  = Churn)
budget_profit <- function(x,
                          budget,
                          fixed_cost = 0,
                          var_cost   = 0,
                          tp_val     = 0,
                          prob_accept = 1,
                          prob_col   = NA,
                          truth_col  = NA,
                          positive   = "Yes") {

  m <- .mi_rank_metrics(x, fixed_cost, {{ var_cost }}, {{ tp_val }}, prob_accept,
                        {{ prob_col }}, {{ truth_col }}, positive)

  pt <- .budget_point(m$cost_sum, m$profit, m$roi, m$gain, m$n, budget)

  dplyr::tibble(budget     = budget,
                n_targeted = pt$n_targeted,
                prop_pop   = pt$prop_pop,
                cost       = pt$cost,
                profit     = pt$profit,
                roi        = pt$roi,
                capture    = pt$capture)
}

#' @rdname budget_profit
#' @param budgets A numeric vector of budgets to evaluate. When `NULL` (the
#'   default) an evenly spaced sequence from 0 to the cost of targeting everyone
#'   is used.
#' @param n_points Number of budgets in the automatically generated sequence
#'   when `budgets` is `NULL`. Defaults to 50.
#' @export
#' @examples
#' budget_frontier(predictions,
#'    fixed_cost = 1000,
#'    var_cost   = 100,
#'    tp_val     = 2000,
#'    prob_col   = Yes,
#'    truth_col  = Churn)
budget_frontier <- function(x,
                            fixed_cost = 0,
                            var_cost   = 0,
                            tp_val     = 0,
                            prob_accept = 1,
                            prob_col   = NA,
                            truth_col  = NA,
                            positive   = "Yes",
                            budgets    = NULL,
                            n_points   = 50) {

  m <- .mi_rank_metrics(x, fixed_cost, {{ var_cost }}, {{ tp_val }}, prob_accept,
                        {{ prob_col }}, {{ truth_col }}, positive)

  if (is.null(budgets)) {
    budgets <- seq(0, max(m$cost_sum), length.out = n_points)
  }

  rows <- lapply(budgets, function(b) {
    pt <- .budget_point(m$cost_sum, m$profit, m$roi, m$gain, m$n, b)
    dplyr::tibble(budget     = b,
                  n_targeted = pt$n_targeted,
                  prop_pop   = pt$prop_pop,
                  cost       = pt$cost,
                  profit     = pt$profit,
                  roi        = pt$roi,
                  capture    = pt$capture)
  })

  dplyr::bind_rows(rows) %>%
    structure(class = c("mi_budget", "tbl_df", "tbl", "data.frame"))
}

# internal: per-row cumulative cost / profit / roi / capture, all ranked by
# descending predicted probability (reuses the exported building blocks so the
# ordering and economics stay identical).
.mi_rank_metrics <- function(x, fixed_cost, var_cost, tp_val, prob_accept,
                             prob_col, truth_col, positive) {
  p <- profit(x, fixed_cost, {{ var_cost }}, {{ tp_val }}, prob_accept = prob_accept,
              prob_col = {{ prob_col }}, truth_col = {{ truth_col }}, positive = positive)
  r <- roi(x, fixed_cost, {{ var_cost }}, {{ tp_val }}, prob_accept = prob_accept,
           prob_col = {{ prob_col }}, truth_col = {{ truth_col }}, positive = positive)
  g <- cumulative_gains(x, prob_col = {{ prob_col }}, truth_col = {{ truth_col }},
                        positive = positive)

  list(cost_sum = r$cost_sum,
       profit   = p$profit,
       roi      = r$roi,
       gain     = g$gain,
       n        = nrow(p))
}

# internal: best affordable operating point for a single budget. Doing nothing
# (profit 0) is always available, so profit is floored at 0.
.budget_point <- function(cost_sum, profit, roi, gain, n, b) {
  feasible <- cost_sum <= b

  if (!any(feasible) || max(profit[feasible]) <= 0) {
    return(list(n_targeted = 0L, prop_pop = 0, cost = 0,
                profit = 0, roi = NA_real_, capture = 0))
  }

  idx <- which(feasible)
  k   <- idx[which.max(profit[feasible])]

  list(n_targeted = k,
       prop_pop   = k / n,
       cost       = cost_sum[k],
       profit     = profit[k],
       roi        = roi[k],
       capture    = gain[k])
}
