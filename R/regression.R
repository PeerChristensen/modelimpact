#' Value gains for a regression model
#'
#' The gains view for a model that predicts a continuous outcome (for example
#' predicted customer lifetime value, spend, or loss) rather than a class.
#' Cases are ranked from highest to lowest predicted value and the curve shows
#' what proportion of the *total realised value* is captured by targeting the
#' top share of the population. It is the regression analogue of
#' [cumulative_gains()].
#'
#' A concentration coefficient (an accuracy ratio, often called a Gini) is
#' attached to the result as the attribute `"gini"`. It compares the model's
#' ranking to a perfect (oracle) ranking that sorts by the realised value: 1
#' means the model orders cases as well as an oracle, 0 means it is no better
#' than random, and negative values mean it is worse than random.
#'
#' @param x A data frame containing a model's predicted value and the realised value.
#' @param pred_col The unquoted name of the column with the model's predicted value.
#' @param value_col The unquoted name of the column with the realised value.
#'   Values are assumed to be non-negative.
#' @param ci Add bootstrap confidence bands? When `TRUE`, the returned data
#'   frame gains `.lower` and `.upper` columns and `autoplot()` draws a ribbon.
#'   Defaults to `FALSE`.
#' @param n_boot Number of bootstrap resamples used when `ci = TRUE`. Defaults to 1000.
#' @param conf_level Width of the confidence band when `ci = TRUE`. Defaults to 0.95.
#'
#' @return
#' A data frame (class `mi_value_gains`) with the columns:
#'
#' row       = row numbers \cr
#' prop_pop  = proportion of the population targeted (row / n) \cr
#' cum_value = cumulated realised value captured \cr
#' gain      = proportion of all realised value captured \cr
#' baseline  = expected gain from random targeting (equal to prop_pop)
#'
#' The concentration coefficient is available as `attr(result, "gini")`.
#'
#' @export
#' @examples
#' df <- data.frame(pred = c(9, 7, 5, 3, 1), value = c(100, 80, 20, 40, 5))
#' vg <- value_gains(df, pred_col = pred, value_col = value)
#' attr(vg, "gini")
value_gains <- function(x,
                        pred_col   = NA,
                        value_col  = NA,
                        ci         = FALSE,
                        n_boot     = 1000,
                        conf_level = 0.95) {

  out <- x %>%
    dplyr::arrange(dplyr::desc({{ pred_col }})) %>%
    dplyr::mutate(row = dplyr::row_number()) %>%
    dplyr::mutate(prop_pop = row / dplyr::n()) %>%
    dplyr::mutate(.val = ({{ value_col }}) + 0) %>%
    dplyr::mutate(cum_value = cumsum(.val)) %>%
    dplyr::mutate(gain = cum_value / sum(.val)) %>%
    dplyr::mutate(baseline = prop_pop) %>%
    dplyr::select(row, prop_pop, cum_value, gain, baseline) %>%
    structure(class = c("mi_value_gains", "tbl_df", "tbl", "data.frame"))

  attr(out, "gini") <- .value_gini(dplyr::pull(x, {{ pred_col }}),
                                   dplyr::pull(x, {{ value_col }}))

  if (isTRUE(ci)) {
    band <- .ci_band(x, n_boot, conf_level, fun = function(d) {
      value_gains(d, pred_col = {{ pred_col }}, value_col = {{ value_col }})$gain
    })
    out$.lower <- band$lower
    out$.upper <- band$upper
  }

  out
}

#' Profit for a regression model
#'
#' The profit view for a model that predicts a continuous outcome. Cases are
#' ranked from highest to lowest predicted value and, as more are targeted, the
#' realised value they bring in (`value_col`) is accumulated and the cost of
#' targeting them subtracted. It is the regression analogue of [profit()], and
#' returns an object that plots the same way (via `autoplot()` / [plot_profit()]).
#'
#' @param x A data frame containing a model's predicted value and the realised value.
#' @param fixed_cost Fixed cost (e.g. of a campaign).
#' @param var_cost Variable cost per targeted case. Either a single value or an
#'   unquoted column name (or vector) giving a per-observation cost.
#' @param prob_accept Probability of the offer being accepted. Variable cost is only incurred when accepted. Defaults to 1.
#' @param pred_col The unquoted name of the column with the model's predicted value.
#' @param value_col The unquoted name of the column with the realised value.
#' @param ci Add bootstrap confidence bands? When `TRUE`, the returned data
#'   frame gains `.lower` and `.upper` columns and `autoplot()` draws a ribbon.
#'   Defaults to `FALSE`.
#' @param n_boot Number of bootstrap resamples used when `ci = TRUE`. Defaults to 1000.
#' @param conf_level Width of the confidence band when `ci = TRUE`. Defaults to 0.95.
#'
#' @return
#' A data frame (class `mi_profit`) with the columns:
#'
#' row    = row numbers \cr
#' pct    = percentiles \cr
#' profit = profit for the number of rows selected
#'
#' @export
#' @examples
#' df <- data.frame(pred = c(9, 7, 5, 3, 1), value = c(100, 80, 20, 40, 5))
#' value_profit(df, var_cost = 10, pred_col = pred, value_col = value)
value_profit <- function(x,
                         fixed_cost  = 0,
                         var_cost    = 0,
                         prob_accept = 1,
                         pred_col    = NA,
                         value_col   = NA,
                         ci          = FALSE,
                         n_boot      = 1000,
                         conf_level  = 0.95) {

  out <- x %>%
    dplyr::arrange(dplyr::desc({{ pred_col }})) %>%
    dplyr::mutate(row = dplyr::row_number()) %>%
    dplyr::mutate(pct = dplyr::ntile(row, 100)) %>%
    dplyr::mutate(cost = ({{ var_cost }}) * prob_accept) %>%
    dplyr::mutate(cost_sum = cumsum(cost) + fixed_cost) %>%
    dplyr::mutate(rev = ({{ value_col }}) + 0) %>%
    dplyr::mutate(cum_rev = cumsum(rev)) %>%
    dplyr::mutate(profit = cum_rev - cost_sum) %>%
    dplyr::select(row, pct, profit) %>%
    structure(class = c("mi_profit", "tbl_df", "tbl", "data.frame"))

  if (isTRUE(ci)) {
    band <- .ci_band(x, n_boot, conf_level, fun = function(d) {
      value_profit(d, fixed_cost, {{ var_cost }}, prob_accept = prob_accept,
                   pred_col = {{ pred_col }}, value_col = {{ value_col }})$profit
    })
    out$.lower <- band$lower
    out$.upper <- band$upper
  }

  out
}

# internal: concentration coefficient (accuracy ratio / Gini) comparing the
# model's ranking of cases to a perfect ranking by realised value.
.value_gini <- function(pred, value) {
  n   <- length(value)
  tot <- sum(value)
  if (n < 2 || tot == 0) return(NA_real_)

  gm <- cumsum(value[order(pred,  decreasing = TRUE)]) / tot
  go <- cumsum(value[order(value, decreasing = TRUE)]) / tot

  xx <- seq_len(n) / n
  # area under a gains curve (including the origin) via the trapezoid rule
  auc <- function(g) {
    gg <- c(0, g)
    xg <- c(0, xx)
    sum((gg[-1] + gg[-length(gg)]) / 2 * diff(xg))
  }

  denom <- auc(go) - 0.5
  if (abs(denom) < .Machine$double.eps^0.5) return(NA_real_)

  (auc(gm) - 0.5) / denom
}
