#' Tidy and glance methods for modelimpact results
#'
#' Every analysis function returns a classed data frame. These methods let those
#' results slot into tidyverse / tidymodels workflows:
#'
#' * `tidy()` returns the result as a plain tibble (dropping the modelimpact
#'   class), which is useful when passing the curve on to other tidy tools.
#' * `glance()` returns a one-row summary of the headline numbers for the
#'   objects where that makes sense (`profit()`, `profit_thresholds()`,
#'   `budget_frontier()`, `value_gains()` and `qini_curve()`).
#'
#' @param x A modelimpact result object.
#' @param ... Unused, for generic compatibility.
#'
#' @return `tidy()` returns a tibble; `glance()` returns a one-row tibble.
#' @name modelimpact-tidiers
#'
#' @examples
#' p <- profit(predictions, fixed_cost = 1000, var_cost = 100, tp_val = 2000,
#'             prob_col = Yes, truth_col = Churn)
#' tidy(p)
#' glance(p)
NULL

#' @importFrom generics tidy
#' @export
generics::tidy

#' @importFrom generics glance
#' @export
generics::glance

# strip the mi_* class so the object behaves as a plain tibble
.mi_tidy <- function(x) {
  class(x) <- class(x)[!grepl("^mi_", class(x))]
  attr(x, "gini")   <- NULL
  attr(x, "auuc")   <- NULL
  attr(x, "metric") <- NULL
  x
}

#' @rdname modelimpact-tidiers
#' @export
tidy.mi_profit <- function(x, ...) .mi_tidy(x)
#' @rdname modelimpact-tidiers
#' @export
tidy.mi_roi <- function(x, ...) .mi_tidy(x)
#' @rdname modelimpact-tidiers
#' @export
tidy.mi_cost_revenue <- function(x, ...) .mi_tidy(x)
#' @rdname modelimpact-tidiers
#' @export
tidy.mi_gains <- function(x, ...) .mi_tidy(x)
#' @rdname modelimpact-tidiers
#' @export
tidy.mi_lift <- function(x, ...) .mi_tidy(x)
#' @rdname modelimpact-tidiers
#' @export
tidy.mi_marginal <- function(x, ...) .mi_tidy(x)
#' @rdname modelimpact-tidiers
#' @export
tidy.mi_thresholds <- function(x, ...) .mi_tidy(x)
#' @rdname modelimpact-tidiers
#' @export
tidy.mi_bootstrap <- function(x, ...) .mi_tidy(x)
#' @rdname modelimpact-tidiers
#' @export
tidy.mi_budget <- function(x, ...) .mi_tidy(x)
#' @rdname modelimpact-tidiers
#' @export
tidy.mi_value_gains <- function(x, ...) .mi_tidy(x)
#' @rdname modelimpact-tidiers
#' @export
tidy.mi_qini <- function(x, ...) .mi_tidy(x)
#' @rdname modelimpact-tidiers
#' @export
tidy.mi_compare <- function(x, ...) .mi_tidy(x)
#' @rdname modelimpact-tidiers
#' @export
tidy.mi_roc <- function(x, ...) .mi_tidy(x)

#' @rdname modelimpact-tidiers
#' @export
glance.mi_profit <- function(x, ...) {
  n   <- nrow(x)
  opt <- which.max(x$profit)
  dplyr::tibble(optimal_row       = opt,
                optimal_pct       = opt / n * 100,
                max_profit        = x$profit[opt],
                profit_target_all = x$profit[n])
}

#' @rdname modelimpact-tidiers
#' @export
glance.mi_thresholds <- function(x, ...) {
  opt <- which.max(x$payoff)
  dplyr::tibble(best_threshold = x$threshold[opt],
                best_payoff    = x$payoff[opt])
}

#' @rdname modelimpact-tidiers
#' @export
glance.mi_budget <- function(x, ...) {
  opt <- which.max(x$profit)
  dplyr::tibble(max_profit    = x$profit[opt],
                budget_at_max = x$budget[opt])
}

#' @rdname modelimpact-tidiers
#' @export
glance.mi_value_gains <- function(x, ...) {
  dplyr::tibble(gini = attr(x, "gini"))
}

#' @rdname modelimpact-tidiers
#' @export
glance.mi_qini <- function(x, ...) {
  dplyr::tibble(qini = attr(x, "qini"),
                auuc = attr(x, "auuc"))
}
