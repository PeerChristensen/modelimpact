#' Qini (uplift) curve
#'
#' For an *uplift* (a.k.a. treatment-effect) model, targeting the customers most
#' likely to respond is not the same as targeting those most likely to respond
#' *because* they were treated. The Qini curve evaluates a model that scores the
#' predicted treatment effect using data from a treated / control experiment.
#' Cases are ranked from highest to lowest predicted uplift and the curve shows
#' the cumulative *incremental* number of positive outcomes attributable to
#' treatment as more of the population is targeted.
#'
#' At the top `k` of the ranking the incremental response is estimated as
#' `R_t - R_c * (N_t / N_c)`, where `R_t`, `R_c` are the positive outcomes and
#' `N_t`, `N_c` the counts in the treated and control groups respectively. The
#' dashed reference line is random targeting. The area between the curve and
#' that line (the **Qini coefficient**) and the area under the curve (**AUUC**)
#' are attached as the attributes `"qini"` and `"auuc"`.
#'
#' @param x A data frame with one row per case, containing the predicted uplift,
#'   the treatment indicator and the observed outcome.
#' @param uplift_col The unquoted name of the column with the predicted uplift score.
#' @param treatment_col The unquoted name of the treatment-indicator column.
#' @param outcome_col The unquoted name of the observed-outcome column.
#' @param positive The value in `outcome_col` that identifies the event of interest. Defaults to 'Yes'.
#' @param treated The value in `treatment_col` that identifies the treated group. Defaults to 1.
#' @param ci Add bootstrap confidence bands? When `TRUE`, the returned data
#'   frame gains `.lower` and `.upper` columns and `autoplot()` draws a ribbon.
#'   Defaults to `FALSE`.
#' @param n_boot Number of bootstrap resamples used when `ci = TRUE`. Defaults to 1000.
#' @param conf_level Width of the confidence band when `ci = TRUE`. Defaults to 0.95.
#'
#' @return
#' A data frame (class `mi_qini`) with the columns:
#'
#' row      = row numbers \cr
#' prop_pop = proportion of the population targeted (row / n) \cr
#' uplift   = cumulative incremental positive outcomes \cr
#' baseline = the random-targeting reference line
#'
#' The Qini coefficient and AUUC are available as `attr(result, "qini")` and
#' `attr(result, "auuc")`.
#'
#' @export
#' @examples
#' set.seed(1)
#' n <- 1000
#' treat <- rbinom(n, 1, 0.5)
#' u <- runif(n)
#' y <- rbinom(n, 1, pmin(0.2 + treat * 0.5 * u, 1))
#' df <- data.frame(score = u, treat = treat, y = ifelse(y == 1, "Yes", "No"))
#' q <- qini_curve(df, uplift_col = score, treatment_col = treat, outcome_col = y)
#' attr(q, "qini")
qini_curve <- function(x,
                       uplift_col    = NA,
                       treatment_col = NA,
                       outcome_col   = NA,
                       positive      = "Yes",
                       treated       = 1,
                       ci            = FALSE,
                       n_boot        = 1000,
                       conf_level    = 0.95) {

  ranked <- x %>% dplyr::arrange(dplyr::desc({{ uplift_col }}))
  t <- as.integer(dplyr::pull(ranked, {{ treatment_col }}) == treated)
  y <- as.integer(dplyr::pull(ranked, {{ outcome_col }}) == positive)
  n <- length(t)

  Nt <- cumsum(t)
  Nc <- cumsum(1L - t)
  Rt <- cumsum(y * t)
  Rc <- cumsum(y * (1L - t))
  ratio  <- ifelse(Nc > 0, Nt / Nc, 0)
  uplift <- Rt - Rc * ratio

  prop_pop <- seq_len(n) / n
  baseline <- prop_pop * uplift[n]

  out <- dplyr::tibble(row      = seq_len(n),
                       prop_pop = prop_pop,
                       uplift   = uplift,
                       baseline = baseline) %>%
    structure(class = c("mi_qini", "tbl_df", "tbl", "data.frame"))

  attr(out, "qini") <- .auc_xy(prop_pop, uplift) - .auc_xy(prop_pop, baseline)
  attr(out, "auuc") <- .auc_xy(prop_pop, uplift)

  if (isTRUE(ci)) {
    band <- .ci_band(x, n_boot, conf_level, fun = function(d) {
      qini_curve(d, uplift_col = {{ uplift_col }}, treatment_col = {{ treatment_col }},
                 outcome_col = {{ outcome_col }}, positive = positive,
                 treated = treated)$uplift
    })
    out$.lower <- band$lower
    out$.upper <- band$upper
  }

  out
}

#' Profit of an uplift campaign
#'
#' Translates a Qini/uplift ranking into money. Cases are ranked from highest to
#' lowest predicted uplift and, as more are targeted, the estimated incremental
#' positive outcomes (see [qini_curve()]) are valued at `tp_val` and the cost of
#' treating them is subtracted. It answers *"how many of the most persuadable
#' cases should we treat to maximise incremental profit?"* and returns an object
#' that plots the same way as [profit()].
#'
#' @param x A data frame with one row per case, containing the predicted uplift,
#'   the treatment indicator and the observed outcome.
#' @param fixed_cost Fixed cost (e.g. of a campaign).
#' @param var_cost Variable cost per treated case. Either a single value or an
#'   unquoted column name (or vector) giving a per-observation cost.
#' @param tp_val The value of one incremental positive outcome.
#' @param prob_accept Probability of the offer being accepted. Variable cost is only incurred when accepted. Defaults to 1.
#' @param uplift_col The unquoted name of the column with the predicted uplift score.
#' @param treatment_col The unquoted name of the treatment-indicator column.
#' @param outcome_col The unquoted name of the observed-outcome column.
#' @param positive The value in `outcome_col` that identifies the event of interest. Defaults to 'Yes'.
#' @param treated The value in `treatment_col` that identifies the treated group. Defaults to 1.
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
#' profit = incremental profit for the number of cases targeted
#'
#' @export
#' @examples
#' set.seed(1)
#' n <- 1000
#' treat <- rbinom(n, 1, 0.5)
#' u <- runif(n)
#' y <- rbinom(n, 1, pmin(0.2 + treat * 0.5 * u, 1))
#' df <- data.frame(score = u, treat = treat, y = ifelse(y == 1, "Yes", "No"))
#' uplift_profit(df, var_cost = 1, tp_val = 100,
#'    uplift_col = score, treatment_col = treat, outcome_col = y)
uplift_profit <- function(x,
                          fixed_cost    = 0,
                          var_cost      = 0,
                          tp_val        = 0,
                          prob_accept   = 1,
                          uplift_col    = NA,
                          treatment_col = NA,
                          outcome_col   = NA,
                          positive      = "Yes",
                          treated       = 1,
                          ci            = FALSE,
                          n_boot        = 1000,
                          conf_level    = 0.95) {

  q <- qini_curve(x, uplift_col = {{ uplift_col }}, treatment_col = {{ treatment_col }},
                  outcome_col = {{ outcome_col }}, positive = positive, treated = treated)

  cost_sum <- x %>%
    dplyr::arrange(dplyr::desc({{ uplift_col }})) %>%
    dplyr::mutate(.c = ({{ var_cost }}) * prob_accept) %>%
    dplyr::mutate(cost_sum = cumsum(.c) + fixed_cost) %>%
    dplyr::pull(cost_sum)

  n <- nrow(q)

  out <- dplyr::tibble(row    = seq_len(n),
                       pct    = dplyr::ntile(seq_len(n), 100),
                       profit = q$uplift * tp_val - cost_sum) %>%
    structure(class = c("mi_profit", "tbl_df", "tbl", "data.frame"))

  if (isTRUE(ci)) {
    band <- .ci_band(x, n_boot, conf_level, fun = function(d) {
      uplift_profit(d, fixed_cost, {{ var_cost }}, tp_val, prob_accept = prob_accept,
                    uplift_col = {{ uplift_col }}, treatment_col = {{ treatment_col }},
                    outcome_col = {{ outcome_col }}, positive = positive,
                    treated = treated)$profit
    })
    out$.lower <- band$lower
    out$.upper <- band$upper
  }

  out
}

# internal: area under a curve defined by points (xx, g), including the origin.
.auc_xy <- function(xx, g) {
  gg <- c(0, g)
  xg <- c(0, xx)
  sum((gg[-1] + gg[-length(gg)]) / 2 * diff(xg))
}
