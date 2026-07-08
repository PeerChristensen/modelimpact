#' Compare several models on a business-impact metric
#'
#' Computes a targeting curve for two or more models so they can be compared and
#' overlaid on a single plot. Rather than picking a model on AUC alone, this lets
#' you choose the one that produces the most profit (or the steepest gains, lift
#' or ROI) at the share of customers you intend to target.
#'
#' @param x A data frame containing one probability column per model and a shared actual outcome/class.
#' @param prob_cols A character vector of column names holding each model's predicted probabilities.
#' @param truth_col The unquoted name of the column with the actual outcome/class.
#' @param metric The curve to compute: one of "profit", "gains", "lift" or "roi".
#' @param fixed_cost Fixed cost (used by "profit" and "roi").
#' @param var_cost Variable cost per targeted customer (used by "profit" and "roi").
#' @param tp_val The average value of a True Positive (used by "profit" and "roi").
#' @param positive The value in `truth_col` that identifies the event of interest. Defaults to 'Yes'.
#'
#' @return
#' A data frame with the following columns:
#'
#' model    = the model (column) name \cr
#' row      = row numbers \cr
#' prop_pop = proportion of the population targeted (row / n) \cr
#' value    = the chosen metric at that point
#'
#' @export
#' @examples
#' compare_models(predictions,
#'    prob_cols = c("Yes", "No"),
#'    truth_col = Churn,
#'    metric    = "gains")

compare_models <- function(x,
                           prob_cols,
                           truth_col = NA,
                           metric    = c("profit", "gains", "lift", "roi"),
                           fixed_cost = 0,
                           var_cost   = 0,
                           tp_val     = 0,
                           positive   = "Yes") {

  metric <- match.arg(metric)

  curves <- lapply(prob_cols, function(m) {
    x %>%
      dplyr::arrange(dplyr::desc(.data[[m]])) %>%
      dplyr::mutate(row = dplyr::row_number()) %>%
      dplyr::mutate(prop_pop = row / dplyr::n()) %>%
      dplyr::mutate(event = dplyr::if_else({{ truth_col }} == positive, 1, 0)) %>%
      dplyr::mutate(cum_events = cumsum(event)) %>%
      dplyr::mutate(gain = cum_events / sum(event)) %>%
      dplyr::mutate(cost_sum = cumsum(var_cost) + fixed_cost) %>%
      dplyr::mutate(cum_rev = cumsum(event * tp_val)) %>%
      dplyr::mutate(profit = cum_rev - cost_sum) %>%
      dplyr::mutate(roi = (cum_rev - cost_sum) / cost_sum) %>%
      dplyr::mutate(lift = gain / prop_pop) %>%
      dplyr::mutate(model = m,
                    value = switch(metric,
                                   profit = profit,
                                   gains  = gain,
                                   lift   = lift,
                                   roi    = roi)) %>%
      dplyr::select(model, row, prop_pop, value)
  })

  structure(dplyr::bind_rows(curves),
            class  = c("mi_compare", "tbl_df", "tbl", "data.frame"),
            metric = metric)
}
