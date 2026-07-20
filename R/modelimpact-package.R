#' modelimpact: assess the business impact of classification models
#'
#' Calculate the financial impact of using a classification model to
#' prioritise or target cases in terms of cost, revenue, profit, ROI, gains,
#' lift and threshold-based payoff, and visualise the results. Although the
#' bundled examples use customer churn, the same tools apply to many other
#' binary classification problems, such as fraud detection, credit default,
#' lead scoring and marketing response, upsell and cross-sell, and predictive
#' maintenance.
#'
#' @keywords internal
"_PACKAGE"

# Quiet R CMD check notes about non-standard evaluation (bare column names used
# inside dplyr and ggplot2 pipelines).
globalVariables(c(
  ".data",
  "fixed_cost", "var_cost", "tp_val", "prob_accept", "prob_col", "truth_col",
  "positive", "pct", "cost", "cost_sum", "cum_rev", "rev", "preds", "profit",
  "roi", "row", "prop_pop", "event", "cum_events", "gain", "baseline", "lift",
  "bin", "n", "events", "revenue", "marginal_profit", "cum_profit",
  "threshold", "payoff", "tp", "fp", "tn", "fn", "tpr", "fpr", "precision",
  "recall", "model", "value", "swing", "high_profit", "low_profit",
  ".row_cost", ".row_rev", ".val", "cum_value", ".c", "uplift", "baseline"
))
