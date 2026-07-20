#' Plot modelimpact results
#'
#' Every analysis function in the package returns a classed data frame that can
#' be visualised directly with \code{\link[ggplot2]{autoplot}()}, for example
#' \code{autoplot(profit(...))}. The `plot_*()` functions are thin,
#' back-compatible wrappers around the corresponding `autoplot()` method.
#' \pkg{ggplot2} is only required when a plot is actually drawn.
#'
#' @param object,data The data frame returned by the matching modelimpact
#'   function (e.g. the output of [profit()] for `autoplot()` / `plot_profit()`).
#' @param slope For ROC objects only: optional slope of an iso-profit line. When
#'   supplied, the cost-sensitive optimal operating point (maximising
#'   `tpr - slope * fpr`) is highlighted. The business-optimal slope equals
#'   `(cost_fp / value_fn) * (n_neg / n_pos)`.
#' @param ... Unused, for S3 compatibility.
#'
#' @return A `ggplot` object.
#' @name modelimpact-plots
NULL

# internal: fail gracefully if ggplot2 is not installed
.check_ggplot <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting. ",
         "Install it with install.packages('ggplot2').", call. = FALSE)
  }
}

# small helper (avoids importing rlang's %||%)
`%||%` <- function(a, b) if (is.null(a)) b else a

# internal: format monetary axis labels as plain numbers (no scientific notation)
.money_labels <- function(x) format(x, scientific = FALSE, big.mark = ",", trim = TRUE)

# ---- autoplot methods -------------------------------------------------------

#' @rdname modelimpact-plots
autoplot.mi_profit <- function(object, ...) {
  .check_ggplot()
  object$pct_pop <- object$row / max(object$row) * 100
  mp <- object[which.max(object$profit), ]
  ggplot2::ggplot(object, ggplot2::aes(x = .data$pct_pop, y = .data$profit)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_vline(xintercept = mp$pct_pop, linetype = "dashed") +
    ggplot2::geom_line(colour = "darkred", linewidth = 1) +
    ggplot2::scale_y_continuous(labels = .money_labels) +
    ggplot2::labs(x = "% targeted", y = "Profit")
}

#' @rdname modelimpact-plots
autoplot.mi_cost_revenue <- function(object, ...) {
  .check_ggplot()
  object$pct_pop <- object$row / max(object$row) * 100
  ggplot2::ggplot(object, ggplot2::aes(x = .data$pct_pop)) +
    ggplot2::geom_line(ggplot2::aes(y = .data$cost_sum), linetype = "dashed") +
    ggplot2::geom_line(ggplot2::aes(y = .data$cum_rev), colour = "darkred", linewidth = 1) +
    ggplot2::scale_y_continuous(labels = .money_labels) +
    ggplot2::labs(x = "% targeted", y = "Costs & revenue")
}

#' @rdname modelimpact-plots
autoplot.mi_roi <- function(object, ...) {
  .check_ggplot()
  object$pct_pop <- object$row / max(object$row) * 100
  ggplot2::ggplot(object, ggplot2::aes(x = .data$pct_pop, y = .data$roi)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_line(colour = "darkred", linewidth = 1) +
    ggplot2::labs(x = "% targeted", y = "ROI")
}

#' @rdname modelimpact-plots
autoplot.mi_gains <- function(object, ...) {
  .check_ggplot()
  ggplot2::ggplot(object, ggplot2::aes(x = .data$prop_pop, y = .data$gain)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    ggplot2::geom_line(colour = "darkred", linewidth = 1) +
    ggplot2::labs(x = "Proportion targeted", y = "Proportion of positives captured")
}

#' @rdname modelimpact-plots
autoplot.mi_lift <- function(object, ...) {
  .check_ggplot()
  ggplot2::ggplot(object, ggplot2::aes(x = .data$prop_pop, y = .data$lift)) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed") +
    ggplot2::geom_line(colour = "darkred", linewidth = 1) +
    ggplot2::labs(x = "Proportion targeted", y = "Lift")
}

#' @rdname modelimpact-plots
autoplot.mi_marginal <- function(object, ...) {
  .check_ggplot()
  ggplot2::ggplot(object, ggplot2::aes(x = factor(.data$bin), y = .data$marginal_profit)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_col(fill = "darkred") +
    ggplot2::scale_y_continuous(labels = .money_labels) +
    ggplot2::labs(x = "Bin (1 = highest probability)", y = "Marginal profit")
}

#' @rdname modelimpact-plots
autoplot.mi_thresholds <- function(object, ...) {
  .check_ggplot()
  opt <- object[which.max(object$payoff), ]
  ggplot2::ggplot(object, ggplot2::aes(x = .data$threshold, y = .data$payoff)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_vline(xintercept = opt$threshold, linetype = "dashed") +
    ggplot2::geom_line(colour = "darkred", linewidth = 1) +
    ggplot2::scale_y_continuous(labels = .money_labels) +
    ggplot2::labs(x = "Threshold", y = "Payoff")
}

#' @rdname modelimpact-plots
autoplot.mi_roc <- function(object, slope = NULL, ...) {
  .check_ggplot()
  p <- ggplot2::ggplot(object, ggplot2::aes(x = .data$fpr, y = .data$tpr)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    ggplot2::geom_line(colour = "darkred", linewidth = 1) +
    ggplot2::labs(x = "False positive rate", y = "True positive rate")

  if (!is.null(slope)) {
    j   <- object$tpr - slope * object$fpr
    opt <- object[which.max(j), ]
    p <- p +
      ggplot2::geom_abline(slope = slope,
                           intercept = opt$tpr - slope * opt$fpr,
                           linetype = "dotted") +
      ggplot2::geom_point(data = opt,
                          ggplot2::aes(x = .data$fpr, y = .data$tpr),
                          size = 3, colour = "black")
  }
  p
}

#' @rdname modelimpact-plots
autoplot.mi_compare <- function(object, ...) {
  .check_ggplot()
  metric <- attr(object, "metric")
  ylab <- switch(metric %||% "value",
                 gains  = "Proportion of positives captured",
                 profit = "Profit",
                 lift   = "Lift",
                 roi    = "ROI",
                 "value")
  # colour models in the order given to compare_models(): first = red, second =
  # blue, then a fallback palette for any further models.
  models <- unique(object$model)
  pal <- rep(c("darkred", "blue", "darkgreen", "orange", "purple"),
             length.out = length(models))
  names(pal) <- models
  object$model <- factor(object$model, levels = models)
  p <- ggplot2::ggplot(object, ggplot2::aes(x = .data$prop_pop, y = .data$value,
                                            colour = .data$model)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_colour_manual(values = pal) +
    ggplot2::labs(x = "Proportion targeted", y = ylab, colour = "Model")
  if (identical(metric, "gains")) {
    p <- p + ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed")
  }
  p
}

#' @rdname modelimpact-plots
autoplot.mi_bootstrap <- function(object, ...) {
  .check_ggplot()
  ggplot2::ggplot(object, ggplot2::aes(x = .data$prop_pop)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
                         fill = "darkred", alpha = 0.2) +
    ggplot2::geom_line(ggplot2::aes(y = .data$median), colour = "darkred", linewidth = 1) +
    ggplot2::scale_y_continuous(labels = .money_labels) +
    ggplot2::labs(x = "Proportion targeted", y = "Profit")
}

#' @rdname modelimpact-plots
autoplot.mi_budget <- function(object, ...) {
  .check_ggplot()
  ggplot2::ggplot(object, ggplot2::aes(x = .data$budget, y = .data$profit)) +
    ggplot2::geom_hline(yintercept = max(object$profit), linetype = "dashed") +
    ggplot2::geom_line(colour = "darkred", linewidth = 1) +
    ggplot2::scale_x_continuous(labels = .money_labels) +
    ggplot2::scale_y_continuous(labels = .money_labels) +
    ggplot2::labs(x = "Budget", y = "Best achievable profit")
}

# ---- back-compatible plot_*() wrappers --------------------------------------

#' @rdname modelimpact-plots
#' @export
plot_profit <- function(data) {
  .check_ggplot(); ggplot2::autoplot(data)
}

#' @rdname modelimpact-plots
#' @export
plot_cost_revenue <- function(data) {
  .check_ggplot(); ggplot2::autoplot(data)
}

#' @rdname modelimpact-plots
#' @export
plot_roi <- function(data) {
  .check_ggplot(); ggplot2::autoplot(data)
}

#' @rdname modelimpact-plots
#' @export
plot_gains <- function(data) {
  .check_ggplot(); ggplot2::autoplot(data)
}

#' @rdname modelimpact-plots
#' @export
plot_lift <- function(data) {
  .check_ggplot(); ggplot2::autoplot(data)
}

#' @rdname modelimpact-plots
#' @export
plot_marginal <- function(data) {
  .check_ggplot(); ggplot2::autoplot(data)
}

#' @rdname modelimpact-plots
#' @export
plot_thresholds <- function(data) {
  .check_ggplot(); ggplot2::autoplot(data)
}

#' @rdname modelimpact-plots
#' @export
plot_roc <- function(data, slope = NULL) {
  .check_ggplot(); ggplot2::autoplot(data, slope = slope)
}

#' @rdname modelimpact-plots
#' @export
plot_budget <- function(data) {
  .check_ggplot(); ggplot2::autoplot(data)
}
