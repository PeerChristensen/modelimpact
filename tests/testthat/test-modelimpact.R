test_that("profit is internally consistent and identifies the known optimum", {
  p <- profit(predictions, fixed_cost = 1000, var_cost = 100, tp_val = 2000,
              prob_col = Yes, truth_col = Churn)
  expect_named(p, c("row", "pct", "profit"))
  expect_equal(nrow(p), nrow(predictions))
  expect_equal(max(p$profit), 70600)
})

test_that("profit, cost_revenue and roi reconcile with each other", {
  cr <- cost_revenue(predictions, 1000, 100, 2000, prob_col = Yes, truth_col = Churn)
  p  <- profit(predictions, 1000, 100, 2000, prob_col = Yes, truth_col = Churn)
  ro <- roi(predictions, 1000, 100, 2000, prob_col = Yes, truth_col = Churn)
  expect_equal(cr$cum_rev - cr$cost_sum, p$profit)
  expect_equal((ro$cum_rev - ro$cost_sum) / ro$cost_sum, ro$roi)
})

test_that("positive class is configurable and backward compatible", {
  p1 <- profit(predictions, 1000, 100, 2000, prob_col = Yes, truth_col = Churn)
  d2 <- predictions
  d2$Churn2 <- ifelse(d2$Churn == "Yes", "1", "0")
  p2 <- profit(d2, 1000, 100, 2000, prob_col = Yes, truth_col = Churn2, positive = "1")
  expect_equal(p1$profit, p2$profit)
})

test_that("profit_thresholds uses fp_val", {
  a <- profit_thresholds(predictions, var_cost = 100, prob_accept = .7, tp_val = 2000,
                         fp_val = 0, tn_val = 0, fn_val = -2000, prob_col = Yes, truth_col = Churn)
  b <- profit_thresholds(predictions, var_cost = 100, prob_accept = .7, tp_val = 2000,
                         fp_val = -500, tn_val = 0, fn_val = -2000, prob_col = Yes, truth_col = Churn)
  expect_false(isTRUE(all.equal(a$payoff, b$payoff)))
})

test_that("cumulative_gains and lift converge correctly", {
  g <- cumulative_gains(predictions, prob_col = Yes, truth_col = Churn)
  l <- lift_curve(predictions, prob_col = Yes, truth_col = Churn)
  expect_equal(max(g$cum_events), sum(predictions$Churn == "Yes"))
  expect_equal(tail(g$gain, 1), 1)
  expect_equal(tail(l$lift, 1), 1)
})

test_that("marginal_profit cumulates to the profit at target-all", {
  m <- marginal_profit(predictions, 1000, 100, 2000, bins = 10, prob_col = Yes, truth_col = Churn)
  p <- profit(predictions, 1000, 100, 2000, prob_col = Yes, truth_col = Churn)
  expect_equal(tail(m$cum_profit, 1), tail(p$profit, 1))
})

test_that("break_even reports the optimum and a valid break-even point", {
  be <- break_even(predictions, 1000, 100, 2000, prob_col = Yes, truth_col = Churn)
  expect_equal(be$max_profit, 70600)
  expect_true(be$breakeven_row >= be$optimal_row)
})

test_that("confusion_payoff reconciles with profit_thresholds", {
  cp <- confusion_payoff(predictions, threshold = 0.3, var_cost = 100, prob_accept = .7,
                         tp_val = 2000, fp_val = 0, tn_val = 0, fn_val = -2000,
                         prob_col = Yes, truth_col = Churn)
  th <- profit_thresholds(predictions, var_cost = 100, prob_accept = .7, tp_val = 2000,
                          fp_val = 0, tn_val = 0, fn_val = -2000, prob_col = Yes, truth_col = Churn)
  expect_equal(cp$tp + cp$fp + cp$tn + cp$fn, nrow(predictions))
  expect_equal(cp$payoff, th$payoff[th$threshold == 0.3])
})

test_that("roc_pr reaches the (1, 1) corner", {
  rp <- roc_pr(predictions, prob_col = Yes, truth_col = Churn)
  expect_equal(tail(rp$tpr, 1), 1)
  expect_equal(tail(rp$fpr, 1), 1)
})

test_that("rank-based profit reconciles with profit_thresholds under prob_accept", {
  p  <- profit(predictions, 0, 100, 2000, prob_accept = .7, prob_col = Yes, truth_col = Churn)
  th <- profit_thresholds(predictions, var_cost = 100, prob_accept = .7, tp_val = 2000,
                          fp_val = 0, tn_val = 0, fn_val = 0, prob_col = Yes, truth_col = Churn)
  k <- sum(predictions$Yes > 0.3)
  expect_equal(p$profit[k], th$payoff[th$threshold == 0.3])
})

test_that("compare_models ranks the informative model above its inverse", {
  cm <- compare_models(predictions, prob_cols = c("Yes", "No"), truth_col = Churn, metric = "gains")
  at20 <- cm[abs(cm$prop_pop - 0.2) < 0.001, ]
  yes <- max(at20$value[at20$model == "Yes"])
  no  <- max(at20$value[at20$model == "No"])
  expect_gt(yes, no)
})

test_that("plot helpers return ggplot objects when ggplot2 is available", {
  skip_if_not_installed("ggplot2")
  p <- profit(predictions, 1000, 100, 2000, prob_col = Yes, truth_col = Churn)
  expect_s3_class(plot_profit(p), "ggplot")
  g <- cumulative_gains(predictions, prob_col = Yes, truth_col = Churn)
  expect_s3_class(plot_gains(g), "ggplot")
})
