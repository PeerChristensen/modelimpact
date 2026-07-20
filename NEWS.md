
# modelimpact 1.2.0

## New features

* Cost and value arguments now accept **per-observation values**, not just a
  single number. `tp_val` and `var_cost` in `cost_revenue()`, `profit()`,
  `roi()`, `marginal_profit()`, `break_even()` and `impact_summary()`, and the
  per-cell value arguments (`tp_val`, `fp_val`, `tn_val`, `fn_val`) in
  `profit_thresholds()`, may be given as an unquoted column name (or a vector)
  so that each case can carry its own value or cost. A single scalar continues
  to work exactly as before.
* New **budget-constrained targeting** helpers: `budget_profit()` reports the
  best profit achievable for a given budget, and `budget_frontier()` sweeps a
  range of budgets to show how achievable profit grows with spend (with an
  `autoplot()` method and `plot_budget()` wrapper).
* Optional **bootstrap confidence bands** on the ranking curves. `profit()`,
  `roi()`, `cost_revenue()`, `cumulative_gains()` and `lift_curve()` gained
  `ci`, `n_boot` and `conf_level` arguments; with `ci = TRUE` the result gains
  `.lower`/`.upper` columns and `autoplot()` draws a ribbon.
* New support for **regression models** that predict a continuous value:
  `value_gains()` (with a concentration / Gini coefficient) and `value_profit()`
  rank cases by predicted value and report the share of realised value captured
  and the profit of targeting the top share. `value_gains()` has its own
  `autoplot()` method and `plot_value_gains()` wrapper.

## Documentation

* The package is now framed around classification impact in general, with
  customer churn as one example. A new "Beyond churn" vignette works through a
  fraud-detection use case.

# modelimpact 1.1.0

## New features

* New ranking-based views: `cumulative_gains()`, `lift_curve()`,
  `marginal_profit()`, `break_even()` and `impact_summary()`.
* New threshold / classification views: `confusion_payoff()`, `roc_pr()` and
  `payoff_grid()`.
* New comparison and robustness tools: `compare_models()`, `bootstrap_profit()`
  and `tornado()`.
* All result objects now have `ggplot2::autoplot()` methods, plus matching
  `plot_*()` helper functions, for one-line visualisation.
* New `run_app()` launches an interactive `shiny` application to explore the
  package and upload your own data (comma- or semicolon-separated CSV).
* `profit()`, `cost_revenue()` and `roi()` gained a `positive` argument so the
  event class no longer has to be labelled `"Yes"`, and a `prob_accept`
  argument that reconciles them with `profit_thresholds()`.

## Bug fixes

* `profit_thresholds()` now correctly applies the `fp_val` (false-positive
  value) argument, which was previously ignored.

# modelimpact 1.0.0

* Added functionality for finding optimal thresholds
