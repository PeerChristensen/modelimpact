
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
