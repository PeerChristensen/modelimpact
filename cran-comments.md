## Update

This is a feature update (1.0.0 -> 1.1.0). It adds new analysis functions
(cumulative gains, lift, marginal profit, break-even, ROC/PR, payoff grid,
model comparison, bootstrap and sensitivity tools), `ggplot2` `autoplot()`
methods, and an optional interactive `shiny` app. It also fixes a bug where
`profit_thresholds()` ignored its `fp_val` argument. All added dependencies
(`ggplot2`, `scales`, `shiny`) are in Suggests and used conditionally.

## Test environments

*   local macOS, R 4.5.2
*   win-builder (devel and release)
*   GitHub Actions: macOS, Windows and Ubuntu (release, devel, oldrel-1)

## R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTE:

*   checking CRAN incoming feasibility ... NOTE
    Maintainer: 'Peer Christensen <hr.pchristensen@gmail.com>'
