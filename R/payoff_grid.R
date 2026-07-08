#' Payoff sensitivity grid
#'
#' Computes payoff across a grid of classification thresholds and offer-acceptance
#' probabilities, so the robustness of the optimal operating point can be judged
#' at a glance (for example as a heatmap). Uses the same value model as
#' [profit_thresholds()] and [confusion_payoff()].
#'
#' @param x A data frame containing predicted probabilities of a target event and the actual outcome/class.
#' @param thresholds A vector of probability cutoffs to evaluate. Defaults to `seq(0, 1, 0.02)`.
#' @param prob_accept A vector of offer-acceptance probabilities to evaluate. Defaults to `seq(0, 1, 0.1)`.
#' @param var_cost Variable cost (e.g. of a campaign offer).
#' @param tp_val The average value of a True Positive. `var_cost` is automatically subtracted.
#' @param fp_val The average cost of a False Positive. `var_cost` is automatically subtracted.
#' @param tn_val The average value of a True Negative.
#' @param fn_val The average cost of a False Negative.
#' @param prob_col The unquoted name of the column with probabilities of the event of interest.
#' @param truth_col The unquoted name of the column with the actual outcome/class.
#' @param positive The value in `truth_col` that identifies the event of interest. Defaults to 'Yes'.
#'
#' @return
#' A data frame with the following columns:
#'
#' threshold   = classification threshold \cr
#' prob_accept = offer-acceptance probability \cr
#' payoff      = payoff for that combination
#'
#' @export
#' @examples
#' payoff_grid(predictions,
#'    var_cost  = 100,
#'    tp_val    = 2000,
#'    fn_val    = -2000,
#'    prob_col  = Yes,
#'    truth_col = Churn)

payoff_grid <- function(x,
                        thresholds  = seq(0, 1, 0.02),
                        prob_accept = seq(0, 1, 0.1),
                        var_cost    = 0,
                        tp_val      = 0,
                        fp_val      = 0,
                        tn_val      = 0,
                        fn_val      = 0,
                        prob_col    = NA,
                        truth_col   = NA,
                        positive    = "Yes") {

  grid <- expand.grid(threshold = thresholds, prob_accept = prob_accept)

  purrr_map <- Map(function(th, pa) {
    confusion_payoff(x,
                     threshold   = th,
                     var_cost    = var_cost,
                     prob_accept = pa,
                     tp_val      = tp_val,
                     fp_val      = fp_val,
                     tn_val      = tn_val,
                     fn_val      = fn_val,
                     prob_col    = {{ prob_col }},
                     truth_col   = {{ truth_col }},
                     positive    = positive)$payoff
  }, grid$threshold, grid$prob_accept)

  dplyr::tibble(threshold   = grid$threshold,
                prob_accept = grid$prob_accept,
                payoff      = unlist(purrr_map))
}
