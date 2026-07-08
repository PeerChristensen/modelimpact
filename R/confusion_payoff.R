#' Confusion matrix and payoff at a single threshold
#'
#' Classifies observations at a single probability threshold, returns the
#' resulting confusion matrix (TP/FP/TN/FN) and the associated payoff. This is
#' the single-threshold companion to [profit_thresholds()], which sweeps every
#' threshold, and uses the same value model.
#'
#' @param x A data frame containing predicted probabilities of a target event and the actual outcome/class.
#' @param threshold Probability cutoff above which an observation is classified as the positive class.
#' @param var_cost Variable cost (e.g. of a campaign offer).
#' @param prob_accept Probability of offer being accepted. Defaults to 1.
#' @param tp_val The average value of a True Positive. `var_cost` is automatically subtracted.
#' @param fp_val The average cost of a False Positive. `var_cost` is automatically subtracted.
#' @param tn_val The average value of a True Negative.
#' @param fn_val The average cost of a False Negative.
#' @param prob_col The unquoted name of the column with probabilities of the event of interest.
#' @param truth_col The unquoted name of the column with the actual outcome/class.
#' @param positive The value in `truth_col` that identifies the event of interest. Defaults to 'Yes'.
#'
#' @return
#' A one-row data frame with the following columns:
#'
#' threshold = the threshold used \cr
#' tp        = number of true positives \cr
#' fp        = number of false positives \cr
#' tn        = number of true negatives \cr
#' fn        = number of false negatives \cr
#' payoff    = total payoff at the threshold
#'
#' @export
#' @examples
#' confusion_payoff(predictions,
#'    threshold   = 0.3,
#'    var_cost    = 100,
#'    prob_accept = .8,
#'    tp_val      = 2000,
#'    fp_val      = 0,
#'    tn_val      = 0,
#'    fn_val      = -2000,
#'    prob_col    = Yes,
#'    truth_col   = Churn)

confusion_payoff <- function(x,
                             threshold   = 0.5,
                             var_cost    = 0,
                             prob_accept = 1,
                             tp_val      = 0,
                             fp_val      = 0,
                             tn_val      = 0,
                             fn_val      = 0,
                             prob_col    = NA,
                             truth_col   = NA,
                             positive    = "Yes") {

  preds <- x %>%
    dplyr::mutate(preds = dplyr::if_else({{ prob_col }} > threshold, "Yes", "No"))

  tp <- preds %>% dplyr::filter(preds == "Yes" & {{ truth_col }} == positive) %>% nrow()
  fp <- preds %>% dplyr::filter(preds == "Yes" & {{ truth_col }} != positive) %>% nrow()
  tn <- preds %>% dplyr::filter(preds == "No"  & {{ truth_col }} != positive) %>% nrow()
  fn <- preds %>% dplyr::filter(preds == "No"  & {{ truth_col }} == positive) %>% nrow()

  tp_vals <- tp_val - (var_cost * prob_accept)
  fp_vals <- fp_val - (var_cost * prob_accept)
  tn_vals <- tn_val
  fn_vals <- fn_val

  payoff <- (tp * tp_vals) + (fp * fp_vals) + (fn * fn_vals) + (tn * tn_vals)

  dplyr::tibble(threshold = threshold, tp = tp, fp = fp, tn = tn, fn = fn, payoff = payoff)
}
