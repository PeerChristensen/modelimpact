#' Find optimal threshold for churn prediction (class)
#'
#' Finds the optimal threshold (from a business perspective) for classifying churners.
#'
#' @param x A data frame containing predicted probabilities of a target event and the actual outcome/class.
#' @param var_cost Variable cost (e.g. of a campaign offer). Either a single
#'   value applied to every case, or an unquoted column name (or vector) giving
#'   a per-observation cost.
#' @param prob_accept Probability of offer being accepted. Defaults to 1.
#' @param tp_val The value of a True Positive. `var_cost` is automatically
#'   subtracted. Either a single value or an unquoted column name (or vector).
#' @param fp_val The cost of a False Positive. `var_cost` is automatically
#'   subtracted. Either a single value or an unquoted column name (or vector).
#' @param tn_val The value of a True Negative. Either a single value or an
#'   unquoted column name (or vector).
#' @param fn_val The cost of a False Negative. Either a single value or an
#'   unquoted column name (or vector).
#' @param prob_col The unquoted name of the column with probabilities of the event of interest.
#' @param truth_col The unquoted name of the column with the actual outcome/class.
#' @param positive The value in `truth_col` that identifies the event of interest. Defaults to 'Yes'.
#'
#' @return
#' A data frame with the following columns:
#'
#' threshold = prediction thresholds \cr
#' payoff    = calculated profit for each threshold
#'
#' @export
#' @examples
#'profit_thresholds(predictions,
#'    var_cost    = 100,
#'    prob_accept = .8,
#'    tp_val      = 2000,
#'    fp_val      = 0,
#'    tn_val      = 0,
#'    fn_val      = -2000,
#'    prob_col    = Yes,
#'    truth_col   = Churn)

profit_thresholds <- function(x,
                              var_cost   = 0,
                              prob_accept = 1,
                              tp_val      = 0,
                              fp_val      = 0,
                              tn_val      = 0,
                              fn_val      = 0,
                              prob_col = NA,
                              truth_col = NA,
                              positive = "Yes") {

results <- NULL

xv <- x %>%
  dplyr::mutate(
    .prob   = {{ prob_col }},
    .is_pos = {{ truth_col }} == positive,
    .tpv    = ({{ tp_val }}) - ({{ var_cost }}) * prob_accept,
    .fpv    = ({{ fp_val }}) - ({{ var_cost }}) * prob_accept,
    .tnv    = ({{ tn_val }}) + 0,
    .fnv    = ({{ fn_val }}) + 0
  )

prob    <- xv$.prob
is_pos  <- xv$.is_pos
tpv     <- xv$.tpv
fpv     <- xv$.fpv
tnv     <- xv$.tnv
fnv     <- xv$.fnv

for ( i in seq(0,1,0.01)) {

  pred_pos <- prob > i

  payoff <- sum(tpv[pred_pos & is_pos]) +
            sum(fpv[pred_pos & !is_pos]) +
            sum(fnv[!pred_pos & is_pos]) +
            sum(tnv[!pred_pos & !is_pos])

  result <- dplyr::tibble(threshold = i, payoff)

  results <- rbind(results,result)
}
class(results) <- c("mi_thresholds", "tbl_df", "tbl", "data.frame")
return(results)
}

