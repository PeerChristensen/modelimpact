#' Find optimal threshold for churn prediction (class)
#'
#' Finds the optimal threshold (from a business perspective) for classifying churners.
#'
#' @param x A data frame containing predicted probabilities of a target event and the actual outcome/class.
#' @param var_cost Variable cost (e.g. of a campaign offer)
#' @param prob_accept Probability of offer being accepted. Defaults to 1.
#' @param tp_val The average value of a True Positive. `var_cost` is automatically subtracted.
#' @param fp_val The average cost of a False Positive. `var_cost` is automatically subtracted.
#' @param tn_val The average value of a True Negative.
#' @param fn_val The average cost of a False Negative.
#' @param prob_col The unquoted name of the column with probabilities of the event of interest.
#' @param truth_col The unquoted name of the column with the actual outcome/class. Possible values are 'Yes' and 'No'.
#'
#' #' @return
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
                              truth_col = NA) {

results <- NULL
for ( i in seq(0,1,0.01)) {

  preds_i <- x %>% dplyr::mutate(preds = dplyr::if_else({{prob_col}} > i,"Yes","No"))

  tp <- preds_i %>% dplyr::filter(preds == "Yes" & {{ truth_col }}  == "Yes") %>% nrow()
  fp <- preds_i %>% dplyr::filter(preds == "Yes" & {{ truth_col }}  == "No") %>% nrow()
  tn <- preds_i %>% dplyr::filter(preds == "No" & {{ truth_col }}  == "No") %>% nrow()
  fn <- preds_i %>% dplyr::filter(preds == "No" & {{ truth_col }}  == "Yes") %>% nrow()

  tp_vals <- {{tp_val}} - ({{var_cost}} * {{prob_accept}})
  fp_vals <- - ({{var_cost}} * {{prob_accept}})
  tn_vals <- {{tn_val}}
  fn_vals <- {{fn_val}}

  payoff <- (tp * tp_vals) + (fp * fp_vals) + (fn * fn_vals) + (tn * tn_vals)

  result <- dplyr::tibble(threshold = i, payoff)

  results <- rbind(results,result)
}
return(results)
}

