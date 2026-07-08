#' ROC and precision-recall curve data
#'
#' Calculates the points of the ROC curve and the precision-recall curve by
#' walking down the observations sorted by descending predicted probability.
#' The returned data frame contains everything needed to draw both curves and to
#' overlay an iso-profit / cost-sensitive operating point.
#'
#' @param x A data frame containing predicted probabilities of a target event and the actual outcome/class.
#' @param prob_col The unquoted name of the column with probabilities of the event of interest.
#' @param truth_col The unquoted name of the column with the actual outcome/class.
#' @param positive The value in `truth_col` that identifies the event of interest. Defaults to 'Yes'.
#'
#' @return
#' A data frame with the following columns:
#'
#' threshold = probability at each operating point \cr
#' tp        = true positives when classifying every row at or above the point as positive \cr
#' fp        = false positives \cr
#' tpr       = true positive rate / recall / sensitivity (tp / all positives) \cr
#' fpr       = false positive rate (fp / all negatives) \cr
#' precision = tp / (tp + fp) \cr
#' recall    = same as tpr
#'
#' @export
#' @examples
#' roc_pr(predictions,
#'    prob_col  = Yes,
#'    truth_col = Churn)

roc_pr <- function(x,
                   prob_col  = NA,
                   truth_col = NA,
                   positive  = "Yes") {

  x %>%
    dplyr::arrange(dplyr::desc({{ prob_col }})) %>%
    dplyr::mutate(threshold = {{ prob_col }}) %>%
    dplyr::mutate(event = dplyr::if_else({{ truth_col }} == positive, 1, 0)) %>%
    dplyr::mutate(row = dplyr::row_number()) %>%
    dplyr::mutate(tp = cumsum(event)) %>%
    dplyr::mutate(fp = row - tp) %>%
    dplyr::mutate(tpr = tp / sum(event)) %>%
    dplyr::mutate(fpr = fp / (dplyr::n() - sum(event))) %>%
    dplyr::mutate(precision = tp / row) %>%
    dplyr::mutate(recall = tpr) %>%
    dplyr::select(threshold, tp, fp, tpr, fpr, precision, recall) %>%
    structure(class = c("mi_roc", "tbl_df", "tbl", "data.frame"))
}
