#' Render a model-impact report
#'
#' Renders a ready-made, parameterised \pkg{quarto} report bundled with the
#' package. The report pulls together the headline `impact_summary()` and the
#' cost/revenue, profit, ROI, gains, lift and marginal-profit views (plus a
#' budget section when a `budget` is supplied) into a single HTML document.
#'
#' Requires the \pkg{quarto} R package and a working
#' [Quarto](https://quarto.org) installation.
#'
#' @param x A data frame containing predicted probabilities of a target event
#'   and the actual outcome/class.
#' @param prob_col The unquoted (or quoted) name of the column with the
#'   probabilities of the event of interest.
#' @param truth_col The unquoted (or quoted) name of the column with the actual
#'   outcome/class.
#' @param positive The value in `truth_col` that identifies the event of interest.
#' @param fixed_cost Fixed cost (e.g. of a campaign).
#' @param var_cost Variable cost per targeted case (a single value).
#' @param tp_val The value of a True Positive (a single value).
#' @param prob_accept Probability of the offer being accepted. Defaults to 1.
#' @param budget Optional budget. When supplied, a budget section is added to
#'   the report.
#' @param output_file Path of the HTML file to create. Defaults to
#'   `"impact-report.html"` in the working directory.
#' @param quiet Suppress Quarto's rendering output? Defaults to `TRUE`.
#'
#' @return The path to the rendered report, invisibly.
#' @export
#' @examples
#' \dontrun{
#' impact_report(predictions,
#'    prob_col   = Yes,
#'    truth_col  = Churn,
#'    fixed_cost = 1000,
#'    var_cost   = 100,
#'    tp_val     = 2000,
#'    output_file = "churn-impact.html")
#' }
impact_report <- function(x,
                          prob_col,
                          truth_col,
                          positive    = "Yes",
                          fixed_cost  = 0,
                          var_cost    = 0,
                          tp_val      = 0,
                          prob_accept = 1,
                          budget      = NULL,
                          output_file = "impact-report.html",
                          quiet       = TRUE) {

  if (!requireNamespace("quarto", quietly = TRUE)) {
    stop("The 'quarto' package is required to render reports. ",
         "Install it with install.packages('quarto').", call. = FALSE)
  }

  prob_name  <- .capture_name(substitute(prob_col))
  truth_name <- .capture_name(substitute(truth_col))

  if (!all(c(prob_name, truth_name) %in% names(x))) {
    stop("`prob_col` and `truth_col` must name columns in `x`.", call. = FALSE)
  }

  template <- system.file("quarto", "impact-report.qmd", package = "modelimpact")
  if (!nzchar(template)) {
    stop("Could not find the bundled report template.", call. = FALSE)
  }

  # render a copy in a temp directory so nothing is written into the package
  qmd       <- tempfile(fileext = ".qmd")
  data_path <- tempfile(fileext = ".rds")
  file.copy(template, qmd, overwrite = TRUE)
  saveRDS(x, data_path)

  quarto::quarto_render(
    input = qmd,
    execute_params = list(
      data_path   = data_path,
      prob_col    = prob_name,
      truth_col   = truth_name,
      positive    = positive,
      fixed_cost  = fixed_cost,
      var_cost    = var_cost,
      tp_val      = tp_val,
      prob_accept = prob_accept,
      budget      = if (is.null(budget)) NA else budget
    ),
    quiet = quiet
  )

  rendered <- file.path(dirname(qmd), sub("\\.qmd$", ".html", basename(qmd)))
  file.copy(rendered, output_file, overwrite = TRUE)

  invisible(output_file)
}

# internal: turn a captured argument (an unquoted symbol or a string) into a
# column-name string.
.capture_name <- function(expr) {
  if (is.character(expr)) expr else deparse(expr)
}
