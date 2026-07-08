#' Launch the interactive modelimpact Shiny app
#'
#' Opens a small Shiny application for exploring the package interactively. You
#' can use the bundled `predictions` data or upload your own CSV (choosing comma
#' or semicolon as the separator), pick the probability and outcome columns and
#' the positive class, adjust the cost/value parameters, and watch every plot
#' update live.
#'
#' Requires the \pkg{shiny} and \pkg{ggplot2} packages, which are only needed for
#' this function.
#'
#' @param ... Additional arguments passed to [shiny::runApp()].
#'
#' @return Called for its side effect of starting the app; returns nothing.
#' @export
#' @examples
#' \dontrun{
#' run_app()
#' }

run_app <- function(...) {
  for (pkg in c("shiny", "ggplot2")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required to run the app. ",
           "Install it with install.packages('", pkg, "').", call. = FALSE)
    }
  }
  app_dir <- system.file("shiny", package = "modelimpact")
  if (!nzchar(app_dir)) {
    stop("Could not find the Shiny app directory. Try reinstalling modelimpact.",
         call. = FALSE)
  }
  shiny::runApp(app_dir, ...)
}
