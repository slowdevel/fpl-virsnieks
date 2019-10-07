#' Runs shiny app
#' @export
run_fpl_virsnieks <- function() {
  app_dir <- system.file("shiny", "fpl-virsnieks", package="fplVirsnieks")
  if (app_dir == "") {
    stop("Could not find shiny app directory. Try reinstalling 'fplVirsnieks'."
         , call.=F)
  }

  shiny::runApp(app_dir, display.mode="normal")
}
