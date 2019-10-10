#' button UI code
#' @export
button_ui <- function(id, label = "Counter") {
  ns <- NS(id)
  tagList(
    actionButton(ns("button_click"), label = label)
  )
}

#' button code for update_gw
#' @export
button_update_gw <- function(input, output, session, state) {
  observeEvent(input$button_click, {
    state <- fplVirsnieks::update_live_fpl(state, full_update=T)
  })
  return(state)
}

#' button code for update_live
#' @export
button_update_live <- function(input, output, session, state) {
  observeEvent(input$button_click, {
    state <- fplVirsnieks::update_live_fpl(state, full_update=F)
    # test write:
    fplVirsnieks::write_dt(state$fpl_teams, "tiims.csv")
  })
  return(state)
}

#' button code for button counter
#' @export
button_counter <- function(input, output, session, x) {
  # count <- reactiveVal(0)
  observeEvent(input$button_click, {
    x(x()+1)
  })
  return(x)
}

