#' Updates live fpl_state
#'
#' update_entire_gameweek parameter, T or F
#' @export
update_live_fpl <- function(state, update_entire_gameweek=F) {
  if (update_entire_gameweek) {
    # gameweek stuff
  }
  # set gw_update_time when gameweek updates are done
  state$gw_update_time <- Sys.time()

  # live fpl queries
  fpl_now <- jsonlite::fromJSON(fplVirsnieks::FPL_NOW_URL)
  gameweek <- which(fpl_now$events$is_current == T)
  fpl_fixtures <- jsonlite::fromJSON(fplVirsnieks::FPL_FIXTURES_URL)
  fpl_live <- jsonlite::fromJSON(paste0(fplVirsnieks::FPL_EVENT_URL, gameweek, "/live/"), flatten=T)

  # tables from live data

  # assign to state
  state$gameweek <- gameweek
  state$fixtures <- data.table(fpl_fixtures)

  # set live_update_time when live updates are done
  state$live_update_time <- Sys.time()

  return(state)
}
