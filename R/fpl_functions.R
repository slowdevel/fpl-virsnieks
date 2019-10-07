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
  # fpl_now
  fpl_now <- jsonlite::fromJSON(fplVirsnieks::FPL_NOW_URL)
    # gameweek from fpl_now
    gameweek <- which(fpl_now$events$is_current == T)
  # fpl_fixtures
  fpl_fixtures <- data.table(jsonlite::fromJSON(fplVirsnieks::FPL_FIXTURES_URL))
    #gameweek status from fpl fixtures
    gameweek_fixtures <- fpl_fixtures[event == gameweek]
    # num_games_started <- sum(gameweek_fixtures$started)
    gameweek_started <- sum(gameweek_fixtures$started) > 0
    gameweek_finished <- sum(gameweek_fixtures$finished) == length(gameweek_fixtures$finished)
    gameweek_final <- fpl_now$events$finished[gameweek] == T
    gameweek_status_level <- 1 + gameweek_started + gameweek_finished + gameweek_final
    gameweek_status <- fplVirsnieks::GAMEWEEK_STATUS_TEXT[gameweek_status_level]
  # fpl_live
  fpl_live <- jsonlite::fromJSON(paste0(fplVirsnieks::FPL_EVENT_URL, gameweek, "/live/"), flatten=T)

  # tables from live data

  # assign to state
  state$gameweek <- gameweek
  state$gameweek_status <- gameweek_status
  state$fixtures <- data.table(fpl_fixtures)

  # set live_update_time when live updates are done
  state$live_update_time <- Sys.time()

  return(state)
}
