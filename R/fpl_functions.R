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

  # -------------------------------------------------------------------------
  # live fpl queries

  # fpl_now
  json_fpl_now <- jsonlite::fromJSON(fplVirsnieks::FPL_NOW_URL)
  # tables from json
  fpl_teams <- data.table(json_fpl_now$teams)
  # gameweek from fpl_now
  gameweek <- which(json_fpl_now$events$is_current == T)

  # fpl_fixtures
  json_fpl_fixtures <- jsonlite::fromJSON(fplVirsnieks::FPL_FIXTURES_URL)
  # table from json
  fpl_fixtures <- fplVirsnieks::create_fpl_fixtures(json_fpl_fixtures, fpl_teams)
  #gameweek status from fpl fixtures
  gameweek_fixtures <- fpl_fixtures[event == gameweek]
  gameweek_started <- sum(gameweek_fixtures$started) > 0
  gameweek_finished <- sum(gameweek_fixtures$finished) == length(gameweek_fixtures$finished)
  gameweek_final <- json_fpl_now$events$finished[gameweek] == T
  gameweek_status_level <- 1 + gameweek_started + gameweek_finished + gameweek_final
  gameweek_status <- fplVirsnieks::GAMEWEEK_STATUS_TEXT[gameweek_status_level]

  # fpl_live
  fpl_live <- jsonlite::fromJSON(paste0(fplVirsnieks::FPL_EVENT_URL, gameweek, "/live/"), flatten=T)

  # tables from live data

  # assign to state
  state$gameweek <- gameweek
  state$gameweek_status <- gameweek_status
  state$fpl_fixtures <- fpl_fixtures

  # set live_update_time when live updates are done
  state$live_update_time <- Sys.time()

  return(state)
}

#'  Create fpl_fixtures data.table
#' @export
create_fpl_fixtures <- function(json_fpl_fixtures, fpl_teams) {
  fpl_fixtures <- data.table(json_fpl_fixtures)
  setkey(fpl_fixtures, id)
  fpl_fixtures[
      fpl_teams
      , team_h_name := i.short_name
      , on="team_h==id"
    ][
      fpl_teams
      , team_a_name := i.short_name
      , on="team_a==id"
    ][
      , ':=' (# fixture status
      status_level = ifelse(finished, 4,
                            ifelse(finished_provisional, 3,
                                   ifelse(started, 2, 1)))
      , status = ifelse(finished
                        , fplVirsnieks::GAMEWEEK_STATUS_TEXT[4],
                        ifelse(finished_provisional
                               , fplVirsnieks::GAMEWEEK_STATUS_TEXT[3],
                               ifelse(started
                                      , fplVirsnieks::GAMEWEEK_STATUS_TEXT[2]
                                      , fplVirsnieks::GAMEWEEK_STATUS_TEXT[1])))
      )
    ]
  return(fpl_fixtures)
}
