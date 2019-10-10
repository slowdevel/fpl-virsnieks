#' Updates live fpl_state
#'
#' full_update parameter, T or F
#' @export
update_live_fpl <- function(state, full_update=F) {
  # waiter::show_waiter()

  season <- fplVirsnieks::current_season()

  # -------------------------------------------------------------------------
  # live fpl queries

  # fpl_now
  json_fpl_now <- jsonlite::fromJSON(fplVirsnieks::FPL_NOW_URL)
  # tables from json
  fpl_teams <- data.table(json_fpl_now$teams)
  # gameweek from fpl_now
  gameweek <- which(json_fpl_now$events$is_current == T)

  # fpl_roster
  fpl_roster <- create_fpl_roster(json_fpl_now)

  # update once-per-gameweek info
  if (full_update) {
    # once-per-gameweek stuff
    # fantasy_snapshot <- fplVirsnieks::get_fantasy_snapshot(season, gameweek)
    # fantasy_season_player_ix <- get_fantasy_season_player_ix(fantasy_snapshot)
    # fantasy_gameweek_player_ix <- get_fantasy_gameweek_player_ix(fantasy_snapshot, gameweek)
    # player_gameweek_history <- fplVirsnieks::create_player_gameweek_history(gameweek)

    # assign once-per-week objects
    # state$fantasy_key <- fplVirsnieks::read_dt("fantasy_key.csv")
    # state$fantasy_snapshot <- fantasy_snapshot
    # state$fantasy_season_player_ix <- fantasy_season_player_ix
    # state$fantasy_gameweek_player_ix <- fantasy_gameweek_player_ix
    # state$player_gameweek_history <- player_gameweek_history

    # set gw_update_time when gameweek updates are done
    state$gw_update_time <- Sys.time()
  }

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

  # team history
  team_history <- fplVirsnieks::create_team_history(fpl_fixtures, fpl_teams)

  # fpl_live
  json_fpl_live <- jsonlite::fromJSON(paste0(fplVirsnieks::FPL_LIVE_URL, gameweek, "/live/"), flatten=T)

  # assign to state
  state$season <- season
  state$gameweek <- gameweek
  state$gameweek_status <- gameweek_status
  state$fpl_fixtures <- fpl_fixtures
  state$fpl_roster <- fpl_roster
  state$fpl_teams <- fpl_teams
  state$team_history <- team_history

  # set live_update_time when live updates are done
  state$live_update_time <- Sys.time()

  # waiter::hide_waiter()
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

#' Creates and returns FPL roster
#' @export
create_fpl_roster <- function(fpl_now) {
  fpl_roster <- data.table(fpl_now$elements)[
    , .(
      fpl_name=web_name
      , fpl_player_id=id
      , fpl_player_code=code
      , fpl_team_id=team
      , fpl_pos_id=element_type)
    ][
      data.table(fpl_now$element_types)[ , .(fpl_pos_id=id, fpl_pos=singular_name_short)]
      , , on=("fpl_pos_id")
      ][
        data.table(fpl_now$teams)[ , .(fpl_team_id=id, fpl_team_code=code, fpl_team=short_name)]
        , , on=("fpl_team_id")
        ][
          order(fpl_name, fpl_team, fpl_pos)
          , .(
            player_name = paste(
              fpl_name, fpl_team, fpl_pos
            )
            , fpl_name
            , fpl_team
            , fpl_pos
            , fpl_player_id
            , fpl_player_code
            , fpl_team_id
            , fpl_team_code
            , fpl_pos_id
          )
          ]
  return(fpl_roster)
}

# Finds and returns player roster info given player id
#' @export
find_player <- function(id, roster) {
  return(roster[fpl_player_id==id])
}

#' Creates and returns FPL team history
#' @export
create_team_history <- function(fixtures, teams) {
  team_ids <- sort(unique(fixtures[,team_h]))
  team_hist <-
    rbindlist(
      lapply(
        team_ids
        , function(x)
          fixtures[
            team_a==x | team_h==x
            , .(
              # original fixture fields
              event
              , id
              , code
              , kickoff_time
              , started
              , finished_provisional
              , finished
              , minutes
              , provisional_start_time
              , stats
              # hame and away recalced to team and team_opp
              , at_home = team_h==x
              , team_id = ifelse(team_h==x, team_h, team_a)
              , team_opp_id = ifelse(team_h==x, team_a, team_h)
              , team_score = ifelse(team_h==x, team_h_score, team_a_score)
              , team_opp_score = ifelse(team_h==x, team_a_score, team_h_score)
              , team_difficulty = ifelse(team_h==x, team_h_difficulty, team_a_difficulty)
              , team_opp_difficulty = ifelse(team_h==x, team_a_difficulty, team_h_difficulty)
            )
            ][
              teams
              , team := i.short_name
              , on = ("team_id==id")
              ][
                teams
                , team_opp_name := i.short_name
                , on = ("team_opp_id==id")
                ][
                  , ':=' (
                    wld = ifelse(team_score > team_opp_score, 1
                                 , ifelse(team_score < team_opp_score, -1, 0))
                    , points = ifelse(team_score > team_opp_score, 3
                                      , ifelse(team_score < team_opp_score, 0, 1))
                    , goal_difference = team_score - team_opp_score
                    )
                  ][
                    , ':=' (
                      cum_points = cumsum(points)
                      , cum_goal_difference = cumsum(goal_difference)
                      , cum_goals = cumsum(team_score)
                      )
                    , team_id
                    ]
      )
    )

  return(team_hist)
}

#' Creates and returns player gameweek (not fixture) history
#' @export
create_player_gameweek_history <- function(gameweek) {
  g <- seq(1, gameweek)
  player_gameweek_history <-
    rbindlist(
      lapply(
        g
        , function(x) {
          fpl_live <- jsonlite::fromJSON(paste0(fplVirsnieks::FPL_LIVE_URL, x, "/live/"))$elements
          n <- seq(1, nrow(fpl_live))
          rbindlist(
            lapply(
              n
              , function(y)
                data.table(gw=x, id=fpl_live[y, "id"], fpl_live[y, "stats"])[ , explain := list(list(fpl_live$explain[[y]]))]
            )
          )
        }
      )
    )
  return(player_gameweek_history)
}
