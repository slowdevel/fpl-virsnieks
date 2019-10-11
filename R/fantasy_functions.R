#' Gets and returns fantasy team information
#'
#' Only need to run once per gameweek, as points don't update live while gameweek underway
#' - calculate live points via fantasy team picks and live player points
#' @export
get_fantasy_snapshot <- function(current_season, gameweek) {
  shiny::withProgress(message="Update Fantasy Data", value=0, {
    fantasy_key <- fplVirsnieks::read_dt("fantasy_key.csv")[season==current_season]
    num_fantasy_teams <- nrow(fantasy_key)
    fantasy_snapshot <- list()
    for (t in 1:num_fantasy_teams) {
      shiny::incProgress(0, detail = paste("Getting data for ", fantasy_key$fantasy_manager[t]))
      team_url <- paste0(FPL_ENTRY_URL, fantasy_key$fantasy_team_code[t], '/')
      entry <- jsonlite::fromJSON(team_url)
      entry_name <- entry$name

      history_url <- paste0(FPL_ENTRY_URL, fantasy_key$fantasy_team_code[t], '/history/')
      entry_history <- jsonlite::fromJSON(history_url)
      history <- data.table(entry_history$current)

      transfers_url <- paste0(FPL_ENTRY_URL, fantasy_key$fantasy_team_code[t], '/transfers/')
      transfers <- data.table(jsonlite::fromJSON(transfers_url))

      picks <- NULL
      for (g in 1:gameweek) {
        picks_url <- paste0(FPL_ENTRY_URL, fantasy_key$fantasy_team_code[t], '/event/', g, '/picks/')
        fantasy_picks <- data.table(jsonlite::fromJSON(picks_url)$picks)
        fantasy_picks[,':='(role=ifelse(is_captain, 'C'
                                        , ifelse(is_vice_captain, 'V'
                                                 , ifelse(position > 11, '*', '')))
                            , season=current_season
                            , gameweek=g
        )]
        picks <- rbindlist(list(picks
                                , fantasy_picks[, c("season", "gameweek", "element"
                                                    , "position", "multiplier", "role"
                                                    , "is_captain", "is_vice_captain")]))


      }
      fantasy_team <- list(key=fantasy_key[t,]
                           , team_name=entry_name
                           , summary=entry
                           , history=history
                           , picks=picks
                           , transfers=transfers
      )
      fantasy_snapshot[[t]] <- fantasy_team

      shiny::incProgress(1/num_fantasy_teams, detail = paste("Done: ", fantasy_key$fantasy_manager[t]))
    }
  })
  return(fantasy_snapshot)

}

# vector of player_ids for every player in any fantasy team this season
get_fantasy_season_player_ix <- function(fantasy_snapshot) {
  fantasy_season_player_ix <-
    as.integer(
      unique(
        rbindlist(
          lapply(
            fantasy_snapshot
            , function(x)
              data.table(
                fpl_player_id=x$picks$element
              )
          )
        )[order(fpl_player_id), fpl_player_id]
      )
    )
  return(fantasy_season_player_ix)
}

# vector of player_ids for every player in any fantasy team during current gameweek
get_fantasy_gameweek_player_ix <- function(fantasy_snapshot, current_gameweek) {
  fantasy_gameweek_player_ix <-
    as.integer(
      unique(
        rbindlist(
          lapply(
            fantasy_snapshot
            , function(x)
              data.table(
                gameweek=x$picks$gameweek
                , fpl_player_id=x$picks$element
              )
          )
        )[gameweek==current_gameweek][order(fpl_player_id), fpl_player_id]
      )
    )
  return(fantasy_gameweek_player_ix)
}
