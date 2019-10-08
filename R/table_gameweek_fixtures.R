#" Queries fpl_fixtures for given gameweek data
#' @export
query_gw_fixtures <- function(fpl_fixtures, gameweek) {
  res <-
    fpl_fixtures[
      event==gameweek
      , .(
        gw = gameweek
        , home = team_h_name
        , h = team_h_score
        , a = team_a_score
        , away = team_a_name
        , time = kickoff_time
        , status_level
        , status
        , id
      )
    ]
  setkey(res, id)
  return(res)
}
