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

#' Formats gw_fixtures_table
#' @export
format_gw_fixtures <- function(gw_fixtures) {
  # col_names <- c("home", "h", "a", "away", "status", "time", "id_a", "id_b", "status_level")
  dt <- DT::datatable(
    gw_fixtures
    , rownames=F
    # , colnames = col_names
    , width = "100%"
    , selection = "single"
    , options = list(dom = 't' # hides top search etc. stuff
                     # , pageLength=15
                     , autoWidth=T
                     , columnDefs = list(
                       # center columns
                       list(className = 'dt-center', targets = c(0, 1, 2, 3, 4, 5)) # DT indices are 0-based!
                       # hide columns
                       , list(visible = F, targets = c(6, 7, 8))
                       # turn off ordering for columns
                       , list(orderable = F, targets = c(0, 1, 2, 3, 4, 5))
                     )
    )
  ) %>% # color rows by fixture status
    DT::formatStyle("status",  target="row", lineHeight='60%'
                    , color = DT::styleEqual(
                      fplVirsnieks::GAMEWEEK_STATUS_TEXT
                      , fplVirsnieks::GAMEWEEK_STATUS_COLORS)
                    )

}

#' Formats fixture stats into html table
#' @export
html_fixture_stats <- function(fixtures, fixture_id, roster) {
  if (length(fixture_id) == 0) {
    return("")
  } else {
    stats <- fixtures[id==fixture_id, stats][[1]]
    team_h_name <- fixtures[id==fixture_id, team_h_name]
    team_a_name <- fixtures[id==fixture_id, team_a_name]
    tbl <- "<table width='100%'>"
    tbl <- paste0(tbl, "<tr><th style = 'text-align: right;'>", team_h_name, "</th><th style = 'text-align: left;'>", team_a_name, "</th></tr>")
    if (nrow(stats) > 0) {
      for (i in 1:nrow(stats)) {
        num_h <- nrow(stats$h[i][[1]])
        num_a <- nrow(stats$a[i][[1]])
        num_max <- max(num_h, num_a)
        if (num_max > 0) {
          tbl <- paste0(tbl, "<tr><th colspan='2' style = 'text-align: center;'>", stats$identifier[i], "</th></tr>")
          for (j in 1:num_max) {
            tbl <- paste0(tbl, "<tr>")
            if (num_h >= j) {
              player <- find_player(stats$h[i][[1]]$element[j], roster)
              tbl <- paste0(
                tbl
                , "<td style = 'text-align: right;'>"
                , player$fpl_name, " ", player$fpl_pos, ": "
                , stats$h[i][[1]]$value[j]
                , "</td>")
            } else {
              tbl <- paste0(tbl, "<td/>")
            }
            if (num_a >= j) {
              player <- find_player(stats$a[i][[1]]$element[j], roster)
              tbl <- paste0(
                tbl
                , "<td style = 'text-align: left;'>"
                , player$fpl_name, " ", player$fpl_pos, ": "
                , stats$a[i][[1]]$value[j]
                , "</td>")
            } else {
              tbl <- paste0(tbl, "<td/>")
            }
            tbl <- paste0(tbl, "</tr>")
          }
        }
      }
    }
    tbl <- paste0(tbl, "</table>")
    return(tbl)
  }
}
