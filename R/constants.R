#' URL for FPL bootstrap-static JSON data - fpl_now
#' @export
FPL_NOW_URL <- "https://fantasy.premierleague.com/api/bootstrap-static/"

#' URL for FPL /fixtures/ - fpl_fixutres
#' @export
FPL_FIXTURES_URL <- "https://fantasy.premierleague.com/api/fixtures/"

#' Root URL for FPL /event/ - Gameweek
#' @export
FPL_EVENT_URL <- "https://fantasy.premierleague.com/api/event/"

#' Root URL for FPL /entry/ - Fantasy Team
#'
#' Needs entry code / team id and suffix, e.g.:
#' paste0("https://fantasy.premierleague.com/api/entry/", <id> , "/")
#' paste0("https://fantasy.premierleague.com/api/entry/", <id> , "/history/")
#' paste0("https://fantasy.premierleague.com/api/entry/", <id> , "/transfers/")
#' paste0("https://fantasy.premierleague.com/api/entry/", <id> , "/event/", <gameweek>, "/picks/")
#' @export
FPL_ENTRY_URL <- "https://fantasy.premierleague.com/api/entry/"

#' Gameweek status codes
#' @export
GAMEWEEK_STATUS_TEXT = c("UPCOMING", "UNDERWAY", "FINISHED", "FINAL")

#' Gameweek status colors
#' @export
GAMEWEEK_STATUS_COLORS = c("gray", "red", "blue", "black")
