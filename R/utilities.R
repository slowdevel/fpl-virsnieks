#' Clears the global environment and console
#' @export
clr <- function() {
  rm(list=ls(envir=.GlobalEnv), envir=.GlobalEnv)
  library(fplVirsnieks)
  cat("\014")
}

#' Calculates current FPL season based on current date
#' @export
current_season <- function() {
  year <- as.numeric(lubridate::year(Sys.time())) - 2000
  month <- as.numeric(lubridate::month(Sys.time()))
  if (month < 7) { # end of the season
    y <- year - 1
  } else { # beginning of the season
    y <- year
  }
  return(as.numeric(paste0(y, y+1)))
}

#' Gets .csv file via data.table::fread from /inst/data/directory
#' @export
read_dt <- function(filename) {
  return(
    data.table::fread(
      # system.file(
      #   "extdata"
      #   , filename
      #   , package="fplVirsnieks"))
      paste0(
        # getwd()
        # ,"/data/"
        "./data/"
        , filename
      )
    )
  )
}

#' Writes .csv file via data.table::fwrite to /inst/data/directory
#' @export
write_dt <- function(data, filename) {
  data.table::fwrite(
    data
    , paste0(
      # system.file("extdata", package="fplVirsnieks")

      # getwd()
      # , "/data/"
      "./data/"
      ,filename
      )
  )
}
