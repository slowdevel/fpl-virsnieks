# this file will run the main shiny app for this package
dir <- system.file("shiny", "fpl-virsnieks", package="fplVirsnieks")
setwd(dir)
shiny::shinyAppDir(".")
