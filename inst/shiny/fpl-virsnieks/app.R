fplVirsnieks::clr()

library(shiny)
library(fplVirsnieks)


fpl_state <- reactiveValues(
  # set launch time when any session started
  launch_time = reactiveVal(Sys.time())
)

# populate fpl_state
fpl_state <- fplVirsnieks::update_live_fpl(fpl_state)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # splash screen
  waiter::use_waiter()
  , waiter::show_waiter_on_load(
    tagList(
      waiter::spin_fading_circles()
      , br()
      , p(style="color: white", "loading FPL Virsnieks...")
    )
  )

  # Application title
  , titlePanel("FPL Virsnieks")

  # Sidebar with a slider input for number of bins
  , sidebarLayout(
    sidebarPanel(
      tags$h3(
        "Welcome to Season", textOutput("season", inline=T)
        , " Gameweek ", textOutput("gameweek", inline=T)
        , " ", textOutput("gw_status", inline=T)
        )
      # , sliderInput("bins",
      #             "Number of bins:",
      #             min = 1,
      #             max = 50,
      #             value = 30)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      DT::DTOutput("dt_fixtures")
      , uiOutput("gw_details")
      # , plotOutput("distPlot")
    )
  )
  , fluidRow(
      column(
        12
        , p(strong("System Start Time: "), textOutput("launch_time", inline=T)
            , strong("Gameweek Update Time: "), textOutput("gw_update_time", inline=T)
            , strong("Live Update Time: "), textOutput("live_update_time", inline=T)
        )
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$season <- renderText({ fpl_state$season })
  output$gameweek <- renderText({ fpl_state$gameweek })
  output$gw_status <- renderText({ fpl_state$gameweek_status })

  output$launch_time <- renderText({ format(fpl_state$launch_time(), "%Y-%m-%d %H:%M:%S")})
  output$gw_update_time <- renderText({ format(fpl_state$gw_update_time, "%Y-%m-%d %H:%M:%S") })
  output$live_update_time <- renderText({ format(fpl_state$live_update_time, "%Y-%m-%d %H:%M:%S") })

  gw_fixtures <- reactive(fplVirsnieks::query_gw_fixtures(
    fpl_state$fpl_fixtures
    , fpl_state$gameweek
  ))
  output$dt_fixtures <- DT::renderDT(
    fplVirsnieks::format_gw_fixtures(
      gw_fixtures()
    )
  )

  output$gw_details <- renderUI({
    # HTML("<strong>Poopa</strong>")

    HTML(fplVirsnieks::html_fixture_stats(fpl_state$fpl_fixtures
                                       , gw_fixtures()[input$dt_fixtures_rows_selected, id]
                                       , fpl_state$fpl_roster
                                       )
      )

    # p(length(gw_fixtures()[input$dt_fixtures_rows_selected, id]))
  })

  # output$distPlot <- renderPlot({
  #   # generate bins based on input$bins from ui.R
  #   x    <- faithful[, 2]
  #   bins <- seq(min(x), max(x), length.out = input$bins + 1)
  #
  #   # draw the histogram with the specified number of bins
  #   hist(x, breaks = bins, col = 'darkgray', border = 'white')
  # })

  fpl_state$season <- fplVirsnieks::current_season()

  # hide splash screen
  waiter::hide_waiter()
}

# Run the application
shinyApp(ui = ui, server = server)
