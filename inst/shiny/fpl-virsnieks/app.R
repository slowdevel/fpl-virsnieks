fplVirsnieks::clr()

library(shiny)
library(fplVirsnieks)


fpl_state <- reactiveValues(
  # set launch time when any session started
  launch_time = reactiveVal(Sys.time())
)

# Define UI for application that draws a histogram
ui <- navbarPage(
  "fpl.grava.net"

  , header = fluidRow(
    # season and gamewek info
    column( 12
            , h4(
              "Season", textOutput("season", inline=T)
              , " Gameweek ", textOutput("gameweek", inline=T)
              , " ", textOutput("gw_status", inline=T)
            )
            , fplVirsnieks::button_ui("button_update_gw", "Update GW")
            , fplVirsnieks::button_ui("button_update_live", "Update Live")
            , "Working directory: ", textOutput("wd", inline=T)

    )
  ) # header

  , tabPanel(
    "Fantasy"

    , fluidRow(
      column( 4
        , selectInput("select_manager", "Manager", "", width = '120px')
      )
      , column( 8
        , DT::DTOutput("dt_fantasy_history")
      )
    )
  )

  , tabPanel(
    "PL Gameweek"

    , fluidRow(
      column( 4
        , selectInput("select_gameweek", "Gameweek", "", width = '60px')
        , DT::DTOutput("dt_fixtures")
      )
      , column( 8
        , uiOutput("gw_details")
      )
    )
  ) # tabPanel Fixtures

  , tabPanel(
    "Teams"

    , fluidRow(
      column( 6
        , DT::DTOutput("dt_team_history")
      )
    )
  ) # tabPanel Teams

  , tabPanel(
    "Players"

    , fluidRow(
      column( 6
        , DT::DTOutput("dt_player_gameweek_history")
      )
    )
  )

  , footer = fluidRow(
    column(
      12
      , p(strong("System Start Time: "), textOutput("launch_time", inline=T)
          , strong("Gameweek Update Time: "), textOutput("gw_update_time", inline=T)
          , strong("Live Update Time: "), textOutput("live_update_time", inline=T)
      ) # p
    ) # column
  ) # footer

  # splash screen
  , waiter::use_waiter()
  , waiter::show_waiter_on_load(
    tagList(
      waiter::spin_fading_circles()
      , br()
      , p(style="color: white", "loading FPL Virsnieks...")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # populate fpl_state
  fpl_state <- fplVirsnieks::update_live_fpl(fpl_state, full_update=T)

  callModule(button_update_gw, "button_update_gw", fpl_state)
  callModule(button_update_live, "button_update_live", fpl_state)

  output$season <- renderText({ fpl_state$season })
  output$gameweek <- renderText({ fpl_state$gameweek })
  output$gw_status <- renderText({ fpl_state$gameweek_status })

  output$wd <- renderText({ getwd() })

  output$launch_time <- renderText({ format(fpl_state$launch_time(), "%Y-%m-%d %H:%M:%S")})
  output$gw_update_time <- renderText({ format(fpl_state$gw_update_time, "%Y-%m-%d %H:%M:%S") })
  output$live_update_time <- renderText({ format(fpl_state$live_update_time, "%Y-%m-%d %H:%M:%S") })

  observe({
    # populate gameweek dropdown with gameweeks
    updateSelectInput(session, "select_gameweek"
                      , choices = seq(1, 38) #fpl_state$gameweek)
                      , selected = fpl_state$gameweek)
    # populate manager dropdown with manager list for current season
    manager_list <- fpl_state$fantasy_key[
      season==fpl_state$season
      , fantasy_manager
      ]
    updateSelectInput(session, "select_manager"
                      , choices = setNames(seq(1,length(manager_list)), manager_list)
                      , selected = 1)
  })

  gw_fixtures <- reactive(fplVirsnieks::query_gw_fixtures(
    fpl_state$fpl_fixtures
    , input$select_gameweek
  ))

  output$dt_fantasy_history <- DT::renderDT({
    fpl_state$fantasy_snapshot[[as.numeric(input$select_manager)]]$history
    # fpl_state$fantasy_key[season==fpl_state$season]
  })

  output$dt_fixtures <- DT::renderDT(
    fplVirsnieks::format_gw_fixtures(
      gw_fixtures()
    )
  )

  output$dt_team_history <- DT::renderDT(
    fpl_state$team_history[ , -c("stats"), with=F]
  )

  output$dt_player_gameweek_history <- DT::renderDT(
    fpl_state$player_gameweek_history[ , -c("explain"), with=F]
  )

  output$gw_details <- renderUI({
    HTML(fplVirsnieks::html_fixture_stats(fpl_state$fpl_fixtures
                                       , gw_fixtures()[input$dt_fixtures_rows_selected, id]
                                       , fpl_state$fpl_roster
                                       )
      )
  })

  # hide splash screen
  waiter::hide_waiter()
}

# Run the application
shinyApp(ui = ui, server = server)
