fplVirsnieks::clr()

library(shiny)


fpl_state <- reactiveValues(
  # set launch time when any session started
  launch_time = reactiveVal(Sys.time())
)

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
  , titlePanel("FPL Virsnieks"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      tags$h3("Welcome to Season", textOutput("season", inline=T))
      , sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
      , p("System Start Time: "), textOutput("launch_time", inline=F) #, tags$br()
      , p("Gameweek Update Time: "), textOutput("gw_update_time", inline=F) #, tags$br()
      , p("Live Update Time: "), textOutput("live_update_time", inline=F) #, tags$br()
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$season <- renderText({
    fpl_state$season
  })

  output$launch_time <- renderText({ format(fpl_state$launch_time(), "%Y-%m-%d %H:%M:%S")})
  output$gw_update_time <- renderText({ format(fpl_state$gw_update_time, "%Y-%m-%d %H:%M:%S") })
  output$live_update_time <- renderText({ format(fpl_state$live_update_time, "%Y-%m-%d %H:%M:%S") })

  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })

  fpl_state$season <- fplVirsnieks::current_season()

  waiter::hide_waiter()
}

# Run the application
shinyApp(ui = ui, server = server)
