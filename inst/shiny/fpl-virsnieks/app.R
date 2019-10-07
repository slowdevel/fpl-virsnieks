fplVirsnieks::clr()

library(shiny)

fpl_state <- reactiveValues(
  season = "----"
)

# Define UI for application that draws a histogram
ui <- fluidPage(

  waiter::use_waiter()
  , waiter::show_waiter_on_load()

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
