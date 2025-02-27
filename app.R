library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Canadian's Policy Attitudes")
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
