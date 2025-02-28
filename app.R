library(shiny)

ui <- fluidPage(
  sidebar = sidebar(
    selectInput(
      inputId = "policy-selector",
      label = "Policy",
      choices = c(
        "Subsidized Housing" = "all_loc_1",
        "Public Transit" = "all_loc_3"
      )
    )
  ),
  titlePanel("Canadian's Policy Attitudes"),
  textOutput("selected_var")
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  initialize_database(con, "data/policy-data.duckdb", table = "policy-data")

  df <- dplyr::tbl(con, "policy-data")

  output$selected_var <- renderText({

  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
