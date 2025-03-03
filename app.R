library(shiny)
library(bslib)

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "data/voter-data.duckdb")
df <- DBI::dbReadTable(con, name = "policyData")
DBI::dbDisconnect(con)

ui <- fluidPage(
  sidebar = sidebar(
    selectInput(
      inputId = "policy_selector",
      label = "Policy",
      choices = c(
        "Subsidized Housing",
        "Public Transit"
      )
    )
  ),
  titlePanel("Canadian's Policy Attitudes"),
  textOutput("selected_var")
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  output$selected_var <- renderText({
    filter <- input$policy_selector |>
      switch(
        "Subsidized Housing" = "all_loc_3",
        "Public Transit" = "all_loc_3"
      )
    return(df[[filter]])
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
