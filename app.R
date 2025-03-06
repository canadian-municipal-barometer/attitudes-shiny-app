library(shiny)
library(bslib)

# load local, client-side Duckdb database
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "data/voter-data.duckdb")
df <- DBI::dbReadTable(con, name = "policyData")
DBI::dbDisconnect(con)

# load statement text list (`statement_lookup`)
load("data/statement-text.rda")

ui <- bslib::page_fillable(
  shiny::titlePanel("Canadian's Policy Attitudes"),
  shiny::selectInput(
    inputId = "policy",
    label = "Select a policy, or begin typing a subject of interest:",
    choices = statement_lookup$statement,
    selectize = TRUE,
    width = "40vw"
  ),
  bslib::layout_columns(
    col_widths = c(3, 9),
    bslib::card(
      shiny::selectInput(
        inputId = "gender",
        label = "Gender:",
        choices = levels(df$gender),
        selectize = FALSE
      ),
      shiny::selectInput(
        inputId = "agecat",
        label = "Age:",
        choices = levels(df$agecat),
        selectize = FALSE
      ),
      shiny::selectInput(
        inputId = "race",
        label = "Race:",
        choices = levels(df$race),
        selectize = FALSE
      ),
      shiny::selectInput(
        inputId = "education",
        label = "Education:",
        choices = levels(df$education),
        selectize = FALSE
      ),
      shiny::selectInput(
        inputId = "income",
        label = "Income:",
        choices = levels(df$income),
        selectize = FALSE
      ),
      shiny::selectInput(
        inputId = "immigrant",
        label = "Immigration Status:",
        choices = levels(df$immigrant),
        selectize = FALSE
      ),
      shiny::selectInput(
        inputId = "province",
        label = "Province:",
        choices = levels(df$province),
        selectize = FALSE
      ),
      shiny::selectInput(
        inputId = "popcat",
        label = "Population:",
        choices = levels(df$popcat),
        selectize = FALSE
      ),
      shiny::selectInput(
        inputId = "homeowner",
        label = "Homeowner:",
        choices = levels(df$homeowner),
        selectize = FALSE
      ),
    ),
    bslib::card(
      shiny::textOutput("predictions")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  output$predictions <- shiny::renderText({
    # policy to filter the data by
    filter <- statement_lookup$var_name[statement_lookup$statement == input$policy]

    # translate the contents of the selectors to variable names

    tmp_df <- df |> dplyr::filter(policy == filter)

    return()
  })
}

# Create Shiny app ----
shiny::shinyApp(ui = ui, server = server)
