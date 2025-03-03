library(shiny)
library(bslib)

# load local, client-side Duckdb database
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "data/voter-data.duckdb")
df <- DBI::dbReadTable(con, name = "policyData")
DBI::dbDisconnect(con)

# load statement text list (`statement_lookup`)
load("data/statement-text.rda")

ui <- page_fillable(
  titlePanel("Canadian's Policy Attitudes"),
  selectInput(
    inputId = "policy_selector",
    label = "Select a policy, or begin typing a subject of interest:",
    choices = statement_lookup$statement,
    selectize = TRUE,
    width = "66vw"
  ),
  layout_columns(
    col_widths = c(3, 9),
    card(
      selectInput(
        inputId = "gender_selector",
        label = "Gender:",
        choices = levels(df$gender),
        selectize = TRUE
      ),
      selectInput(
        inputId = "age_selector",
        label = "Age:",
        choices = levels(df$agecat),
        selectize = TRUE
      ),
      selectInput(
        inputId = "race_selector",
        label = "Race:",
        choices = levels(df$race),
        selectize = TRUE
      ),
      selectInput(
        inputId = "educ_selector",
        label = "Education:",
        choices = levels(df$education),
        selectize = TRUE
      ),
      selectInput(
        inputId = "income_selector",
        label = "Income:",
        choices = levels(df$income),
        selectize = TRUE
      ),
      selectInput(
        inputId = "immig_selector",
        label = "Immigration Status:",
        choices = levels(df$immigrant),
        selectize = TRUE
      )
    ),
    card(
      textOutput("selected_var")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  output$selected_var <- renderText({
    filter <- statement_lookup$var_name[statement_lookup$statement == input$policy_selector]
    return(filter)
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
