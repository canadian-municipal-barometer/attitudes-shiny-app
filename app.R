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
    inputId = "policy",
    label = "Select a policy, or begin typing a subject of interest:",
    choices = statement_lookup$statement,
    selectize = TRUE,
    width = "66vw"
  ),
  layout_columns(
    col_widths = c(3, 9),
    card(
      selectInput(
        inputId = "gender",
        label = "Gender:",
        choices = levels(df$gender),
        selectize = FALSE
      ),
      selectInput(
        inputId = "agecat",
        label = "Age:",
        choices = levels(df$agecat),
        selectize = FALSE
      ),
      selectInput(
        inputId = "race",
        label = "Race:",
        choices = levels(df$race),
        selectize = FALSE
      ),
      selectInput(
        inputId = "education",
        label = "Education:",
        choices = levels(df$education),
        selectize = FALSE
      ),
      selectInput(
        inputId = "income",
        label = "Income:",
        choices = levels(df$income),
        selectize = FALSE
      ),
      selectInput(
        inputId = "immigrant",
        label = "Immigration Status:",
        choices = levels(df$immigrant),
        selectize = FALSE
      ),
      selectInput(
        inputId = "province",
        label = "Province:",
        choices = levels(df$province),
        selectize = FALSE
      ),
      selectInput(
        inputId = "homeowner",
        label = "Homeowner:",
        choices = levels(df$homeowner),
        selectize = FALSE
      ),
    ),
    card(
      textOutput("selected_var")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  output$selected_var <- renderText({
    # policy to filter the data by
    filter <- statement_lookup$var_name[statement_lookup$statement == input$policy]

    # translate the contents of the selectors to variable names

    tmp_df <- glm()

    return(nrow(tmp_df))
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
