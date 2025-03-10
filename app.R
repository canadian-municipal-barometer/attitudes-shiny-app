# load local, client-side Duckdb database
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "data/voter-data-char.duckdb")
df <- DBI::dbReadTable(con, name = "policyData")
DBI::dbDisconnect(con)

# load statement text list (`statements`)
load("data/statement-text.rda")

# create policy/group lookup
policy_lookup <- statements |>
  dplyr::group_by(group_id, group_name) |>
  dplyr::summarize(var = list(unique(var_name), .groups = "drop"))

ui <- bslib::page_fillable(
  shiny::titlePanel("Canadian's Policy Attitudes"),
  bslib::layout_columns(
    col_widths = c(3, 9),
    bslib::card(
      shiny::selectInput(
        inputId = "gender",
        label = "Gender:",
        choices = c("Woman", "Man"),
        selectize = FALSE
      ),
      shiny::selectInput(
        inputId = "agecat",
        label = "Age:",
        choices = c("18-29", "30-44", "45-59", "60+"),
        selectize = FALSE
      ),
      shiny::selectInput(
        inputId = "race",
        label = "Race:",
        choices = c(
          "Racialized minority",
          "White"
        ),
        selectize = FALSE
      ),
      shiny::selectInput(
        inputId = "education",
        label = "Education:",
        choices = c(
          "Less than high school",
          "High school",
          "Associate's degree or trades",
          "Bachelor's degree",
          "Post-graduate degree"
        ),
        selectize = FALSE
      ),
      shiny::selectInput(
        inputId = "income",
        label = "Income:",
        choices = c(
          "Less than $49,999",
          "$50,000 to $99,999",
          "$100,000 to $149,999",
          "$150,000 to $199,999",
          "$200,000 or more"
        ),
        selectize = FALSE
      ),
      shiny::selectInput(
        inputId = "immigrant",
        label = "Immigration Status:",
        choices = c("Yes", "No"),
        selectize = FALSE
      ),
      shiny::selectInput(
        inputId = "province",
        label = "Province:",
        choices = c(
          "Alberta",
          "British Columbia",
          "Manitoba",
          "New Brunswick",
          "Newfoundland and Labrador",
          "Nova Scotia",
          "Ontario",
          "Prince Edward Island",
          "Quebec",
          "Saskatchewan",
          "Territories"
        ),
        selectize = FALSE
      ),
      shiny::selectInput(
        inputId = "popcat",
        label = "Population:",
        choices = c(
          "3000-9,999",
          "10,000-49,999",
          "50,000-249,999",
          "250,000-999,999",
          "1,000,000+"
        ),
        selectize = FALSE
      ),
      shiny::selectInput(
        inputId = "homeowner",
        label = "Homeowner:",
        choices = c("Yes", "No"),
        selectize = FALSE
      )
    ),
    shiny::mainPanel(

      # select the filtered policies
      shiny::selectInput(
        inputId = "policy",
        label = "Select a policy, or begin typing a subject of interest:",
        choices = statements$statement,
        selectize = TRUE,
        width = "auto"
      ),
      shiny::plotOutput(
        "predictions",
        width = "100%",
        height = "400px"
      )
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  output$predictions <- shiny::renderPlot({
    # policy to filter the data by
    filter <- statements$var_name[statements$statement == input$policy]

    # translate the contents of the selectors to variable names

    tmp_df <- df |> dplyr::filter(policy == filter)

    model <- nnet::multinom(
      factor(outcome) ~
        factor(gender) +
        factor(education) +
        factor(province) +
        factor(agecat) +
        factor(race) +
        factor(homeowner) +
        factor(income) +
        factor(immigrant) +
        factor(popcat),
      data = tmp_df
    )

    pred_data <- data.frame(
      gender = input$gender,
      education = input$education,
      province = input$province,
      agecat = input$agecat,
      race = input$race,
      homeowner = input$homeowner,
      income = input$income,
      immigrant = input$immigrant,
      popcat = input$popcat
    )

    preds <- predict(model, pred_data, type = "probs")
    preds <- round(preds * 100, 0)
    preds <- tidyr::tibble(
      cats = names(preds),
      probs = preds
    )

    preds$cats <- factor(
      preds$cats,
      levels = c(
        "No opinion",
        "Disagree",
        "Agree"
      ),
      labels = c(
        "No opinion",
        "Disagree",
        "Agree"
      ),
      ordered = TRUE
    )

    ggplot2::ggplot(preds, ggplot2::aes(x = cats, y = probs, fill = cats)) +
      ggplot2::geom_col() +
      ggplot2::coord_flip() +
      ggplot2::geom_text(ggplot2::aes(label = paste0(probs, "%")), nudge_y = 3.5) +
      ggplot2::theme_minimal() +
      ggplot2::scale_fill_manual(
        values = c(
          "Agree" = "#00e335",
          "Disagree" = "#e30000",
          "No opinion" = "gray"
        )
      ) +
      ggplot2::theme(
        legend.position = "none",
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
      )
  })
}

# Create Shiny app ----
shiny::shinyApp(ui = ui, server = server)
