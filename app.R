# data prep --------------------

# load `df` object: main voter data
load("data/voter-data.rda")
# load `statements` object: main statement data lookup
load("data/statements.rda")
# load `tags` object: choice set for "policy_group" input
load("data/unique-tags.rda")

# choices for "policy" input need to be set. They can match the policies
# belonging to the first tag of the first statement
# must match the `selected` parameter of the "policy_group" selector
default_policies <- statements$statement[
  statements$tags %in% statements$tags[1][1]
]

# main --------------------

# belonging to the first row's group name
ui <- fluidPage(
  # set CSS
  # Use flexbox to align the whole app in the center of the viewer
  tags$head(
    tags$style(HTML("
      body, html {
        height: 100%;
        margin: 0;
      }
      .flex-container {
        display: flex;
        justify-content: center;
        height: 100%;
        width: 100%;
      }
      .shiny-layout { /* formats sidebarLayout */
        align-items: start;
      }
      .main-panel {
        width: 70vw;
        min-width: 800px;
      }
      #plot-container { background: transparent !important; }
      #header {
        display: flex;
        align-items: center;
        justify-content: space-between;
        height: 150px;
      }
    "))
  ),
  # set HTML divs to be formatted by the above CSS
  div(
    class = "flex-container",
    div(
      class = "shiny-layout",
      div(
        id = "header",
        titlePanel("Canadians' Municipal Policy Attitudes"),
        img(
          src =
            "https://www.cmb-bmc.ca/wp-content/uploads/2024/09/logo-bmc-cmb.svg"
        )
      ),
      sidebarLayout(
        fluid = TRUE,
        sidebarPanel(
          style = "
              max-width: 35vw;
              min-width: 225px;
              background-color: #e6eff7 !important;
            ",
          selectInput(
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
              "Saskatchewan"
            ),
            selectize = FALSE
          ),
          selectInput(
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
          radioButtons(
            inputId = "gender",
            label = "Gender:",
            choices = list(
              "Woman" = "Woman",
              "Man" = "Man"
            ),
            inline = TRUE
          ),
          selectInput(
            inputId = "agecat",
            label = "Age:",
            choices = c(
              "18-29",
              "30-44",
              "45-59",
              "60+"
            ),
            selectize = FALSE
          ),
          radioButtons(
            inputId = "race",
            label = "Race:",
            choices = list(
              "Racialized minority" = "Racialized minority",
              "White" = "White"
            )
          ),
          radioButtons(
            inputId = "immigrant",
            label = "Immigrant:",
            choices = list(
              "Yes" = "Yes",
              "No" = "No"
            ),
            inline = TRUE
          ),
          radioButtons(
            inputId = "homeowner",
            label = "Homeowner:",
            choices = list(
              "Yes" = "Yes",
              "No" = "No"
            ),
            inline = TRUE
          ),
          selectInput(
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
          selectInput(
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
          )
        ),
        div(
          class = "main-panel",
          mainPanel(
            tabsetPanel(
              type = "pill",
              tabPanel(
                title = "Plot",
                # spacing hack
                h1("\n"),
                # select the policy group to filter by
                selectInput(
                  inputId = "policy_group",
                  label = "Policy domain:",
                  choices = statement_tags,
                  multiple = TRUE,
                  selected = "Housing",
                  selectize = TRUE,
                  width = "30%"
                ),
                # select the filtered policies
                selectInput(
                  inputId = "policy",
                  label = "Select a policy:",
                  # updated in `server` first time `policy_group` input used
                  choices = default_policies,
                  selectize = FALSE,
                  width = "auto"
                ),
                div(
                  id = "plot-container",
                  plotOutput(
                    "predictions",
                    width = "100%",
                    height = "400px"
                  )
                )
              ),
              tabPanel(
                title = "Instructions",
                width = "auto",
                h1("\n"),
                p("Welcome!"),
                p(
                  "This interactive app allows you to explore the policy",
                  "attitudes of specific demographic groups on the largest and",
                  "most diverse set of municipal policy issues ever included",
                  "in a survey of Canadians."
                ),
                p(
                  "In the first menu of the “Plot” tab above, select a policy",
                  " domain; then, in the second menu, select a specific policy."
                ),
                p(
                  "Finally, adjust the set of characteristics in the panel to",
                  "the left to see how different groups view the policy you",
                  "have selected. When you're done, you can select a new",
                  "policy by again using the menus above the plot."
                )
              ),
              tabPanel(
                title = "Details",
                h1("\n"),
                p(
                  "The data for this app come from the Canadian Municipal",
                  "Barometer’s annual Citizen Survey. Currently, it uses the",
                  "2025 data. It will soon be updated with more questions from",
                  "the 2025 survey, and in future years new surveys will be",
                  "added."
                ),
                p(
                  "Weights were constructed using iterative proportional",
                  "fitting (see ",
                  a(
                    "DeBell and Krosnick",
                    href = "https://www.electionstudies.org/wp-content/uploads/2018/04/nes012427.pdf"
                  ),
                  "for details)."
                ),
                p(
                  "Note that due to a small number of responses in Prince",
                  "Edward Island, many of the policy issues for that province",
                  "do not produce reliable estimates of public opinion, which",
                  "sometimes leads to odd results when Prince Edward Island is",
                  "selected."
                )
              )
            )
          )
        )
      )
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  observeEvent(input$policy_group, {
    selected_policies <- statements |>
      dplyr::filter(
        purrr::map_lgl(tags, function(x) any(x %in% input$policy_group))
      ) |>
      dplyr::pull(statement)

    updateSelectInput(
      session,
      "policy",
      choices = selected_policies
    )
  })

  output$predictions <- renderPlot(
    {
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
        data = tmp_df,
        weights = tmp_df$wgt
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
        ggplot2::geom_text(
          ggplot2::aes(label = paste0(probs, "%")),
          nudge_y = 3.5
        ) +
        ggplot2::theme_minimal(base_size = 20) +
        ggplot2::scale_fill_manual(
          values = c(
            "Agree" = "#0091AC",
            "Disagree" = "#000",
            "No opinion" = "#6C6E74"
          )
        ) +
        ggplot2::theme(
          legend.position = "none",
          axis.title.x = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank()
        )
    },
    bg = "transparent"
  )
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
