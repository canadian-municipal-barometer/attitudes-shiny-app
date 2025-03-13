# data prep --------------------

# load local, client-side Duckdb database
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "data/voter-data-char.duckdb")
df <- DBI::dbReadTable(con, name = "policyData")
DBI::dbDisconnect(con)

# load statement text object (`statements`)
load("data/statement-text.rda")

# create policy/group lookup
policy_lookup <- statements |>
  dplyr::group_by(group_id, group_name) |>
  dplyr::summarize(var = list(unique(var_name), .groups = "drop"))

# choices for "policy" input need to be set. They should match the policies
# belonging to the first group in the data
default_policies <- statements$statement[
  statements$group_name == statements$group_name[1]
]

# main --------------------

# belonging to the first row's group name
ui <- shiny::fluidPage(
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
        shiny::titlePanel("Canadians' Municipal Policy Attitudes"),
        shiny::img(
          src =
            "https://www.cmb-bmc.ca/wp-content/uploads/2024/09/logo-bmc-cmb.svg"
        )
      ),
      shiny::sidebarLayout(
        fluid = TRUE,
        shiny::sidebarPanel(
          style = "
              max-width: 35vw;
              min-width: 225px;
              background-color: #e6eff7 !important;
            ",
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
              "Saskatchewan"
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
          shiny::radioButtons(
            inputId = "gender",
            label = "Gender:",
            choices = list(
              "Woman" = "Woman",
              "Man" = "Man"
            ),
            inline = TRUE
          ),
          shiny::selectInput(
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
          shiny::radioButtons(
            inputId = "race",
            label = "Race:",
            choices = list(
              "Racialized minority" = "Racialized minority",
              "White" = "White"
            )
          ),
          shiny::radioButtons(
            inputId = "immigrant",
            label = "Immigrant:",
            choices = list(
              "Yes" = "Yes",
              "No" = "No"
            ),
            inline = TRUE
          ),
          shiny::radioButtons(
            inputId = "homeowner",
            label = "Homeowner:",
            choices = list(
              "Yes" = "Yes",
              "No" = "No"
            ),
            inline = TRUE
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
          )
        ),
        div(
          class = "main-panel",
          shiny::mainPanel(
            shiny::tabsetPanel(
              type = "pill",
              shiny::tabPanel(
                title = "Plot",
                # spacing hack
                shiny::h1("\n"),
                # select the policy group to filter by
                shiny::selectInput(
                  inputId = "policy_group",
                  label = "Policy area:",
                  choices = unique(statements$group_name),
                  selectize = FALSE,
                  width = "20%"
                ),
                # select the filtered policies
                shiny::selectInput(
                  inputId = "policy",
                  label = "Select a policy:",
                  choices = default_policies,
                  selectize = FALSE,
                  width = "auto"
                ),
                div(
                  id = "plot-container",
                  shiny::plotOutput(
                    "predictions",
                    width = "100%",
                    height = "400px"
                  )
                )
              ),
              shiny::tabPanel(
                title = "Instructions",
                width = "auto",
                shiny::h1("\n"),
                shiny::p("Welcome!"),
                shiny::p("This interactive app allows you to explore the policy attitudes of specific demographic groups on the largest and most diverse set of municipal policy issues ever included in a survey of Canadians."),
                shiny::p("In the first menu of the “Plot” tab above, select a policy area; then, in the second menu, select a specific policy."),
                shiny::p("Finally, adjust the set of characteristics in the panel to the left to see how different groups view the policy you have selected. When you're done, you can select a new policy by again using the menus above the plot.")
              ),
              shiny::tabPanel(
                title = "Details",
                shiny::h1("\n"),
                shiny::p("The data for this app come from the Canadian Municipal Barometer’s annual Citizen Survey. Currently, it uses the 2025 data. It will soon be updated with more questions from the 2025 survey, and in future years new surveys will be added."),
                shiny::p(
                  "Responses were weighted using the protocol outlined in",
                  shiny::a(
                    "Computing Weights for American National Election Study Survey Data",
                    href = "https://www.electionstudies.org/wp-content/uploads/2018/04/nes012427.pdf"
                  ),
                  "by DeBell and Krosnick."
                ),
                shiny::p("Note that due to a small number of responses in Prince Edward Island, many of the policy issues for that province do not produce reliable estimates of public opinion.")
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
  shiny::observeEvent(input$policy_group, {
    selected_policies <- statements$statement[
      statements$group_name == input$policy_group
    ]
    shiny::updateSelectInput(
      session,
      "policy",
      choices = selected_policies
    )
  })

  output$predictions <- shiny::renderPlot(
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
shiny::shinyApp(ui = ui, server = server)
