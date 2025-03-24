library(shiny)
library(shiny.i18n)
source("helpers.R")

# data prep --------------------

# load `df` object: main voter data
load("data/voter-data.rda")
# load `statements` object: main statement data lookup
load("data/statements.rda")
# load `tags` object: choice set for "policy_group" input
load("data/unique-tags.rda")

# Choices for "policy" input need to be set.
# must match the `selected` parameter of the "policy_group" selector
default_policies <- statements$statement[
  statements$tags %in% statements$tags[1][1]
]

# Set the error that is displayed if model inputs aren't present for a policy
input_err <- "The combination of the policy question and demographic characteristics that you have selected aren't in the data. Please make another selection." # nolint


# Translation --------------------
# load translation file
i18n <- Translator$new(translation_csvs_path = "data/translation/")
# Set initial language
i18n$set_translation_language("en")

ui <- fluidPage(
  # set CSS for elements that don't accept a style argument in their constructor
  tags$head(
    tags$style(HTML(
      "
      body, html {
        height: 100%;
        margin: 0;
      }
      /* formats sidebarLayout */
      .shiny-layout {
        align-items: start; /* aligned with the top of the parent div */
      }
    "
    ))
  ),
  # set HTML divs to be formatted by the above CSS
  div(
    class = "flex-container",
    style = "
      display: flex;
      justify-content: center;
      height: 100%;
      width: 100%;
    ",
    div(
      class = "shiny-layout",
      div(
        id = "header",
        style = "
          display: flex;
          align-items: center;
          justify-content: space-between;
          height: 150px;
        ",
        titlePanel("Canadians' Municipal Policy Attitudes"),
        img(
          src = "https://www.cmb-bmc.ca/wp-content/uploads/2024/09/logo-bmc-cmb.svg" # nolint
        ),
      ),
      div(
        id = "lang-toggle",
        actionButton(
          "lang_button",
          "FR",
          style = "
            color: gray;
            font-weight: bold;
            border: 0px;
            /* to move outside the bounds of its parent */
            position: absolute;
            bottom: -37px;
            right: 10px;
            /* to ensure the button is above the divs it overlaps */
            z-index: 1000;
          "
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
              "Woman",
              "Man"
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
              "Racialized minority",
              "White"
            )
          ),
          radioButtons(
            inputId = "immigrant",
            label = "Immigrant:",
            choices = list(
              i18n$t("Yes"),
              i18n$t("No")
            ),
            inline = TRUE
          ),
          radioButtons(
            inputId = "homeowner",
            label = "Homeowner:",
            choices = list(
              i18n$t("Yes"),
              i18n$t("No")
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
              "$50,000 - $99,999",
              "$100,000 - $149,999",
              "$150,000 - $199,999",
              "$200,000 or more"
            ),
            selectize = FALSE
          )
        ),
        div(
          class = "main-panel",
          style = "
            width: 70vw;
            min-width: 800px;
          ",
          mainPanel(
            tabsetPanel(
              type = "pill",
              selected = "Instructions",
              tabPanel(
                title = "Plot",
                # prevent lazy loading
                loadOnActivate = FALSE,
                # spacing hack
                h1("\n"),
                p(
                  "Please select one or more policy domains, and then choose a specific question from the drop-down menu below." # nolint
                ),
                p(
                  'To reset the policy domains, click "Reset".'
                ),
                # select the policy group to filter by
                div(
                  style = "
                    display: flex;
                    align-items: flex-start;
                     align-items: end;
                   ",
                  uiOutput(
                    outputId = "select_domain",
                    # Make the select_domain div's formatting match the reset button # nolint
                    style = "
                      align-items: bottom;
                    "
                  ),
                  actionButton("delete", "Reset", style = "margin: 15px")
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
                  style = "
                    background: transparent !important;
                  ",
                  plotOutput(
                    "predictions",
                    width = "100%",
                    height = "400px"
                  )
                )
              ),
              tabPanel(
                title = "Instructions",
                h1("\n"),
                p("Welcome!"),
                p(
                  "This interactive app allows you to explore the policy attitudes of specific demographic groups on the largest and most diverse set of municipal policy issues ever included in a survey of Canadians." # nolint
                ),
                p(
                  'In the first menu of the "Plot" tab above, select one or more policy domains. The second menu contains specific policy statements belonging to the policy domains you selected. Use the second menu to view public opinion on a specific policy. To clear the policy domain box, press the "Reset" button.' # nolint
                ),
                p(
                  "Finally, adjust the set of characteristics in the panel to the left to see how different groups view the policy you have selected. When you're done, you can select a new policy by again using the menus above the plot." # nolint
                )
              ),
              tabPanel(
                title = "Details",
                h1("\n"),
                p(
                  "The data for this app come from the Canadian Municipal Barometer's annual", # nolint
                  a(
                    "Citizen Survey",
                    href = "https://www.cmb-bmc.ca/wp-content/uploads/2025/03/CMB-2025-General-Population-Codebook.pdf", # nolint
                    .noWS = c("after")
                  ),
                  ". Currently, it uses the 2025 data. It will soon be updated with more questions from the 2025 survey, and, in future years, new surveys will be added." # nolint
                ),
                p(
                  "Weights were constructed using iterative proportional fitting (see ", # nolint
                  a(
                    "DeBell and Krosnick",
                    href = "https://www.electionstudies.org/wp-content/uploads/2018/04/nes012427.pdf", # nolint
                    .noWS = c("after")
                  ),
                  ")."
                ),
                p(
                  "Note that due to a small number of responses in Prince Edward Island, many of the policy issues for that province do not produce reliable estimates of public opinion, which sometimes leads to odd results when Prince Edward Island is selected." # nolint
                )
              )
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # UI Rendering --------------------

  # Policy domain menu
  output$select_domain <- renderUI({
    selectInput(
      inputId = "policy_group",
      label = "Policy domain:",
      choices = statement_tags, # nolint
      multiple = TRUE,
      selected = "Housing",
      selectize = TRUE,
      width = "300px"
    )
  })
  # Reset button
  observeEvent(input$delete, {
    updateSelectInput(
      session,
      "policy_group",
      choices = statement_tags, # nolint
      selected = character(0)
    )
  })
  # update policy statement menu based on policy domain menu
  observeEvent(input$policy_group, {
    selected_policies <- statements |> # nolint
      dplyr::filter(
        purrr::map_lgl(tags, function(x) any(x %in% input$policy_group))
      ) |>
      dplyr::pull(statement) # nolint

    updateSelectInput(
      session,
      "policy",
      choices = selected_policies
    )
  })
  # toggle language
  observeEvent(input$lang_toggle, {
    print(paste("Language change!", input$lang_toggle))
    # actionButton values start a 0 and go up by 1 every time they activate
    # So, all odd values input$lang_toggle will occur when the app is in English
    # and this the language should be updated to French
    if (input$lang_toggle %% 2 == 1) {
      shiny.i18n::update_lang(language = "fr", session = session)
    } else {
      shiny.i18n::update_lang(language = "en", session = session)
    }
  })

  # plot --------------------
  output$predictions <- renderPlot(
    {
      # reactive objects (input) need to be digested in a reactive block (in
      # this case, renderPlot)
      selected <- un_translate_input(input = input) # nolint

      # policy to filter the data by
      filter <- statements$var_name[statements$statement == input$policy] # nolint

      # translate the contents of the selectors to variable names

      tmp_df <- df |> dplyr::filter(policy == filter) # nolint

      # verify that tmp_df has the levels needed for the model to run
      validate(
        need(input$province %in% tmp_df$province, input_err)
      )

      model <- nnet::multinom(
        factor(outcome) ~ factor(gender) + factor(education) + factor(province) + factor(agecat) + factor(race) + factor(homeowner) + factor(income) + factor(immigrant) + factor(popcat), # nolint
        data = tmp_df,
        weights = tmp_df$wgt
      )

      pred_data <- data.frame(
        gender = input$gender,
        education = input$education,
        province = input$province,
        agecat = input$agecat,
        race = input$race,
        homeowner = selected["homeowner"],
        income = input$income,
        immigrant = selected["immigrant"],
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

      plot <- ggplot2::ggplot(
        preds,
        ggplot2::aes(x = cats, y = probs, fill = cats) # nolint
      ) +
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
      return(plot)
    },
    bg = "transparent"
  )
  # disable any lag due to server-rendering and lazy loading for "select_domain"
  outputOptions(output, "select_domain", suspendWhenHidden = FALSE)
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
