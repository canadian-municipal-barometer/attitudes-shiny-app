library(shiny)
library(shiny.i18n)
source("helpers.R")
source("components/language_update.R")
source("components/render_sidebar.R")
source("components/render_mainpanel.R")
source("components/render_attitudes_plot.R")

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

# translation file
i18n <- Translator$new(translation_csvs_path = "data/translation/")

ui <- fluidPage(
  # necessary for shiny.i18n reactive translation
  shiny.i18n::usei18n(i18n),
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
      /* put the selected policy in bold so it serves as a title for the plot */
      #policy-div .selectize-input {
        font-weight: bold;
      }
      @media (max-width: 1100px) {
        .main-panel {
          width: auto;
          min-width: 400px;
        }
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
        uiOutput("title_panel"),
        img(
          src = "https://www.cmb-bmc.ca/wp-content/uploads/2024/09/logo-bmc-cmb.svg" # nolint
        ),
      ),
      div(
        id = "lang-toggle",
        style = "
          display: flex;
          justify-content: flex-end;
          /* to move outside the bounds of its parent */
          position: relative;
        ",
        uiOutput("language_toggle")
      ),
      sidebarLayout(
        fluid = TRUE,
        # from "components/"
        render_sidebar(),
        div(
          class = "main-panel",
          style = "
            width: 70vw;
            min-width: 800px;
          ",
          uiOutput("mainpanel")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # reactive i18n object for use in `update*` functions under `language_update`
  i18n_r <- reactiveValues(translator = i18n)

  # standard update of the translator object for server-rendered UI elements
  translator <- reactive({
    if (input$lang_toggle %% 2 == 1) {
      i18n$set_translation_language("fr")
    } else {
      i18n$set_translation_language("en")
    }
    i18n
  })

  # UI Rendering --------------------

  # title panel
  output$title_panel <- renderUI(
    # BUG: need to figure out how to use a translator object on strings like this one
    titlePanel(translator$t("Canadians' Municipal Policy Attitudes"))
  )

  # language toggle
  output$language_toggle <- renderUI(
    actionButton(
      "lang_toggle",
      i18n$t("FR"),
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
  )

  # main panel

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

  # language toggle observer
  language_update(session, input, i18n_r) # nolint

  # mainpanel --------------------
  output$mainpanel <- render_mainpanel(default_policies) # nolint

  # plot --------------------
  output$predictions <- render_attitudes_plot(input, input_err, statements, df) # nolint

  # disable any lag due to server-rendering and lazy loading for "select_domain"
  outputOptions(output, "select_domain", suspendWhenHidden = FALSE)
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
