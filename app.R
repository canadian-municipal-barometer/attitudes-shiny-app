library(shiny)
library(shiny.i18n)
source("helpers.R")
source("components/language_update.R")
source("components/render_sidebar.R")
source("components/render_mainpanel.R")
source("components/render_attitudes_plot.R")

ui <- fluidPage(
  # formatting --------------------
  # set CSS for elements that don't accept a style argument in their function
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
        max-width: 80vw;
      }
      /* put the selected policy in bold so it serves as a title for the plot */
      #policy-div .selectize-input {
        font-weight: bold;
      }
      .col-sm-4 {
        max-width: 30%;
      }
      .col-sm-8 {
        max-width: 100%;
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
  # layout --------------------
  # set HTML divs to be formatted by the above CSS
  # CSS Flexbox formatting is being used on the next two divs such that the app
  # centers on the page rather than only stretching horizontally
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
          max-width: 80vw;
        ",
        uiOutput("title"),
        a(
          img(
            src = "https://www.cmb-bmc.ca/wp-content/uploads/2024/09/logo-bmc-cmb.svg" # nolint
          ),
          href = "https://www.cmb-bmc.ca/"
        ),
      ),
      # the language toggle button is a custom feature and doesn't fit well with
      # any existing elements, so it gets its own div. It is also using some
      # cheeky CSS to overlap the title div of the tabPanel (in mainPanel).
      div(
        id = "lang-toggle",
        style = "
          display: flex;
          justify-content: flex-end;
          /* to move outside the bounds of its parent */
          position: relative;
        ",
        # language toggle
        # needs to defined statically to avoid circular reactivity
        actionButton(
          "lang_toggle",
          "FR",
          style = "
              color: gray;
              font-weight: bold;
              border: 0px;
              /* to move outside the bounds of its parent */
              position: absolute;
              bottom: -37px;
              right: 2vw;
              /* to ensure the button is above the divs it overlaps */
              z-index: 1000;
            "
        )
      ),
      sidebarLayout(
        fluid = TRUE,
        # from "components/"
        uiOutput("sidebar_contents"),
        div(
          class = "main-panel",
          style = "
            width: 80vw;
            min-width: 800px;
          ",
          uiOutput("mainpanel")
        )
      )
    )
  )
)

# data prep --------------------

# `df`: main voter data
load("data/voter-data.rda")
# `statements_en`, `statements_fr`, `statement_tags_en`, `statement_tags_fr`
load("data/statements.rda")

# Set the error that is displayed if model inputs aren't present for a policy
input_err <- "The combination of the policy question and demographic characteristics that you have selected aren't in the data. Please make another selection." # nolint

# load translation file to create shiny.i18n translator object
translator <- Translator$new(translation_csvs_path = "data/translation/")

server <- function(input, output, session) {
  statements_data <- reactiveValues(
    en = statements_en, # nolint
    fr = statements_fr, # nolint
    tags_en = statement_tags_en, # nolint
    tags_fr = statements_tags_fr # nolint
  )

  # set statement data (including tags) and shiny.i18n translator based on
  # the current value of `lang_toggle`

  statements <- eventReactive(input$lang_toggle, {
    if (state %% 2 == 1) {
      statements_data$fr
    } else {
      statements_data$en
    }
  })

  statement_tags <- eventReactive(input$lang_toggle, {
    if (state %% 2 == 1) {
      statements_data$tags_fr
    } else {
      statements_data$tags_en
    }
  })

  i18n <- reactive({
    state <- input$lang_toggle
    if (state %% 2 == 1) {
      translator$set_translation_language("fr")
    } else {
      translator$set_translation_language("en")
    }
    return(translator)
  })

  # UI Rendering --------------------

  # title panel
  output$title <- renderUI({
    titlePanel(i18n()$t("Canadians' Municipal Policy Attitudes"))
  })

  # sidebar
  output$sidebar_contents <- render_sidebar(translator = i18n) # nolint

  # mainPanel
  output$mainpanel <- render_mainpanel(
    translator = i18n
  )

  # update the static language button's label on translation toggle
  observe({
    updateActionButton(session, "lang_toggle", label = i18n()$t("FR"))
  })

  # plot
  output$predictions <- render_attitudes_plot(
    input = input,
    input_err = input_err,
    statements = statements,
    df = df,
    translator = i18n
  )

  # main panel observers

  # Policy domain menu
  output$select_domain <- renderUI({
    selectInput(
      inputId = "policy_group",
      label = i18n()$t("Policy domain:"),
      choices = statement_tags, # nolint
      multiple = TRUE,
      selected = "Housing",
      selectize = TRUE,
      width = "325px"
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

  # disable any lag due to server-rendering and lazy loading for "select_domain"
  outputOptions(output, "select_domain", suspendWhenHidden = FALSE)
}

shinyApp(ui = ui, server = server)
