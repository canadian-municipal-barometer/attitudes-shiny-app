library(shiny)
library(shiny.i18n)
source("helpers.R")
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
        id = "lang_toggle",
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

# `tbl`: main voter data
load("data/voter-data.rda")
# `statements_en`, `statements_fr`, `statement_tags_en`, `statement_tags_fr`
load("data/statements.rda")

# Set the error that is displayed if model inputs aren't present for a policy
input_err <- "The combination of the policy question and demographic characteristics that you have selected aren't in the data. Please make another selection." # nolint

# load translation file to create shiny.i18n translator object
translator <- Translator$new(translation_csvs_path = "data/translation/")

server <- function(input, output, session) {
  # set statement data (including tags) and shiny.i18n translator based on
  # the current value of `lang_toggle`

  # Initialize reactive values
  current_lang <- reactiveVal("en")
  translator_r <- reactiveVal(translator)
  statements <- reactiveVal(statements_en)
  statement_tags <- reactiveVal(statement_tags_en)

  # Handle language toggle
  observeEvent(input$lang_toggle, {
    message("lang_toggle button pressed")
    # Toggle language between English and French
    if (current_lang() == "en") {
      current_lang("fr")
      translator_r()$set_translation_language("fr")
      statements(statements_fr)
      statement_tags(statement_tags_fr)
      message(str(statements()))
      message(str(statement_tags()))
    } else {
      current_lang("en")
      translator_r()$set_translation_language("en")
      statements(statements_en)
      statement_tags(statement_tags_en)
      message(str(statements()))
      message(str(statement_tags()))
    }
  })

  observeEvent(input$lang_toggle, {
    message("lang_toggle label updated")
    # Update button text
    updateActionButton(
      session,
      "lang_toggle",
      # Update without shiny.i18n to avoid circular dependency
      label = ifelse(current_lang() == "en", "FR", "EN")
    )
  })

  # UI Rendering --------------------

  # title panel
  output$title <- renderUI({
    titlePanel(translator_r()$t("Canadians' Municipal Policy Attitudes"))
  })

  # sidebar
  output$sidebar_contents <- render_sidebar(translator = translator_r) # nolint

  # mainPanel
  output$mainpanel <- render_mainpanel(
    translator = translator_r,
    statements = statements
  )

  # filter the data used in the plot
  filtered_df <- reactive({
    message("filter_data reactive context entered")
    req(input$policy)
    filter_data(
      reactive_input = input,
      statements = statements,
      tbl = tbl
    )
  })

  # un-translated inputs if they were translated to French in the UI
  selected <- reactive({
    un_translate_input(reactive_input = input) # nolint
  })

  # plot
  output$predictions <- render_attitudes_plot(
    selected = selected,
    input_err = input_err,
    statements = statements,
    filtered_df = filtered_df,
    translator = translator_r
  )

  # main panel observers

  # Policy domain menu
  output$select_domain <- renderUI({
    req(statement_tags())
    selectInput(
      inputId = "policy_group",
      label = translator_r()$t("Policy domain:"),
      choices = statement_tags(),
      multiple = TRUE,
      selected = statement_tags()[1],
      selectize = TRUE,
      width = "325px"
    )
  })

  # Reset button
  observeEvent(input$delete, {
    updateSelectInput(
      session,
      "policy_group",
      choices = statement_tags(),
      selected = character(0)
    )
  })

  # update policy statement menu based on policy domain menu
  observeEvent(input$policy_group, {
    req(statements())
    message("policy selector choice observer called")
    data <- statements()

    selected_policies <- data |>
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
}

shinyApp(ui = ui, server = server)
