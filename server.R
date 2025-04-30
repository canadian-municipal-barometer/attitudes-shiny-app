library(shiny)

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
  cat("server function entered")

  # Initialize reactive values
  current_lang_r <- reactiveVal("en")
  # input_state_r <- reactiveVal(TRUE)
  translator_r <- reactiveVal(translator)
  statements_r <- reactiveVal(statements_en) # nolint
  statement_tags_r <- reactiveVal(statement_tags_en)
  svy_data_r <- reactiveVal(tbl) #nolint

  # Handle language toggle
  observeEvent(input$lang_toggle, {
    # Toggle language between English and French
    if (current_lang_r() == "en") {
      statements_r(statements_fr)
      statement_tags_r(statement_tags_fr)

      updateActionButton(
        session,
        "lang_toggle",
        # Update without shiny.i18n to avoid circular dependency
        label = ifelse(current_lang_r() == "en", "FR", "EN")
      )

      current_lang_r("fr")

      # NOTE: Disabling shiny.i18n translation while implementing data update
      # upon language toggle

      # translator_r()$set_translation_language("fr")
    } else {
      # TODO:
      message("English lang toggle branch")
    }
    message("\nlang_toggle complete")
    message(paste("`current_lang`:", current_lang_r(), "\n"))
  })

  # main reactive elements --------------------

  # Policy domain menu
  output$select_domain <- renderUI({
    message("`select_domain` initialized")
    selectInput(
      inputId = "select_domain",
      label = translator_r()$t("Policy domain:"),
      choices = statement_tags_en, # nolint
      selectize = TRUE,
      width = "325px"
    )
  })

  # policy statements menu
  output$policy <- renderUI({
    selectInput(
      inputId = "policy",
      label = translator_r()$t("Select a policy:"),
      # updated in `server` first time `select_domain` input used
      choices = NULL,
      selectize = TRUE,
      width = "auto",
    )
  })

  observeEvent(statement_tags_r(), {
    message("\n`select_domain` UI update")
    updateSelectInput(
      session,
      "select_domain",
      choices = statement_tags_r()
    )
  })

  # update policy statement menu based on policy domain menu
  observeEvent(input$select_domain, {
    message("\n`select_domain` observer")
    statements_update(
      session = session,
      statements_r = statements_r,
      domain = input$select_domain
    )
  })

  # UI Rendering --------------------

  # title panel
  output$title <- renderUI({
    titlePanel(translator_r()$t("Canadians' Municipal Policy Attitudes"))
  })

  # sidebar
  output$sidebar_contents <- render_sidebar(translator = translator_r) # nolint

  filtered_svy_r <- eventReactive(input$policy, {
    filter_statements(
      statements = statements_r,
      svy_data_r = svy_data_r,
      policy = input$policy
    )
  })

  # un-translated inputs if they were translated to French in the UI
  user_selected <- reactive({
    un_translate_input(input)
  })

  # plot
  output$predictions <- render_attitudes_plot(
    statements_r = statements_r,
    filtered_svy_data_r = filtered_svy_r, # nolint
    translator_r = translator_r,
    user_selected = user_selected,
    input_err = input_err
  )

  # mainpanel
  output$mainpanel <- render_mainpanel(
    translator_r = translator_r,
    statements_r = statements_r
  )
}
