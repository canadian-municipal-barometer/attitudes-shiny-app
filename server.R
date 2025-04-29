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
  current_lang <- reactiveVal("en")
  input_state <- reactiveVal(TRUE)
  translator_r <- reactiveVal(translator)
  statements <- reactiveVal(statements_en)
  data <- reactiveVal()

  # Handle language toggle
  observeEvent(input$lang_toggle, {
    # Toggle language between English and French
    if (current_lang() == "en") {
      updateSelectInput(
        session = session,
        inputId = "policy_domain",
        choices = statement_tags_en, # nolint
        selected = statement_tags_en[1]
      )
      selected <- statements_update(
        session = session,
        statement_data = statements_en, # nolint
        domain = statement_tags_en[1] # nolint
      )
      plot_data <- filter_statements(statement_tags_en, selected)
      data(plot_data)
      statements(statements_en)

      current_lang("fr")
      translator_r()$set_translation_language("fr")
    } else {
      # TODO:
      current_lang("en")
      translator_r()$set_translation_language("en")
    }
    message("\nlang_toggle complete")
    message(paste("`current_lang`:", current_lang(), "\n"))
    # invalidate `input_state` because of updates to underlying statements and
    # tags data
    input_state(FALSE)
    message(paste("`input_state` = ", input_state()))
  })

  observeEvent(input$lang_toggle, {
    message("\n`lang_toggle` label updated\n")
    # Update button text
    updateActionButton(
      session,
      "lang_toggle",
      # Update without shiny.i18n to avoid circular dependency
      label = ifelse(current_lang() == "en", "FR", "EN")
    )
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

  # update policy statement menu based on policy domain menu
  observeEvent(input$select_domain, {
    message("\n`select_domain` observer")
    statements_update(
      session,
      statements,
      input$select_domain
    )
  })

  # UI Rendering --------------------

  # title panel
  output$title <- renderUI({
    titlePanel(translator_r()$t("Canadians' Municipal Policy Attitudes"))
  })

  # sidebar
  output$sidebar_contents <- render_sidebar(translator = translator_r) # nolint

  filtered_svy <- eventReactive(input$policy, {
    filter_statements(
      statements = statements,
      svy_data = tbl,
      policy = input$policy
    )
  })

  # plot
  output$predictions <- render_attitudes_plot(
    statements = statements,
    input_err = input_err,
    input = reactive({
      input
    }),
    tbl = filtered_svy, # nolint
    translator = translator_r,
    input_state = input_state
  )

  # mainPanel
  output$mainpanel <- render_mainpanel(
    translator = translator_r,
    statements = statements
  )
}
