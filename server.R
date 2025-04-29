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
  statements <- reactiveVal(statements_en) # nolint
  statement_tags <- reactiveVal(statement_tags_en) # nolint

  observe({
    message(paste(
      "\n=====",
      "\n`statements() updated`",
      "\n---`statements$statement[1]`:",
      statements()$statement[1],
      "\n\n`statement_tags()` updated",
      "\n---`statement_tags[1]`:",
      statement_tags()[1],
      "\n"
    ))
  })

  # Handle language toggle
  observeEvent(input$lang_toggle, {
    # Toggle language between English and French
    if (current_lang() == "en") {
      statements(statements_fr) # nolint
      statement_tags(statement_tags_fr) # nolint
      current_lang("fr")
      translator_r()$set_translation_language("fr")
    } else {
      statements(statements_en) # nolint
      statement_tags(statement_tags_en) # nolint
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

  reactive_policy <- eventReactive(input$policy, {
    input$policy
  })

  reactive_domain <- eventReactive(input$policy_domain, {
    input$policy_domain
  })

  observe({
    message("\n===================================================")
    message("state observer entered")
    message(paste("\n`reactive_policy` (`input$policy`) =", reactive_policy()))
    message(paste(
      "\nstatements()$statement[1]` = ",
      statements()$statement[1]
    ))
    message(paste(
      "\n`reactive_domain` (`input$select_domain`) =",
      reactive_domain()
    ))
    message(paste("\n`statement_tags()[1]` = ", statement_tags()[1]))
    # only operate while in the updating state, when `input_state` is invalidated # nolint

    req(input_state() == FALSE)
    message("`input_state` observer in invalidated state")
    # once data and UI values are aligned, validate `input_state`
    req(statements()$statement[1] == reactive_policy)
    req(statement_tags()[1] == reactive_domain)
    input_state(TRUE)
  })

  # main reactive elements --------------------

  # Policy domain menu
  # BUG: This probably shouldn't run at all after the app is initially loaded,
  # except to re-set it's label using the translator
  output$select_domain <- renderUI({
    message("`select_domain` initialized")
    selectInput(
      inputId = "select_domain",
      label = translator_r()$t("Policy domain:"),
      choices = NULL,
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

  observeEvent(statement_tags(), {
    isolate(statements())

    # NOTE: only runs when translation is triggered
    message("\n`statement_tags()` observer")

    tags_update(session, statement_tags) # nolint

    statements_update(
      session,
      statements,
      input$select_domain
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

  # mainPanel
  output$mainpanel <- render_mainpanel(
    translator = translator_r,
    statements = statements
  )

  # plot
  output$predictions <- render_attitudes_plot(
    statements = statements,
    input_err = input_err,
    input = reactive({
      input
    }),
    tbl = tbl, # nolint
    translator = translator_r,
    input_state = input_state
  )
}
