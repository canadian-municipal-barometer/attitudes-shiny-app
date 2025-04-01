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

ui <- fluidPage(
  # necessary for shiny.i18n reactive translation
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
        uiOutput("sidebar_contents"),
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

# load translation file to create shiny.i18n translator object
translator <- Translator$new(translation_csvs_path = "data/translation/")

server <- function(input, output, session) {
  # convert translator to reactive object
  i18n <- reactive({
    observeEvent()
    selected <- input$selected_language
    if (length(selected) > 0 && selected %in% translator$get_languages()) {
      translator$set_translation_language(selected)
    }
    translator
  })

  # UI Rendering --------------------

  # title panel
  output$title_panel <- renderUI({
    titlePanel("Canadians' Municipal Policy Attitudes")
  })

  # language toggle
  output$language_toggle <- renderUI(
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

  # sidebar
  output$sidebar_contents <- render_sidebar() # nolint

  # mainpanel
  output$mainpanel <- render_mainpanel(default_policies) # nolint

  # plot
  output$predictions <- render_attitudes_plot(input, input_err, statements, df) # nolint

  # disable any lag due to server-rendering and lazy loading for "select_domain"
  outputOptions(output, "select_domain", suspendWhenHidden = FALSE)
}

shinyApp(ui = ui, server = server)
