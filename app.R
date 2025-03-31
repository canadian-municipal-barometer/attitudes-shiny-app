library(shiny)
library(shiny.i18n)
source("helpers.R")
source("components/update_translation.R")
source("components/declare_sidebar.R")
source("components/declare_mainpanel.R")

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
i18n$set_translation_language("en")

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
        .mail-panel {
          width: 100%;
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
        titlePanel("Canadians' Municipal Policy Attitudes"),
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
      ),
      sidebarLayout(
        fluid = TRUE,
        # from "components/"
        declare_sidebar(),
        div(
          class = "main-panel",
          style = "
            width: 70vw;
            min-width: 800px;
          ",
          declare_mainpanel(default_policies),
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # UI Rendering --------------------

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

  # Language toggle

  # make reactive i18n object
  i18n_r <- reactiveValues(translator = i18n)

  # language toggle observer
  update_translation(session, input, i18n_r)

  # plot --------------------
  output$predictions <- renderPlot(
    {
      # un-translated inputs if they were translated to French in the UI
      # reactive objects need to be digested in a reactive block (in
      selected <- un_translate_input(reactive_input = input) # nolint

      # policy to filter the data by
      filter <- statements$var_name[statements$statement == input$policy] # nolint

      # translate the contents of the selectors to variable names

      tmp_df <- df |> dplyr::filter(policy == filter) # nolint

      # verify that tmp_df has the levels needed for the model to run
      validate(
        need(input$province %in% tmp_df$province, input_err)
      )

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
            factor(popcat), # nolint
        data = tmp_df,
        weights = tmp_df$wgt
      )

      print(selected["province"][1])
      print(input$popcat)
      print(selected["gender"])
      print(input$agecat)
      print(selected["race"])
      print(selected["immigrant"])
      print(selected["homeowner"])
      print(selected["education"])
      print(selected["income"])

      pred_data <- data.frame(
        province = selected["province"],
        popcat = input$popcat,
        gender = selected["gender"],
        agecat = input$agecat,
        race = selected["race"],
        immigrant = selected["immigrant"],
        homeowner = selected["homeowner"],
        education = selected["education"],
        income = selected["income"]
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
