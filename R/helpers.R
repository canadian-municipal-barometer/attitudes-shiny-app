library(shiny)

# reassignment, because case_match rejects the reactive type without evaluating
# it to a vector
# If inputs are already in English, nothing happens.
un_translate_input <- function(reactive_input) {
  cat("---`un_translate_selected` ran")
  selected <- list()

  selected["province"] <- reactive_input$province |>
    switch(
      "Alberta" = "Alberta",
      "Colombie-Britannique" = "British Columbia",
      "Manitoba" = "Manitoba",
      "Nouveau-Brunswick" = "New Brunswick",
      "Terre-Neuve-et-Labrador" = "Newfoundland and Labrador",
      "Nouvelle-Écosse" = "Nova Scotia",
      "Ontario" = "Ontario",
      "Île-du-Prince-Édouard" = "Prince Edward Island",
      "Québec" = "Quebec",
      "Saskatchewan" = "Saskatchewan",
      reactive_input$province
    )

  selected["agecat"] <- reactive_input$agecat

  selected["popcat"] <- reactive_input$popcat

  selected["gender"] <- reactive_input$gender |>
    switch(
      "Homme" = "Man",
      "Femme" = "Woman",
      reactive_input$gender
    )

  selected["race"] <- reactive_input$race |>
    switch(
      "Minorité racisée" = "Racialized minority",
      "Blanc·che" = "White",
      reactive_input$race
    )

  selected["immigrant"] <- reactive_input$immigrant |>
    switch(
      "Oui" = "Yes",
      "Non" = "No",
      reactive_input$immigrant
    )

  selected["homeowner"] <- reactive_input$homeowner |>
    switch(
      "Oui" = "Yes",
      "Non" = "No",
      reactive_input$homeowner
    )

  selected["education"] <- reactive_input$education |>
    switch(
      "Moins que les études secondaires" = "Less than high school",
      "Diplôme d’études secondaires" = "High school",
      "Apprentissage/Diplôme d’études professionnelles (DEP)" = "Associate's degree or trades", # nolint
      "Baccalauréat" = "Bachelor's degree",
      "Maitrise, doctorat, diplôme professionnel" = "Post-graduate degree",
      reactive_input$education
    )

  selected["income"] <- reactive_input$income |>
    switch(
      "Moins de $49,999" = "Less than $49,999",
      "200,000 $ ou plus" = "$200,000 or more",
      reactive_input$income
    )

  return(selected)
}

statements_update <- function(session = session, statements_r, domain) {
  cat("---`statements_update` ran\n")

  filtered_statements <- statements_r() |>
    dplyr::filter(
      purrr::map_lgl(tags, function(x) any(x %in% domain))
    ) |>
    dplyr::pull(statement) # nolint

  updateSelectInput(
    session,
    "policy",
    choices = filtered_statements,
    selected = filtered_statements[1]
  )
}

filter_statements <- function(statements, svy_data_r, policy) {
  # Find the selected policy in statements
  index <- which(statements()$statement == policy) # input$policy
  val <- statements()$var_name[index]
  tbl <- svy_data_r() |> dplyr::filter(policy == val)
  return(tbl)
}

lang_update <- function(
  session = session,
  tags,
  statements,
  data,
  svy_data_r,
  lang_status,
  translator
) {
  # BUG: throws vague errors
  updateSelectInput(
    session = session,
    inputId = "policy_domain",
    choices = tags, # nolint
    selected = tags[1]
  )
  statements_update(
    session = session,
    statement_data = statements, # nolint
    domain = tags[1] # nolint
  )

  lang_status("fr")
  translator()$set_translation_language("fr")

  # this is meant to be setting initialization state for once the language
  # is toggled. After initial plot data is set, the goal is to have the
  # rest of the reactivity take over, as it does before language toggle is
  # pressed
  plot_data <- filter_statements(
    statements = statements,
    svy_data = data,
    # NOTE: Should this be a call to statements()$statement[1]?
    policy = statements$statement[1]
  )
  svy_data(plot_data)
}
