library(shiny)

# reassignment, because case_match rejects the reactive type without evaluating
# it to a vector
# If inputs are already in English, nothing happens.
un_translate_input <- function(reactive_input) {
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

filter_data <- function(
  reactive_input,
  statements,
  tbl
) {
  req(reactive_input$policy, statements)
  message("filter_data called")

  # Get current statements data
  current_statements <- statements()

  message("reactive_input$policy:", reactive_input$policy)

  # Find the selected policy in statements
  policy_index <- which(current_statements$statement == reactive_input$policy)

  message(paste("policy_index:", policy_index))

  # Get the var_name for the selected policy
  filter_value <- current_statements$var_name[policy_index]

  message(paste("filter_value:", filter_value))

  # Print debug information
  # message("Selected policy: ", reactive_input$policy)
  # message("Filter value: ", filter_value)

  final <- tbl[tbl$policy == filter_value, ]

  return(final)
}

update_policy_menus <- function(session, statements, statement_tags, input) {
  updateSelectInput(
    session,
    "policy_group",
    choices = statement_tags(),
    selected = statement_tags()[1]
  )
  update_policy_statements(session, statements, input$policy_group)
  message("Update policy_group called by lang_toggle")
}

update_policy_statements <- function(session, statements, policy_group) {
  data <- statements()

  filtered_statements <- data |>
    dplyr::filter(
      purrr::map_lgl(tags, function(x) any(x %in% policy_group))
    ) |>
    dplyr::pull(statement)

  updateSelectInput(
    session,
    "policy",
    choices = filtered_statements,
    selected = filtered_statements[1]
  )
}
