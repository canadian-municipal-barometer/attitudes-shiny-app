library(shiny)

# uses switch statements to convert french to english, because the model only
# runs on English values.
# Also converts the input object to a list, which is required by
# `render_attitudes_plot` even when no language translation occurs.
un_translate_input <- function(input) {
  cat("---`un_translate_input` ran")
  selected <- list()

  selected["province"] <- input$province |>
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
      input$province
    )

  selected["agecat"] <- input$agecat

  selected["popcat"] <- input$popcat

  selected["gender"] <- input$gender |>
    switch(
      "Homme" = "Man",
      "Femme" = "Woman",
      input$gender
    )

  selected["race"] <- input$race |>
    switch(
      "Minorité racisée" = "Racialized minority",
      "Blanc·che" = "White",
      input$race
    )

  selected["immigrant"] <- input$immigrant |>
    switch(
      "Oui" = "Yes",
      "Non" = "No",
      input$immigrant
    )

  selected["homeowner"] <- input$homeowner |>
    switch(
      "Oui" = "Yes",
      "Non" = "No",
      input$homeowner
    )

  selected["education"] <- input$education |>
    switch(
      "Moins que les études secondaires" = "Less than high school",
      "Diplôme d’études secondaires" = "High school",
      "Apprentissage/Diplôme d’études professionnelles (DEP)" = "Associate's degree or trades", # nolint
      "Baccalauréat" = "Bachelor's degree",
      "Maitrise, doctorat, diplôme professionnel" = "Post-graduate degree",
      input$education
    )

  selected["income"] <- input$income |>
    switch(
      "Moins de $49,999" = "Less than $49,999",
      "200,000 $ ou plus" = "$200,000 or more",
      input$income
    )

  return(selected)
}

statements_update <- function(
  session = session,
  translator_r,
  statements_r,
  domain
) {
  cat("---`statements_update` ran\n")

  filtered_statements <- statements_r() |>
    dplyr::filter(
      purrr::map_lgl(tags, function(x) any(x %in% domain))
    ) |>
    dplyr::pull(statement) # nolint

  updateSelectInput(
    session,
    "policy",
    label = translator_r()$t("Select a policy:"),
    choices = filtered_statements,
    selected = filtered_statements[1]
  )
}

filter_statements <- function(statements, svy_data_r, policy) {
  req(!is.null(policy) & policy != "")
  # Find the selected policy in statements
  index <- which(statements()$statement == policy) # input$policy
  val <- statements()$var_name[index]
  tbl <- svy_data_r() |> dplyr::filter(policy == val)
  return(tbl)
}
