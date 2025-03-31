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
