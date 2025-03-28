# reassignment, because case_match rejects the reactive type without evaluating
# it to a vector
# If inputs are already in English, nothing happens.
un_translate_input <- function(input) {
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

  selected["popcat"] <- input$popcat |>
    switch("3000-9,999",
      "10,000-49,999",
      "50,000-249,999",
      "250,000-999,999",
      "1,000,000+"
    )

  selected["homeowner"] <- input$homeowner |>
    switch(
      "Oui" = "Yes",
      "Non" = "No",
      input$homeowner
    )

  selected["immigrant"] <- input$immigrant |>
    switch(
      "Oui" = "Yes",
      "Non" = "No",
      input$immigrant
    )

  return(selected)
}
