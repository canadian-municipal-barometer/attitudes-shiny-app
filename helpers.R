# reassignment, because case_match rejects the reactive type without evaluating
# it to a vector
# If inputs are already in English, nothing happens.
un_translate_input <- function(input) {
  selected <- list()

  selected["homeowner"] <- input$homeowner |>
    switch(
      "Oui" = "Yes",
      "Non" = "No",
      # The default value if no match is made is what the original expression
      # evaluates to:
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
