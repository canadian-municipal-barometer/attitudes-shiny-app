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

simple_plot <- function(preds) {
  ggplot2::ggplot(
    preds,
    ggplot2::aes(x = cats, y = probs, fill = cats) # nolint
  ) +
    ggplot2::geom_col() +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(probs, "%")),
      hjust = -0.1,
      size = 5
    ) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal(base_size = 20) +
    ggplot2::scale_fill_manual(
      values = c(
        "#6C6E74",
        "#000",
        "#0091AC"
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
}

natl_avg_plot <- function(preds) {
  ggplot2::ggplot(
    preds,
    ggplot2::aes(x = cats, y = probs, fill = fill_group, group = group) # nolint
  ) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::coord_flip() +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(probs, "%")),
      position = ggplot2::position_dodge(width = 0.9),
      hjust = -0.1,
      size = 5
    ) +
    ggplot2::theme_minimal(base_size = 20) +
    ggplot2::scale_fill_manual(
      values = c(
        "National Average" = "#c7c7c7",
        "No opinion" = "#6C6E74",
        "Disagree" = "#000000",
        "Agree" = "#0091AC"
      ),
      breaks = c("National Average")
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
}
