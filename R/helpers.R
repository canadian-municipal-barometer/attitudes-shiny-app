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

build_plot <- function(
  preds,
  filtered_svy_data_r,
  show_natl_avg,
  natl_avg,
  current_lang_r
) {
  if (show_natl_avg()) {
    # prepare data, assuming English is the current language
    policy_i <- filtered_svy_data_r()$policy[1]
    natl_avg_i <- natl_avg[[policy_i]]
    natl_avg_i$fill_group <- "National average"
    preds$fill_group <- preds$cats
    preds$fill_group <- preds$fill_group |>
      factor(ordered = TRUE) |>
      forcats::fct_rev()
    preds <- dplyr::bind_rows(preds, natl_avg_i)
    preds$group <- preds$group |>
      forcats::fct_rev()

    # translate the plot data to french if current language is French
    if (current_lang_r() == "fr") {
      preds$cats <- preds$cats |>
        forcats::fct_recode(
          "Pas d'opinion" = "No opinion",
          "Désaccord" = "Disagree",
          "D'accord" = "Agree"
        )
      preds$fill_group <- preds$fill_group |>
        forcats::fct_collapse(
          "Moyenne nationale" = "National average",
          "Pas d'opinion" = "No opinion",
          "Désaccord" = "Disagree",
          "D'accord" = "Agree"
        )
    }

    # call the plot constructor
    plot <- natl_avg_plot(preds)
  } else {
    # call the plot constructor
    plot <- simple_plot(preds)
  }

  # add scale spec (dependent on current language)
  if (current_lang_r() == "en") {
    plot <- plot +
      ggplot2::scale_fill_manual(
        values = c(
          "National average" = "#c7c7c7",
          "No opinion" = "#6C6E74",
          "Disagree" = "#000000",
          "Agree" = "#0091AC"
        ),
        # This makes its contents the only label in the legend
        breaks = c("National average")
      )
  } else {
    plot <- plot +
      ggplot2::scale_fill_manual(
        values = c(
          "Moyenne nationale" = "#c7c7c7",
          "Pas d'opinion" = "#6C6E74",
          "Désaccord" = "#000000",
          "D'accord" = "#0091AC"
        ),
        # This makes its contents the only label in the legend
        breaks = c("Moyenne nationale")
      )
  }
  return(plot)
}
