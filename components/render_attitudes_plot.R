library(shiny)
source("helpers.R")

render_attitudes_plot <- function(
  statements,
  input_err,
  input,
  tbl,
  translator
) {
  plot <- renderPlot(
    {
      req(statements())
      message("render_attitudes_plot called")

      # Find the selected policy in statements
      policy_index <- which(statements()$statement == input$policy)

      # Get the var_name for the selected policy
      filter_value <- statements()$var_name[policy_index]
      tbl <- tbl |>
        dplyr::filter(policy == filter_value)

      # un-translated inputs if they were translated to French in the UI
      user_selected <- un_translate_input(reactive_input = input) # nolint

      # only take a reactive dep on `selected`
      translator <- isolate(translator())

      # verify that data has the levels needed for the model to run
      validate(
        need(user_selected["province"] %in% tbl$province, input_err)
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
        data = tbl,
        weights = tbl$wgt
      )

      pred_data <- data.frame(
        province = user_selected["province"],
        popcat = user_selected["popcat"],
        gender = user_selected["gender"],
        agecat = user_selected["agecat"],
        race = user_selected["race"],
        immigrant = user_selected["immigrant"],
        homeowner = user_selected["homeowner"],
        education = user_selected["education"],
        income = user_selected["income"]
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
        ggplot2::scale_x_discrete(
          labels = c(
            "Agree" = translator$t("Agree"),
            "Disagree" = translator$t("Disagree"),
            "No opinion" = translator$t("No opinion")
          )
        ) +
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
      return(plot)
    },
    bg = "transparent"
  )
}
