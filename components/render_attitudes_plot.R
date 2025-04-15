library(shiny)

render_attitudes_plot <- function(
  reactive_input,
  input_err,
  statements,
  data,
  translator
) {
  plot <- renderPlot(
    {
      # un-translated inputs if they were translated to French in the UI
      # reactive objects need to be digested in a reactive block (in
      selected <- un_translate_input(reactive_input = reactive_input) # nolint

      # verify that data has the levels needed for the model to run
      validate(
        need(reactive_input$province %in% data()$province, input_err)
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
        data = data(),
        weights = data()$wgt
      )

      print(selected["province"][1])
      print(reactive_input$popcat)
      print(selected["gender"])
      print(reactive_input$agecat)
      print(selected["race"])
      print(selected["immigrant"])
      print(selected["homeowner"])
      print(selected["education"])
      print(selected["income"])

      pred_data <- data.frame(
        province = selected["province"],
        popcat = reactive_input$popcat,
        gender = selected["gender"],
        agecat = reactive_input$agecat,
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
        ggplot2::scale_x_discrete(
          labels = c(
            "Agree" = translator()$t("Agree"),
            "Disagree" = translator()$t("Disagree"),
            "No opinion" = translator()$t("No opinion")
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
