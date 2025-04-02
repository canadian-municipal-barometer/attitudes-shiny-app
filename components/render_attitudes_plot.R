library(shiny)

render_attitudes_plot <- function(
  input,
  input_err,
  statements,
  df,
  translator
) {
  plot <- renderPlot(
    {
      # un-translated inputs if they were translated to French in the UI
      # reactive objects need to be digested in a reactive block (in
      selected <- un_translate_input(reactive_input = input) # nolint

      # policy to filter the data by
      filter <- statements$var_name[statements$statement == input$policy] # nolint

      # translate the contents of the selectors to variable names

      tmp_df <- df |> dplyr::filter(policy == filter) # nolint

      # verify that tmp_df has the levels needed for the model to run
      validate(
        need(input$province %in% tmp_df$province, input_err)
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
        data = tmp_df,
        weights = tmp_df$wgt
      )

      print(selected["province"][1])
      print(input$popcat)
      print(selected["gender"])
      print(input$agecat)
      print(selected["race"])
      print(selected["immigrant"])
      print(selected["homeowner"])
      print(selected["education"])
      print(selected["income"])

      pred_data <- data.frame(
        province = selected["province"],
        popcat = input$popcat,
        gender = selected["gender"],
        agecat = input$agecat,
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
        ggplot2::scale_fill_manual(
          values = c(
            "Agree" = "#0091AC",
            "Disagree" = "#000",
            "No opinion" = "#6C6E74"
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
  return(plot)
}
