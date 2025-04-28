library(shiny)
source("helpers.R")

render_attitudes_plot <- function(
  statements,
  input_err,
  input,
  tbl,
  translator,
  policy_state
) {
  plot <- renderPlot(
    {
      statements <- isolate(statements)
      tbl <- isolate(tbl)
      translator <- isolate(translator)
      policy_state <- isolate(policy_state)

      message(paste("\n`render_attitudes_plot` attempted"))
      message(paste("\n`policy_state` =", policy_state()))

      req(
        ((is.null(policy_state()) == FALSE) & (policy_state() != "")) &
          (policy_state() == input()$policy)
      )

      # Find the selected policy in statements
      policy_index <- which(statements()$statement == input()$policy)

      message(paste("`policy_index`:", policy_index))

      # Get the var_name for the selected policy
      filter_value <- statements()$var_name[policy_index]

      message(paste("`filter_value`:", filter_value))
      message(paste("`tbl$policy[1]`:", tbl$policy[1]))

      tbl <- tbl |>
        dplyr::filter(policy == filter_value)

      # un-translated inputs if they were translated to French in the UI
      user_selected <- un_translate_input(reactive_input = input()) # nolint

      # verify that data has the levels needed for the model to run
      validate(
        need(user_selected["province"] %in% tbl$province, input_err)
      )

      # an immediately invoked function
      model <- (function() {
        sink("/dev/null") # disable console logging
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
        sink()
        if (!is.null(model)) {
          message("\n---model fit successful\n")
          return(model)
        }
      })()

      validate(
        need(model, "We're sorry. There seems to have been error.")
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
