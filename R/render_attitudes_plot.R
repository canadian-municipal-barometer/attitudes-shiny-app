library(shiny)

render_attitudes_plot <- function(
  statements_r,
  filtered_svy_data_r,
  natl_avg,
  show_natl_avg,
  current_lang_r,
  user_selected,
  input_err
) {
  plot <- renderPlot(
    {
      message("`renderPlot` called")

      statements_r <- isolate(statements_r)
      filtered_svy_data_r <- isolate(filtered_svy_data_r)
      user_selected <- user_selected()

      # verify that data has the levels needed for the model to run
      validate(
        need(
          user_selected["province"] %in% filtered_svy_data_r()$province,
          input_err()
        )
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
          data = filtered_svy_data_r(),
          weights = filtered_svy_data_r()$wgt
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
        probs = preds,
        group = as.factor("preds")
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

      message(paste("preds:", preds))

      if (show_natl_avg()) {
        policy_i <- filtered_svy_data_r()$policy[1]
        natl_avg_i <- natl_avg[[policy_i]]
        preds <- dplyr::bind_rows(preds, natl_avg_i)
        preds$fill_group <- ifelse(preds$group == "preds", preds$cats, "comp")
        natl_avg_plot(preds)
      } else {
        simple_plot(preds)
      }
    },
    bg = "transparent"
  )
}
