library(shiny)

update_translation <- function(session, input, i18n_r) {
  observeEvent(input$lang_toggle, {
    # actionButton values start a 0 and go up by 1 every time it activate
    # So, all odd values of input$lang_toggle will occur when the app is in
    # English and thus the language should be updated to French.
    if (input$lang_toggle %% 2 == 1) {
      lang <- "fr"
    } else {
      lang <- "en"
    }
    i18n_r$translator$set_translation_language(lang)

    print(paste("Language change! Button val:", input$lang_toggle))
    print(paste("Current language:", lang))

    # UI updates for menu item language
    updateSelectInput(
      session,
      "province",
      label = i18n_r$translator$t("Province:"),
      choices = i18n_r$translator$t(
        c(
          "Alberta",
          "British Columbia",
          "Manitoba",
          "New Brunswick",
          "Newfoundland and Labrador",
          "Nova Scotia",
          "Ontario",
          "Prince Edward Island",
          "Quebec",
          "Saskatchewan"
        )
      )
    )
    updateSelectInput(
      session,
      "population",
      label = i18n_r$translator$t("Population:")
    )
    updateRadioButtons(
      session,
      "gender",
      label = i18n_r$translator$t("Gender:"),
      choices = i18n_r$translator$t(
        c(
          "Man",
          "Woman"
        )
      )
    )
    updateSelectInput(
      session,
      "agecat",
      label = i18n_r$translator$t("Age:")
      # choices don't need translation
    )
    updateRadioButtons(
      session,
      "race",
      label = i18n_r$translator$t("Race:"),
      choices = i18n_r$translator$t(
        c(
          "Racialized minority",
          "White"
        )
      )
    )
    updateRadioButtons(
      session,
      "immigrant",
      label = i18n_r$translator$t("Immigrant:"),
      choices = i18n_r$translator$t(
        c(
          "Yes",
          "No"
        )
      )
    )
    updateRadioButtons(
      session,
      "homeowner",
      label = i18n_r$translator$t("Homeowner:"),
      choices = i18n_r$translator$t(
        c(
          "Yes",
          "No"
        )
      )
    )
    updateSelectInput(
      session,
      "education",
      label = i18n_r$translator$t("Education:"),
      choices = i18n_r$translator$t(
        c(
          "Less than high school",
          "High school",
          "Associate's degree or trades",
          "Bachelor's degree",
          "Post-graduate degree"
        )
      )
    )
    updateSelectInput(
      session,
      "income",
      label = i18n_r$translator$t("Income:"),
      choices = i18n_r$translator$t(
        c(
          "Less than $49,999",
          "$50,000 - $99,999",
          "$100,000 - $149,999",
          "$150,000 - $199,999",
          "$200,000 or more"
        )
      )
    )
  })
}
