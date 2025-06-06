library(shiny)

render_sidebar <- function(translator) {
  renderUI({
    message("\n`sidebar_contents` declared\n")
    sidebarPanel(
      style = "
          max-width: 32vw;
          min-width: 225px;
          background-color: #e6eff7 !important;
          ",
      selectInput(
        inputId = "province",
        label = translator()$t("Province:"),
        choices = translator()$t(c(
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
        )),
        selectize = FALSE
      ),
      selectInput(
        inputId = "popcat",
        label = translator()$t("Population:"),
        choices = c(
          "3000-9,999",
          "10,000-49,999",
          "50,000-249,999",
          "250,000-999,999",
          "1,000,000+"
        ),
        selectize = FALSE
      ),
      radioButtons(
        inputId = "gender",
        label = translator()$t("Gender:"),
        choices = translator()$t(c(
          "Woman",
          "Man"
        )),
        inline = TRUE
      ),
      selectInput(
        inputId = "agecat",
        label = translator()$t("Age:"),
        choices = c(
          "18-29",
          "30-44",
          "45-59",
          "60+"
        ),
        selectize = FALSE
      ),
      radioButtons(
        inputId = "race",
        label = translator()$t("Race:"),
        choices = translator()$t(c(
          "Racialized minority",
          "White"
        ))
      ),
      radioButtons(
        inputId = "immigrant",
        label = translator()$t("Immigrant:"),
        choices = translator()$t(c(
          "Yes",
          "No"
        )),
        inline = TRUE
      ),
      radioButtons(
        inputId = "homeowner",
        label = translator()$t("Homeowner:"),
        choices = translator()$t(c(
          "Yes",
          "No"
        )),
        inline = TRUE
      ),
      selectInput(
        inputId = "education",
        label = translator()$t("Education:"),
        choices = translator()$t(c(
          "Less than high school",
          "High school",
          "Associate's degree or trades",
          "Bachelor's degree",
          "Post-graduate degree"
        )),
        selectize = FALSE
      ),
      selectInput(
        inputId = "income",
        label = translator()$t("Income:"),
        choices = c(
          translator()$t("Less than $49,999"),
          "$50,000 - $99,999",
          "$100,000 - $149,999",
          "$150,000 - $199,999",
          translator()$t("$200,000 or more")
        ),
        selectize = FALSE
      ),
      br(),
      shinyWidgets::materialSwitch(
        inputId = "avg_switch",
        label = translator()$t("Compare to the national average"),
        value = TRUE,
        status = "primary"
      )
    )
  })
}
