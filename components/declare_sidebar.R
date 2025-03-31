library(shiny)

declare_sidebar <- function() {
  sidebarPanel(
    style = "
          max-width: 35vw;
          min-width: 225px;
          background-color: #e6eff7 !important;
          ",
    selectInput(
      inputId = "province",
      label = "Province:",
      choices = c(
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
      ),
      selectize = FALSE
    ),
    selectInput(
      inputId = "popcat",
      label = "Population:",
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
      label = "Gender:",
      choices = c(
        "Woman",
        "Man"
      ),
      inline = TRUE
    ),
    selectInput(
      inputId = "agecat",
      label = "Age:",
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
      label = "Race:",
      choices = c(
        "Racialized minority",
        "White"
      )
    ),
    radioButtons(
      inputId = "immigrant",
      label = "Immigrant:",
      choices = c(
        "Yes",
        "No"
      ),
      inline = TRUE
    ),
    radioButtons(
      inputId = "homeowner",
      label = "Homeowner:",
      choices = c(
        "Yes",
        "No"
      ),
      inline = TRUE
    ),
    selectInput(
      inputId = "education",
      label = "Education:",
      choices = c(
        "Less than high school",
        "High school",
        "Associate's degree or trades",
        "Bachelor's degree",
        "Post-graduate degree"
      ),
      selectize = FALSE
    ),
    selectInput(
      inputId = "income",
      label = "Income:",
      choices = c(
        "Less than $49,999",
        "$50,000 - $99,999",
        "$100,000 - $149,999",
        "$150,000 - $199,999",
        "$200,000 or more"
      ),
      selectize = FALSE
    ),
  )
}
