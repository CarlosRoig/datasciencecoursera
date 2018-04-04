#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Football result predictor"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("selectLigas", label = h3("Select  a league"),
                  choices = list("Premier Leage" = "PL", "Primera División" = "PD", "Bundesliga" = "BL1",
                                 "Ligue 1" = "FL1", "Serie A" = "SA"), selected = NULL),
      uiOutput("selectHome"),
      uiOutput("selectAway"),
      actionButton("predict", "Predict")
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot1")
    )
  )
))
