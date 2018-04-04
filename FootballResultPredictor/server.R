#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(footballR)
library(dplyr)
library(ggplot2)

epl_data <- c("")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  percent <- function(x, digits = 2) {
    round(x * 100, digits = digits)
  }

  teams <- reactive({
    token <- "dea3747efc114c6b91da6e4606304323"
    liga <- input$selectLigas
    season <- 2017
    epl_id <- fdo_listComps(token=token, season = season,response = "minified") %>% filter(league==liga) %>% .$id
    epl_data <<- fdo_listCompFixtures(token=token, id = epl_id, response = "minified")$fixtures %>%
      jsonlite::flatten() %>% filter(status=="FINISHED") %>%
      rename(home=homeTeamName, away=awayTeamName, homeGoals=result.goalsHomeTeam,
             awayGoals=result.goalsAwayTeam) %>%
      select(home,away,homeGoals,awayGoals)
    temp <- epl_data[order(unique(epl_data$home)), ]$home
    temp
  })

  output$selectHome <- renderUI({
    withProgress(message = 'Loading data ', value = 20, {
      selectInput("home", label="Select home team", choices = teams(), selected = NULL)
    })
  })
  output$selectAway <- renderUI({
    selectInput("away", label="Select away team", choices = teams(), selected = NULL)
  })

  observeEvent(input$predict, {
    epl_data <- epl_data
    poisson_model <-
      rbind(
        data.frame(goals=epl_data$homeGoals,
                   team=epl_data$home,
                   opponent=epl_data$away,
                   home=1),
        data.frame(goals=epl_data$awayGoals,
                   team=epl_data$away,
                   opponent=epl_data$home,
                   home=0)) %>%
      glm(goals ~ home + team +opponent, family=poisson(link=log),data=.)

    simulate_match <- function(foot_model, homeTeam, awayTeam, max_goals=10){
      home_goals_avg <- predict(foot_model,
                                data.frame(home=1, team=homeTeam,
                                           opponent=awayTeam), type="response")
      away_goals_avg <- predict(foot_model,
                                data.frame(home=0, team=awayTeam,
                                           opponent=homeTeam), type="response")
      matrix(percent(dpois(0:max_goals, home_goals_avg) %o% dpois(0:max_goals, away_goals_avg)), nrow = 5, ncol = 5)
    }

    simulation <- simulate_match(poisson_model, input$home, input$away, max_goals=4)

    sum(simulation[lower.tri(simulation)])
    sum(diag(simulation))
    sum(simulation[upper.tri(simulation)])
    output$porcentaje <- renderText(as.character(sum(simulation[upper.tri(simulation)])))
  })

})
