library(footballR)
library(dplyr)
library(skellam)
library(ggplot2)
library(purrr)
library(tidyr)
library(abettor)
library(RCurl)

epl_id <-fdo_listComps(season = 2016,response = "minified") %>% filter(league=="PD") %>% .$id
epl_data <- fdo_listCompFixtures(id = epl_id, response = "minified")$fixtures %>%
  jsonlite::flatten() %>% filter(status=="FINISHED") %>%
  rename(home=homeTeamName, away=awayTeamName, homeGoals=result.goalsHomeTeam,
         awayGoals=result.goalsAwayTeam) %>%
  select(home,away,homeGoals,awayGoals)
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
  dpois(0:max_goals, home_goals_avg) %o% dpois(0:max_goals, away_goals_avg)
}

simulate_match(poisson_model, "Valencia CF", "FC Barcelona", max_goals=4)
