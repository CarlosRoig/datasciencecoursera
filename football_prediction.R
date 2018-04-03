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
