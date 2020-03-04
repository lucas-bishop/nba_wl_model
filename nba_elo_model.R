##Validating 538's ELO NBA prediction model

library(tidyverse)
library(lubridate)
library(broom)

current_date <- now()


# Load and format basketball games that have already been played
all_game_data <- read_csv(file="https://projects.fivethirtyeight.com/nba-model/nba_elo.csv",
                      col_types=cols(date = col_date(),
                                     date=col_date(),
                                     team1=col_character(),
                                     team2=col_character(),
                                     season=col_integer(),
                                     score1=col_integer(),
                                     score2=col_integer())
                      ) %>% filter(date < current_date)
# make df for this only this season
current_season <- all_game_data %>% filter(season == "2020")

# add new columns that show the favorite and if they won based on ELO model
favorite_win_prob <- all_game_data %>%
  mutate(fav_538_won=ifelse(elo_prob1>elo_prob2, score1 > score2, score2 > score1),
         fav_538_prob=ifelse(elo_prob1>elo_prob2, elo_prob1, elo_prob2)) %>%
  select(season, date, team1, team2, fav_538_won, fav_538_prob)


