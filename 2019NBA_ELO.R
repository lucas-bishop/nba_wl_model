##Validating 538's ELO NBA prediction model

library(tidyverse)
library(lubridate)
library(broom)

current_date <- now()


# Load and format baseball games that have already been played
all_game_data <- read_csv(file="nba_elo.csv",
                      col_types=cols(date = col_date(),
                                     date=col_date(),
                                     team1=col_character(),
                                     team2=col_character(),
                                     season=col_integer(),
                                     score1=col_integer(),
                                     score2=col_integer())
                      ) %>% filter(date < current_date)


