## Day 3 of NBA win prob modeling, this script will focus on how people have betted on games
# To begin we need to find the data that includes betting favorites and history of this data

# To do this we will need to scrape some data from online oddsbooks
# We need date, name of the teams, the scores, and the moneylines for each game

#  sportsbook_html <- read_html("https://www.sportsbookreview.com/betting-odds/nba-basketball/money-line/?date=20100306")
### I realized the problems with the strange html code is from not being in classic view.
### I amgoing to create a new variable for the html of the classic view url to do rest of the analysis and then Ican merge

library(tidyverse)
library(lubridate)
library(rvest)
library(broom)

#define function that calculates win pct based on two teams' win percentages, with rules for first couple days of the season.
get_wp <- function(A, B){
  
  if((A == 0 && B == 0) || (A == 1 && B == 1)){
    0.5
  } else {
    A * (1-B) / (A*(1-B) + B*(1-A))
  }
  
}

#at time of writing this code, the NBA season has been postponed due to coronavirus.
#Since the games are still being logged on our data source while they are postponed, we need to make a variable for the last normal day of NBA schedule
#if you use current date, postponed games will screw up downstream analysis
last_games_day <- as.Date(c('2020-03-10'))

# Load and format basketball games that have already been played. I will focus only on post 1995 stats, since I've been alive
all_game_data <- read_csv(file="https://projects.fivethirtyeight.com/nba-model/nba_elo.csv",
                          col_types=cols(date = col_date(),
                                         date=col_date(),
                                         team1=col_character(),
                                         team2=col_character(),
                                         season=col_integer(),
                                         score1=col_integer(),
                                         score2=col_integer())
) %>% filter(date < last_games_day, season >= 1995) %>% 
  select(-'carm-elo1_pre', -'carm-elo1_post', -'carm-elo2_pre', -'carm-elo2_post', -'carm-elo_prob1',
         -'carm-elo_prob2', -'raptor1_pre', -'raptor2_pre', -'raptor_prob1', -'raptor_prob2')

# add new columns that show the favorite and if they won based on ELO model. I will do since 1995 since I have been baskterball fan since then
favorite_win_prob <- all_game_data %>%
  mutate(fav_538_won=ifelse(elo_prob1>elo_prob2, score1 > score2, score2 > score1),
         fav_538_prob=ifelse(elo_prob1>elo_prob2, elo_prob1, elo_prob2)) %>%
  select(season, date, team1, team2, score1, score2, fav_538_won, fav_538_prob)


wl_live <- favorite_win_prob %>% 
  mutate(win1=score1 > score2,
         win2=score2 > score1,
         team_win1=paste(team1, win1, sep = "_"),
         team_win2=paste(team2, win2, sep = "_")) %>% 
  gather(one_two, team_win, team_win1, team_win2) %>% 
  separate(team_win, into = c("team", "win"), sep = "_", convert = TRUE) %>% 
  arrange(date) %>% 
  group_by(season, team) %>% 
  #add a lag to reflect the win pct at beginning of day, before games. and add '0' placeholder at first poisiton
  mutate(wins = c(0, na.omit(lag(cumsum(win)))), 
         losses = c(0, na.omit(lag(cumsum(!win)))), 
         win_pct = c(0, na.omit(wins / (wins+losses)))
  ) %>% ungroup() %>% 
  select(season, team, date, win_pct)#select your favorite team and use tail() function to check output wl_pct is accurate

#now want to see how win pct changes year to year, grouped by team
wl_season <- favorite_win_prob %>%
  mutate(win1=score1>score2,
         win2=score2>score1,
         team_win1 = paste(team1, win1, sep="_"),
         team_win2 = paste(team2, win2, sep="_")) %>%
  gather(one_two, team_win, team_win1, team_win2) %>%
  separate(team_win, into=c("team", "win"), sep="_", convert=TRUE) %>%
  group_by(team, season) %>%
  summarize(current_avg = mean(win)) %>%
  #notice we are saying all teams started at .500 prior to 1995 season, which isn't true but needs placeholder
  mutate(prev_avg = c(0.5, na.omit(lag(current_avg)))) %>%
  ungroup()

#now join the two other dfs we created with live win_pct change
favorite_win_prob <- favorite_win_prob %>%
  inner_join(., wl_live, by=c("team1"="team", "season", "date")) %>%
  inner_join(., wl_live, by=c("team2"="team", "season", "date")) %>%
  #now want to use those win percenatages at point in the season to calculate probabliities
  mutate(win_prob1=map2_dbl(win_pct.x, win_pct.y, get_wp),
         win_prob2=1-win_prob1,
         fav_wplive_won=ifelse(win_prob1 > win_prob2, score1 > score2, score2 > score1),
         fav_wplive_prob=ifelse(win_prob1 > win_prob2, win_prob1, win_prob2)) %>%
  select(season, date, team1, team2, score1, score2, 
         fav_538_won, fav_538_prob, fav_wplive_won, fav_wplive_prob)

# now want to join in the wl_season df
favorite_win_prob <- favorite_win_prob %>%
  inner_join(., wl_season, by=c("team1"="team", "season")) %>%
  inner_join(., wl_season, by=c("team2"="team", "season")) %>%
  mutate(win_prob1=map2_dbl(current_avg.x, current_avg.y, get_wp),
         win_prob2=1-win_prob1,
         fav_wpcurrent_won=ifelse(win_prob1 > win_prob2, score1 > score2, score2 > score1),
         fav_wpcurrent_prob=ifelse(win_prob1 > win_prob2, win_prob1, win_prob2),
         
         win_prob1=map2_dbl(prev_avg.x, prev_avg.y, get_wp),
         win_prob2=1-win_prob1,
         fav_wpprev_won=ifelse(win_prob1 > win_prob2, score1 > score2, score2 > score1),
         fav_wpprev_prob=ifelse(win_prob1 > win_prob2, win_prob1, win_prob2)
  ) %>%
  select(season, date, team1, team2, score1, score2, fav_538_won, fav_538_prob, 
         fav_wplive_won, fav_wplive_prob, fav_wpcurrent_won, fav_wpcurrent_prob, fav_wpprev_won, fav_wpprev_prob)

# write function to convert moneyline odds. from: bettingexpert.com 
get_moneyline_prob <- function(x){
  if(x < 100) {
    -x/(-x+100)
  } else {
    100 / (x+100)
  }
}

fwp <- c("LAL", "MIA", "CLE", "GSW", "MIN", "UTA", "CHO", "DET", "MEM", 
         "BOS", "PHI", "ORL", "PHO", "LAC", "DAL", "DEN", "CHI", "TOR",
         "ATL", "SAS", "IND", "HOU", "NYK", "POR", "MIL", "WAS", "NOK", "SAC",
         "NOP", "OKC", "BRK")
ml <- c("L.A. Lakers", "Miami", "Cleveland", "Golden State", "Minnesota", "Utah", "Charlotte",
        "Detroit", "Memphis", "Boston", "Philadelphia", "Orlando", "Phoenix", "L.A. Clippers", "Dallas",
        "Denver", "Chicago", "Toronto", "Atlanta", "San Antonio", "Indiana", "Houston", "New York", 
        "Portland", "Milwaukee", "Washington", "New Orleans", "Sacramento", "New Orleans", "Oklahoma City", "Brooklyn")


name_convert <- tibble(fwp=fwp, ml=ml)
favorite_win_prob <- favorite_win_prob %>% filter(season >= 2007)

## Read in csv file created from the get_moneylines.R script
favorite_win_prob <- read_csv("data/moneylines.csv", 
         col_types = cols(score1 = col_integer(), score2 = col_integer())) %>% 
  drop_na() %>% select(-X1) %>%
  mutate(date = as.Date(date, "%m/%d/%Y","%Y-%m-%d")) %>% 
  mutate(moneyline_prob1=map_dbl(moneyline1, get_moneyline_prob)) %>% 
  mutate(moneyline_prob2=map_dbl(moneyline2, get_moneyline_prob)) %>% 
  mutate(fav_ml_live_won=ifelse(moneyline_prob1 > moneyline_prob2, score1 > score2, score2 > score1),
        fav_ml_live_prob=ifelse(moneyline_prob1 > moneyline_prob2, moneyline_prob1, moneyline_prob2)) %>% 
  inner_join(., name_convert, by=c("team1"="ml")) %>%
  inner_join(., name_convert, by=c("team2"="ml")) %>%
  select(-team1, -team2) %>%
  rename(team1=fwp.x, team2=fwp.y) %>% 
  inner_join(favorite_win_prob, .,
             by=c("date", "team1" = "team2", "team2"="team1",
                  "score1"="score2", "score2"="score1")) %>% 
  select(-moneyline1, -moneyline2, -moneyline_prob1, -moneyline_prob2)

# after doing this we have a dataset with moneylines and outcomes for 13744 games - very robust
# this isn't all games since 2007 season because of team name changes and defunct teams having their moneylines wiped

tidy_win_prob <- favorite_win_prob %>% 
  ##mutate one column for each model now that we have what we need
  mutate(FiveThirtyEight=paste(fav_538_won, fav_538_prob, sep="_"),
         wplive=paste(fav_wplive_won, fav_wplive_prob, sep="_"),
         wpcurrent=paste(fav_wpcurrent_won, fav_wpcurrent_prob, sep="_"),
         wpprev=paste(fav_wpprev_won, fav_wpprev_prob, sep="_"),
         #adding in probability model we just calc'd based on moneylines
         money_winprob=paste(fav_ml_live_won, fav_ml_live_prob, sep="_")) %>%
  select(-starts_with("fav")) %>%
  gather(model, won_prob, FiveThirtyEight, wplive, wpcurrent, wpprev, money_winprob) %>%
  separate(won_prob, into=c("won", "prob"), sep="_", convert=TRUE)
# Now we have a line for each game across each season for each model with the favorite and the model's probability that they won

overall_win_prob <- tidy_win_prob %>% group_by(model) %>% 
  summarize(mean=mean(won))



  
  
  
  
  
