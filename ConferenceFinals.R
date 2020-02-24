## NBA Exploratory Data Analysis

library(tidyverse)
library(ggplot2)

setwd('C:/Users/bisho/Desktop/R_NBA/')


team_pergame <- read_csv('C:/Users/bisho/Desktop/R_NBA/2019_TeamPerGame.csv') %>% 
  select(-Rk)
opp_pergame <- read_csv('C:/Users/bisho/Desktop/R_NBA/2019_OpponentPerGame.csv') %>% 
  select(-Rk)
team_per100 <- read_csv('C:/Users/bisho/Desktop/R_NBA/2019_TeamPer100p.csv') %>% 
  select(-Rk)
opp_per100 <- read_csv('C:/Users/bisho/Desktop/R_NBA/2019_OpponentPer100p.csv') %>% 
  select(-Rk)


#Create the team shooting breakdown with single, unduplicated column names
teamshooting_col_names <- names(read_csv("2019_team_shots.csv", 
                                 n_max = 0, skip = 2))
team_shooting <- read_csv('C:/Users/bisho/Desktop/R_NBA/2019_team_shots.csv', col_names = teamshooting_col_names, skip = 3) %>% 
  rename('%ofFGA_2P'='2P', '%ofFGA_0-3'='0-3', '%ofFGA_3-10'='3-10', 
         '%ofFGA_10-16'='10-16','%ofFGA_16-3pt'='16-3pt','%ofFGA_3P'='3P',
         'FG%_2P'='2P_1', 'FG%_0-3'='0-3_1', 'FG%_3-10'='3-10_1', 
         'FG%_10-16'='10-16_1','FG%_16-3pt'='16-3pt_1','FG%_3P'='3P_1',
         '%Astd_2P' = '%Ast\'d','dunks_%FGA'='%FGA','dunks_#Md'='Md.',
         'layups_%FGA'='%FGA_1','layups_#Md'='Md._1', '%Astd_3P'='%Ast\'d_1',
         '%3PA_corner'='%3PA', '3P%_corner'='3P%') %>% 
  select(-Att., -Md._2)

#Create the opponent's shooting breakdown with single, unduplicated columns names
oppshooting_col_names <- names(read_csv("2019_opponent_shots.csv", 
                                         n_max = 0, skip = 2))
opp_shooting <- read_csv('C:/Users/bisho/Desktop/R_NBA/2019_opponent_shots.csv', col_names = oppshooting_col_names, skip = 3) %>% 
  rename('%ofFGA_2P'='2P', '%ofFGA_0-3'='0-3', '%ofFGA_3-10'='3-10', 
         '%ofFGA_10-16'='10-16','%ofFGA_16-3pt'='16-3pt','%ofFGA_3P'='3P',
         'FG%_2P'='2P_1', 'FG%_0-3'='0-3_1', 'FG%_3-10'='3-10_1', 
         'FG%_10-16'='10-16_1','FG%_16-3pt'='16-3pt_1','FG%_3P'='3P_1',
         '%Astd_2P' = '%Ast\'d','dunks_%FGA'='%FGA','dunks_#Md'='Md.',
         'layups_%FGA'='%FGA_1','layups_#Md'='Md._1', '%Astd_3P'='%Ast\'d_1',
         '%3PA_corner'='%3PA', '3P%_corner'='3P%')

#Create the misc stats data frame with single, unduplicated column names
misc_col_names <- names(read_csv("2019_team_misc.csv", 
                                   n_max = 0, skip = 1))
misc_stats <- read_csv('2019_team_misc.csv', col_names = misc_col_names, skip =2) %>% 
  rename('opp_efg%' = 'eFG%_1',
         'opp_TOV%' = 'TOV%_1',
         'oppFT/FGA' = 'FT/FGA_1')


# REMEMBER eFG% is calculated by (FG + 0.5 * 3P) / FGA

##Probably going to do a gather of my data frames to get it into a 'Tidy' format
##Then can have one data frame with a columns: Team, year, metric, Value, playoffs(y/n), etc. 

## Mutate a new variable for possesion index of some sort...taking into account TOV and ORB

  
  
  
  
  
  
  