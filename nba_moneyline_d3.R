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

classic_html <- read_html("https://classic.sportsbookreview.com/betting-odds/nba-basketball/money-line/?date=20100306")

parse_table_row <- function(row){
  team_names <- row %>% 
    html_nodes("span.team-name") %>% html_text()
  
  scores <- row %>% 
    html_nodes("span.current-score") %>%
    html_text()
  
  moneylines <- row %>%
    html_node("div.eventLine-book") %>% html_nodes("div.eventLine-book-value") %>% 
    html_text()
  
  c(team_names, scores, moneylines)
}


classic_html %>% html_nodes("div.holder-complete") %>% map(., parse_table_row)
  
  


