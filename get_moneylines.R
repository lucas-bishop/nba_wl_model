library(tidyverse)
library(lubridate)
library(rvest)

parse_table_row <- function(row){
  dates <- row %>% html_nodes("div.status-complete") %>% html_attr("rel")
  
  team_names <- row %>% 
    html_nodes("span.team-name") %>% html_text()
  
  scores <- row %>% 
    html_nodes("span.current-score") %>%
    html_text()
  
  moneylines <- row %>%
    html_node("div.eventLine-book") %>% html_nodes("div.eventLine-book-value") %>% 
    html_text()
  # in MLB tutorial this chunk of the function will look a little different because of the way baseball and nba data is read in
  tibble(dates=as.Date(dates),
         team1=team_names[1], 
         team2=team_names[2],
         score2=as.numeric(scores[2]),
         score1=as.numeric(scores[1]),
         moneyline1=as.numeric(str_replace(moneylines[2], "\\+", "")),
         moneyline2=as.numeric(str_replace(moneylines[1], "\\+", ""))
  )
}


pull_moneyline_data <- function(url){
  print(url)
  
  read_html(url) %>%
    html_nodes("div.holder-complete") %>%
    map_dfr(., parse_table_row)
  
}


classic_html_pages <- favorite_win_prob %>%
  # the moneyline information start at the 2006 season on this website so start from there
  filter(date >= "2006-11-01") %>%
  pull(date) %>%
  unique() %>%
  str_replace_all(., "-", "") %>%
  paste0("https://classic.sportsbookreview.com/betting-odds/nba-basketball/money-line/?date=", .)


moneylines <- map_dfr(classic_html_pages, pull_moneyline_data)

write.csv(moneylines, "data/moneylines.csv")

