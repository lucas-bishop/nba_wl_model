## Day 3 of NBA win prob modeling, this script will focus on how people have betted on games
# To begin we need to find the data that includes betting favorites and history of this data

# To do this we will need to scrape some data from online oddsbooks
# We need date, name of the teams, the scores, and the moneylines for each game

sportsbook_html <- read_html("https://www.sportsbookreview.com/betting-odds/nba-basketball/money-line/?date=20100306")

# picking nodes with only the information you need is tricky- depending on the webpage 
# some of the div's will have more logical names than the rnadom letters and numbers here

team_names <- sportsbook_html %>% html_nodes("div._3A-gC._2DWLf") %>% .[[1]] %>% 
  html_nodes("span._3O1Gx") %>% html_text()

scores <- sportsbook_html %>% html_nodes("div._3A-gC._2DWLf") %>% .[[1]] %>% 
  html_nodes("div._1Y3rN._2trL6 div:nth-child(n)") %>%
  html_text()

date <- sportsbook_html

moneylines <- sportsbook_html