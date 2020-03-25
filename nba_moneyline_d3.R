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

### I realized the problems with the strange html code is from not being in classic view.
### I amgoing to create a new variable for the html of the classic view url to do rest of the analysis and then Ican merge

old_html <- read_html("https://classic.sportsbookreview.com/betting-odds/nba-basketball/?date=20100306")

date <- sportsbook_html %>% html_nodes("div._2STB6") %>% html_text()

moneyline <- old_html %>% html_nodes("div.holder-complete") %>% .[[1]] %>%
  html_nodes("div.eventLine-book") %>% 
  html_text()


