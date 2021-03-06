# need a final script to call in our analysis that will simulate our payouts/losses/etc
# basically a script that will tell us how much we win/lose if we bet $100 on every game based on models

library(tidyverse)
library(lubridate)
library(broom)
library(ggthemes)

#create function we will call when adding the payout columns to our df
get_payout <- function(moneyline, bet=100){
  
  ifelse(moneyline < 0, -bet * 100/moneyline, moneyline)
  
}

#test get_payout function to get understanding of how it works
get_payout(-300)#returns 33.33 because you are betting on heavy favorite with only $100
get_payout(200)#on flip side if u bet on heavy underdog you double your money

# Load in our team names tibble that converts team names from both formats into one, excludes teams that aren't represented in both. For example Seattle != OKC in this
fwp <- c("LAL", "MIA", "CLE", "GSW", "MIN", "UTA", "CHO", "DET", "MEM", 
         "BOS", "PHI", "ORL", "PHO", "LAC", "DAL", "DEN", "CHI", "TOR",
         "ATL", "SAS", "IND", "HOU", "NYK", "POR", "MIL", "WAS", "SAC",
         "NOP", "OKC", "BRK")
ml <- c("L.A. Lakers", "Miami", "Cleveland", "Golden State", "Minnesota", "Utah", "Charlotte",
        "Detroit", "Memphis", "Boston", "Philadelphia", "Orlando", "Phoenix", "L.A. Clippers", "Dallas",
        "Denver", "Chicago", "Toronto", "Atlanta", "San Antonio", "Indiana", "Houston", "New York", 
        "Portland", "Milwaukee", "Washington", "Sacramento", "New Orleans", "Oklahoma City", "Brooklyn")

name_convert <- tibble(fwp=fwp, ml=ml)


# Create df that has payout attached to each game using function we created above
payout <- read_csv("data/moneylines.csv", 
         col_types = cols(score1 = col_integer(), score2 = col_integer())) %>% 
  drop_na() %>% select(-X1) %>%
  mutate(date = as.Date(date, "%m/%d/%Y","%Y-%m-%d")) %>% 
  mutate(moneyline_prob1=map_dbl(moneyline1, get_moneyline_prob)) %>% 
  mutate(moneyline_prob2=map_dbl(moneyline2, get_moneyline_prob)) %>% 
  mutate(fav_ml_live_won=ifelse(moneyline_prob1 > moneyline_prob2, score1 > score2, score2 > score1),
         fav_ml_live_prob=ifelse(moneyline_prob1 > moneyline_prob2, moneyline_prob1, moneyline_prob2),
         money_payout1 = get_payout(moneyline1),
         money_payout2 = get_payout(moneyline2)) %>% 
  inner_join(., name_convert, by=c("team1"="ml")) %>%
  inner_join(., name_convert, by=c("team2"="ml")) %>%
  select(-team1, -team2) %>%
  rename(team1=fwp.x, team2=fwp.y) %>% 
  mutate(team_payout1=paste(team1, money_payout1, sep="_"),
         team_payout2=paste(team2, money_payout2, sep="_")) %>%
  gather(one_two, team_payout, team_payout1, team_payout2) %>%
  select(-moneyline1, -moneyline2, -money_payout1, -money_payout2, -one_two) %>%
  separate(team_payout, into=c("team", "payout"), sep="_", convert=TRUE) %>%
  select(-moneyline_prob1, -moneyline_prob2, -fav_ml_live_won, -fav_ml_live_prob)


# this will have a winnings column for each model, grouped by day
# so by end of season each model will have a winnings # associated with it, summed by day
daily_winnings <- read_csv("data/tidy_model_data.csv") %>%
  inner_join(., payout, by=c("date", "team1", "team2", "score1", "score2", "team")) %>%
  mutate(payout=ifelse(won, payout, -100)) %>% 
  group_by(season, date, model) %>%
  summarize(days_winnings=sum(payout)) %>%
  ungroup() %>%
  group_by(season, model) %>%
  mutate(cumulative_winnings = cumsum(days_winnings)) %>%
  ungroup()

annual_winnings <- daily_winnings %>%
   mutate(year = year(date)) %>%
   group_by(model, year) %>%
   summarize(years_winnings = sum(days_winnings)) %>%
   ungroup()
 
total_winnings <- annual_winnings %>%
   group_by(model) %>%
   summarize(total_winnings = sum(years_winnings)) %>%
   arrange(desc(total_winnings))
 
#plot top 3 models
daily_winnings %>%
   filter(model=="FiveThirtyEight" | model=="money_winprob" |  model=="wpcurrent") %>%
   ggplot(aes(x=date, y=cumulative_winnings, group=model, color=model)) +
   geom_hline(aes(yintercept=0)) +
   geom_line() +
   facet_wrap(.~season, scales = "free_x", ncol = 7) +
   scale_color_manual(name=NULL,
                      breaks=c("FiveThirtyEight", "money_winprob", "wpcurrent"),
                      labels=c("538", "Moneyline", "WP Curent"),
                      values=c("red", "green", "blue")) +
   labs(x="Date", y="Winnings",
        title="Gambling loses money - regardless of model",
        subtitle="All data since 2007 season when moneyline data became available") +
   theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 5), 
         axis.text.y = element_text(size = 5)) + 
   ggsave(filename = "annual_winnings.png", width = 15, height = 15, units = 'cm')
       
       
       