##Validating 538's ELO NBA prediction model

library(tidyverse)
library(lubridate)
library(broom)

current_date <- now()


# Load and format basketball games that have already been played. I will focus only on post 1995 stats
all_game_data <- read_csv(file="https://projects.fivethirtyeight.com/nba-model/nba_elo.csv",
                      col_types=cols(date = col_date(),
                                     date=col_date(),
                                     team1=col_character(),
                                     team2=col_character(),
                                     season=col_integer(),
                                     score1=col_integer(),
                                     score2=col_integer())
                      ) %>% filter(date < current_date, season >= 1995)

# add new columns that show the favorite and if they won based on ELO model. I will do since 1995 since I have been baskterball fan since then
favorite_win_prob <- all_game_data %>%
  mutate(fav_538_won=ifelse(elo_prob1>elo_prob2, score1 > score2, score2 > score1),
         fav_538_prob=ifelse(elo_prob1>elo_prob2, elo_prob1, elo_prob2)) %>%
  select(season, date, team1, team2, fav_538_won, fav_538_prob)

overall_win_prob <- mean(favorite_win_prob$fav_538_won, na.rm = TRUE)

# in history of games played, plot the fraction of games that the favorite won
favorite_win_prob %>% 
  group_by(season) %>%
  summarize(fraction_favorite_won = mean(fav_538_won)) %>%
  ggplot(aes(x=season, y=fraction_favorite_won)) +
  geom_hline(aes(yintercept=overall_win_prob), color="lightgray") +
  geom_line() + 
  theme_classic() +
  coord_cartesian(ylim=c(0,1)) +
  labs(x="Season", y="Proportion of games that favorite won",
       title="The 538 model does a better than average job\n of predicting the winner of NBA games")


#plot the observed versus expected fraction of games won
all_predicted_observed <- favorite_win_prob %>% 
  mutate(fav_538_prob = round(fav_538_prob, digits=2)) %>%
  group_by(fav_538_prob) %>%
  summarize(games = n(),
            wins = sum(fav_538_won, na.rm = TRUE),
            observed = wins / games)

binomial_df <- all_predicted_observed %>%
  mutate(prob = fav_538_prob) %>% 
  group_by(fav_538_prob) %>%
  nest() %>%
  ## now have a data frame of data frames basically, want to know probability of success ad confidence intervals for each row(df) in our larger df
  mutate(binomial = map(data, function(df)
                              tidy(binom.test(x=as.integer(df$games * df$prob), 
                              n=df$games), 
                              p=df$prob)
                        )) %>% 
  unnest(cols = c(data, binomial)) %>% 
  ##now have a 1 x 4 tibble from all_predicted_observed and a binomial fit 1 x 8 df joined together grouped by each 538_prob
  select(fav_538_prob, games, wins, observed, conf.low, conf.high)



binomial_df %>%
  ggplot(aes(x=fav_538_prob, y=observed)) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), fill="lightblue") +
  geom_abline(aes(intercept=0, slope=1), color="darkgray") +
  geom_point() +
  theme_classic() + 
  coord_cartesian(ylim=c(0,1)) +
  labs(x="Predicted Probability of Winning",
       y="Observed Probability of Winning",
       title="The 538 model underpredicts the true ability of the favorite\n to win",
       subtitle="All games from 1995 season to present")
## this means that if a team is predicted to win 70% of the time (x axis), they acually win ~74% of the time.






















