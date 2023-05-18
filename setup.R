library(tidyverse)
library(lubridate)

url = 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-30/matches.csv'
data = read_csv(url)

fin_data = data |>
  select(-c(match_id, team1_away_or_home, team2_home_away)) |>
  select(-c(time_of_day, series, player_of_match, player_of_match_team, venue)) |>
  select(-c(ball_remaining, ground, ground_city)) |>
  mutate(match_date = mdy(match_date), ground = ground_country) |>
  select(-c(ground_country, score_team1, score_team2, wickets_team1, wickets_team2, margin, margin_type, toss, toss_decision))

fin_data = fin_data |>
  filter(!is.na(match_date))

write_csv(x = fin_data, file = "cricket.csv")
