
### MLB Scorigami

``` r
# loading in existing data
end_games = read_csv("end_games.csv")

# getting vector of all dates from opening day to yesterday
season_dates = seq.Date(from = as_date("2023-03-30"), to = Sys.Date() - 1, by = 1)

# getting vector of dates where no games were played for all-star weekend
asg_dates = seq.Date(from = as_date("2023-07-10"), to = as_date("2023-07-13"), by = 1)

# removing the all-star weekend dates from the season_dates vector along with existing dates
season_dates = season_dates[!season_dates %in% asg_dates]

# vector of dates to populate new data for
loop_dates = season_dates[!season_dates %in% end_games$date]

# initializing data frame to load new end games
raw_end_games = data.frame()

# looping through new dates
if (length(loop_dates) > 0) {
  for (i in 1:length(season_dates)) {
    new_data = mlb_game_pks(date = season_dates[i]) |>
      select(game_pk, date = officialDate,
             away_team = teams.away.team.name, away_score = teams.away.score,
             home_score = teams.home.score, home_team = teams.home.team.name) |>
      mutate(win_team = ifelse(home_score > away_score, home_team, away_team),
             lose_team = ifelse(home_score > away_score, away_team, home_team),
             win_score = ifelse(home_score > away_score, home_score, away_score),
             lose_score = ifelse(home_score > away_score, away_score, home_score),
             final_score = paste0(win_score, "-", lose_score))
    
    raw_end_games = rbind(raw_end_games, new_data)
  }
  new_end_games = raw_end_games |>
    mutate(date = as_date(date)) |>
    filter(date <= Sys.Date() - 1 & !is.na(home_score))
  
  end_games = rbind(end_games, new_end_games)
}

# rewriting csv of all end game data
write_csv(end_games, "end_games.csv")

# getting counts of final scores
final_score_counts = end_games |>
  count(final_score) |>
  rename(occurrences = n)

# function to find how many times a score has occurred
get_n_occurrences = function(score) {
  data = final_score_counts |>
    filter(final_score == score)
  
  if (nrow(data) == 0) return(0)
  return(data$occurrences)
}

# function to improve a date's readability
better_date = function(date) {
  return(paste0(month(date, label = T, abbr = F), " ", day(date), ", ", year(date)))
}

# function to find a score's last occurrence
get_last_occurrence = function(score) {
  return(end_games |>
    filter(final_score == score) |>
    arrange(desc(date)) |>
    pull(date) |>
    max() |>
    better_date())
}

# generating final data to be uploaded to google drive
final_data = final_score_counts |>
  separate(final_score, into = c("win_score", "lose_score"), sep = "-", remove = F) |>
  mutate(last = sapply(final_score, get_last_occurrence),
         win_score = as.integer(win_score),
         lose_score = as.integer(lose_score)) |>
  rename(`Final Score` = final_score,
         `Winning Score` = win_score,
         `Losing Score` = lose_score,
         `Occurrences` = occurrences,
         `Last Occurred` = last)

# identifier for google sheet
my_ss = "162eB6p8gmlVdq45Vcml6MqDXYe82-e8_2AXqK7kB4YA"

# clearing existing data in sheet
range_clear(
  ss = my_ss,
  sheet = "Sheet1",
  range = "A:C",
  reformat = T
)

# repopulating sheet with updated data
range_write(
  ss = my_ss,
  data = final_data
)
```
