library(hoopR)
library(tidyverse)
years <- 2024

schedule <- load_mbb_team_box(years)
schedule <- schedule %>% select(game_id, team_id, opponent_team_id, team_home_away)

teams <- espn_mbb_teams(years)

# write schedule as a csv called raw_sched
write.csv(schedule, "raw_sched.csv")
write.csv(teams, "teams_list.csv")
