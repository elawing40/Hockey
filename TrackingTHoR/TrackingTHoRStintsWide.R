#HALO Stints Wide

library(tidyverse)
library(arrow)

HALOStints <- read_parquet("~/Desktop/Analytics Projects/HALO Hackathon/stints.parquet")

HALOStints
HALOPlayers
HALOGames

HALOGoaliesIDs <- HALOPlayers %>%
  filter(primary_position == "G") %>%
  pull(player_id)

HALOStintsStrengthStates <- HALOStints %>%
  mutate(
         StrengthState = case_when(
           n_home_skaters == 5 & n_away_skaters == 5 & is_home_net_empty == FALSE & is_away_net_empty == FALSE ~ "5on5",
           n_home_skaters == 4 & n_away_skaters == 4 & is_home_net_empty == FALSE & is_away_net_empty == FALSE ~ "4on4",
           n_home_skaters == 3 & n_away_skaters == 3 & is_home_net_empty == FALSE & is_away_net_empty == FALSE~ "3on3",
           TRUE ~ "Uneven Strength State"
         ))

HALOStintsStrengthStatesNoGoalies <- HALOStintsStrengthStates %>%
  filter(!(player_id %in% HALOGoaliesIDs))

HALOStintsStrengthStatesHomeAway <- HALOStintsStrengthStatesNoGoalies %>%
  left_join(HALOGames, by = "game_id") %>%
  mutate(isHome = ifelse(team_id == home_team_id, 1, 0))

HALOStintsStrengthStatesWide <- HALOStintsStrengthStatesHomeAway %>%
  group_by(game_id, game_stint) %>%
  arrange(desc(isHome), .by_group = TRUE) %>%
  summarise(
    game_id = first(game_id),
    period = first(period),
    period_time_start = first(period_time_start),
    period_time_end = first(period_time_end),
    game_stint = first(game_stint),
    StrengthState = first(StrengthState),
    HomePlayer1 = {p <- player_id[isHome == 1]; if(length(p) >= 1) p[1] else NA_character_},
    HomePlayer2 = {p <- player_id[isHome == 1]; if(length(p) >= 2) p[2] else NA_character_},
    HomePlayer3 = {p <- player_id[isHome == 1]; if(length(p) >= 3) p[3] else NA_character_},
    HomePlayer4 = {p <- player_id[isHome == 1]; if(length(p) >= 4) p[4] else NA_character_},
    HomePlayer5 = {p <- player_id[isHome == 1]; if(length(p) >= 5) p[5] else NA_character_},
    AwayPlayer1 = {p <- player_id[isHome == 0]; if(length(p) >= 1) p[1] else NA_character_},
    AwayPlayer2 = {p <- player_id[isHome == 0]; if(length(p) >= 2) p[2] else NA_character_},
    AwayPlayer3 = {p <- player_id[isHome == 0]; if(length(p) >= 3) p[3] else NA_character_},
    AwayPlayer4 = {p <- player_id[isHome == 0]; if(length(p) >= 4) p[4] else NA_character_},
    AwayPlayer5 = {p <- player_id[isHome == 0]; if(length(p) >= 5) p[5] else NA_character_},
    .groups = "drop"
  ) %>%
  mutate(
    StrengthStateStartTime = period_time_start,
    StrengthStateEndTime = period_time_end
  ) %>%
  group_by(game_id, period) %>%
  mutate(
    StrengthStateStint = n()
  )

# TrackingTHORStrengthStatesTimes <- HALOStintsStrengthStatesWide %>%
#   group_by(game_id, period) %>%
#   mutate(StintStartAndLastEndMatch = ifelse(period_time_start == lag(period_time_end), 1, 0),
#          BreakInStints = ifelse(StintStartAndLastEndMatch==0, 1, 0),
#          StrengthStateStint = cumsum(replace_na(BreakInStints, 0))) %>%
#   ungroup() %>%
#   group_by(game_id, period, StrengthStateStint) %>%
#   summarise(
#     StrengthStateStartTime = first(period_time_start),
#     StrengthStateEndTime = last(period_time_end),
#     StrengthState = first(StrengthState),
#     .groups = "drop"
#   )

