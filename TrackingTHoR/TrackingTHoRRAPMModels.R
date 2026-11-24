#RAPM setup and training
library(tidyverse)
library(data.table)
library(Matrix)
library(glmnet)
library(parallel)

#Bring over predictions from the XGBoost model to the events dataframe 

HALOESEventSummaryWithPreds <- HALOEventsTrackingDistancesToPuckEventSummaryWithZoneSetOffenseDefense %>%
  mutate(
    goal_prob_raw = HALOEvents3Closest12SecCal$raw
  )


MeanGoalProbByEvent <- HALOESEventSummaryWithPreds %>%
  group_by(event_type) %>%
  summarise(
    MeanGoalProb = mean(goal_prob_raw, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(MeanGoalProb))

quantile(HALOESEventSummaryWithPreds$goal_prob_raw, c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.92,
                                                             0.94, 0.95, 0.97, 0.99, 1))

# All even-strength shots with an xG value (deflections carry xG, deflected shots are NA and excluded)
HALOxGShots_dt <- as.data.table(HALOEventsTimeToNextTeamGoal)[
  !is.na(sl_xg_all_shots),
  .(game_id, period, shot_time = period_time, shot_team = team, shot_xg = sl_xg_all_shots)
]

# One row per event in the RAPM frame, with its forward window
HALOxGEvents_dt <- as.data.table(HALOESEventSummaryWithPreds)[
  , .(game_id, sl_event_id, period, period_time, EventTeam)
]
HALOxGEvents_dt[, `:=`(row_id = .I, window_end = period_time + 12)]

# Sum the event team's xG for shots in [event time, event time + 12]. A shot's own xG is included.
HALOxGWindow_dt <- HALOxGShots_dt[
  HALOxGEvents_dt,
  on = .(game_id, period, shot_team = EventTeam, shot_time >= period_time, shot_time <= window_end),
  .(row_id = i.row_id,
    xGWithinNext12Sec = sum(shot_xg, na.rm = TRUE),
    ShotCountWithinNext12Sec = sum(!is.na(shot_xg))),
  by = .EACHI
]

HALOESEventSummaryWithPreds <- HALOESEventSummaryWithPreds %>%
  mutate(
    xGWithinNext12Sec        = HALOxGWindow_dt$xGWithinNext12Sec[match(seq_len(n()), HALOxGWindow_dt$row_id)],
    ShotCountWithinNext12Sec = HALOxGWindow_dt$ShotCountWithinNext12Sec[match(seq_len(n()), HALOxGWindow_dt$row_id)]
  )

# Sanity checks
stopifnot(nrow(HALOxGWindow_dt) == nrow(HALOESEventSummaryWithPreds),
          !anyNA(HALOESEventSummaryWithPreds$xGWithinNext12Sec))
summary(HALOESEventSummaryWithPreds$xGWithinNext12Sec)
mean(HALOESEventSummaryWithPreds$xGWithinNext12Sec > 0)   # share of events with any shot in the window

# For shot events, the window xG should be at least the shot's own xG
HALOESEventSummaryWithPreds %>%
  filter(event_type == "shot", !is.na(sl_xg_all_shots)) %>%
  summarise(share_ok = mean(xGWithinNext12Sec >= sl_xg_all_shots - 1e-9))

#Need to use the even-strength stints and the dataframe with the goal probabilities to build a dataframe with the players on the ice for each event
HALOStintsEvenStrengthWide
HALOESEventSummaryWithPreds

HALOStintsEvenStrengthWide.DT <- as.data.table(HALOStintsEvenStrengthWide)
HALOESEventSummaryWithPreds.DT <- as.data.table(HALOESEventSummaryWithPreds)

HALOStintsLineup <- as_tibble(HALOStintsEvenStrengthWide) %>%
  select(game_id, period, period_time_start, period_time_end,
         HomePlayer1, HomePlayer2, HomePlayer3, HomePlayer4, HomePlayer5,
         AwayPlayer1, AwayPlayer2, AwayPlayer3, AwayPlayer4, AwayPlayer5) %>%
  distinct()

anyDuplicated(HALOStintsLineup[, c("game_id", "period", "period_time_start", "period_time_end")])  # expect 0
nrow(HALOStintsLineup) 

HALOStintsLineup.DT <- as.data.table(HALOStintsLineup)
setkey(HALOStintsLineup.DT, game_id, period, period_time_start, period_time_end)

HALOESEventSummaryWithPreds.DT <- as.data.table(HALOESEventSummaryWithPreds)
HALOESEventSummaryWithPreds.DT[, period_time_end := period_time]

HALOEventsWithLineup <- foverlaps(
  HALOESEventSummaryWithPreds.DT,
  HALOStintsLineup.DT,
  by.x = c("game_id", "period", "period_time", "period_time_end"),
  by.y = c("game_id", "period", "period_time_start", "period_time_end"),
  type = "within",
  nomatch = NA
) %>% as_tibble()

HALOEventsWithLineupWithEventTypeSliced <- HALOEventsWithLineup %>%
  group_by(game_id, sl_event_id) %>%
  slice(if (first(event_type == "faceoff")) n() else 1) %>% #if the event is the faceoff, take the second instance for the updated lineup, anything else take who was on the ice immediately before
  ungroup()

HALOEventsWithLineupForRAPM <- HALOEventsWithLineupWithEventTypeSliced %>%
  select(isHomeEvent, HomePlayer1, HomePlayer2, HomePlayer3, HomePlayer4, HomePlayer5,
         AwayPlayer1, AwayPlayer2, AwayPlayer3, AwayPlayer4, AwayPlayer5, isHomeOffense,
         HomePlayer1TrackingID, HomePlayer2TrackingID, HomePlayer3TrackingID, HomePlayer4TrackingID, HomePlayer5TrackingID,
         HomePlayer1DistanceToEvent, HomePlayer2DistanceToEvent, HomePlayer3DistanceToEvent, HomePlayer4DistanceToEvent, HomePlayer5DistanceToEvent,
         AwayPlayer1TrackingID, AwayPlayer2TrackingID, AwayPlayer3TrackingID, AwayPlayer4TrackingID, AwayPlayer5TrackingID,
         AwayPlayer1DistanceToEvent, AwayPlayer2DistanceToEvent, AwayPlayer3DistanceToEvent, AwayPlayer4DistanceToEvent, AwayPlayer5DistanceToEvent,
         goal_prob_raw, isHomeEvent, StrengthState, HomeTeamScoreDiff, ZoneByHomePerpsective, ZoneByAwayPerpsective) %>%
  mutate(
    isHome = ifelse(isHomeEvent==1, 1, 0),
    is4on4 = coalesce(ifelse(StrengthState == "4on4", 1, 0), 0),
    is3on3 = coalesce(ifelse(StrengthState == "3on3", 1, 0), 0),
  )

HALOEventsWithLineupForRAPMHomeEvents <- HALOEventsWithLineupForRAPM %>%
  filter(isHomeEvent == 1)

HALOEventsWithLineupForRAPMAwayEvents <- HALOEventsWithLineupForRAPM %>%
  filter(isHomeEvent == 0)

HALOPlayersIDs <- HALOPlayers %>%
  filter(position_group != "G") %>%
  pull(player_id)

BuildHomeRAPMMatrixConstant <- function(events_df, player_ids) {
  
  all_cols  <- c(paste0(player_ids, "_O"), paste0(player_ids, "_D"))
  col_index <- setNames(seq_along(all_cols), all_cols)
  
  home_player_cols <- c("HomePlayer1", "HomePlayer2", "HomePlayer3",
                        "HomePlayer4", "HomePlayer5")
  away_player_cols <- c("AwayPlayer1", "AwayPlayer2", "AwayPlayer3",
                        "AwayPlayer4", "AwayPlayer5")
  
  home_triplets <- lapply(home_player_cols, function(player_col) {
    player_col_names <- paste0(events_df[[player_col]], "_O")
    valid <- !is.na(events_df[[player_col]]) & player_col_names %in% all_cols
    list(
      rows = which(valid),
      cols = unname(col_index[player_col_names[valid]])
    )
  })
  
  away_triplets <- lapply(away_player_cols, function(player_col) {
    player_col_names <- paste0(events_df[[player_col]], "_D")
    valid <- !is.na(events_df[[player_col]]) & player_col_names %in% all_cols
    list(
      rows = which(valid),
      cols = unname(col_index[player_col_names[valid]])
    )
  })
  
  all_triplets <- c(home_triplets, away_triplets)
  row_indices  <- unlist(lapply(all_triplets, `[[`, "rows"))
  col_indices  <- unlist(lapply(all_triplets, `[[`, "cols"))
  
  sparse_matrix <- sparseMatrix(
    i = row_indices,
    j = col_indices,
    x = 1,
    dims = c(nrow(events_df), length(all_cols)),
    dimnames = list(NULL, all_cols)
  )
  sparse_matrix
  
  OtherRAPMVariablesDF <- events_df %>%
    mutate(
      ScoreDiffUp3Plus = ifelse(HomeTeamScoreDiff == 3, 1, 0),
      ScoreDiffUp2 = ifelse(HomeTeamScoreDiff == 2, 1, 0),
      ScoreDiffUp1 = ifelse(HomeTeamScoreDiff == 1, 1, 0),
      ScoreDiffDown1 = ifelse(HomeTeamScoreDiff == -1, 1, 0),
      ScoreDiffDown2 = ifelse(HomeTeamScoreDiff == -2, 1, 0),
      ScoreDiffDown3Plus = ifelse(HomeTeamScoreDiff == -3, 1, 0)
    ) %>%
    select(isHome, is4on4, is3on3, ScoreDiffUp3Plus, ScoreDiffUp2, ScoreDiffUp1, 
           ScoreDiffDown1, ScoreDiffDown2, ScoreDiffDown3Plus)
  
  OtherRAPMVariablesMatrix <- as.matrix(OtherRAPMVariablesDF)
  FullRAPMMatrix <- cbind(sparse_matrix, OtherRAPMVariablesMatrix)
  return(FullRAPMMatrix)
}


BuildAwayRAPMMatrixConstant <- function(events_df, player_ids) {
  
  all_cols  <- c(paste0(player_ids, "_O"), paste0(player_ids, "_D"))
  col_index <- setNames(seq_along(all_cols), all_cols)
  
  home_player_cols <- c("HomePlayer1", "HomePlayer2", "HomePlayer3",
                        "HomePlayer4", "HomePlayer5")
  away_player_cols <- c("AwayPlayer1", "AwayPlayer2", "AwayPlayer3",
                        "AwayPlayer4", "AwayPlayer5")
  
  home_triplets <- lapply(home_player_cols, function(player_col) {
    player_col_names <- paste0(events_df[[player_col]], "_D")
    valid <- !is.na(events_df[[player_col]]) & player_col_names %in% all_cols
    list(
      rows = which(valid),
      cols = unname(col_index[player_col_names[valid]])
    )
  })
  
  away_triplets <- lapply(away_player_cols, function(player_col) {
    player_col_names <- paste0(events_df[[player_col]], "_O")
    valid <- !is.na(events_df[[player_col]]) & player_col_names %in% all_cols
    list(
      rows = which(valid),
      cols = unname(col_index[player_col_names[valid]])
    )
  })
  
  all_triplets <- c(home_triplets, away_triplets)
  row_indices  <- unlist(lapply(all_triplets, `[[`, "rows"))
  col_indices  <- unlist(lapply(all_triplets, `[[`, "cols"))
  
  sparse_matrix <- sparseMatrix(
    i = row_indices,
    j = col_indices,
    x = 1,
    dims = c(nrow(events_df), length(all_cols)),
    dimnames = list(NULL, all_cols)
  )
  sparse_matrix
  
  OtherRAPMVariablesDF <- events_df %>%
    mutate(
      ScoreDiffUp3Plus = ifelse(HomeTeamScoreDiff == -3, 1, 0),
      ScoreDiffUp2 = ifelse(HomeTeamScoreDiff == -2, 1, 0),
      ScoreDiffUp1 = ifelse(HomeTeamScoreDiff == -1, 1, 0),
      ScoreDiffDown1 = ifelse(HomeTeamScoreDiff == 1, 1, 0),
      ScoreDiffDown2 = ifelse(HomeTeamScoreDiff == 2, 1, 0),
      ScoreDiffDown3Plus = ifelse(HomeTeamScoreDiff == 3, 1, 0)
    ) %>%
    select(isHome, is4on4, is3on3, ScoreDiffUp3Plus, ScoreDiffUp2, ScoreDiffUp1, 
           ScoreDiffDown1, ScoreDiffDown2, ScoreDiffDown3Plus)
  
  OtherRAPMVariablesMatrix <- as.matrix(OtherRAPMVariablesDF)
  FullRAPMMatrix <- cbind(sparse_matrix, OtherRAPMVariablesMatrix)
  return(FullRAPMMatrix)
}

HALOConstantRAPMMatrixHome <- BuildHomeRAPMMatrixConstant(HALOEventsWithLineupForRAPMHomeEvents, HALOPlayersIDs)
HALOConstantRAPMMatrixAway <- BuildAwayRAPMMatrixConstant(HALOEventsWithLineupForRAPMAwayEvents, HALOPlayersIDs)

HALOConstantRAPMMatrixFull <- rbind(HALOConstantRAPMMatrixHome, HALOConstantRAPMMatrixAway)

HALOConstantRAPMWeights <- rep(1, times = nrow(HALOConstantRAPMMatrixFull))
HALOGoalProbabilites <- c(HALOEventsWithLineupForRAPMHomeEvents$goal_prob_raw,HALOEventsWithLineupForRAPMAwayEvents$goal_prob_raw)

set.seed(03032002)
HALORAPMConstantCV <- cv.glmnet(x = HALOConstantRAPMMatrixFull, y = HALOGoalProbabilites, weights = HALOConstantRAPMWeights,
                           alpha = 0, nfolds = 10, standardize = FALSE, parallel=TRUE )

HALORAPMConstantGLM <- glmnet(x = HALOConstantRAPMMatrixFull, y = HALOGoalProbabilites, weights = HALOConstantRAPMWeights,
                         alpha = 0, lambda = HALORAPMConstantCV$lambda.1se, standardize = FALSE, parallel=TRUE )


HALO_RAPM_Constant_Coefficients <- data.frame(
  column_name = rownames(HALORAPMConstantGLM$beta),
  coefficient = as.vector(HALORAPMConstantGLM$beta)
) %>%
  filter(grepl("_O$|_D$", column_name)) %>%
  mutate(
    player_id = gsub("_O$|_D$", "", column_name),
    role      = ifelse(grepl("_O$", column_name), "Offense", "Defense")
  ) %>%
  select(player_id, role, coefficient) %>%
  pivot_wider(
    names_from  = role,
    values_from = coefficient,
    names_glue  = "THoR_{role}"
  ) %>%
  left_join(
    HALOPlayers %>% select(player_id, player_name, position_group),
    by = "player_id"
  ) %>%
  mutate(OffenseRank = rank(desc(THoR_Offense)),
         DefenseRank = rank((THoR_Defense))) %>%
  relocate(player_id, player_name, position_group, 
           THoR_Offense, THoR_Defense) %>%
  arrange((OffenseRank))

HALOPlayerPlaysBySidePerGame_3Players <- HALOEventsWithLineupWithEventTypeSliced %>%
  select(game_id, sl_event_id, isHomeEvent, matches("^(Home|Away)Player[1-5]$")) %>%
  pivot_longer(cols = matches("^(Home|Away)Player[1-5]$"), names_to = "slot", values_to = "player_id") %>%
  filter(!is.na(player_id)) %>%
  distinct(game_id, sl_event_id, slot, player_id, isHomeEvent) %>%
  mutate(on_home_side = startsWith(slot, "Home"),
         role = ifelse(on_home_side == (isHomeEvent == 1), "O", "D")) %>%   # same side as the event team = _O
  count(game_id, player_id, role) %>%
  pivot_wider(names_from = role, values_from = n, values_fill = 0) %>%
  group_by(player_id) %>%
  summarise(games_played = n(),
            avg_O_plays_per_game = mean(O),
            avg_D_plays_per_game = mean(D),
            .groups = "drop")

HALOPlayerPlaysBySidePerGame_3Players

HALOAvgOffensivePlaysPerGame <- mean(HALOPlayerPlaysBySidePerGame_3Players$avg_O_plays_per_game)
HALOAvgDefensivePlaysPerGame <- mean(HALOPlayerPlaysBySidePerGame_3Players$avg_D_plays_per_game)

HALO_THoR_Constant_Coefficients_Wins <- HALO_RAPM_Constant_Coefficients %>%
  mutate(
    OffensiveWins = (THoR_Offense*HALOAvgOffensivePlaysPerGame*72)/6,
    DefensiveWins = -(THoR_Defense*HALOAvgDefensivePlaysPerGame*72)/6, #Flipping Defensive Wins sign because the coefficient for good defense is negative
    WinsCreated = OffensiveWins+DefensiveWins,
    WinsCreatedRank = rank(desc(WinsCreated))
  ) %>%
  arrange(desc(WinsCreated))

write_csv(HALO_THoR_Constant_Coefficients_Wins, "/Users/evanlawing/Desktop/HALO_THoR_Constant_Coefficients_Wins.csv")


PlayerEventSummary <- function(data, player_id_input) {
  
  # Get player name for display
  player_name_display <- HALOPlayers %>%
    filter(player_id == player_id_input) %>%
    pull(player_name)
  
  # Events where player is on offense
  offensive_events <- data %>%
    filter(
      (isHomeOffense == 1 & (HomePlayer1 == player_id_input | HomePlayer2 == player_id_input |
                               HomePlayer3 == player_id_input | HomePlayer4 == player_id_input |
                               HomePlayer5 == player_id_input)) |
        (isHomeOffense == 0 & (AwayPlayer1 == player_id_input | AwayPlayer2 == player_id_input |
                                 AwayPlayer3 == player_id_input | AwayPlayer4 == player_id_input |
                                 AwayPlayer5 == player_id_input))
    ) %>%
    group_by(event_type) %>%
    summarise(
      OffensiveEvents    = n(),
      AvgGoalProb_Off    = mean(goal_prob_raw, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Events where player is on defense
  defensive_events <- data %>%
    filter(
      (isHomeOffense == 0 & (HomePlayer1 == player_id_input | HomePlayer2 == player_id_input |
                               HomePlayer3 == player_id_input | HomePlayer4 == player_id_input |
                               HomePlayer5 == player_id_input)) |
        (isHomeOffense == 1 & (AwayPlayer1 == player_id_input | AwayPlayer2 == player_id_input |
                                 AwayPlayer3 == player_id_input | AwayPlayer4 == player_id_input |
                                 AwayPlayer5 == player_id_input))
    ) %>%
    group_by(event_type) %>%
    summarise(
      DefensiveEvents    = n(),
      AvgGoalProb_Def    = mean(goal_prob_raw, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Join offensive and defensive summaries
  summary_table <- full_join(offensive_events, defensive_events, by = "event_type") %>%
    mutate(
      player_id   = player_id_input,
      player_name = player_name_display,
      OffensiveEvents  = replace_na(OffensiveEvents, 0),
      DefensiveEvents  = replace_na(DefensiveEvents, 0),
      TotalEvents      = OffensiveEvents + DefensiveEvents
    ) %>%
    relocate(player_id, player_name, event_type, 
             OffensiveEvents, AvgGoalProb_Off,
             DefensiveEvents, AvgGoalProb_Def,
             TotalEvents) %>%
    arrange(desc(TotalEvents))
  
  return(summary_table)
}

#Mavrik Bourque
View(PlayerEventSummary(HALOEventsWithLineupWithEventTypeSliced, "41273737-16e3-50f4-d08a-03a1fc11edf5"))

#Logan Stankoven
View(PlayerEventSummary(HALOEventsWithLineupWithEventTypeSliced, "8dfc4cd9-4bed-d27e-f82a-aac7483571b8"))

#Brandt Clarke
View(PlayerEventSummary(HALOEventsWithLineupWithEventTypeSliced, "bc0901c4-4d45-8b85-d17f-37826e9e5b4a"))


