#Playoff Weighted RAPM

library(dplyr)
library(tidyr)
library(purrr)
library(data.table)
library(nhlscraper)
library(survey)
library(Matrix)
library(glmnet)
library(ggplot2)
library(lubridate)

RAPMSeasonsOfInterest <- c(20232024, 20242025, 20252026)

RSGamesIn2023_2026 <- games() %>%
  filter(seasonId %in% RAPMSeasonsOfInterest, gameTypeId!=1)

RAPMAnalysisGameIDs <- RSGamesIn2023_2026$gameId

RSGamesIn2023_2026PORoundsAndGameNum <- RSGamesIn2023_2026 %>%
  filter(gameTypeId == 3, period != 1) %>%
  mutate(
    gameId_str = as.character(gameId),
    PlayoffRound = substr(gameId_str, 8,8),
    SeriesKey = pmap_chr(
      list(homeTeamId,visitingTeamId),
      \(h,v) paste(sort(c(h,v)), collapse = "_")),
    GameWinner = ifelse(homeScore > visitingScore, homeTeamId, visitingTeamId),
    GameLoser = ifelse(homeScore > visitingScore, visitingTeamId, homeTeamId)
    ) %>%
  arrange(seasonId, SeriesKey, easternStartTime) %>%
  group_by(seasonId, SeriesKey) %>%
  mutate(SeriesGameNumber = row_number(),
         Team1 = as.integer(sub("_.*", "", SeriesKey)),
         Team2 = as.integer(sub(".*_", "", SeriesKey)),
         Team1Wins_pre = cumsum(lag(GameWinner == Team1, default = FALSE)),
         Team2Wins_pre = cumsum(lag(GameWinner == Team2, default = FALSE)),
         Team1Wins_post = cumsum(GameWinner == Team1),
         Team2Wins_post = cumsum(GameWinner == Team2),
         SeriesWinner = ifelse(Team1Wins_post == 4, Team1, 
                               ifelse(Team2Wins_post == 4, Team2, NA_integer_)),
         LeadingTeamWins = pmax(Team1Wins_pre, Team2Wins_pre),
         TrailingTeamWins = pmin(Team1Wins_pre, Team2Wins_pre)) %>%
  ungroup() %>%
  select(gameId, PlayoffRound, SeriesGameNumber, GameWinner, GameLoser,
         LeadingTeamWins, TrailingTeamWins, Team1Wins_pre, Team2Wins_pre, Team1Wins_post, Team2Wins_post, Team1, Team2, SeriesWinner)

RSGamesIn2023_2026WithPOInformation <- RSGamesIn2023_2026 %>%
  merge(RSGamesIn2023_2026PORoundsAndGameNum, by = "gameId", all.x = TRUE)

TeamScheduleLong <- RSGamesIn2023_2026WithPOInformation %>%
  select(gameId, gameDate, seasonId, homeTeamId, visitingTeamId) %>%
  pivot_longer(
    cols = c(homeTeamId, visitingTeamId),
    names_to = "homeAway",
    values_to = "teamId"
  ) %>%
  mutate(homeAway = ifelse(homeAway == "homeTeamId", "home", "away"))

CalculateDaysBetween <- TeamScheduleLong %>%
  group_by(seasonId, teamId) %>%
  mutate(DaysSinceLastGame = difftime(as.Date(gameDate), as.Date(lag(gameDate)), units = "days"),
         BTB = DaysSinceLastGame == 1) %>%
  ungroup() %>%
  arrange(teamId)

BTB_flags <- CalculateDaysBetween %>%
  select(gameId, teamId, seasonId, DaysSinceLastGame, BTB)

RSGamesIn2023_2026WithPOInformationWithBTB <- RSGamesIn2023_2026WithPOInformation %>%
  left_join(BTB_flags, by = c("gameId", "homeTeamId" = "teamId")) %>%
  rename(homeDaysSinceLastGame = DaysSinceLastGame,
         homeBTB = BTB) %>%
  left_join(BTB_flags, by = c("gameId", "visitingTeamId" = "teamId")) %>%
  rename(awayDaysSinceLastGame = DaysSinceLastGame,
         awayBTB = BTB)


shiftcharts2023_24 <- shift_charts(season = 20232024 )%>%
  filter(gameId %in% RAPMAnalysisGameIDs)
shiftcharts2024_25 <- shift_charts(season = 20242025) %>%
  filter(gameId %in% RAPMAnalysisGameIDs)
shiftcharts2025_26 <- shift_charts(season = 20252026) %>%
  filter(gameId %in% RAPMAnalysisGameIDs)


ShiftSummaryCreator <- function(df, df2) {
  breakpoints <- df %>%
    group_by(gameId) %>%
      reframe(
        breakpoint = sort(unique(c(startSecondsElapsedInGame, endSecondsElapsedInGame)))
      )
  
  windows <- breakpoints %>%
    group_by(gameId) %>%
    mutate(
      window_start = breakpoint,
      window_end = lead(breakpoint)
    ) %>%
    filter(!is.na(window_end)) %>%
    select(gameId, window_start, window_end) %>%
    ungroup()
  
  stints <- windows %>%
    left_join(df, by = "gameId", relationship = "many-to-many") %>%
    filter(
      startSecondsElapsedInGame <= window_start,
      endSecondsElapsedInGame   >= window_end
    ) %>%
    group_by(gameId, teamId, window_start, window_end) %>%
    arrange(desc(duration), .by_group = TRUE) %>%
    filter(row_number() <= 6) %>%         
    summarize(
      players   = list(sort(unique(playerId))),
      n_skaters = n_distinct(playerId),
      .groups   = "drop"
    )
  
  game_teams <- df2 %>%
    select(gameId, homeTeamId, visitingTeamId)
  
  stints_with_teams_joiined <- stints %>%
    left_join(game_teams, by = "gameId") %>%
    mutate(isHomeTeam = teamId == homeTeamId)
  
  stints_summarized <- stints_with_teams_joiined %>%
    group_by(gameId, window_start, window_end) %>%
      summarize(
        homeTeamId = first(homeTeamId),
        awayTeamId = first(visitingTeamId),
        home_players = players[isHomeTeam][1],
        away_players = players[!isHomeTeam][1],
        home_skaters = n_skaters[isHomeTeam][1],
        away_skaters = n_skaters[!isHomeTeam][1],
        .groups = "drop"
      ) %>%
    mutate(stint_duration = window_end-window_start) %>%
    group_by(gameId) %>%
    mutate(all_players_this = map2(home_players, away_players, \(h,a) sort(c(h,a))),
           all_players_next = lead(all_players_this),
           different_players_check = map2(
             all_players_this, all_players_next, \(x,y) if (is.null(y)) character(0) else(setdiff(x,y))
           ),
           lineup_changed = map_lgl(different_players_check, \(x) length(x)>0)) %>% ungroup()
  
  stints_summarized2 <- stints_summarized %>%
    group_by(gameId) %>%
    mutate(stintId = cumsum(lineup_changed)) %>%
    group_by(gameId, stintId) %>%
    summarize(
      stint_start    = min(window_start),
      stint_end      = max(window_end),
      stint_duration = sum(stint_duration),
      homeTeamId     = first(homeTeamId),
      awayTeamId     = first(awayTeamId),
      home_players   = list(first(home_players)), 
      away_players   = list(first(away_players)), 
      home_skaters   = first(home_skaters),
      away_skaters   = first(away_skaters),
      .groups        = "drop"
    ) %>%
    select(-stintId)
 return(stints_summarized2) 
}

ShiftSummary2023_24 <- ShiftSummaryCreator(shiftcharts2023_24, RSGamesIn2023_2026)
ShiftSummary2024_25 <- ShiftSummaryCreator(shiftcharts2024_25, RSGamesIn2023_2026)
ShiftSummary2025_26 <- ShiftSummaryCreator(shiftcharts2025_26, RSGamesIn2023_2026)

PxP2023_24 <- gc_play_by_plays(20232024) %>%
  filter(gameId %in% RAPMAnalysisGameIDs)
PxP2024_25 <- gc_play_by_plays(20242025) %>%
  filter(gameId %in% RAPMAnalysisGameIDs)
PxP2025_26 <- gc_play_by_plays(20252026) %>%
  filter(gameId %in% RAPMAnalysisGameIDs)

PxP2023_24withxG <- calculate_xG(PxP2023_24)
PxP2024_25withxG <- calculate_xG(PxP2024_25)
PxP2025_26withxG <- calculate_xG(PxP2025_26)

FaceoffLocation <- function(df){
     FOLocationDataframe <- df %>%
       mutate(
         FOLocationBasedOnHomeTeam = case_when(
           eventTypeDescKey != "faceoff" ~ NA_character_,
           homeTeamDefendingSide == "left" & xCoord >= 25 ~ "O",
           homeTeamDefendingSide == "left" & xCoord <= -25 ~ "D",
           homeTeamDefendingSide == "left" ~ "N",
           homeTeamDefendingSide == "right" & xCoord >= 25 ~ "D",
           homeTeamDefendingSide == "right" & xCoord <=-25 ~ "O",
           homeTeamDefendingSide == "right" ~ "N"
         )
       )
     return(FOLocationDataframe)
}

ShiftSummaryAndPxPMerge <- function(PxPWithxG, ShiftSummary){
  
  stints_with_xg_score_diff <- PxPWithxG %>%
    mutate(
      HomeShotxG = ifelse(isHome == TRUE, xG, 0),
      AwayShotxG = ifelse(isHome == FALSE, xG, 0),
      ScoreDifferentialForHomeTeam = homeGoals - awayGoals
    )
  
  stints_with_xg_breakout_and_fo <- FaceoffLocation(stints_with_xg_score_diff)
  
  first_non_na <- function(x) {
    clean <- na.omit(x)
    if (length(clean) == 0) return(NA)
    first(clean)
  }
  
  events_dt <- as.data.table(stints_with_xg_breakout_and_fo)
  
  stints_dt <- as.data.table(
    ShiftSummary %>% select(gameId, stint_start, stint_end)
  )
  
  stints_dt[, `:=`(ws = stint_start, we = stint_end)]
  
  setkey(stints_dt, gameId, stint_start, stint_end)
  
  stints_and_PxP_dt <- stints_dt[
    events_dt,
    on = .(
      gameId     == gameId,
      stint_start <= secondsElapsedInGame,
      stint_end   >= secondsElapsedInGame
    ),
    nomatch = NA,
    mult = "first"
  ]
  
  stints_and_PxP <- as_tibble(stints_and_PxP_dt) %>%
    select(-stint_start, -stint_end) %>%
    rename(stint_start = ws,
           stint_end   = we)
  
  stint_collapsed_summary <- stints_and_PxP %>%
    group_by(gameId, stint_start, stint_end) %>%
      summarise(
        firstSituationCode = first(situationCode),
        lastSituationCode = last(situationCode),
        stint_total_xg = sum(xG, na.rm = TRUE),
        stint_home_xg = sum(HomeShotxG, na.rm = TRUE),
        stint_away_xg = sum(AwayShotxG, na.rm = TRUE),
        stint_start_score_diffirential = first_non_na(ScoreDifferentialForHomeTeam),
        first_fo_location = first_non_na(FOLocationBasedOnHomeTeam),
        home_ozone_fos = sum(FOLocationBasedOnHomeTeam == "O", na.rm = TRUE),
        home_nzone_fos = sum(FOLocationBasedOnHomeTeam == "N", na.rm = TRUE),
        home_dzone_fos = sum(FOLocationBasedOnHomeTeam == "D", na.rm = TRUE),
        total_fos = sum(FOLocationBasedOnHomeTeam %in% c("O", "N", "D"), na.rm = TRUE),
        .groups = "drop"
      )
  
  finished_shift_summary <- ShiftSummary %>%
    left_join(stint_collapsed_summary, by = join_by(gameId, stint_start, stint_end))
  
  es_situation_codes <- c("1331","1441","1551")
  
  finished_shift_summary_es <- finished_shift_summary %>%
    filter(firstSituationCode %in% es_situation_codes, 
           lastSituationCode %in% es_situation_codes)
  
  return(finished_shift_summary_es)
}


PxPwithStintEnds2023_24 <- ShiftSummaryAndPxPMerge(PxP2023_24withxG, ShiftSummary2023_24) 
PxPwithStintEnds2024_25 <- ShiftSummaryAndPxPMerge(PxP2024_25withxG, ShiftSummary2024_25)
PxPwithStintEnds2025_26 <- ShiftSummaryAndPxPMerge(PxP2025_26withxG, ShiftSummary2025_26)

ExpandSkaters <- function(df){
  ExpandedSkatersDF <- df %>%
    mutate(HomeSkater1 = map_int(home_players, \(x) x[1]),
           HomeSkater2 = map_int(home_players, \(x) x[2]),
           HomeSkater3 = map_int(home_players, \(x) x[3]),
           HomeSkater4 = map_int(home_players, \(x) x[4]),
           HomeSkater5 = map_int(home_players, \(x) x[5]),
           HomeSkater6 = map_int(home_players, \(x) x[6]),
           AwaySkater1 = map_int(away_players, \(x) x[1]),
           AwaySkater2 = map_int(away_players, \(x) x[2]),
           AwaySkater3 = map_int(away_players, \(x) x[3]),
           AwaySkater4 = map_int(away_players, \(x) x[4]),
           AwaySkater5 = map_int(away_players, \(x) x[5]),
           AwaySkater6 = map_int(away_players, \(x) x[6]))
  
  return(ExpandedSkatersDF)
}

PxPwithStintEnds2023_24Expanded <- ExpandSkaters(PxPwithStintEnds2023_24)
PxPwithStintEnds2024_25Expanded <- ExpandSkaters(PxPwithStintEnds2024_25)
PxPwithStintEnds2025_26Expanded <- ExpandSkaters(PxPwithStintEnds2025_26)

BringOverGameInfo <- function(StintsExpanded, GameInfo){
  StintsExpandedWithGameInfo <- StintsExpanded %>%
    merge(GameInfo, by = "gameId", all.x=TRUE)
}

PxPwithStintEnds2023_24ExpandedGameInfo <- BringOverGameInfo(PxPwithStintEnds2023_24Expanded,RSGamesIn2023_2026WithPOInformationWithBTB)
PxPwithStintEnds2024_25ExpandedGameInfo <- BringOverGameInfo(PxPwithStintEnds2024_25Expanded,RSGamesIn2023_2026WithPOInformationWithBTB)
PxPwithStintEnds2025_26ExpandedGameInfo <- BringOverGameInfo(PxPwithStintEnds2025_26Expanded,RSGamesIn2023_2026WithPOInformationWithBTB)

AddPlayoffWeights <- function(df){
  df %>%
    mutate(
      PlayoffRound = as.integer(PlayoffRound),
      SeriesWeight = case_when(
        gameTypeId != 3 ~ 1,
        PlayoffRound == 1 ~ 2,
        PlayoffRound == 2 ~ 3,
        PlayoffRound == 3 ~ 4,
        PlayoffRound == 4 ~ 5
      ),
      SeriesLeverage = case_when(
        gameTypeId != 3 ~ 1,
        TRUE ~ 0.99/((4-LeadingTeamWins)+(4-TrailingTeamWins)-0.99)
      ),
      RAPMWeight = (SeriesWeight+SeriesLeverage)*stint_duration
    )
}

PxPwithStintEnds2023_24ExpandedGameInfoWeights <- AddPlayoffWeights(PxPwithStintEnds2023_24ExpandedGameInfo)
PxPwithStintEnds2024_25ExpandedGameInfoWeights <- AddPlayoffWeights(PxPwithStintEnds2024_25ExpandedGameInfo)
PxPwithStintEnds2025_26ExpandedGameInfoWeights <- AddPlayoffWeights(PxPwithStintEnds2025_26ExpandedGameInfo)

PxPwithStintEnds2023_24ExpandedGameInfoWeightsxGFP60 <- PxPwithStintEnds2023_24ExpandedGameInfoWeights %>%
  mutate(TotalxGFPer60 = (stint_total_xg/stint_duration)*3600,
         HomexGFPer60 = (stint_home_xg/stint_duration)*3600,
         AwayxGFPer60 = (stint_away_xg/stint_duration)*3600) %>%
  filter(HomexGFPer60<=100&AwayxGFPer60<=100)

PxPwithStintEnds2024_25ExpandedGameInfoWeightsxGFP60 <- PxPwithStintEnds2024_25ExpandedGameInfoWeights %>%
  mutate(TotalxGFPer60 = (stint_total_xg/stint_duration)*3600,
         HomexGFPer60 = (stint_home_xg/stint_duration)*3600,
         AwayxGFPer60 = (stint_away_xg/stint_duration)*3600) %>%
  filter(HomexGFPer60<=100&AwayxGFPer60<=100)

PxPwithStintEnds2025_26ExpandedGameInfoWeightsxGFP60 <- PxPwithStintEnds2025_26ExpandedGameInfoWeights %>%
  mutate(TotalxGFPer60 = (stint_total_xg/stint_duration)*3600,
         HomexGFPer60 = (stint_home_xg/stint_duration)*3600,
         AwayxGFPer60 = (stint_away_xg/stint_duration)*3600) %>%
  filter(HomexGFPer60<=100&AwayxGFPer60<=100)



all_nhl_players <- players()
all_nhl_playersPIDandName <- all_nhl_players %>%
  select(playerId, playerFullName, positionCode) %>%
  mutate(positionGroup = case_when(
    positionCode %in% c("L","R","C") ~ "F",
    positionCode == "D" ~ "D",
    positionCode == "G" ~ "G"
  ))

nhl_teams <- teams()

RAPMMatrixCreationHomeBased <- function(df, all_players_df){
  all_players <- data.frame(PlayerID=sort(unique(c(unlist(df$home_players), unlist(df$away_players)))))
  only_skaters <- all_players_df %>%
    filter(positionCode != "G") %>%
    pull(playerId) 
  all_skaters <- all_players %>%
    filter(PlayerID %in% only_skaters)
  n_stints <- nrow(df)
  player_team_combos <- pmap(
    list(df$home_players, df$away_players, df$homeTeamId.x, df$awayTeamId),
    \(hp, ap, ht, at) {
      home_valid <- intersect(as.character(hp), as.character(all_skaters$PlayerID))
      away_valid <- intersect(as.character(ap), as.character(all_skaters$PlayerID))
      c(paste0(home_valid, "_", ht), paste0(away_valid, "_", at))
    }
  ) %>% unlist() %>% unique() %>% sort()
  
  off_cols <- paste0(player_team_combos, "_O")
  def_cols <- paste0(player_team_combos, "_D")
  all_cols  <- c(off_cols, def_cols)
  
  n_cols <- length(all_cols)
  
  X <- Matrix(0, nrow = n_stints, ncol = n_cols, sparse = TRUE)
  colnames(X) <- all_cols
  
  for (i in seq_len(n_stints)) {
    home_valid <- intersect(as.character(df$home_players[[i]]), as.character(all_skaters$PlayerID))
    away_valid <- intersect(as.character(df$away_players[[i]]), as.character(all_skaters$PlayerID))
    
    home_tagged <- paste0(home_valid, "_", df$homeTeamId.x[i])
    away_tagged <- paste0(away_valid, "_", df$awayTeamId[i])
    
    X[i, paste0(home_tagged, "_O")] <- 1
    X[i, paste0(away_tagged, "_D")] <- 1
  }
  return(X)
}

RAPMMatrixCreationAwayBased <- function(df, all_players_df){
  all_players <- data.frame(PlayerID=sort(unique(c(unlist(df$home_players), unlist(df$away_players)))))
  only_skaters <- all_players_df %>%
    filter(positionCode != "G") %>%
    pull(playerId) 
  all_skaters <- all_players %>%
    filter(PlayerID %in% only_skaters)
  n_stints <- nrow(df)
  player_team_combos <- pmap(
    list(df$home_players, df$away_players, df$homeTeamId.x, df$awayTeamId),
    \(hp, ap, ht, at) {
      home_valid <- intersect(as.character(hp), as.character(all_skaters$PlayerID))
      away_valid <- intersect(as.character(ap), as.character(all_skaters$PlayerID))
      c(paste0(home_valid, "_", ht), paste0(away_valid, "_", at))
    }
  ) %>% unlist() %>% unique() %>% sort()
  
  off_cols <- paste0(player_team_combos, "_O")
  def_cols <- paste0(player_team_combos, "_D")
  all_cols  <- c(off_cols, def_cols)
  
  n_cols <- length(all_cols)
  
  X <- Matrix(0, nrow = n_stints, ncol = n_cols, sparse = TRUE)
  colnames(X) <- all_cols
  
  for (i in seq_len(n_stints)) {
    home_valid <- intersect(as.character(df$home_players[[i]]), as.character(all_skaters$PlayerID))
    away_valid <- intersect(as.character(df$away_players[[i]]), as.character(all_skaters$PlayerID))
    
    home_tagged <- paste0(home_valid, "_", df$homeTeamId.x[i])
    away_tagged <- paste0(away_valid, "_", df$awayTeamId[i])
    
    X[i, paste0(away_tagged, "_O")] <- 1
    X[i, paste0(home_tagged, "_D")] <- 1
  }
  return(X)
}

OtherHomeRAPMVariables <- function(RAPMMatrix, OtherColumnVariablesDF){
  RAPMMatrixVariables <- OtherColumnVariablesDF %>%
    mutate(isHome = 1,
           onBTB = ifelse(!is.na(homeBTB) & homeBTB == TRUE, 1, 0),
           is4on4 = ifelse(firstSituationCode == "1441", 1, 0),
           ScoreStateUp3orMore = ifelse(stint_start_score_diffirential>= 3, 1, 0), #tie game is the reference
           ScoreStateUp2 = ifelse(stint_start_score_diffirential==2, 1, 0),
           ScoreStateUp1 = ifelse(stint_start_score_diffirential==1, 1, 0),
           ScoreStateDown1 = ifelse(stint_start_score_diffirential==-1, 1, 0),
           ScoreStateDown2 = ifelse(stint_start_score_diffirential==-2, 1, 0),
           ScoreStateDown3orMore = ifelse(stint_start_score_diffirential<=-3, 1, 0),
           OffensiveZoneFOInStint = ifelse(home_ozone_fos>0, 1, 0), # neutral zone faceoffs is the reference
           DefensiveZoneFOInStint = ifelse(home_dzone_fos>0, 1, 0)
           ) %>%
    select(isHome,onBTB,is4on4,ScoreStateUp3orMore,ScoreStateUp2,ScoreStateUp1,ScoreStateDown1,
           ScoreStateDown2,ScoreStateDown3orMore,OffensiveZoneFOInStint,DefensiveZoneFOInStint)
  
  RAPM_covariate_matrix <- as.matrix(RAPMMatrixVariables)
  RAPMMatrixFull <- cbind(RAPM_covariate_matrix, RAPMMatrix)
  return(RAPMMatrixFull)
}

OtherAwayRAPMVariables <- function(RAPMMatrix, OtherColumnVariablesDF){
  RAPMMatrixVariables <- OtherColumnVariablesDF %>%
    mutate(isHome = 0,
           onBTB = ifelse(!is.na(awayBTB) & awayBTB == TRUE, 1, 0),
           is4on4 = ifelse(firstSituationCode == "1441", 1, 0),
           ScoreStateUp3orMore = ifelse(-stint_start_score_diffirential>= 3, 1, 0), #score state variable refers to home team, taking negative to set for away team
           ScoreStateUp2 = ifelse(-stint_start_score_diffirential==2, 1, 0),
           ScoreStateUp1 = ifelse(-stint_start_score_diffirential==1, 1, 0),
           ScoreStateDown1 = ifelse(-stint_start_score_diffirential==-1, 1, 0),
           ScoreStateDown2 = ifelse(-stint_start_score_diffirential==-2, 1, 0),
           ScoreStateDown3orMore = ifelse(-stint_start_score_diffirential<=-3, 1, 0),
           OffensiveZoneFOInStint = ifelse(home_dzone_fos>0, 1, 0), #flipped zones since the location is based on the home team
           DefensiveZoneFOInStint = ifelse(home_ozone_fos>0, 1, 0)
    ) %>%
    select(isHome,onBTB,is4on4,ScoreStateUp3orMore,ScoreStateUp2,ScoreStateUp1,ScoreStateDown1,
           ScoreStateDown2,ScoreStateDown3orMore,OffensiveZoneFOInStint,DefensiveZoneFOInStint)
  
  RAPM_covariate_matrix <- as.matrix(RAPMMatrixVariables)
  RAPMMatrixFull <- cbind(RAPM_covariate_matrix, RAPMMatrix)
  return(RAPMMatrixFull)
}


ExtractRAPMCoefs <- function(coef_object, offense_col, defense_col, total_col){
  
  coefs_df <- data.frame(
    column_name = rownames(coef_object),
    coefficient = as.vector(coef_object)
  )
  
  coefs_df_players <- coefs_df %>%
    filter(grepl("_O$|_D$", column_name))
  
  coefs_split_out <- coefs_df_players %>%
    mutate(
      side = sub(".*_([OD])$", "\\1", column_name),
      player_and_team = sub("_[OD]$", "", column_name),
      teamId = as.integer(sub(".*_", "", player_and_team)),
      playerId = as.integer(sub("_[^_]+$", "", player_and_team))
    ) %>%
    select(-player_and_team, -column_name)
  
  coefs_df_wide <- coefs_split_out %>%
    pivot_wider(
      names_from = side,
      values_from = coefficient
      ) %>%
    rename(
      Offense_RAPM = O,
      Defense_RAPM = D
      )
  
  coefs_df_wide_with_rank <- coefs_df_wide %>%
    mutate(
      Total_RAPM = Offense_RAPM-Defense_RAPM,
      Offense_RAPM_Rank = min_rank(desc(Offense_RAPM)),
      Defense_RAPM_Rank = min_rank(Defense_RAPM),
      Total_RAPM_Rank = min_rank(desc(Total_RAPM))
    )
  
  coefs_with_names <- coefs_df_wide_with_rank %>%
    left_join(all_nhl_playersPIDandName, by="playerId") %>%
    left_join(nhl_teams %>% select(teamId, teamFullName, teamTriCode), by="teamId") %>%
    arrange(desc(Total_RAPM))
  
  coefs_final <- coefs_with_names %>%
    rename(
      !!offense_col := Offense_RAPM,
      !!defense_col := Defense_RAPM,
      !!total_col   := Total_RAPM,
      !!paste0(offense_col, "_Rank") := Offense_RAPM_Rank,
      !!paste0(defense_col, "_Rank") := Defense_RAPM_Rank,
      !!paste0(total_col,   "_Rank") := Total_RAPM_Rank
    )
  
  return(coefs_final)
}

set.seed(03032002)

HomeRAPMMatrix2023_24 <- RAPMMatrixCreationHomeBased(PxPwithStintEnds2023_24ExpandedGameInfoWeightsxGFP60, all_nhl_players)
AwayRAPMMatrix2023_24 <- RAPMMatrixCreationAwayBased(PxPwithStintEnds2023_24ExpandedGameInfoWeightsxGFP60, all_nhl_players)

HomeRAPMMatrix2023_24Final <- OtherHomeRAPMVariables(HomeRAPMMatrix2023_24, PxPwithStintEnds2023_24ExpandedGameInfoWeightsxGFP60)
AwayRAPMMatrix2023_24Final <- OtherAwayRAPMVariables(AwayRAPMMatrix2023_24, PxPwithStintEnds2023_24ExpandedGameInfoWeightsxGFP60)
RAPMMatrix2023_24FinalCombined <- rbind(HomeRAPMMatrix2023_24Final,AwayRAPMMatrix2023_24Final)
RAPMWeights2023_24 <- c(PxPwithStintEnds2023_24ExpandedGameInfoWeightsxGFP60$RAPMWeight,PxPwithStintEnds2023_24ExpandedGameInfoWeightsxGFP60$RAPMWeight)
xGFP60ForRAPM2023_24 <- c(PxPwithStintEnds2023_24ExpandedGameInfoWeightsxGFP60$HomexGFPer60,PxPwithStintEnds2023_24ExpandedGameInfoWeightsxGFP60$AwayxGFPer60)

RAPM2023_24CV <- cv.glmnet(x = RAPMMatrix2023_24FinalCombined, y = xGFP60ForRAPM2023_24, weights = RAPMWeights2023_24,
                           alpha = 0, nfolds = 10, standardize = FALSE, parallel=TRUE )

RAPM2023_24GLM <- glmnet(x = RAPMMatrix2023_24FinalCombined, y = xGFP60ForRAPM2023_24, weights = RAPMWeights2023_24,
                           alpha = 0, lambda = RAPM2023_24CV$lambda.1se, standardize = FALSE, parallel=TRUE )

RAPM2023_24Coef <- coef(RAPM2023_24GLM)

POWRAPM2023_24Coef <- ExtractRAPMCoefs(RAPM2023_24Coef,"POW_Offense_RAPM",
                                       "POW_Defense_RAPM","POW_Total_RAPM")


POUWRAPMWeights2023_24 <- c(PxPwithStintEnds2023_24ExpandedGameInfoWeightsxGFP60$stint_duration,PxPwithStintEnds2023_24ExpandedGameInfoWeightsxGFP60$stint_duration)
POUWxGFP60ForRAPM2023_24 <- c(PxPwithStintEnds2023_24ExpandedGameInfoWeightsxGFP60$HomexGFPer60,PxPwithStintEnds2023_24ExpandedGameInfoWeightsxGFP60$AwayxGFPer60)

POUWRAPM2023_24CV <- cv.glmnet(x = RAPMMatrix2023_24FinalCombined, y = POUWxGFP60ForRAPM2023_24, weights = POUWRAPMWeights2023_24,
                           alpha = 0, nfolds = 10, standardize = FALSE, parallel=TRUE )

POUWRAPM2023_24GLM <- glmnet(x = RAPMMatrix2023_24FinalCombined, y = POUWxGFP60ForRAPM2023_24, weights = POUWRAPMWeights2023_24,
                         alpha = 0, lambda = POUWRAPM2023_24CV$lambda.1se, standardize = FALSE, parallel=TRUE )

POUWRAPM2023_24Coef <- coef(POUWRAPM2023_24GLM)

POUWRAPM2023_24Coef2 <- ExtractRAPMCoefs(POUWRAPM2023_24Coef,"POUW_Offense_RAPM",
                                       "POUW_Defense_RAPM","POUW_Total_RAPM")

BothRAPMs2023_24 <- left_join(POWRAPM2023_24Coef,POUWRAPM2023_24Coef2, by=c("playerId", "teamId")) %>%
  mutate(TotalRankDifference = POUW_Total_RAPM_Rank-POW_Total_RAPM_Rank,
         OffenseRankDifference = POUW_Offense_RAPM_Rank-POW_Offense_RAPM_Rank,
         DefenseRankDifference = POUW_Defense_RAPM_Rank-POW_Defense_RAPM_Rank)

ggplot(data = BothRAPMs2023_24, aes(x=POUW_Total_RAPM_Rank, y=POW_Total_RAPM_Rank)) +
  geom_point() + labs(title = "Total RAPM Rank 2023-24 Playoff-Weighted vs. Playoff Unweighted",
                      subtitle = "R^2=0.998",
                      x="Playoffs Unweighted Total RAPM Rank",
                      y="Playoffs Weighted Total RAPM Rank") + theme_classic()

cor(BothRAPMs2023_24$POW_Defense_RAPM_Rank, BothRAPMs2023_24$POUW_Defense_RAPM_Rank)


PlayDataCombined2023_24 <- merge(NSTData2023_24PO, NSTData2023_24RS, by = "Player", all.x=TRUE) %>%
  mutate(POxGFP60 = (xGF.x/(TOI.x*60))*3600,
         RSxGFP60 = (xGF.y/(TOI.y*60))*3600,
         xGFP60Diff = POxGFP60-RSxGFP60,
         POxGAP60 = (xGA.x/(TOI.x*60))*3600,
         RSxGAP60 = (xGA.y/(TOI.y*60))*3600,
         xGAP60Diff = POxGAP60-RSxGAP60) %>%
  merge(BothRAPMs2023_24, by.x="Player", by.y="playerFullName.x", all.x=TRUE)

cor(PlayDataCombined2023_24$OffenseRankDifference, PlayDataCombined2023_24$xGFP60Diff, use = "complete.obs", method = "spearman")

cor(PlayDataCombined2023_24$DefenseRankDifference, PlayDataCombined2023_24$xGAP60Diff, use = "complete.obs", method = "spearman")

ggplot(data = PlayDataCombined2023_24, aes(x=RankDifference, y=xGFP60Diff)) +
  geom_point() + labs(title = "2023-24 Season: Difference in Total RAPM Ranks vs. Difference in xGF/60",
                      subtitle = "Separman Correlation = 0.265",
                      x="Difference in Total RAPM Ranks (PO-Unweighted vs. PO-Weighted)",
                      y="Difference in xGF/60 (PO xGF/60 - RS xGF/60") + theme_classic()

HomeRAPMMatrix2024_25 <- RAPMMatrixCreationHomeBased(PxPwithStintEnds2024_25ExpandedGameInfoWeightsxGFP60, all_nhl_players)
AwayRAPMMatrix2024_25 <- RAPMMatrixCreationAwayBased(PxPwithStintEnds2024_25ExpandedGameInfoWeightsxGFP60, all_nhl_players)

HomeRAPMMatrix2024_25Final <- OtherHomeRAPMVariables(HomeRAPMMatrix2024_25, PxPwithStintEnds2024_25ExpandedGameInfoWeightsxGFP60)
AwayRAPMMatrix2024_25Final <- OtherAwayRAPMVariables(AwayRAPMMatrix2024_25, PxPwithStintEnds2024_25ExpandedGameInfoWeightsxGFP60)
RAPMMatrix2024_25FinalCombined <- rbind(HomeRAPMMatrix2024_25Final,AwayRAPMMatrix2024_25Final)
RAPMWeights2024_25 <- c(PxPwithStintEnds2024_25ExpandedGameInfoWeightsxGFP60$RAPMWeight,PxPwithStintEnds2024_25ExpandedGameInfoWeightsxGFP60$RAPMWeight)
xGFP60ForRAPM2024_25 <- c(PxPwithStintEnds2024_25ExpandedGameInfoWeightsxGFP60$HomexGFPer60,PxPwithStintEnds2024_25ExpandedGameInfoWeightsxGFP60$AwayxGFPer60)

PlayoffWeightedRAPM2024_25CV <- cv.glmnet(x = RAPMMatrix2024_25FinalCombined, y = xGFP60ForRAPM2024_25, weights = RAPMWeights2024_25,
                           alpha = 0, nfolds = 10, standardize = FALSE, parallel=TRUE )

PlayoffWeightedRAPM2024_25GLM <- glmnet(x = RAPMMatrix2024_25FinalCombined, y = xGFP60ForRAPM2024_25, weights = RAPMWeights2024_25,
                           alpha = 0, lambda = PlayoffWeightedRAPM2024_25CV$lambda.1se, standardize = FALSE, parallel=TRUE )

RAPM2024_25Coef <- coef(PlayoffWeightedRAPM2024_25GLM)

POWRAPM2024_25Coef <- ExtractRAPMCoefs(RAPM2024_25Coef,"POW_Offense_RAPM",
                                         "POW_Defense_RAPM","POW_Total_RAPM")

POUWRAPMWeights2024_25 <- c(PxPwithStintEnds2024_25ExpandedGameInfoWeightsxGFP60$stint_duration,PxPwithStintEnds2024_25ExpandedGameInfoWeightsxGFP60$stint_duration)
POUWxGFP60ForRAPM2024_25 <- c(PxPwithStintEnds2024_25ExpandedGameInfoWeightsxGFP60$HomexGFPer60,PxPwithStintEnds2024_25ExpandedGameInfoWeightsxGFP60$AwayxGFPer60)

POUWRAPM2024_25CV <- cv.glmnet(x = RAPMMatrix2024_25FinalCombined, y = POUWxGFP60ForRAPM2024_25, weights = POUWRAPMWeights2024_25,
                               alpha = 0, nfolds = 10, standardize = FALSE, parallel=TRUE )

POUWRAPM2024_25GLM <- glmnet(x = RAPMMatrix2024_25FinalCombined, y = POUWxGFP60ForRAPM2024_25, weights = POUWRAPMWeights2024_25,
                             alpha = 0, lambda = POUWRAPM2024_25CV$lambda.1se, standardize = FALSE, parallel=TRUE )

POUWRAPM2024_25Coef <- coef(POUWRAPM2024_25GLM)

POUWRAPM2024_25Coef2 <- ExtractRAPMCoefs(POUWRAPM2024_25Coef,"POUW_Offense_RAPM",
                                         "POUW_Defense_RAPM","POUW_Total_RAPM")

BothRAPMs2024_25 <- left_join(POWRAPM2024_25Coef,POUWRAPM2024_25Coef2, by=c("playerId", "teamId")) %>%
  mutate(TotalRankDifference = POUW_Total_RAPM_Rank-POW_Total_RAPM_Rank,
         OffenseRankDifference = POUW_Offense_RAPM_Rank-POW_Offense_RAPM_Rank,
         DefenseRankDifference = POUW_Defense_RAPM_Rank-POW_Defense_RAPM_Rank)


ggplot(data = BothRAPMs2024_25, aes(x=POUW_Total_RAPM_Rank, y=POW_Total_RAPM_Rank)) +
  geom_point() + labs(title = "Total RAPM Rank 2024-25 Playoff-Weighted vs. Playoff Unweighted",
                      subtitle = "R^2=0.993",
                      x="Playoffs Unweighted Total RAPM Rank",
                      y="Playoffs Weighted Total RAPM Rank") + theme_classic()

cor(BothRAPMs2024_25$POW_Defense_RAPM_Rank, BothRAPMs2024_25$POUW_Defense_RAPM_Rank)

PlayDataCombined2024_25 <- merge(NSTData2024_25PO, NSTData2024_25RS, by = "Player", all.x=TRUE) %>%
  mutate(POxGFP60 = (xGF.x/(TOI.x*60))*3600,
         RSxGFP60 = (xGF.y/(TOI.y*60))*3600,
         xGFP60Diff = POxGFP60-RSxGFP60,
         POxGAP60 = (xGA.x/(TOI.x*60))*3600,
         RSxGAP60 = (xGA.y/(TOI.y*60))*3600,
         xGAP60Diff = POxGAP60-RSxGAP60) %>%
  filter(xGFP60Diff < 10) %>%
  merge(BothRAPMs2024_25, by.x="Player", by.y="playerFullName.x", all.x=TRUE)

cor(PlayDataCombined2024_25$xGFP60Diff, PlayDataCombined2024_25$OffenseRankDifference, use = "complete.obs" ,method = "spearman")
cor(PlayDataCombined2024_25$xGAP60Diff, PlayDataCombined2024_25$DefenseRankDifference, use = "complete.obs" ,method = "spearman")

ggplot(data = PlayDataCombined2024_25, aes(x=RankDifference, y=xGFP60Diff)) +
  geom_point() + labs(title = "2024-25 Season: Difference in Total RAPM Ranks vs. Difference in xGF/60",
                      subtitle = "Separman Correlation = 0.136",
                      x="Difference in Total RAPM Ranks (PO-Unweighted vs. PO-Weighted)",
                      y="Difference in xGF/60 (PO xGF/60 - RS xGF/60") + theme_classic()

HomeRAPMMatrix2025_26 <- RAPMMatrixCreationHomeBased(PxPwithStintEnds2025_26ExpandedGameInfoWeightsxGFP60, all_nhl_players)
AwayRAPMMatrix2025_26 <- RAPMMatrixCreationAwayBased(PxPwithStintEnds2025_26ExpandedGameInfoWeightsxGFP60, all_nhl_players)

HomeRAPMMatrix2025_26Final <- OtherHomeRAPMVariables(HomeRAPMMatrix2025_26, PxPwithStintEnds2025_26ExpandedGameInfoWeightsxGFP60)
AwayRAPMMatrix2025_26Final <- OtherAwayRAPMVariables(AwayRAPMMatrix2025_26, PxPwithStintEnds2025_26ExpandedGameInfoWeightsxGFP60)
RAPMMatrix2025_26FinalCombined <- rbind(HomeRAPMMatrix2025_26Final,AwayRAPMMatrix2025_26Final)
RAPMWeights2025_26 <- c(PxPwithStintEnds2025_26ExpandedGameInfoWeightsxGFP60$RAPMWeight,PxPwithStintEnds2025_26ExpandedGameInfoWeightsxGFP60$RAPMWeight)
xGFP60ForRAPM2025_26 <- c(PxPwithStintEnds2025_26ExpandedGameInfoWeightsxGFP60$HomexGFPer60,PxPwithStintEnds2025_26ExpandedGameInfoWeightsxGFP60$AwayxGFPer60)

RAPM2025_26CV <- cv.glmnet(x = RAPMMatrix2025_26FinalCombined, y = xGFP60ForRAPM2025_26, weights = RAPMWeights2025_26,
                           alpha = 0, nfolds = 10, standardize = FALSE, parallel=TRUE )

RAPM2025_26GLM <- glmnet(x = RAPMMatrix2025_26FinalCombined, y = xGFP60ForRAPM2025_26, weights = RAPMWeights2025_26,
                         alpha = 0, lambda = RAPM2025_26CV$lambda.1se, standardize = FALSE, parallel=TRUE )

RAPM2025_26Coef <- coef(RAPM2025_26GLM)

POWRAPM2025_26Coef <- ExtractRAPMCoefs(RAPM2025_26Coef,"POW_Offense_RAPM",
                                       "POW_Defense_RAPM","POW_Total_RAPM")


POUWRAPMWeights2025_26 <- c(PxPwithStintEnds2025_26ExpandedGameInfoWeightsxGFP60$stint_duration,PxPwithStintEnds2025_26ExpandedGameInfoWeightsxGFP60$stint_duration)
POUWxGFP60ForRAPM2025_26 <- c(PxPwithStintEnds2025_26ExpandedGameInfoWeightsxGFP60$HomexGFPer60,PxPwithStintEnds2025_26ExpandedGameInfoWeightsxGFP60$AwayxGFPer60)

POUWRAPM2025_26CV <- cv.glmnet(x = RAPMMatrix2025_26FinalCombined, y = POUWxGFP60ForRAPM2025_26, weights = POUWRAPMWeights2025_26,
                               alpha = 0, nfolds = 10, standardize = FALSE, parallel=TRUE )

POUWRAPM2025_26GLM <- glmnet(x = RAPMMatrix2025_26FinalCombined, y = POUWxGFP60ForRAPM2025_26, weights = POUWRAPMWeights2025_26,
                             alpha = 0, lambda = POUWRAPM2025_26CV$lambda.1se, standardize = FALSE, parallel=TRUE )

POUWRAPM2025_26Coef <- coef(POUWRAPM2025_26GLM)

POUWRAPM2025_26Coef2 <- ExtractRAPMCoefs(POUWRAPM2025_26Coef,"POUW_Offense_RAPM",
                                         "POUW_Defense_RAPM","POUW_Total_RAPM")

BothRAPMs2025_26 <- left_join(POWRAPM2025_26Coef,POUWRAPM2025_26Coef2, by=c("playerId", "teamId")) %>%
  mutate(TotalRankDifference = POUW_Total_RAPM_Rank-POW_Total_RAPM_Rank,
         OffenseRankDifference = POUW_Offense_RAPM_Rank-POW_Offense_RAPM_Rank,
         DefenseRankDifference = POUW_Defense_RAPM_Rank-POW_Defense_RAPM_Rank)

RAPMZScoreCreator <- function(BothRAPMsDF){
  ForwardsOnly <- BothRAPMsDF %>%
    filter(positionGroup.x == "F")
  
  DefenseOnly <- BothRAPMsDF %>%
    filter(positionGroup.x == "D")
  
  ForwardsWithZScores <- ForwardsOnly %>%
    mutate(
      POWxGFZScore = as.vector(scale(POW_Offense_RAPM)),
      POWxGAZScore = as.vector(scale(POW_Defense_RAPM)),
      POUWxGFZScore = as.vector(scale(POUW_Offense_RAPM)),
      POUWxGAZScore = as.vector(scale(POUW_Defense_RAPM))
    )
  
  DefenseWithZScores <- DefenseOnly %>%
    mutate(
      POWxGFZScore = as.vector(scale(POW_Offense_RAPM)),
      POWxGAZScore = as.vector(scale(POW_Defense_RAPM)),
      POUWxGFZScore = as.vector(scale(POUW_Offense_RAPM)),
      POUWxGAZScore = as.vector(scale(POUW_Defense_RAPM))
    )
  
  AllPlayersWithZScores <- rbind(ForwardsWithZScores,DefenseWithZScores) %>%
    mutate(
      xGF_Z_Score_diff = POWxGFZScore-POUWxGFZScore,
      xGA_Z_Score_diff = POWxGAZScore-POUWxGAZScore
    )
  
  return(AllPlayersWithZScores)
}

RAPMsWithZScores2023_24 <- RAPMZScoreCreator(BothRAPMs2023_24)
RAPMsWithZScores2024_25 <- RAPMZScoreCreator(BothRAPMs2024_25)
RAPMsWithZScores2025_26 <- RAPMZScoreCreator(BothRAPMs2025_26)

RAPMBarChartCreator <- function(RAPMsWithZScores, playerName, teamTriCode, season){
  
  PlayerOfInterestZScores <- RAPMsWithZScores %>%
    filter(playerFullName.x==playerName, teamTriCode.x==teamTriCode) %>%
    select(POWxGFZScore, POUWxGFZScore, POWxGAZScore, POUWxGAZScore) %>%
    mutate(POWxGAZScore = -POWxGAZScore, #flipping the sign of the z-scores to display in the same manner as good offense
           POUWxGAZScore = -POUWxGAZScore) %>% 
    rename(`Playoff Weighted xGF` = POWxGFZScore,
           `No Playoff Weighting xGF` = POUWxGFZScore,
           `Playoff Weighted xGA` = POWxGAZScore,
           `No Playoff Weighting xGA` = POUWxGAZScore)
  
  if(nrow(PlayerOfInterestZScores) == 0){
    stop(paste("No data found for", playerName, "on", teamTriCode, 
               "- check spelling and team code"))
  }
  
  PlayerOfInterestZScoresLong <- PlayerOfInterestZScores %>%
    pivot_longer(cols = everything(), names_to = "Model", values_to = "Z_Score") %>%
    mutate(
      Model = factor(Model, levels = c(
        "Playoff Weighted xGF",
        "No Playoff Weighting xGF",
        "Playoff Weighted xGA",
        "No Playoff Weighting xGA"
      ))
    )
  
  POIZScorePlot <- ggplot(data = PlayerOfInterestZScoresLong, aes(x=Model, y = Z_Score, fill = Z_Score > 0)) +
    geom_bar(stat = "identity") + scale_fill_manual( values = c("TRUE" = "steelblue", "FALSE" = "firebrick"), guide  = "none") +
    labs(title = paste(playerName, "-", teamTriCode, "Even Strength RAPM Z-Scores", season),
                                       subtitle = "Additional Playoff Weighting vs. No Additional Playoff Weighting",
                                       x = NULL, y="Z-Score", caption = "@EDLAnalytics") + theme_minimal()
  
  return(POIZScorePlot)
}

RAPMBarChartCreator(RAPMsWithZScores2024_25, "Martin Necas", "COL", "2024-25")

