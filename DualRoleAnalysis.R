#Dual Role Players Analysis

library(hockeyR)
library(tidyverse)

DOrForward <- function(PBPData, team_abbrev = "SJS") {
  # Define columns
  cols_pos_home <- paste0("home_on_", 1:7, "_position")
  cols_pos_away <- paste0("away_on_", 1:7, "_position")
  
  # Determine if SJS is the home team for each row
  is_home <- PBPData$home_abbreviation == team_abbrev
  
  # Preallocate result vector
  FlexPosition <- character(nrow(PBPData))
  
  # Loop through rows
  for (i in seq_len(nrow(PBPData))) {
    pos_cols <- if (is_home[i]) cols_pos_home else cols_pos_away
    positions <- unlist(PBPData[i, pos_cols])
    
    D_count <- sum(positions == "D", na.rm = TRUE)
    
    FlexPosition[i] <- ifelse(D_count > 2, "F", "D")
  }
  
  PBPData$FlexPosition <- FlexPosition
  return(PBPData)
}

#Brent Burns 2013-14 Forward Data
View(head(load_pbp('2013-14')))

Sharks2013_14PBP <- load_pbp('2013-14') %>%
  dplyr::filter(home_abbreviation == "SJS" | away_abbreviation == "SJS")

BrentBurns2013_14PBP <- Sharks2013_14PBP %>%
  mutate(has_player = if_any(c(home_on_1, home_on_2, home_on_3, home_on_4, home_on_5, home_on_6, home_on_7,
                               away_on_1, away_on_2, away_on_3, away_on_4, away_on_5, away_on_6, away_on_7),
                             ~. == "Brent.Burns")) %>%
  filter(strength_state =="5v5" & has_player == TRUE & is.na(xg) == FALSE) 

View(BrentBurns2013_14PBP)

View(allPlayersLookup)

allPlayersLookup$CompressName <- gsub(" ", ".", allPlayersLookup$name, fixed = TRUE)
allPlayersLookup$CompressName <- gsub("-", ".", allPlayersLookup$CompressName, fixed = TRUE)
allPlayersLookup$CompressName <- gsub("'", ".", allPlayersLookup$CompressName, fixed = TRUE)

BrentBurns2013_14PBP$home_on_1_position <- allPlayersLookup$position[match(BrentBurns2013_14PBP$home_on_1, allPlayersLookup$CompressName)]
BrentBurns2013_14PBP$home_on_2_position <- allPlayersLookup$position[match(BrentBurns2013_14PBP$home_on_2, allPlayersLookup$CompressName)]
BrentBurns2013_14PBP$home_on_3_position <- allPlayersLookup$position[match(BrentBurns2013_14PBP$home_on_3, allPlayersLookup$CompressName)]
BrentBurns2013_14PBP$home_on_4_position <- allPlayersLookup$position[match(BrentBurns2013_14PBP$home_on_4, allPlayersLookup$CompressName)]
BrentBurns2013_14PBP$home_on_5_position <- allPlayersLookup$position[match(BrentBurns2013_14PBP$home_on_5, allPlayersLookup$CompressName)]
BrentBurns2013_14PBP$home_on_6_position <- allPlayersLookup$position[match(BrentBurns2013_14PBP$home_on_6, allPlayersLookup$CompressName)]
BrentBurns2013_14PBP$home_on_7_position <- allPlayersLookup$position[match(BrentBurns2013_14PBP$home_on_7, allPlayersLookup$CompressName)]

BrentBurns2013_14PBP$away_on_1_position <- allPlayersLookup$position[match(BrentBurns2013_14PBP$away_on_1, allPlayersLookup$CompressName)]
BrentBurns2013_14PBP$away_on_2_position <- allPlayersLookup$position[match(BrentBurns2013_14PBP$away_on_2, allPlayersLookup$CompressName)]
BrentBurns2013_14PBP$away_on_3_position <- allPlayersLookup$position[match(BrentBurns2013_14PBP$away_on_3, allPlayersLookup$CompressName)]
BrentBurns2013_14PBP$away_on_4_position <- allPlayersLookup$position[match(BrentBurns2013_14PBP$away_on_4, allPlayersLookup$CompressName)]
BrentBurns2013_14PBP$away_on_5_position <- allPlayersLookup$position[match(BrentBurns2013_14PBP$away_on_5, allPlayersLookup$CompressName)]
BrentBurns2013_14PBP$away_on_6_position <- allPlayersLookup$position[match(BrentBurns2013_14PBP$away_on_6, allPlayersLookup$CompressName)]
BrentBurns2013_14PBP$away_on_7_position <- allPlayersLookup$position[match(BrentBurns2013_14PBP$away_on_7, allPlayersLookup$CompressName)]

BrentBurns2013_14PBPWithPos <- DOrForward(BrentBurns2013_14PBP)

BrentBurnsLongData <- BrentBurns2013_14PBPWithPos %>%
  select(starts_with("home_on_"), starts_with("away_on_"), xg, event_team_abbr, FlexPosition, home_abbreviation, away_abbreviation) %>%
  pivot_longer(cols = starts_with("home_on"), names_to = "slot", values_to = "player") %>%
  mutate(shooting_team = event_team_abbr,
         team = home_abbreviation) %>%
  bind_rows(
    BrentBurns2013_14PBPWithPos %>%
      select(starts_with("home_on_"), starts_with("away_on_"), xg, event_team_abbr, FlexPosition, home_abbreviation, away_abbreviation) %>%
      pivot_longer(cols = starts_with("away_on_"), names_to = "slot", values_to = "player") %>%
      mutate(shooting_team = event_team_abbr,
             team = away_abbreviation)
  ) %>%
  filter(team == "SJS") %>%
  dplyr::select(xg, FlexPosition, slot, player, shooting_team, team) %>%
  filter(!grepl("position$|6$|7$", slot))
  
BrentBurnsTeammateSummaries <- BrentBurnsLongData %>%
  mutate(
    CF = if_else(team == shooting_team, 1, 0),  # Shot attempt for
    CA = if_else(team != shooting_team, 1, 0)   # Shot attempt against
  ) %>%
  group_by(player, FlexPosition) %>%
  summarise(
    total_CF = sum(CF, na.rm = TRUE),
    total_CA = sum(CA, na.rm = TRUE),
    events = n(),
    .groups = "drop"
  )


#Brent Burns Defenseman Data 2014-15
#Brent Burns 2013-14 Forward Data
Sharks2014_15PBP <- load_pbp('2014-15') %>%
  dplyr::filter(home_abbreviation == "SJS" | away_abbreviation == "SJS")

BrentBurns2014_15PBP <- Sharks2014_15PBP %>%
  mutate(has_player = if_any(c(home_on_1, home_on_2, home_on_3, home_on_4, home_on_5, home_on_6, home_on_7,
                               away_on_1, away_on_2, away_on_3, away_on_4, away_on_5, away_on_6, away_on_7),
                             ~. == "Brent.Burns")) %>%
  filter(strength_state =="5v5" & has_player == TRUE & is.na(xg) == FALSE) 

View(BrentBurns2014_15PBP)

BrentBurns2014_15PBP$home_on_1_position <- allPlayersLookup$position[match(BrentBurns2014_15PBP$home_on_1, allPlayersLookup$CompressName)]
BrentBurns2014_15PBP$home_on_2_position <- allPlayersLookup$position[match(BrentBurns2014_15PBP$home_on_2, allPlayersLookup$CompressName)]
BrentBurns2014_15PBP$home_on_3_position <- allPlayersLookup$position[match(BrentBurns2014_15PBP$home_on_3, allPlayersLookup$CompressName)]
BrentBurns2014_15PBP$home_on_4_position <- allPlayersLookup$position[match(BrentBurns2014_15PBP$home_on_4, allPlayersLookup$CompressName)]
BrentBurns2014_15PBP$home_on_5_position <- allPlayersLookup$position[match(BrentBurns2014_15PBP$home_on_5, allPlayersLookup$CompressName)]
BrentBurns2014_15PBP$home_on_6_position <- allPlayersLookup$position[match(BrentBurns2014_15PBP$home_on_6, allPlayersLookup$CompressName)]
BrentBurns2014_15PBP$home_on_7_position <- allPlayersLookup$position[match(BrentBurns2014_15PBP$home_on_7, allPlayersLookup$CompressName)]

BrentBurns2014_15PBP$away_on_1_position <- allPlayersLookup$position[match(BrentBurns2014_15PBP$away_on_1, allPlayersLookup$CompressName)]
BrentBurns2014_15PBP$away_on_2_position <- allPlayersLookup$position[match(BrentBurns2014_15PBP$away_on_2, allPlayersLookup$CompressName)]
BrentBurns2014_15PBP$away_on_3_position <- allPlayersLookup$position[match(BrentBurns2014_15PBP$away_on_3, allPlayersLookup$CompressName)]
BrentBurns2014_15PBP$away_on_4_position <- allPlayersLookup$position[match(BrentBurns2014_15PBP$away_on_4, allPlayersLookup$CompressName)]
BrentBurns2014_15PBP$away_on_5_position <- allPlayersLookup$position[match(BrentBurns2014_15PBP$away_on_5, allPlayersLookup$CompressName)]
BrentBurns2014_15PBP$away_on_6_position <- allPlayersLookup$position[match(BrentBurns2014_15PBP$away_on_6, allPlayersLookup$CompressName)]
BrentBurns2014_15PBP$away_on_7_position <- allPlayersLookup$position[match(BrentBurns2014_15PBP$away_on_7, allPlayersLookup$CompressName)]

BrentBurns2014_15PBPWithPos <- DOrForward(BrentBurns2014_15PBP)

BrentBurnsLongData2014_15 <- BrentBurns2014_15PBPWithPos %>%
  select(starts_with("home_on_"), starts_with("away_on_"), xg, event_team_abbr, FlexPosition, home_abbreviation, away_abbreviation) %>%
  pivot_longer(cols = starts_with("home_on"), names_to = "slot", values_to = "player") %>%
  mutate(shooting_team = event_team_abbr,
         team = home_abbreviation) %>%
  bind_rows(
    BrentBurns2014_15PBPWithPos %>%
      select(starts_with("home_on_"), starts_with("away_on_"), xg, event_team_abbr, FlexPosition, home_abbreviation, away_abbreviation) %>%
      pivot_longer(cols = starts_with("away_on_"), names_to = "slot", values_to = "player") %>%
      mutate(shooting_team = event_team_abbr,
             team = away_abbreviation)
  ) %>%
  filter(team == "SJS") %>%
  dplyr::select(xg, FlexPosition, slot, player, shooting_team, team) %>%
  filter(!grepl("position$|6$|7$", slot))

BrentBurnsTeammateSummaries2014_15 <- BrentBurnsLongData2014_15 %>%
  mutate(
    CF = if_else(team == shooting_team, 1, 0),  # Shot attempt for
    CA = if_else(team != shooting_team, 1, 0)   # Shot attempt against
  ) %>%
  group_by(player, FlexPosition) %>%
  summarise(
    total_CF = sum(CF, na.rm = TRUE),
    total_CA = sum(CA, na.rm = TRUE),
    events = n(),
    .groups = "drop"
  )

BrentBurnsFullTeammateSummaries <- full_join(BrentBurnsTeammateSummaries,BrentBurnsTeammateSummaries2014_15,by = c("player","FlexPosition")) %>%
  mutate(
    total_CF = coalesce(total_CF.x, 0) + coalesce(total_CF.y, 0),
    total_CA = coalesce(total_CA.x, 0) + coalesce(total_CA.y, 0),
    total_events = coalesce(events.x,0) + coalesce(events.y,0)
  ) %>%
  select(player, FlexPosition, total_CF, total_CA, everything()) %>%
  select(-ends_with(".x"), -ends_with(".y"))
  

#Dustin Byfuglien 2013-14 Season Played Both
Jets2013_14PBP <- load_pbp('2013-14') %>%
  dplyr::filter(home_abbreviation == "WPG" | away_abbreviation == "WPG")

DustinByfuglien2013_14PBP <- Jets2013_14PBP %>%
  mutate(has_player = if_any(c(home_on_1, home_on_2, home_on_3, home_on_4, home_on_5, home_on_6, home_on_7,
                               away_on_1, away_on_2, away_on_3, away_on_4, away_on_5, away_on_6, away_on_7),
                             ~. == "Dustin.Byfuglien")) %>%
  filter(strength_state =="5v5" & has_player == TRUE & is.na(xg) == FALSE) 

View(DustinByfuglien2013_14PBP)

DustinByfuglien2013_14PBP$home_on_1_position <- allPlayersLookup$position[match(DustinByfuglien2013_14PBP$home_on_1, allPlayersLookup$CompressName)]
DustinByfuglien2013_14PBP$home_on_2_position <- allPlayersLookup$position[match(DustinByfuglien2013_14PBP$home_on_2, allPlayersLookup$CompressName)]
DustinByfuglien2013_14PBP$home_on_3_position <- allPlayersLookup$position[match(DustinByfuglien2013_14PBP$home_on_3, allPlayersLookup$CompressName)]
DustinByfuglien2013_14PBP$home_on_4_position <- allPlayersLookup$position[match(DustinByfuglien2013_14PBP$home_on_4, allPlayersLookup$CompressName)]
DustinByfuglien2013_14PBP$home_on_5_position <- allPlayersLookup$position[match(DustinByfuglien2013_14PBP$home_on_5, allPlayersLookup$CompressName)]
DustinByfuglien2013_14PBP$home_on_6_position <- allPlayersLookup$position[match(DustinByfuglien2013_14PBP$home_on_6, allPlayersLookup$CompressName)]
DustinByfuglien2013_14PBP$home_on_7_position <- allPlayersLookup$position[match(DustinByfuglien2013_14PBP$home_on_7, allPlayersLookup$CompressName)]

DustinByfuglien2013_14PBP$away_on_1_position <- allPlayersLookup$position[match(DustinByfuglien2013_14PBP$away_on_1, allPlayersLookup$CompressName)]
DustinByfuglien2013_14PBP$away_on_2_position <- allPlayersLookup$position[match(DustinByfuglien2013_14PBP$away_on_2, allPlayersLookup$CompressName)]
DustinByfuglien2013_14PBP$away_on_3_position <- allPlayersLookup$position[match(DustinByfuglien2013_14PBP$away_on_3, allPlayersLookup$CompressName)]
DustinByfuglien2013_14PBP$away_on_4_position <- allPlayersLookup$position[match(DustinByfuglien2013_14PBP$away_on_4, allPlayersLookup$CompressName)]
DustinByfuglien2013_14PBP$away_on_5_position <- allPlayersLookup$position[match(DustinByfuglien2013_14PBP$away_on_5, allPlayersLookup$CompressName)]
DustinByfuglien2013_14PBP$away_on_6_position <- allPlayersLookup$position[match(DustinByfuglien2013_14PBP$away_on_6, allPlayersLookup$CompressName)]
DustinByfuglien2013_14PBP$away_on_7_position <- allPlayersLookup$position[match(DustinByfuglien2013_14PBP$away_on_7, allPlayersLookup$CompressName)]

DustinByfuglien2013_14PBPWithPos <- DOrForward(DustinByfuglien2013_14PBP, "WPG")

DustinByfuglienLongData <- DustinByfuglien2013_14PBPWithPos %>%
  select(starts_with("home_on_"), starts_with("away_on_"), xg, event_team_abbr, FlexPosition, home_abbreviation, away_abbreviation) %>%
  pivot_longer(cols = starts_with("home_on"), names_to = "slot", values_to = "player") %>%
  mutate(shooting_team = event_team_abbr,
         team = home_abbreviation) %>%
  bind_rows(
    DustinByfuglien2013_14PBPWithPos %>%
      select(starts_with("home_on_"), starts_with("away_on_"), xg, event_team_abbr, FlexPosition, home_abbreviation, away_abbreviation) %>%
      pivot_longer(cols = starts_with("away_on_"), names_to = "slot", values_to = "player") %>%
      mutate(shooting_team = event_team_abbr,
             team = away_abbreviation)
  ) %>%
  filter(team == "WPG") %>%
  dplyr::select(xg, FlexPosition, slot, player, shooting_team, team) %>%
  filter(!grepl("position$|6$|7$", slot))

DustinByfuglienTeammateSummaries <- DustinByfuglienLongData %>%
  mutate(
    CF = if_else(team == shooting_team, 1, 0),  # Shot attempt for
    CA = if_else(team != shooting_team, 1, 0)   # Shot attempt against
  ) %>%
  group_by(player, FlexPosition) %>%
  summarise(
    total_CF = sum(CF, na.rm = TRUE),
    total_CA = sum(CA, na.rm = TRUE),
    events = n(),
    .groups = "drop"
  )


#Brendan Smith during the 2019-20 season
Rangers2019_20PBP <- load_pbp('2019-20') %>%
  dplyr::filter(home_abbreviation == "NYR" | away_abbreviation == "NYR")

BrendanSmith2019_20PBP <- Rangers2019_20PBP %>%
  mutate(has_player = if_any(c(home_on_1, home_on_2, home_on_3, home_on_4, home_on_5, home_on_6, home_on_7,
                               away_on_1, away_on_2, away_on_3, away_on_4, away_on_5, away_on_6, away_on_7),
                             ~. == "Brendan.Smith")) %>%
  filter(strength_state =="5v5" & has_player == TRUE & is.na(xg) == FALSE) 

View(BrendanSmith2019_20PBP)

BrendanSmith2019_20PBP$home_on_1_position <- allPlayersLookup$position[match(BrendanSmith2019_20PBP$home_on_1, allPlayersLookup$CompressName)]
BrendanSmith2019_20PBP$home_on_2_position <- allPlayersLookup$position[match(BrendanSmith2019_20PBP$home_on_2, allPlayersLookup$CompressName)]
BrendanSmith2019_20PBP$home_on_3_position <- allPlayersLookup$position[match(BrendanSmith2019_20PBP$home_on_3, allPlayersLookup$CompressName)]
BrendanSmith2019_20PBP$home_on_4_position <- allPlayersLookup$position[match(BrendanSmith2019_20PBP$home_on_4, allPlayersLookup$CompressName)]
BrendanSmith2019_20PBP$home_on_5_position <- allPlayersLookup$position[match(BrendanSmith2019_20PBP$home_on_5, allPlayersLookup$CompressName)]
BrendanSmith2019_20PBP$home_on_6_position <- allPlayersLookup$position[match(BrendanSmith2019_20PBP$home_on_6, allPlayersLookup$CompressName)]
BrendanSmith2019_20PBP$home_on_7_position <- allPlayersLookup$position[match(BrendanSmith2019_20PBP$home_on_7, allPlayersLookup$CompressName)]

BrendanSmith2019_20PBP$away_on_1_position <- allPlayersLookup$position[match(BrendanSmith2019_20PBP$away_on_1, allPlayersLookup$CompressName)]
BrendanSmith2019_20PBP$away_on_2_position <- allPlayersLookup$position[match(BrendanSmith2019_20PBP$away_on_2, allPlayersLookup$CompressName)]
BrendanSmith2019_20PBP$away_on_3_position <- allPlayersLookup$position[match(BrendanSmith2019_20PBP$away_on_3, allPlayersLookup$CompressName)]
BrendanSmith2019_20PBP$away_on_4_position <- allPlayersLookup$position[match(BrendanSmith2019_20PBP$away_on_4, allPlayersLookup$CompressName)]
BrendanSmith2019_20PBP$away_on_5_position <- allPlayersLookup$position[match(BrendanSmith2019_20PBP$away_on_5, allPlayersLookup$CompressName)]
BrendanSmith2019_20PBP$away_on_6_position <- allPlayersLookup$position[match(BrendanSmith2019_20PBP$away_on_6, allPlayersLookup$CompressName)]
BrendanSmith2019_20PBP$away_on_7_position <- allPlayersLookup$position[match(BrendanSmith2019_20PBP$away_on_7, allPlayersLookup$CompressName)]

BrendanSmith2019_20PBPWithPos <- DOrForward(BrendanSmith2019_20PBP, "NYR")

BrendanSmithLongData <- BrendanSmith2019_20PBPWithPos %>%
  select(starts_with("home_on_"), starts_with("away_on_"), xg, event_team_abbr, FlexPosition, home_abbreviation, away_abbreviation) %>%
  pivot_longer(cols = starts_with("home_on"), names_to = "slot", values_to = "player") %>%
  mutate(shooting_team = event_team_abbr,
         team = home_abbreviation) %>%
  bind_rows(
    BrendanSmith2019_20PBPWithPos %>%
      select(starts_with("home_on_"), starts_with("away_on_"), xg, event_team_abbr, FlexPosition, home_abbreviation, away_abbreviation) %>%
      pivot_longer(cols = starts_with("away_on_"), names_to = "slot", values_to = "player") %>%
      mutate(shooting_team = event_team_abbr,
             team = away_abbreviation)
  ) %>%
  filter(team == "NYR") %>%
  dplyr::select(xg, FlexPosition, slot, player, shooting_team, team) %>%
  filter(!grepl("position$|6$|7$", slot))

BrendanSmithTeammateSummaries <- BrendanSmithLongData %>%
  mutate(
    CF = if_else(team == shooting_team, 1, 0),  # Shot attempt for
    CA = if_else(team != shooting_team, 1, 0)   # Shot attempt against
  ) %>%
  group_by(player, FlexPosition) %>%
  summarise(
    total_CF = sum(CF, na.rm = TRUE),
    total_CA = sum(CA, na.rm = TRUE),
    events = n(),
    .groups = "drop"
  )

#Brendan Smith during the 2023-24 season
#fixing function to adjust for 2023-24 naming conventions
DOrForward2023_24 <- function(PBPData, team_abbrev = "SJS") {
  # Define columns
  cols_pos_home <- paste0("home_on_", 1:7, "_position")
  cols_pos_away <- paste0("away_on_", 1:7, "_position")
  
  # Determine if SJS is the home team for each row
  is_home <- PBPData$home_abbr == team_abbrev
  
  # Preallocate result vector
  FlexPosition <- character(nrow(PBPData))
  
  # Loop through rows
  for (i in seq_len(nrow(PBPData))) {
    pos_cols <- if (is_home[i]) cols_pos_home else cols_pos_away
    positions <- unlist(PBPData[i, pos_cols])
    
    D_count <- sum(positions == "D", na.rm = TRUE)
    
    FlexPosition[i] <- ifelse(D_count > 2, "F", "D")
  }
  
  PBPData$FlexPosition <- FlexPosition
  return(PBPData)
}

Devils2023_24PBP <- load_pbp('2023-24') %>%
  dplyr::filter(home_abbr == "NJD" | away_abbr == "NJD")

BrendanSmith2023_24PBP <- Devils2023_24PBP %>%
  mutate(has_player = if_any(c(home_on_1, home_on_2, home_on_3, home_on_4, home_on_5, home_on_6, home_on_7,
                               away_on_1, away_on_2, away_on_3, away_on_4, away_on_5, away_on_6, away_on_7),
                             ~. == "Brendan Smith")) %>%
  filter(strength_state =="5v5" & has_player == TRUE) 

View(BrendanSmith2023_24PBP)

BrendanSmith2023_24PBP$home_on_1_position <- allPlayersLookup$position[match(BrendanSmith2023_24PBP$home_on_1, allPlayersLookup$name)]
BrendanSmith2023_24PBP$home_on_2_position <- allPlayersLookup$position[match(BrendanSmith2023_24PBP$home_on_2, allPlayersLookup$name)]
BrendanSmith2023_24PBP$home_on_3_position <- allPlayersLookup$position[match(BrendanSmith2023_24PBP$home_on_3, allPlayersLookup$name)]
BrendanSmith2023_24PBP$home_on_4_position <- allPlayersLookup$position[match(BrendanSmith2023_24PBP$home_on_4, allPlayersLookup$name)]
BrendanSmith2023_24PBP$home_on_5_position <- allPlayersLookup$position[match(BrendanSmith2023_24PBP$home_on_5, allPlayersLookup$name)]
BrendanSmith2023_24PBP$home_on_6_position <- allPlayersLookup$position[match(BrendanSmith2023_24PBP$home_on_6, allPlayersLookup$name)]
BrendanSmith2023_24PBP$home_on_7_position <- allPlayersLookup$position[match(BrendanSmith2023_24PBP$home_on_7, allPlayersLookup$name)]

BrendanSmith2023_24PBP$away_on_1_position <- allPlayersLookup$position[match(BrendanSmith2023_24PBP$away_on_1, allPlayersLookup$name)]
BrendanSmith2023_24PBP$away_on_2_position <- allPlayersLookup$position[match(BrendanSmith2023_24PBP$away_on_2, allPlayersLookup$name)]
BrendanSmith2023_24PBP$away_on_3_position <- allPlayersLookup$position[match(BrendanSmith2023_24PBP$away_on_3, allPlayersLookup$name)]
BrendanSmith2023_24PBP$away_on_4_position <- allPlayersLookup$position[match(BrendanSmith2023_24PBP$away_on_4, allPlayersLookup$name)]
BrendanSmith2023_24PBP$away_on_5_position <- allPlayersLookup$position[match(BrendanSmith2023_24PBP$away_on_5, allPlayersLookup$name)]
BrendanSmith2023_24PBP$away_on_6_position <- allPlayersLookup$position[match(BrendanSmith2023_24PBP$away_on_6, allPlayersLookup$name)]
BrendanSmith2023_24PBP$away_on_7_position <- allPlayersLookup$position[match(BrendanSmith2023_24PBP$away_on_7, allPlayersLookup$name)]

BrendanSmith2023_24PBPWithPos <- DOrForward2023_24(BrendanSmith2023_24PBP, "NJD")

BrendanSmithLongData2023_24 <- BrendanSmith2023_24PBPWithPos %>%
  pivot_longer(
    cols = matches("^home_on_[1-7]$"),  # Only include player name columns, not IDs
    names_to = "slot",
    values_to = "player"
  ) %>%
  mutate(
    shooting_team = event_team_abbr,
    team = home_abbr,
    FlexPosition = FlexPosition
  ) %>%
  bind_rows(
    BrendanSmith2023_24PBPWithPos %>%
      pivot_longer(
        cols = matches("^away_on_[1-7]$"),
        names_to = "slot",
        values_to = "player"
      ) %>%
      mutate(
        shooting_team = event_team_abbr,
        team = away_abbr,
        FlexPosition = FlexPosition
      )
  ) %>%
  filter(team == "NJD") %>%
  select(FlexPosition, slot, player, shooting_team, team) %>%
  filter(!grepl("position$|6$|7$", slot))

BrendanSmithTeammateSummaries2023_24 <- BrendanSmithLongData2023_24 %>%
  mutate(
    CF = if_else(team == shooting_team, 1, 0),  # Shot attempt for
    CA = if_else(team != shooting_team, 1, 0)   # Shot attempt against
  ) %>%
  group_by(player, FlexPosition) %>%
  summarise(
    total_CF = sum(CF, na.rm = TRUE),
    total_CA = sum(CA, na.rm = TRUE),
    events = n(),
    .groups = "drop"
  )


#Mark Pysyk during the 2019-20 season
Panthers2019_20PBP <- load_pbp('2019-20') %>%
  dplyr::filter(home_abbreviation == "FLA" | away_abbreviation == "FLA")

MarkPysyk2019_20PBP <- Panthers2019_20PBP %>%
  mutate(has_player = if_any(c(home_on_1, home_on_2, home_on_3, home_on_4, home_on_5, home_on_6, home_on_7,
                               away_on_1, away_on_2, away_on_3, away_on_4, away_on_5, away_on_6, away_on_7),
                             ~. == "Mark.Pysyk")) %>%
  filter(strength_state =="5v5" & has_player == TRUE & is.na(xg) == FALSE) 

View(MarkPysyk2019_20PBP)

MarkPysyk2019_20PBP$home_on_1_position <- allPlayersLookup$position[match(MarkPysyk2019_20PBP$home_on_1, allPlayersLookup$CompressName)]
MarkPysyk2019_20PBP$home_on_2_position <- allPlayersLookup$position[match(MarkPysyk2019_20PBP$home_on_2, allPlayersLookup$CompressName)]
MarkPysyk2019_20PBP$home_on_3_position <- allPlayersLookup$position[match(MarkPysyk2019_20PBP$home_on_3, allPlayersLookup$CompressName)]
MarkPysyk2019_20PBP$home_on_4_position <- allPlayersLookup$position[match(MarkPysyk2019_20PBP$home_on_4, allPlayersLookup$CompressName)]
MarkPysyk2019_20PBP$home_on_5_position <- allPlayersLookup$position[match(MarkPysyk2019_20PBP$home_on_5, allPlayersLookup$CompressName)]
MarkPysyk2019_20PBP$home_on_6_position <- allPlayersLookup$position[match(MarkPysyk2019_20PBP$home_on_6, allPlayersLookup$CompressName)]
MarkPysyk2019_20PBP$home_on_7_position <- allPlayersLookup$position[match(MarkPysyk2019_20PBP$home_on_7, allPlayersLookup$CompressName)]

MarkPysyk2019_20PBP$away_on_1_position <- allPlayersLookup$position[match(MarkPysyk2019_20PBP$away_on_1, allPlayersLookup$CompressName)]
MarkPysyk2019_20PBP$away_on_2_position <- allPlayersLookup$position[match(MarkPysyk2019_20PBP$away_on_2, allPlayersLookup$CompressName)]
MarkPysyk2019_20PBP$away_on_3_position <- allPlayersLookup$position[match(MarkPysyk2019_20PBP$away_on_3, allPlayersLookup$CompressName)]
MarkPysyk2019_20PBP$away_on_4_position <- allPlayersLookup$position[match(MarkPysyk2019_20PBP$away_on_4, allPlayersLookup$CompressName)]
MarkPysyk2019_20PBP$away_on_5_position <- allPlayersLookup$position[match(MarkPysyk2019_20PBP$away_on_5, allPlayersLookup$CompressName)]
MarkPysyk2019_20PBP$away_on_6_position <- allPlayersLookup$position[match(MarkPysyk2019_20PBP$away_on_6, allPlayersLookup$CompressName)]
MarkPysyk2019_20PBP$away_on_7_position <- allPlayersLookup$position[match(MarkPysyk2019_20PBP$away_on_7, allPlayersLookup$CompressName)]

MarkPysyk2019_20PBPWithPos <- DOrForward(MarkPysyk2019_20PBP, "FLA")

MarkPysykLongData <- MarkPysyk2019_20PBPWithPos %>%
  select(starts_with("home_on_"), starts_with("away_on_"), xg, event_team_abbr, FlexPosition, home_abbreviation, away_abbreviation) %>%
  pivot_longer(cols = starts_with("home_on"), names_to = "slot", values_to = "player") %>%
  mutate(shooting_team = event_team_abbr,
         team = home_abbreviation) %>%
  bind_rows(
    MarkPysyk2019_20PBPWithPos %>%
      select(starts_with("home_on_"), starts_with("away_on_"), xg, event_team_abbr, FlexPosition, home_abbreviation, away_abbreviation) %>%
      pivot_longer(cols = starts_with("away_on_"), names_to = "slot", values_to = "player") %>%
      mutate(shooting_team = event_team_abbr,
             team = away_abbreviation)
  ) %>%
  filter(team == "FLA") %>%
  dplyr::select(xg, FlexPosition, slot, player, shooting_team, team) %>%
  filter(!grepl("position$|6$|7$", slot))

MarkPysykTeammateSummaries <- MarkPysykLongData %>%
  mutate(
    CF = if_else(team == shooting_team, 1, 0),  # Shot attempt for
    CA = if_else(team != shooting_team, 1, 0)   # Shot attempt against
  ) %>%
  group_by(player, FlexPosition) %>%
  summarise(
    total_CF = sum(CF, na.rm = TRUE),
    total_CA = sum(CA, na.rm = TRUE),
    events = n(),
    .groups = "drop"
  )


#Calculate CF% for each player
BrentBurnsFullTeammateSummaries <- BrentBurnsFullTeammateSummaries %>%
  mutate(CFPercent = total_CF/(total_CF+total_CA))
DustinByfuglienTeammateSummaries <- DustinByfuglienTeammateSummaries %>%
  mutate(CFPercent = total_CF/(total_CF+total_CA))
BrendanSmithTeammateSummaries <- BrendanSmithTeammateSummaries %>%
  mutate(CFPercent = total_CF/(total_CF+total_CA))
BrendanSmithTeammateSummaries2023_24 <- BrendanSmithTeammateSummaries2023_24 %>%
  mutate(CFPercent = total_CF/(total_CF+total_CA))
MarkPysykTeammateSummaries <- MarkPysykTeammateSummaries %>%
  mutate(CFPercent = total_CF/(total_CF+total_CA))

#imported teammate data for each player
#add a FlexPosition column as without to bring over to other dataframes
BrentBurnsTeammates2013_15$FlexPosition <- "Without"
DustinByfuglienTeammates2013_14$FlexPosition <- "Without"
BrendanSmithTeammates2019_20$FlexPosition <- "Without"
BrendanSmithTeammates2023_24$FlexPosition <- "Without"
MarkPysykTeammates2019_20$FlexPosition <- "Without"

BrentBurnsTeammates2013_15$player <- gsub(" ", ".", BrentBurnsTeammates2013_15$With, fixed = TRUE)
DustinByfuglienTeammates2013_14$player <- gsub(" ", ".", DustinByfuglienTeammates2013_14$With, fixed = TRUE)
BrendanSmithTeammates2019_20$player <- gsub(" ", ".", BrendanSmithTeammates2019_20$With, fixed = TRUE)
BrendanSmithTeammates2023_24$player <- gsub(".", " ", BrendanSmithTeammates2023_24$With, fixed = TRUE)
MarkPysykTeammates2019_20$player <- gsub(" ", ".", MarkPysykTeammates2019_20$With, fixed = TRUE)

BrentBurnsTeammates2013_15ForMerge <- BrentBurnsTeammates2013_15 %>%
  dplyr::select(player, FlexPosition, CF..Without.Burns) %>%
  rename(CFPercent = CF..Without.Burns) %>%
  mutate(CFPercent = CFPercent/100)

DustinByfuglienTeammates2013_14ForMerge <- DustinByfuglienTeammates2013_14 %>%
  dplyr::select(player, FlexPosition, CF..Without.Byfuglien) %>%
  rename(CFPercent = CF..Without.Byfuglien) %>%
  mutate(CFPercent = CFPercent/100)

BrendanSmithTeammates2019_20ForMerge <- BrendanSmithTeammates2019_20 %>%
  dplyr::select(player, FlexPosition, CF..Without.Smith) %>%
  rename(CFPercent = CF..Without.Smith) %>%
  mutate(CFPercent = CFPercent/100)

BrendanSmithTeammates2023_24ForMerge <- BrendanSmithTeammates2023_24 %>%
  dplyr::select(player, FlexPosition, CF..Without.Smith) %>%
  rename(CFPercent = CF..Without.Smith) %>%
  mutate(CFPercent = CFPercent/100)

MarkPysykTeammates2019_20ForMerge <- MarkPysykTeammates2019_20 %>%
  dplyr::select(player, FlexPosition, CF..Without.Pysyk) %>%
  rename(CFPercent = CF..Without.Pysyk) %>%
  mutate(CFPercent = CFPercent/100)

BrentBurnsTeammateSummariesAll <- bind_rows(BrentBurnsFullTeammateSummaries, BrentBurnsTeammates2013_15ForMerge) %>% arrange(player)
DustinByfuglienTeammateSummariesAll <- bind_rows(DustinByfuglienTeammateSummaries, DustinByfuglienTeammates2013_14ForMerge) %>% arrange(player)
BrendanSmith2019_20TeammateSummariesAll <- bind_rows(BrendanSmithTeammateSummaries, BrendanSmithTeammates2019_20ForMerge) %>% arrange(player)
BrendanSmith2023_24TeammateSummariesAll <- bind_rows(BrendanSmithTeammateSummaries2023_24, BrendanSmithTeammates2023_24ForMerge) %>% arrange(player)
MarkPysykTeammateSummariesAll <- bind_rows(MarkPysykTeammateSummaries, MarkPysykTeammates2019_20ForMerge) %>% arrange(player)

#Bring in player data from given years to give weights for the without corsi data

FullPlayerData2013_14$player <- gsub(" ", ".", FullPlayerData2013_14$Player, fixed = TRUE)
FullPlayerData2013_15$player <- gsub(" ", ".", FullPlayerData2013_15$Player, fixed = TRUE)
FullPlayerData2019_20$player <- gsub(" ", ".", FullPlayerData2019_20$Player, fixed = TRUE)

#match up data on full data for the season, will need to subtract the values with from the totals to get the without
BrentBurnsTeammateSummariesAll <- BrentBurnsTeammateSummariesAll %>% 
  inner_join(FullPlayerData2013_15, by= "player") %>%
  mutate(total_CF = coalesce(total_CF, CF)) 
BrentBurnsTeammateSummariesAll <- BrentBurnsTeammateSummariesAll %>% 
  inner_join(FullPlayerData2013_15, by= "player") %>%
  mutate(total_CA = coalesce(total_CA, CA.y)) %>%
  select(player, FlexPosition, total_CF, total_CA, total_events,CFPercent)

DustinByfuglienTeammateSummariesAll <- DustinByfuglienTeammateSummariesAll %>% 
  inner_join(FullPlayerData2013_14, by= "player") %>%
  mutate(total_CF = coalesce(total_CF, CF)) 
DustinByfuglienTeammateSummariesAll <- DustinByfuglienTeammateSummariesAll %>% 
  inner_join(FullPlayerData2013_14, by= "player") %>%
  mutate(total_CA = coalesce(total_CA, CA.y)) %>%
  select(player, FlexPosition, total_CF, total_CA, events, CFPercent)

BrendanSmith2019_20TeammateSummariesAll <- BrendanSmith2019_20TeammateSummariesAll %>% 
  inner_join(FullPlayerData2019_20, by= "player") %>%
  mutate(total_CF = coalesce(total_CF, CF)) 
BrendanSmith2019_20TeammateSummariesAll <- BrendanSmith2019_20TeammateSummariesAll %>% 
  inner_join(FullPlayerData2019_20, by= "player") %>%
  mutate(total_CA = coalesce(total_CA, CA.y)) %>%
  select(player, FlexPosition, total_CF, total_CA, events, CFPercent)

BrendanSmith2023_24TeammateSummariesAll <- BrendanSmith2023_24TeammateSummariesAll %>% 
  inner_join(FullPlayerData2023_24, by= "player") %>%
  mutate(total_CF = coalesce(total_CF, CF)) 
BrendanSmith2023_24TeammateSummariesAll <- BrendanSmith2023_24TeammateSummariesAll %>% 
  inner_join(FullPlayerData2023_24, by= "player") %>%
  mutate(total_CA = coalesce(total_CA, CA.y)) %>%
  select(player, FlexPosition, total_CF, total_CA, events,CFPercent)

MarkPysykTeammateSummariesAll <- MarkPysykTeammateSummariesAll %>% 
  inner_join(FullPlayerData2019_20, by= "player") %>%
  mutate(total_CF = coalesce(total_CF, CF)) 
MarkPysykTeammateSummariesAll <- MarkPysykTeammateSummariesAll %>% 
  inner_join(FullPlayerData2019_20, by= "player") %>%
  mutate(total_CA = coalesce(total_CA, CA.y)) %>%
  select(player, FlexPosition, total_CF, total_CA, events,CFPercent)

#calculate the true total_CF and total_CA without the dual role player on the ice
Burns_pivot_for_calculation <- BrentBurnsTeammateSummariesAll %>%
  select(player, FlexPosition, total_CF, total_CA, total_events) %>%
  #filter out "without" rows temporarily, we'll use them separately
  filter(FlexPosition != "Without") %>%
  pivot_wider(
    names_from = FlexPosition,
    values_from = c(total_CF, total_CA, total_events),
    names_glue = "{.value}_{FlexPosition}"
  )

Burns_without_data <- BrentBurnsTeammateSummariesAll %>%
  filter(FlexPosition == "Without") %>%
  select(player, original_total_CF = total_CF, original_total_CA = total_CA) 

Burns_combined_for_correction <- left_join(
  Burns_pivot_for_calculation,
  Burns_without_data,
  by = "player"
)

Burns_corrected_without_data <- Burns_combined_for_correction %>%
  mutate(
    #calculate true total_CF when Burns is OFF ice
    total_CF_Without_Calculated = original_total_CF - coalesce(total_CF_F, 0) - coalesce(total_CF_D, 0),
    # Calculate true total_CA when Burns is OFF ice
    total_CA_Without_Calculated = original_total_CA - coalesce(total_CA_F, 0) - coalesce(total_CA_D, 0)
  ) %>%
  # Calculate true events_Without and CFPercent_Without
  mutate(
    events_Without_Calculated = total_CF_Without_Calculated + total_CA_Without_Calculated,
    CFPercent_Without_Calculated = total_CF_Without_Calculated / events_Without_Calculated,
    FlexPosition = "Without"
  ) %>%
  # Now select and rename the columns to match the desired output format
  select(
    player,
    FlexPosition, # Now this column exists as a literal string "Without"
    total_CF = total_CF_Without_Calculated,
    total_CA = total_CA_Without_Calculated,
    total_events = events_Without_Calculated,
    CFPercent = CFPercent_Without_Calculated
  )


Burns_final_corrected_data <- BrentBurnsTeammateSummariesAll %>%
  filter(FlexPosition != "Without") %>% # Remove original "Without" rows
  bind_rows(Burns_corrected_without_data) %>% # Add the newly calculated "Without" rows
  arrange(player, FlexPosition) %>%
  rename(events = total_events)

#Dustin Byfuglien dataframe manipulation
Byfuglien_pivot_for_calculation <- DustinByfuglienTeammateSummariesAll %>%
  select(player, FlexPosition, total_CF, total_CA, events) %>%
  filter(FlexPosition != "Without") %>%
  pivot_wider(
    names_from = FlexPosition,
    values_from = c(total_CF, total_CA, events),
    names_glue = "{.value}_{FlexPosition}"
  )

Byfuglien_without_data <- DustinByfuglienTeammateSummariesAll %>%
  filter(FlexPosition == "Without") %>%
  select(player, original_total_CF = total_CF, original_total_CA = total_CA) 

Byfuglien_combined_for_correction <- left_join(
  Byfuglien_pivot_for_calculation,
  Byfuglien_without_data,
  by = "player"
)

Byfuglien_corrected_without_data <- Byfuglien_combined_for_correction %>%
  mutate(
    total_CF_Without_Calculated = original_total_CF - coalesce(total_CF_F, 0) - coalesce(total_CF_D, 0),
    total_CA_Without_Calculated = original_total_CA - coalesce(total_CA_F, 0) - coalesce(total_CA_D, 0)
  ) %>%
  mutate(
    events_Without_Calculated = total_CF_Without_Calculated + total_CA_Without_Calculated,
    CFPercent_Without_Calculated = total_CF_Without_Calculated / events_Without_Calculated,
    FlexPosition = "Without"
  ) %>%
  select(
    player,
    FlexPosition,
    total_CF = total_CF_Without_Calculated,
    total_CA = total_CA_Without_Calculated,
    events = events_Without_Calculated,
    CFPercent = CFPercent_Without_Calculated
  )

Byfuglien_final_corrected_data <- DustinByfuglienTeammateSummariesAll %>%
  filter(FlexPosition != "Without") %>% 
  bind_rows(Byfuglien_corrected_without_data) %>%
  arrange(player, FlexPosition) 

#Brendan Smith 2019-20 dataframe manipulation
Smith2019_20_pivot_for_calculation <- BrendanSmith2019_20TeammateSummariesAll %>%
  select(player, FlexPosition, total_CF, total_CA, events) %>%
  filter(FlexPosition != "Without") %>%
  pivot_wider(
    names_from = FlexPosition,
    values_from = c(total_CF, total_CA, events),
    names_glue = "{.value}_{FlexPosition}"
  )

Smith2019_20_without_data <- BrendanSmith2019_20TeammateSummariesAll %>%
  filter(FlexPosition == "Without") %>%
  select(player, original_total_CF = total_CF, original_total_CA = total_CA) 

Smith2019_20_combined_for_correction <- left_join(
  Smith2019_20_pivot_for_calculation,
  Smith2019_20_without_data,
  by = "player"
)

Smith2019_20_corrected_without_data <- Smith2019_20_combined_for_correction %>%
  mutate(
    total_CF_Without_Calculated = original_total_CF - coalesce(total_CF_F, 0) - coalesce(total_CF_D, 0),
    total_CA_Without_Calculated = original_total_CA - coalesce(total_CA_F, 0) - coalesce(total_CA_D, 0)
  ) %>%
  mutate(
    events_Without_Calculated = total_CF_Without_Calculated + total_CA_Without_Calculated,
    CFPercent_Without_Calculated = total_CF_Without_Calculated / events_Without_Calculated,
    FlexPosition = "Without"
  ) %>%
  select(
    player,
    FlexPosition, # Now this column exists as a literal string "Without"
    total_CF = total_CF_Without_Calculated,
    total_CA = total_CA_Without_Calculated,
    events = events_Without_Calculated,
    CFPercent = CFPercent_Without_Calculated
  )

Smith2019_20_final_corrected_data <- BrendanSmith2019_20TeammateSummariesAll %>%
  filter(FlexPosition != "Without") %>%
  bind_rows(Smith2019_20_corrected_without_data) %>%
  arrange(player, FlexPosition)

#Brendan Smith 2023-24 dataframe manipulation
Smith2023_24_pivot_for_calculation <- BrendanSmith2023_24TeammateSummariesAll %>%
  select(player, FlexPosition, total_CF, total_CA, events) %>%
  filter(FlexPosition != "Without") %>%
  pivot_wider(
    names_from = FlexPosition,
    values_from = c(total_CF, total_CA, events),
    names_glue = "{.value}_{FlexPosition}"
  )

Smith2023_24_without_data <- BrendanSmith2023_24TeammateSummariesAll %>%
  filter(FlexPosition == "Without") %>%
  select(player, original_total_CF = total_CF, original_total_CA = total_CA) 

Smith2023_24_combined_for_correction <- left_join(
  Smith2023_24_pivot_for_calculation,
  Smith2023_24_without_data,
  by = "player"
)

Smith2023_24_corrected_without_data <- Smith2023_24_combined_for_correction %>%
  mutate(
    total_CF_Without_Calculated = original_total_CF - coalesce(total_CF_F, 0) - coalesce(total_CF_D, 0),
    total_CA_Without_Calculated = original_total_CA - coalesce(total_CA_F, 0) - coalesce(total_CA_D, 0)
  ) %>%
  mutate(
    events_Without_Calculated = total_CF_Without_Calculated + total_CA_Without_Calculated,
    CFPercent_Without_Calculated = total_CF_Without_Calculated / events_Without_Calculated,
    FlexPosition = "Without"
  ) %>%
  select(
    player,
    FlexPosition,
    total_CF = total_CF_Without_Calculated,
    total_CA = total_CA_Without_Calculated,
    events = events_Without_Calculated,
    CFPercent = CFPercent_Without_Calculated
  )

Smith2023_24_final_corrected_data <- BrendanSmith2023_24TeammateSummariesAll %>%
  filter(FlexPosition != "Without") %>%
  bind_rows(Smith2023_24_corrected_without_data) %>% 
  arrange(player, FlexPosition) 

#Mark Pysyk dataframe manipulation
Pysyk_pivot_for_calculation <- MarkPysykTeammateSummariesAll %>%
  select(player, FlexPosition, total_CF, total_CA, events) %>%
  filter(FlexPosition != "Without") %>%
  pivot_wider(
    names_from = FlexPosition,
    values_from = c(total_CF, total_CA, events),
    names_glue = "{.value}_{FlexPosition}"
  )

Pysyk_without_data <- MarkPysykTeammateSummariesAll %>%
  filter(FlexPosition == "Without") %>%
  select(player, original_total_CF = total_CF, original_total_CA = total_CA) 

Pysyk_combined_for_correction <- left_join(
  Pysyk_pivot_for_calculation,
  Pysyk_without_data,
  by = "player"
)

Pysyk_corrected_without_data <- Pysyk_combined_for_correction %>%
  mutate(
    total_CF_Without_Calculated = original_total_CF - coalesce(total_CF_F, 0) - coalesce(total_CF_D, 0),
    total_CA_Without_Calculated = original_total_CA - coalesce(total_CA_F, 0) - coalesce(total_CA_D, 0)
  ) %>%
  mutate(
    events_Without_Calculated = total_CF_Without_Calculated + total_CA_Without_Calculated,
    CFPercent_Without_Calculated = total_CF_Without_Calculated / events_Without_Calculated,
    FlexPosition = "Without"
  ) %>%
  select(
    player,
    FlexPosition,
    total_CF = total_CF_Without_Calculated,
    total_CA = total_CA_Without_Calculated,
    events = events_Without_Calculated,
    CFPercent = CFPercent_Without_Calculated
  )

Pysyk_final_corrected_data <- MarkPysykTeammateSummariesAll %>%
  filter(FlexPosition != "Without") %>%
  bind_rows(Pysyk_corrected_without_data) %>% 
  arrange(player, FlexPosition) 

#filter out all players that have less than 30 events at D or F
Burns_filtered_data <- Burns_final_corrected_data %>%
  group_by(player) %>%
  filter(all(events >= 30)) %>%
  filter(n_distinct(FlexPosition) == 3) %>%
  ungroup()

Byfuglien_filtered_data <- Byfuglien_final_corrected_data %>%
  group_by(player) %>%
  filter(all(events >= 30)) %>%
  filter(n_distinct(FlexPosition) == 3) %>%
  ungroup()

Smith2019_20_filtered_data <- Smith2019_20_final_corrected_data %>%
  group_by(player) %>%
  filter(all(events >= 30)) %>%
  filter(n_distinct(FlexPosition) == 3) %>%
  ungroup()

Smith2023_24_filtered_data <- Smith2023_24_final_corrected_data %>%
  group_by(player) %>%
  filter(all(events >= 30)) %>%
  filter(n_distinct(FlexPosition) == 3) %>%
  ungroup()

Pysyk_filtered_data <- Pysyk_final_corrected_data %>%
  group_by(player) %>%
  filter(all(events >= 30)) %>%
  filter(n_distinct(FlexPosition) == 3) %>%
  ungroup()

#create a model for each dual role player to determine their impact on CF% 
library(lme4)
library(emmeans)
Burns_filtered_data <- Burns_filtered_data %>%
  mutate(FlexPosition = factor(FlexPosition, levels = c("Without", "D", "F")))

Burns_Model <- glmer(cbind(total_CF, total_CA) ~ FlexPosition + (1|player),
                     data = Burns_filtered_data,
                     family = binomial(link = "logit"))
summary(Burns_Model)

Burns_Pairwise1 <- emmeans(Burns_Model, ~ FlexPosition)
Burns_pairwise_comparisons <- contrast(Burns_Pairwise1, method = "pairwise")

#Byfuglein Model
Byfuglien_filtered_data <- Byfuglien_filtered_data %>%
  mutate(FlexPosition = factor(FlexPosition, levels = c("Without", "D", "F")))

Byfuglien_Model <- glmer(cbind(total_CF, total_CA) ~ FlexPosition + (1|player),
                     data = Byfuglien_filtered_data,
                     family = binomial(link = "logit"))
summary(Byfuglien_Model)

Byguflien_Pairwise1 <- emmeans(Byfuglien_Model, ~ FlexPosition)
Byfuglien_pairwise_comparisons <- contrast(Byguflien_Pairwise1, method = "pairwise")

#Smith 2019-20 Model
Smith2019_20_filtered_data <- Smith2019_20_filtered_data %>%
  mutate(FlexPosition = factor(FlexPosition, levels = c("Without", "D", "F")))

Smith2019_20_Model <- glmer(cbind(total_CF, total_CA) ~ FlexPosition + (1|player),
                         data = Smith2019_20_filtered_data,
                         family = binomial(link = "logit"))
summary(Smith2019_20_Model)

Smith2019_20_Pairwise1 <- emmeans(Smith2019_20_Model, ~ FlexPosition)
Smith2019_20_pairwise_comparisons <- contrast(Smith2019_20_Pairwise1, method = "pairwise")

#Smith 2023-24 Model
Smith2023_24_filtered_data <- Smith2023_24_filtered_data %>%
  mutate(FlexPosition = factor(FlexPosition, levels = c("Without", "D", "F")))

Smith2023_24_Model <- glmer(cbind(total_CF, total_CA) ~ FlexPosition + (1|player),
                            data = Smith2023_24_filtered_data,
                            family = binomial(link = "logit"))
summary(Smith2023_24_Model)

Smith2023_24_Pairwise1 <- emmeans(Smith2023_24_Model, ~ FlexPosition)
Smith2023_24_pairwise_comparisons <- contrast(Smith2023_24_Pairwise1, method = "pairwise")

#Smith Overall Model
SmithBothYears_filtered <- rbind(Smith2019_20_filtered_data, Smith2023_24_filtered_data)
levels(DualRolePlayers_filtered$FlexPosition) #verify that flexpositions is still a factor

SmithBothYears_Model <- glmer(cbind(total_CF, total_CA) ~ FlexPosition + (1|player),
                            data = SmithBothYears_filtered,
                            family = binomial(link = "logit"))
summary(SmithBothYears_Model)

SmithBothYears_Pairwise1 <- emmeans(SmithBothYears_Model, ~ FlexPosition)
SmithBothYears_pairwise_comparisons <- contrast(SmithBothYears_Pairwise1, method = "pairwise")

#Pysyk Model
Pysyk_filtered_data <- Pysyk_filtered_data %>%
  mutate(FlexPosition = factor(FlexPosition, levels = c("Without", "D", "F")))

Pysyk_Model <- glmer(cbind(total_CF, total_CA) ~ FlexPosition + (1|player),
                         data = Pysyk_filtered_data,
                         family = binomial(link = "logit"))
summary(Pysyk_Model)

Pysyk_Pairwise1 <- emmeans(Pysyk_Model, ~ FlexPosition)
Pysyk_pairwise_comparisons <- contrast(Pysyk_Pairwise1, method = "pairwise")

#Overall model
DualRolePlayers_filtered <- rbind(Burns_filtered_data, Byfuglien_filtered_data,
                                  Smith2019_20_filtered_data, Smith2023_24_filtered_data,
                                  Pysyk_filtered_data)
levels(DualRolePlayers_filtered$FlexPosition) #verify that flexpositions is still a factor

FullDualRole_Model <- glmer(cbind(total_CF, total_CA) ~ FlexPosition + (1|player),
                     data = DualRolePlayers_filtered,
                     family = binomial(link = "logit"))
summary(FullDualRole_Model)

FullDualRole_Pairwise1 <- emmeans(FullDualRole_Model, ~ FlexPosition)
FullDualRole_pairwise_comparisons <- contrast(FullDualRole_Pairwise1, method = "pairwise")

