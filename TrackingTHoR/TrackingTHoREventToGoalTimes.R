#Time from Event to Goal
library(tidyr)
library(dplyr)
library(ggplot2)
library(arrow)
library(purrr)
library(data.table)

#Load in all SportLogiq data for HALO Hackathon
HALOEvents <- read_parquet("~/Desktop/Analytics Projects/HALO Hackathon/events.parquet")
HALOTracking <- read_parquet("~/Desktop/Analytics Projects/HALO Hackathon/tracking.parquet")
HALOPlayers <- read_parquet("~/Desktop/Analytics Projects/HALO Hackathon/players.parquet")
HALOGames <- read_parquet("~/Desktop/Analytics Projects/HALO Hackathon/games.parquet")

HALOEventsWithHomeAwayTeam <- HALOEvents %>%
  left_join(HALOGames %>% select(game_id, home_team, away_team), by = "game_id")

#Found all even strength time bands in the stints wide file
HALOStintsStrengthStatesWide

HALOEvents_dt <- as.data.table(HALOEventsWithHomeAwayTeam)
HALOEvents_dt[, row_id := .I] 

HALOStrengthStatesTimes_dt <- as.data.table(HALOStintsStrengthStatesWide)

setkey(HALOStrengthStatesTimes_dt, game_id, period, StrengthStateStartTime, StrengthStateEndTime)

HALOEvents_interval <- HALOEvents_dt[, .(
  row_id,
  game_id, 
  period, 
  period_time, 
  period_time_end = period_time
)]
setkey(HALOEvents_interval, game_id, period, period_time, period_time_end)

HALOEvents_overlap_result <- foverlaps(
  HALOEvents_interval,
  HALOStrengthStatesTimes_dt,
  by.x = c("game_id", "period", "period_time", "period_time_end"),
  by.y = c("game_id", "period", "StrengthStateStartTime", "StrengthStateEndTime"),
  type = "within",
  nomatch = NA
)

HALOEventsStrengthStatesStrength <- HALOEvents_overlap_result[
  !is.na(StrengthStateStint),
  .(row_id, StrengthStateStint, StrengthStateStartTime, StrengthStateEndTime, StrengthState)
][HALOEvents_dt, on = "row_id", nomatch = NULL] %>%
  as_tibble()

HALOEventsStrengthStatesStrengthSliced <- HALOEventsStrengthStatesStrength %>%
  group_by(game_id, sl_event_id) %>%
  slice(if (first(event_type) == "faceoff") n() else 1) %>%
  ungroup()


HALOEventsStrengthStatesStrengthWithGoalDiff <- HALOEventsStrengthStatesStrengthSliced %>%
  arrange(game_id, period, period_time, sl_event_id) %>%
  group_by(game_id) %>%
  mutate(
    HomeTeamGoals = lag(cumsum(event_type == "goal" & 
                                 !is.na(team) & 
                                 !is.na(home_team) & 
                                 team == home_team), default = 0),
    RoadTeamGoals = lag(cumsum(event_type == "goal" & 
                                 !is.na(team) & 
                                 !is.na(away_team) & 
                                 team == away_team), default = 0),
    HomeTeamScoreDiff = case_when(
      HomeTeamGoals == RoadTeamGoals ~  0L,
      HomeTeamGoals - RoadTeamGoals == 1 ~  1L,
      HomeTeamGoals - RoadTeamGoals == 2 ~  2L,
      HomeTeamGoals - RoadTeamGoals > 2 ~  3L,
      HomeTeamGoals - RoadTeamGoals == -1 ~ -1L,
      HomeTeamGoals - RoadTeamGoals == -2 ~ -2L,
      HomeTeamGoals - RoadTeamGoals < -2 ~ -3L
    )
  ) %>%
  ungroup()

HALOEventsEvenStrength <- HALOEventsStrengthStatesStrengthWithGoalDiff %>%
  filter(StrengthState != "Uneven Strength State")

#Filter out events with no x, y location. These are the setup of a faceoff, whistles, penalties
HALOEventsEvenStrength2 <- HALOEventsEvenStrength %>%
  filter(complete.cases(x, y))

#Calculate Time to next goal
HALOEventsTimeToNextTeamGoal <- HALOEventsEvenStrength2 %>%
  group_by(game_id, period) %>%
  mutate(TimeToNextTeamGoal = map_dbl(seq_len(n()), function(i){
    current_time = period_time[i]
    current_team = team[i]
    future_team_goal_times <- period_time[
      event_type=="goal" &
      team==current_team &
      period_time>current_time
    ]
    if(length(future_team_goal_times) == 0){
      NA_real_
    } else {
      min(future_team_goal_times) - current_time
    }
    })) %>%
  ungroup() %>%
  mutate(
    TeamGoalWithinNext3Sec = ifelse(TimeToNextTeamGoal<=3,1,0),
    TeamGoalWithinNext5Sec = ifelse(TimeToNextTeamGoal<=5,1,0),
    TeamGoalWithinNext7Sec = ifelse(TimeToNextTeamGoal<=7,1,0),
    TeamGoalWithinNext10Sec = ifelse(TimeToNextTeamGoal<=10,1,0),
    TeamGoalWithinNext11Sec = ifelse(TimeToNextTeamGoal<=11,1,0),
    TeamGoalWithinNext12Sec = ifelse(TimeToNextTeamGoal<=12,1,0),
    TeamGoalWithinNext15Sec = ifelse(TimeToNextTeamGoal<=15,1,0),
    TeamGoalWithinNext17Sec = ifelse(TimeToNextTeamGoal<=17,1,0),
    TeamGoalWithinNext20Sec = ifelse(TimeToNextTeamGoal<=20,1,0),
    TeamGoalWithinNext25Sec = ifelse(TimeToNextTeamGoal<=25,1,0),
    TeamGoalWithinNext30Sec = ifelse(TimeToNextTeamGoal<=30,1,0)
  )



hist(HALOEventsTimeToNextTeamGoal$TimeToNextTeamGoal)

HALOEventsTimeToNextTeamGoalSummary <- HALOEventsTimeToNextTeamGoal %>%
  summarise(
    TotalPlays = n(),
    TeamGoalWithinNext3SecCount = sum(TeamGoalWithinNext3Sec==1, na.rm = TRUE),
    TeamGoalWithinNext5SecCount = sum(TeamGoalWithinNext5Sec==1, na.rm = TRUE),
    TeamGoalWithinNext7SecCount = sum(TeamGoalWithinNext7Sec==1, na.rm = TRUE),
    TeamGoalWithinNext10SecCount = sum(TeamGoalWithinNext10Sec==1, na.rm = TRUE),
    TeamGoalWithinNext11SecCount = sum(TeamGoalWithinNext11Sec==1, na.rm = TRUE),
    TeamGoalWithinNext12SecCount = sum(TeamGoalWithinNext12Sec==1, na.rm = TRUE),
    TeamGoalWithinNext15SecCount = sum(TeamGoalWithinNext15Sec==1, na.rm = TRUE),
    TeamGoalWithinNext17SecCount = sum(TeamGoalWithinNext17Sec==1, na.rm = TRUE),
    TeamGoalWithinNext20SecCount = sum(TeamGoalWithinNext20Sec==1, na.rm = TRUE),
    TeamGoalWithinNext25SecCount = sum(TeamGoalWithinNext25Sec==1, na.rm = TRUE),
    TeamGoalWithinNext30SecCount = sum(TeamGoalWithinNext30Sec==1, na.rm = TRUE),
    TeamGoalWithinNext3SecPercent = (TeamGoalWithinNext3SecCount/TotalPlays)*100,
    TeamGoalWithinNext5SecPercent = (TeamGoalWithinNext5SecCount/TotalPlays)*100,
    TeamGoalWithinNext7SecPercent = (TeamGoalWithinNext7SecCount/TotalPlays)*100,
    TeamGoalWithinNext10SecPercent = (TeamGoalWithinNext10SecCount/TotalPlays)*100,
    TeamGoalWithinNext11SecPercent = (TeamGoalWithinNext11SecCount/TotalPlays)*100,
    TeamGoalWithinNext12SecPercent = (TeamGoalWithinNext12SecCount/TotalPlays)*100,
    TeamGoalWithinNext15SecPercent = (TeamGoalWithinNext15SecCount/TotalPlays)*100,
    TeamGoalWithinNext17SecPercent = (TeamGoalWithinNext17SecCount/TotalPlays)*100,
    TeamGoalWithinNext20SecPercent = (TeamGoalWithinNext20SecCount/TotalPlays)*100,
    TeamGoalWithinNext25SecPercent = (TeamGoalWithinNext25SecCount/TotalPlays)*100,
    TeamGoalWithinNext30SecPercent = (TeamGoalWithinNext30SecCount/TotalPlays)*100
  )

HALOEventsTimeToNextTeamGoalPercentWider <- HALOEventsTimeToNextTeamGoalSummary %>%
  select(TeamGoalWithinNext3SecPercent, TeamGoalWithinNext5SecPercent, TeamGoalWithinNext7SecPercent,
         TeamGoalWithinNext10SecPercent, TeamGoalWithinNext11SecPercent, TeamGoalWithinNext12SecPercent,
         TeamGoalWithinNext15SecPercent, TeamGoalWithinNext17SecPercent, TeamGoalWithinNext20SecPercent,
         TeamGoalWithinNext25SecPercent, TeamGoalWithinNext30SecPercent) %>%
  pivot_longer(
    cols = c(TeamGoalWithinNext3SecPercent, TeamGoalWithinNext5SecPercent, TeamGoalWithinNext7SecPercent,
             TeamGoalWithinNext10SecPercent, TeamGoalWithinNext11SecPercent, TeamGoalWithinNext12SecPercent,
             TeamGoalWithinNext15SecPercent, TeamGoalWithinNext17SecPercent, TeamGoalWithinNext20SecPercent,
             TeamGoalWithinNext25SecPercent, TeamGoalWithinNext30SecPercent),
    names_to = "Seconds",
    values_to = "Percent"
  ) %>%
  mutate(
    Seconds = as.numeric(gsub("TeamGoalWithinNext(\\d+)SecPercent", "\\1", Seconds))
  ) %>%
  arrange(Seconds) %>%
  mutate(Slope = (Percent-lag(Percent))/(Seconds-lag(Seconds)))

AllEventsSlopes <- HALOEventsTimeToNextTeamGoalPercentWider %>%
  select(Seconds, Slope) %>%
  pivot_wider(names_from = Seconds, values_from = Slope, names_glue = "Slope_{lag(Seconds)}_{Seconds}") %>%
  mutate(event_type = "All Events") %>%
  relocate(event_type)


HALOEventDataTimeToNextGoalPlot <- ggplot(data = HALOEventsTimeToNextTeamGoalPercentWider, aes(x = Seconds, y = Percent)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = unique(HALOEventsTimeToNextTeamGoalPercentWider$Seconds)) +
  labs(x = "Seconds Until Next Team Goal", y = "Percent of Plays", 
       title = "Cumulative % of Plays with a Team Goal Within X Seconds")



#look at cumulative graph by play type and see where slope is roughly the same
#event types
unique(HALOEventsTimeToNextTeamGoal$event_type)

CumulativeGoalPercentGraphByEvent <- function(event_dataframe = HALOEventsTimeToNextTeamGoal, event_type_choice){
  
  specific_event_dataframe <- event_dataframe %>%
    filter(event_type == event_type_choice)
  
  specific_event_dataframe_summary <- specific_event_dataframe %>%
    summarise(
      TotalPlays = n(),
      TeamGoalWithinNext3SecCount = sum(TeamGoalWithinNext3Sec==1, na.rm = TRUE),
      TeamGoalWithinNext5SecCount = sum(TeamGoalWithinNext5Sec==1, na.rm = TRUE),
      TeamGoalWithinNext7SecCount = sum(TeamGoalWithinNext7Sec==1, na.rm = TRUE),
      TeamGoalWithinNext10SecCount = sum(TeamGoalWithinNext10Sec==1, na.rm = TRUE),
      TeamGoalWithinNext11SecCount = sum(TeamGoalWithinNext11Sec==1, na.rm = TRUE),
      TeamGoalWithinNext12SecCount = sum(TeamGoalWithinNext12Sec==1, na.rm = TRUE),
      TeamGoalWithinNext15SecCount = sum(TeamGoalWithinNext15Sec==1, na.rm = TRUE),
      TeamGoalWithinNext17SecCount = sum(TeamGoalWithinNext17Sec==1, na.rm = TRUE),
      TeamGoalWithinNext20SecCount = sum(TeamGoalWithinNext20Sec==1, na.rm = TRUE),
      TeamGoalWithinNext25SecCount = sum(TeamGoalWithinNext25Sec==1, na.rm = TRUE),
      TeamGoalWithinNext30SecCount = sum(TeamGoalWithinNext30Sec==1, na.rm = TRUE),
      TeamGoalWithinNext3SecPercent = (TeamGoalWithinNext3SecCount/TotalPlays)*100,
      TeamGoalWithinNext5SecPercent = (TeamGoalWithinNext5SecCount/TotalPlays)*100,
      TeamGoalWithinNext7SecPercent = (TeamGoalWithinNext7SecCount/TotalPlays)*100,
      TeamGoalWithinNext10SecPercent = (TeamGoalWithinNext10SecCount/TotalPlays)*100,
      TeamGoalWithinNext11SecPercent = (TeamGoalWithinNext11SecCount/TotalPlays)*100,
      TeamGoalWithinNext12SecPercent = (TeamGoalWithinNext12SecCount/TotalPlays)*100,
      TeamGoalWithinNext15SecPercent = (TeamGoalWithinNext15SecCount/TotalPlays)*100,
      TeamGoalWithinNext17SecPercent = (TeamGoalWithinNext17SecCount/TotalPlays)*100,
      TeamGoalWithinNext20SecPercent = (TeamGoalWithinNext20SecCount/TotalPlays)*100,
      TeamGoalWithinNext25SecPercent = (TeamGoalWithinNext25SecCount/TotalPlays)*100,
      TeamGoalWithinNext30SecPercent = (TeamGoalWithinNext30SecCount/TotalPlays)*100
    )
  
  specific_event_dataframe_summary_wide <- specific_event_dataframe_summary %>%
    select(TeamGoalWithinNext3SecPercent, TeamGoalWithinNext5SecPercent, TeamGoalWithinNext7SecPercent,
           TeamGoalWithinNext10SecPercent, TeamGoalWithinNext11SecPercent, TeamGoalWithinNext12SecPercent,
           TeamGoalWithinNext15SecPercent, TeamGoalWithinNext17SecPercent, TeamGoalWithinNext20SecPercent,
           TeamGoalWithinNext25SecPercent, TeamGoalWithinNext30SecPercent) %>%
    pivot_longer(
      cols = c(TeamGoalWithinNext3SecPercent, TeamGoalWithinNext5SecPercent, TeamGoalWithinNext7SecPercent,
               TeamGoalWithinNext10SecPercent, TeamGoalWithinNext11SecPercent, TeamGoalWithinNext12SecPercent,
               TeamGoalWithinNext15SecPercent, TeamGoalWithinNext17SecPercent, TeamGoalWithinNext20SecPercent,
               TeamGoalWithinNext25SecPercent, TeamGoalWithinNext30SecPercent),
      names_to = "Seconds",
      values_to = "Percent"
    ) %>%
    mutate(
      Seconds = as.numeric(gsub("TeamGoalWithinNext(\\d+)SecPercent", "\\1", Seconds))
    ) %>%
    arrange(Seconds) %>%
    mutate(Slope = (Percent-lag(Percent))/(Seconds-lag(Seconds)))
  
  event_type_time_to_next_goal_plot <- ggplot(data = specific_event_dataframe_summary_wide, aes(x = Seconds, y = Percent)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = unique(specific_event_dataframe_summary_wide$Seconds)) +
    labs(x = "Seconds Until Next Team Goal", y = "Percent of Plays", 
         title = "Cumulative % of Plays with a Team Goal Within X Seconds",
         subtitle = paste0("Event of interest: ", event_type_choice))
  
  return(event_type_time_to_next_goal_plot)
}


CumulativeGoalPercentGraphByEvent(HALOEventsTimeToNextTeamGoal, "lpr")
CumulativeGoalPercentGraphByEvent(HALOEventsTimeToNextTeamGoal, "shot")
CumulativeGoalPercentGraphByEvent(HALOEventsTimeToNextTeamGoal, "faceoff")
CumulativeGoalPercentGraphByEvent(HALOEventsTimeToNextTeamGoal, "pass")
CumulativeGoalPercentGraphByEvent(HALOEventsTimeToNextTeamGoal, "reception")
CumulativeGoalPercentGraphByEvent(HALOEventsTimeToNextTeamGoal, "dumpout")
CumulativeGoalPercentGraphByEvent(HALOEventsTimeToNextTeamGoal, "puckprotection")
CumulativeGoalPercentGraphByEvent(HALOEventsTimeToNextTeamGoal, "failedpasslocation")
CumulativeGoalPercentGraphByEvent(HALOEventsTimeToNextTeamGoal, "dumpin")
CumulativeGoalPercentGraphByEvent(HALOEventsTimeToNextTeamGoal, "dumpinagainst")
CumulativeGoalPercentGraphByEvent(HALOEventsTimeToNextTeamGoal, "controlledbreakout")
CumulativeGoalPercentGraphByEvent(HALOEventsTimeToNextTeamGoal, "pressure")
CumulativeGoalPercentGraphByEvent(HALOEventsTimeToNextTeamGoal, "check")
CumulativeGoalPercentGraphByEvent(HALOEventsTimeToNextTeamGoal, "receptionprevention")
CumulativeGoalPercentGraphByEvent(HALOEventsTimeToNextTeamGoal, "carry")
CumulativeGoalPercentGraphByEvent(HALOEventsTimeToNextTeamGoal, "block")


SlopeOfGoalPercentByEvent <- function(event_dataframe = HALOEventsTimeToNextTeamGoal, event_type_choice){
  
  specific_event_dataframe <- event_dataframe %>%
    filter(event_type == event_type_choice)
  
  specific_event_dataframe_summary <- specific_event_dataframe %>%
    summarise(
      TotalPlays = n(),
      TeamGoalWithinNext3SecCount = sum(TeamGoalWithinNext3Sec==1, na.rm = TRUE),
      TeamGoalWithinNext5SecCount = sum(TeamGoalWithinNext5Sec==1, na.rm = TRUE),
      TeamGoalWithinNext7SecCount = sum(TeamGoalWithinNext7Sec==1, na.rm = TRUE),
      TeamGoalWithinNext10SecCount = sum(TeamGoalWithinNext10Sec==1, na.rm = TRUE),
      TeamGoalWithinNext11SecCount = sum(TeamGoalWithinNext11Sec==1, na.rm = TRUE),
      TeamGoalWithinNext12SecCount = sum(TeamGoalWithinNext12Sec==1, na.rm = TRUE),
      TeamGoalWithinNext15SecCount = sum(TeamGoalWithinNext15Sec==1, na.rm = TRUE),
      TeamGoalWithinNext17SecCount = sum(TeamGoalWithinNext17Sec==1, na.rm = TRUE),
      TeamGoalWithinNext20SecCount = sum(TeamGoalWithinNext20Sec==1, na.rm = TRUE),
      TeamGoalWithinNext25SecCount = sum(TeamGoalWithinNext25Sec==1, na.rm = TRUE),
      TeamGoalWithinNext30SecCount = sum(TeamGoalWithinNext30Sec==1, na.rm = TRUE),
      TeamGoalWithinNext3SecPercent = (TeamGoalWithinNext3SecCount/TotalPlays)*100,
      TeamGoalWithinNext5SecPercent = (TeamGoalWithinNext5SecCount/TotalPlays)*100,
      TeamGoalWithinNext7SecPercent = (TeamGoalWithinNext7SecCount/TotalPlays)*100,
      TeamGoalWithinNext10SecPercent = (TeamGoalWithinNext10SecCount/TotalPlays)*100,
      TeamGoalWithinNext11SecPercent = (TeamGoalWithinNext11SecCount/TotalPlays)*100,
      TeamGoalWithinNext12SecPercent = (TeamGoalWithinNext12SecCount/TotalPlays)*100,
      TeamGoalWithinNext15SecPercent = (TeamGoalWithinNext15SecCount/TotalPlays)*100,
      TeamGoalWithinNext17SecPercent = (TeamGoalWithinNext17SecCount/TotalPlays)*100,
      TeamGoalWithinNext20SecPercent = (TeamGoalWithinNext20SecCount/TotalPlays)*100,
      TeamGoalWithinNext25SecPercent = (TeamGoalWithinNext25SecCount/TotalPlays)*100,
      TeamGoalWithinNext30SecPercent = (TeamGoalWithinNext30SecCount/TotalPlays)*100
    )
  
  specific_event_dataframe_summary_wide <- specific_event_dataframe_summary %>%
    select(TeamGoalWithinNext3SecPercent, TeamGoalWithinNext5SecPercent, TeamGoalWithinNext7SecPercent,
           TeamGoalWithinNext10SecPercent, TeamGoalWithinNext11SecPercent, TeamGoalWithinNext12SecPercent,
           TeamGoalWithinNext15SecPercent, TeamGoalWithinNext17SecPercent, TeamGoalWithinNext20SecPercent,
           TeamGoalWithinNext25SecPercent, TeamGoalWithinNext30SecPercent) %>%
    pivot_longer(
      cols = c(TeamGoalWithinNext3SecPercent, TeamGoalWithinNext5SecPercent, TeamGoalWithinNext7SecPercent,
               TeamGoalWithinNext10SecPercent, TeamGoalWithinNext11SecPercent, TeamGoalWithinNext12SecPercent,
               TeamGoalWithinNext15SecPercent, TeamGoalWithinNext17SecPercent, TeamGoalWithinNext20SecPercent,
               TeamGoalWithinNext25SecPercent, TeamGoalWithinNext30SecPercent),
      names_to = "Seconds",
      values_to = "Percent"
    ) %>%
    mutate(
      Seconds = as.numeric(gsub("TeamGoalWithinNext(\\d+)SecPercent", "\\1", Seconds))
    ) %>%
    arrange(Seconds) %>%
    mutate(Slope = (Percent-lag(Percent))/(Seconds-lag(Seconds)))
  
  specific_event_dataframe_slopes_wide <- specific_event_dataframe_summary_wide %>%
    select(Seconds, Slope) %>%
    pivot_wider(names_from = Seconds, values_from = Slope, names_glue = "Slope_{lag(Seconds)}_{Seconds}") %>%
    mutate(event_type = event_type_choice) %>%
    relocate(event_type)
    
  
  return(specific_event_dataframe_slopes_wide)
}

LPRSlopes <- SlopeOfGoalPercentByEvent(HALOEventsTimeToNextTeamGoal, "lpr")
ShotSlopes <- SlopeOfGoalPercentByEvent(HALOEventsTimeToNextTeamGoal, "shot")
FaceoffSlopes <- SlopeOfGoalPercentByEvent(HALOEventsTimeToNextTeamGoal, "faceoff")
PassSlopes <- SlopeOfGoalPercentByEvent(HALOEventsTimeToNextTeamGoal, "pass")
ReceptionSlopes <- SlopeOfGoalPercentByEvent(HALOEventsTimeToNextTeamGoal, "reception")
DumpoutSlopes <- SlopeOfGoalPercentByEvent(HALOEventsTimeToNextTeamGoal, "dumpout")
PuckProtectionSlopes <- SlopeOfGoalPercentByEvent(HALOEventsTimeToNextTeamGoal, "puckprotection")
FailedPassLocationSlopes <- SlopeOfGoalPercentByEvent(HALOEventsTimeToNextTeamGoal, "failedpasslocation")
DumpInSlopes <- SlopeOfGoalPercentByEvent(HALOEventsTimeToNextTeamGoal, "dumpin")
DumpInAgainstSlopes <- SlopeOfGoalPercentByEvent(HALOEventsTimeToNextTeamGoal, "dumpinagainst")
ControlledBreakoutSlopes <- SlopeOfGoalPercentByEvent(HALOEventsTimeToNextTeamGoal, "controlledbreakout")
PressureSlopes <- SlopeOfGoalPercentByEvent(HALOEventsTimeToNextTeamGoal, "pressure")
CheckSlopes <- SlopeOfGoalPercentByEvent(HALOEventsTimeToNextTeamGoal, "check")
ReceptionPreventionSlopes <- SlopeOfGoalPercentByEvent(HALOEventsTimeToNextTeamGoal, "receptionprevention")
CarrySlopes <- SlopeOfGoalPercentByEvent(HALOEventsTimeToNextTeamGoal, "carry")
BlockSlopes <- SlopeOfGoalPercentByEvent(HALOEventsTimeToNextTeamGoal, "block")


SlopeComparisonDataframe <- bind_rows(AllEventsSlopes, LPRSlopes, ShotSlopes,
                                      FaceoffSlopes, PassSlopes, ReceptionSlopes,
                                      DumpoutSlopes, PuckProtectionSlopes, FailedPassLocationSlopes, 
                                      DumpInSlopes, DumpInAgainstSlopes, ControlledBreakoutSlopes,
                                      PressureSlopes, CheckSlopes, ReceptionPreventionSlopes, 
                                      CarrySlopes, BlockSlopes)

range(SlopeComparisonDataframe$Slope_3_5)
range(SlopeComparisonDataframe$Slope_5_7)
range(SlopeComparisonDataframe$Slope_7_10)
range(SlopeComparisonDataframe$Slope_10_11)
range(SlopeComparisonDataframe$Slope_11_12)
range(SlopeComparisonDataframe$Slope_12_15)
range(SlopeComparisonDataframe$Slope_15_17)
range(SlopeComparisonDataframe$Slope_17_20)


slope_long <- SlopeComparisonDataframe %>%
  select(-any_of("Slope_NA_3")) %>%                 # all NA, no interval to plot
  pivot_longer(
    cols = starts_with("Slope_"),
    names_to = "Interval",
    names_prefix = "Slope_",
    values_to = "Slope"
  ) %>%
  mutate(
    Interval = factor(gsub("_", "-", Interval),
                      levels = unique(gsub("_", "-", sub("Slope_", "", 
                                                         setdiff(grep("^Slope_", names(SlopeComparisonDataframe), value = TRUE), "Slope_NA_3"))))),
    IsAll = event_type == "All Events"
  )

ggplot(slope_long, aes(x = Interval, y = Slope, group = event_type, color = event_type)) +
  geom_line(aes(linewidth = IsAll, alpha = IsAll)) +
  geom_point(aes(alpha = IsAll), size = 1.8) +
  scale_linewidth_manual(values = c(`FALSE` = 0.7, `TRUE` = 1.8), guide = "none") +
  scale_alpha_manual(values = c(`FALSE` = 0.7, `TRUE` = 1), guide = "none") +
  labs(
    x = "Seconds Interval (Time Until Next Team Goal)",
    y = "Slope (Change in Cumulative % of Plays per Second)",
    title = "Slope of Cumulative Goal % by Event Type",
    subtitle = "'All Events' shown as the thicker line",
    color = "Event Type"
  ) +
  theme_minimal()


#Smoothing slope to see the differences
#Going to find the cumulative xG per 300 shots at each time threshold
HALOEventsTimeToNextTeamGoalEndThresholdTime <- HALOEventsTimeToNextTeamGoal %>%
  mutate(
    TimeEndThreshold3Sec = case_when(period_time <= 1197 & period != 4 ~ period_time + 3,
                                     period_time > 1197 & period != 4 ~ 1200,
                                     period_time <= 297 & period == 4 ~ period_time + 3,
                                     period_time > 297 & period == 4 & !is.na(game_stint) ~ 299.9,
                                     period_time == 300 & period == 4 & is.na(game_stint) ~ NA),
    TimeEndThreshold5Sec = case_when(period_time <= 1195 & period != 4 ~ period_time + 5,
                                     period_time > 1195 & period != 4 ~ 1200,
                                     period_time <= 295 & period == 4 ~ period_time + 5,
                                     period_time > 295 & period == 4 & !is.na(game_stint) ~ 299.9,
                                     period_time == 300 & period == 4 & is.na(game_stint) ~ NA),
    TimeEndThreshold7Sec = case_when(period_time <= 1193 & period != 4 ~ period_time + 7,
                                     period_time > 1193 & period != 4 ~ 1200,
                                     period_time <= 293 & period == 4 ~ period_time + 7,
                                     period_time > 293 & period == 4 & !is.na(game_stint) ~ 299.9,
                                     period_time == 300 & period == 4 & is.na(game_stint) ~ NA),
    TimeEndThreshold10Sec = case_when(period_time <= 1190 & period != 4 ~ period_time + 10,
                                     period_time > 1190 & period != 4 ~ 1200,
                                     period_time <= 290 & period == 4 ~ period_time + 10,
                                     period_time > 290 & period == 4 & !is.na(game_stint) ~ 299.9,
                                     period_time == 300 & period == 4 & is.na(game_stint) ~ NA),
    TimeEndThreshold11Sec = case_when(period_time <= 1189 & period != 4 ~ period_time + 11,
                                      period_time > 1189 & period != 4 ~ 1200,
                                      period_time <= 289 & period == 4 ~ period_time + 11,
                                      period_time > 289 & period == 4 & !is.na(game_stint) ~ 299.9,
                                      period_time == 300 & period == 4 & is.na(game_stint) ~ NA),
    TimeEndThreshold11Sec = case_when(period_time <= 1189 & period != 4 ~ period_time + 11,
                                      period_time > 1189 & period != 4 ~ 1200,
                                      period_time <= 289 & period == 4 ~ period_time + 11,
                                      period_time > 289 & period == 4 & !is.na(game_stint) ~ 299.9,
                                      period_time == 300 & period == 4 & is.na(game_stint) ~ NA),
    TimeEndThreshold12Sec = case_when(period_time <= 1188 & period != 4 ~ period_time + 12,
                                      period_time > 1188 & period != 4 ~ 1200,
                                      period_time <= 288 & period == 4 ~ period_time + 12,
                                      period_time > 288 & period == 4 & !is.na(game_stint) ~ 299.9,
                                      period_time == 300 & period == 4 & is.na(game_stint) ~ NA),
    TimeEndThreshold12Sec = case_when(period_time <= 1188 & period != 4 ~ period_time + 12,
                                      period_time > 1188 & period != 4 ~ 1200,
                                      period_time <= 288 & period == 4 ~ period_time + 12,
                                      period_time > 288 & period == 4 & !is.na(game_stint) ~ 299.9,
                                      period_time == 300 & period == 4 & is.na(game_stint) ~ NA),
    TimeEndThreshold15Sec = case_when(period_time <= 1185 & period != 4 ~ period_time + 15,
                                      period_time > 1185 & period != 4 ~ 1200,
                                      period_time <= 285 & period == 4 ~ period_time + 15,
                                      period_time > 285 & period == 4 & !is.na(game_stint) ~ 299.9,
                                      period_time == 300 & period == 4 & is.na(game_stint) ~ NA),
    TimeEndThreshold17Sec = case_when(period_time <= 1183 & period != 4 ~ period_time + 17,
                                      period_time > 1183 & period != 4 ~ 1200,
                                      period_time <= 283 & period == 4 ~ period_time + 17,
                                      period_time > 283 & period == 4 & !is.na(game_stint) ~ 299.9,
                                      period_time == 300 & period == 4 & is.na(game_stint) ~ NA),
    TimeEndThreshold20Sec = case_when(period_time <= 1180 & period != 4 ~ period_time + 20,
                                      period_time > 1180 & period != 4 ~ 1200,
                                      period_time <= 280 & period == 4 ~ period_time + 20,
                                      period_time > 280 & period == 4 & !is.na(game_stint) ~ 299.9,
                                      period_time == 300 & period == 4 & is.na(game_stint) ~ NA)
    )


HALOEvents_xG_dt <- as.data.table(HALOEventsTimeToNextTeamGoalEndThresholdTime)
HALOEvents_xG_dt[, row_id := .I]

ShotsWithxG <- HALOEvents_xG_dt[
  !is.na(sl_xg_all_shots),
  .(game_id, period, period_time, sl_xg_all_shots, team)
]
setkey(ShotsWithxG, game_id, period, period_time, period_time)

get_cumulative_xg <- function(events_dt, threshold_col) {
  
  intervals <- events_dt[, .(
    row_id,
    game_id,
    period,
    event_team = team, 
    start_time = period_time,
    end_time   = get(threshold_col)
  )]
  intervals <- intervals[!is.na(end_time)]
  setkey(intervals, game_id, period, start_time, end_time)
  
  ShotsWithxG_interval <- copy(ShotsWithxG)
  ShotsWithxG_interval[, period_time_end := period_time]
  setkey(ShotsWithxG_interval, game_id, period, period_time, period_time_end)
  
  joined <- foverlaps(
    ShotsWithxG_interval,
    intervals,
    by.x = c("game_id", "period", "period_time", "period_time_end"),
    by.y = c("game_id", "period", "start_time", "end_time"),
    type = "within",
    nomatch = NA
  )
  
  xg_sums <- joined[
    !is.na(row_id) & team == event_team,
    .(cumulative_xg = sum(sl_xg_all_shots, na.rm = TRUE),
      shot_count    = .N),  # .N counts rows per group
    by = row_id
  ]
  
  all_rows <- events_dt[, .(row_id)]
  xg_sums <- xg_sums[all_rows, on = "row_id"]
  xg_sums[is.na(cumulative_xg), cumulative_xg := 0]
  xg_sums[is.na(shot_count),    shot_count    := 0L]
  
  xg_sums
}

thresholds <- c(
  "TimeEndThreshold3Sec",  "TimeEndThreshold5Sec",  "TimeEndThreshold7Sec",
  "TimeEndThreshold10Sec", "TimeEndThreshold11Sec",  "TimeEndThreshold12Sec",
  "TimeEndThreshold15Sec", "TimeEndThreshold17Sec",  "TimeEndThreshold20Sec"
)

xg_col_names <- c(
  "CumulativexG_3Sec",  "CumulativexG_5Sec",  "CumulativexG_7Sec",
  "CumulativexG_10Sec", "CumulativexG_11Sec", "CumulativexG_12Sec",
  "CumulativexG_15Sec", "CumulativexG_17Sec", "CumulativexG_20Sec"
)

shot_count_col_names <- c(
  "ShotCount_3Sec",  "ShotCount_5Sec",  "ShotCount_7Sec",
  "ShotCount_10Sec", "ShotCount_11Sec", "ShotCount_12Sec",
  "ShotCount_15Sec", "ShotCount_17Sec", "ShotCount_20Sec"
)

for (i in seq_along(thresholds)) {
  xg_result <- get_cumulative_xg(HALOEvents_xG_dt, thresholds[i])
  setnames(xg_result, 
           c("cumulative_xg", "shot_count"), 
           c(xg_col_names[i], shot_count_col_names[i]))
  HALOEvents_xG_dt <- xg_result[HALOEvents_xG_dt, on = "row_id"]
}

HALOEventsWithCumulativexG <- as_tibble(HALOEvents_xG_dt)


Plot_xGPer300Shots_ByEventType <- function(EventsDFWith_xG,
                                           event_types = NULL,        # NULL = all event types
                                           exclude_events = "shot",
                                           include_all = TRUE) {       # adds a thick "All Events" reference line
  
  secs <- c(3, 5, 7, 10, 11, 12, 15, 17, 20)
  
  df <- EventsDFWith_xG %>%
    filter(!event_type %in% exclude_events)
  
  if (!is.null(event_types)) {
    df <- df %>% filter(event_type %in% event_types)
  }
  
  # xG per 300 shots at each threshold (same logic as your single-event function)
  for (s in secs) {
    cum_col <- paste0("CumulativexG_", s, "Sec")
    cnt_col <- paste0("ShotCount_", s, "Sec")
    df[[paste0("xGPer300Shots_", s)]] <- ifelse(df[[cnt_col]] > 0,
                                                (df[[cum_col]] / df[[cnt_col]]) * 300,
                                                0)
  }
  
  if (include_all) {
    df <- bind_rows(df, df %>% mutate(event_type = "All Events"))
  }
  
  DFForPlotting <- df %>%
    group_by(event_type) %>%
    summarise(across(starts_with("xGPer300Shots_"), ~ mean(.x, na.rm = TRUE)),
              .groups = "drop") %>%
    pivot_longer(
      cols = starts_with("xGPer300Shots_"),
      names_to = "Seconds",
      names_prefix = "xGPer300Shots_",
      values_to = "Avg_xGPer300Shots"
    ) %>%
    mutate(
      Seconds = as.numeric(Seconds),
      IsAll = event_type == "All Events"
    )
  
  ggplot(DFForPlotting,
         aes(x = Seconds, y = Avg_xGPer300Shots, color = event_type, group = event_type)) +
    geom_line(aes(linewidth = IsAll, alpha = IsAll)) +
    geom_point(aes(alpha = IsAll), size = 1.8) +
    scale_linewidth_manual(values = c(`FALSE` = 0.7, `TRUE` = 1.8), guide = "none") +
    scale_alpha_manual(values = c(`FALSE` = 0.75, `TRUE` = 1), guide = "none") +
    scale_x_continuous(breaks = secs) +
    labs(
      x = "Seconds Until Next Team Goal",
      y = "xG Per 300 Shots",
      title = "Average xG Per 300 Shots Within X Seconds, by Event Type",
      subtitle = paste0("Excluded: ", paste(exclude_events, collapse = ", ")),
      color = "Event Type"
    ) +
    theme_minimal()
}
  

Plot_xGPer300Shots(HALOEventsWithCumulativexG, "lpr")
Plot_xGPer300Shots(HALOEventsWithCumulativexG, "shot")
Plot_xGPer300Shots(HALOEventsWithCumulativexG, "faceoff")
Plot_xGPer300Shots(HALOEventsWithCumulativexG, "pass")
Plot_xGPer300Shots(HALOEventsWithCumulativexG, "reception")
Plot_xGPer300Shots(HALOEventsWithCumulativexG, "dumpout")
Plot_xGPer300Shots(HALOEventsWithCumulativexG, "puckprotection")
Plot_xGPer300Shots(HALOEventsWithCumulativexG, "dumpin")
Plot_xGPer300Shots(HALOEventsWithCumulativexG, "dumpinagainst")
Plot_xGPer300Shots(HALOEventsWithCumulativexG, "controlledbreakout")
Plot_xGPer300Shots(HALOEventsWithCumulativexG, "pressure")
Plot_xGPer300Shots(HALOEventsWithCumulativexG, "check")
Plot_xGPer300Shots(HALOEventsWithCumulativexG, "receptionprevention")
Plot_xGPer300Shots(HALOEventsWithCumulativexG, "carry")
Plot_xGPer300Shots(HALOEventsWithCumulativexG, "block")
Plot_xGPer300Shots_ByEventType(HALOEventsWithCumulativexG, event_types = c("lpr", "faceoff", "pass", "reception", "dumpout", "puckprotection",
                                                                           "dumpin", "dumpinagainst", "controlledbreakout", "pressure", "check",
                                                                           "receptionprevention","carry","block"))


#find slopes from the the xG Per 300 Shots
Slopes_xGPer300Shots <- function(EventsDFWith_xG, event_type_choice) {
  
  specific_event_dataframe <- EventsDFWith_xG %>%
    filter(event_type == event_type_choice) %>%
    mutate(
      xGPer300Shots_3  = ifelse(ShotCount_3Sec  > 0, (CumulativexG_3Sec  / ShotCount_3Sec)  * 300, 0),
      xGPer300Shots_5  = ifelse(ShotCount_5Sec  > 0, (CumulativexG_5Sec  / ShotCount_5Sec)  * 300, 0),
      xGPer300Shots_7  = ifelse(ShotCount_7Sec  > 0, (CumulativexG_7Sec  / ShotCount_7Sec)  * 300, 0),
      xGPer300Shots_10 = ifelse(ShotCount_10Sec > 0, (CumulativexG_10Sec / ShotCount_10Sec) * 300, 0),
      xGPer300Shots_11 = ifelse(ShotCount_11Sec > 0, (CumulativexG_11Sec / ShotCount_11Sec) * 300, 0),
      xGPer300Shots_12 = ifelse(ShotCount_12Sec > 0, (CumulativexG_12Sec / ShotCount_12Sec) * 300, 0),
      xGPer300Shots_15 = ifelse(ShotCount_15Sec > 0, (CumulativexG_15Sec / ShotCount_15Sec) * 300, 0),
      xGPer300Shots_17 = ifelse(ShotCount_17Sec > 0, (CumulativexG_17Sec / ShotCount_17Sec) * 300, 0),
      xGPer300Shots_20 = ifelse(ShotCount_20Sec > 0, (CumulativexG_20Sec / ShotCount_20Sec) * 300, 0)
    )
  
  specific_event_dataframe_long <- specific_event_dataframe %>%
    summarise(
      xGPer300Shots_3  = mean(xGPer300Shots_3,  na.rm = TRUE),
      xGPer300Shots_5  = mean(xGPer300Shots_5,  na.rm = TRUE),
      xGPer300Shots_7  = mean(xGPer300Shots_7,  na.rm = TRUE),
      xGPer300Shots_10 = mean(xGPer300Shots_10, na.rm = TRUE),
      xGPer300Shots_11 = mean(xGPer300Shots_11, na.rm = TRUE),
      xGPer300Shots_12 = mean(xGPer300Shots_12, na.rm = TRUE),
      xGPer300Shots_15 = mean(xGPer300Shots_15, na.rm = TRUE),
      xGPer300Shots_17 = mean(xGPer300Shots_17, na.rm = TRUE),
      xGPer300Shots_20 = mean(xGPer300Shots_20, na.rm = TRUE)
    ) %>%
    pivot_longer(
      cols = everything(),
      names_to = "Seconds",
      values_to = "Avg_xGPer300Shots"
    ) %>%
    mutate(
      Seconds = as.numeric(gsub("xGPer300Shots_(\\d+)", "\\1", Seconds))
    ) %>%
    arrange(Seconds) %>%
    mutate(
      Slope     = (Avg_xGPer300Shots - lag(Avg_xGPer300Shots)) / (Seconds - lag(Seconds)),
      SlopeName = paste0("Slope_", lag(Seconds), "_", Seconds)
    ) %>%
    filter(!is.na(Slope))
  
  specific_event_dataframe_slopes_wide <- specific_event_dataframe_long %>%
    select(SlopeName, Slope) %>%
    pivot_wider(names_from = SlopeName, values_from = Slope) %>%
    mutate(event_type = event_type_choice) %>%
    relocate(event_type)
  
  return(specific_event_dataframe_slopes_wide)
}

LPRSlopes_xGPer300Shots <- Slopes_xGPer300Shots(HALOEventsWithCumulativexG, "lpr")
ShotSlopes_xGPer300Shots <- Slopes_xGPer300Shots(HALOEventsWithCumulativexG, "shot")
FaceoffSlopes_xGPer300Shots <- Slopes_xGPer300Shots(HALOEventsWithCumulativexG, "faceoff")
PassSlopes_xGPer300Shots <- Slopes_xGPer300Shots(HALOEventsWithCumulativexG, "pass")
ReceptionSlopes_xGPer300Shots <- Slopes_xGPer300Shots(HALOEventsWithCumulativexG, "reception")
DumpoutSlopes_xGPer300Shots <- Slopes_xGPer300Shots(HALOEventsWithCumulativexG, "dumpout")
PuckProtectionSlopes_xGPer300Shots <- Slopes_xGPer300Shots(HALOEventsWithCumulativexG, "puckprotection")
DumpInSlopes_xGPer300Shots <- Slopes_xGPer300Shots(HALOEventsWithCumulativexG, "dumpin")
DumpInAgainstSlopes_xGPer300Shots <- Slopes_xGPer300Shots(HALOEventsWithCumulativexG, "dumpinagainst")
ControlledBreakoutSlopes_xGPer300Shots <- Slopes_xGPer300Shots(HALOEventsWithCumulativexG, "controlledbreakout")
PressureSlopes_xGPer300Shots <- Slopes_xGPer300Shots(HALOEventsWithCumulativexG, "pressure")
CheckSlopes_xGPer300Shots <- Slopes_xGPer300Shots(HALOEventsWithCumulativexG, "check")
ReceptionPreventionSlopes_xGPer300Shots <- Slopes_xGPer300Shots(HALOEventsWithCumulativexG, "receptionprevention")
CarrySlopes_xGPer300Shots <- Slopes_xGPer300Shots(HALOEventsWithCumulativexG, "carry")
BlockSlopes_xGPer300Shots <- Slopes_xGPer300Shots(HALOEventsWithCumulativexG, "block")


#not including shots in the slope comparison because it has a different smoothed distribution
xGPer300ShotsSlopeComparisonDataframe <- bind_rows(LPRSlopes_xGPer300Shots,
                                      FaceoffSlopes_xGPer300Shots, PassSlopes_xGPer300Shots, ReceptionSlopes_xGPer300Shots,
                                      DumpoutSlopes_xGPer300Shots, PuckProtectionSlopes_xGPer300Shots,
                                      DumpInSlopes_xGPer300Shots, DumpInAgainstSlopes_xGPer300Shots, ControlledBreakoutSlopes_xGPer300Shots,
                                      PressureSlopes_xGPer300Shots, CheckSlopes_xGPer300Shots, ReceptionPreventionSlopes_xGPer300Shots, 
                                      CarrySlopes_xGPer300Shots, BlockSlopes_xGPer300Shots)

range(xGPer300ShotsSlopeComparisonDataframe$Slope_3_5)
range(xGPer300ShotsSlopeComparisonDataframe$Slope_5_7)
range(xGPer300ShotsSlopeComparisonDataframe$Slope_7_10)
range(xGPer300ShotsSlopeComparisonDataframe$Slope_10_11)
range(xGPer300ShotsSlopeComparisonDataframe$Slope_11_12)
range(xGPer300ShotsSlopeComparisonDataframe$Slope_12_15)
range(xGPer300ShotsSlopeComparisonDataframe$Slope_15_17)
range(xGPer300ShotsSlopeComparisonDataframe$Slope_17_20)

