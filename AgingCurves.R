#Aging Curves
library(dplyr)
library(zoo)
library(ggplot2)
library(data.table)
library(purrr)
library(stringr)

NHLSeasonsStartEnd <- data.frame(Season = c("2007-08","2008-09", "2009-10", "2010-11",
                                               "2011-12", "2013", "2013-14", "2014-15", "2015-16",
                                               "2016-17", "2017-18", "2018-19", "2019-20", "2020-21",
                                               "2021-22", "2022-23","2023-24", "2024-25"),
                                    StartDate = c(as.Date("2007-09-27"),as.Date("2008-10-04"),
                                                  as.Date("2009-10-01"),as.Date("2010-10-07"),
                                                  as.Date("2011-10-06"), as.Date("2013-01-19"),
                                                  as.Date("2013-10-01"), as.Date("2014-10-08"),
                                                  as.Date("2015-10-07"), as.Date("2016-10-12"),
                                                  as.Date("2017-10-04"), as.Date("2018-10-03"),
                                                  as.Date("2019-10-02"), as.Date("2021-01-13"),
                                                  as.Date("2021-10-12"), as.Date("2022-10-07"),
                                                  as.Date("2023-10-10"), as.Date("2024-10-04")),
                                    EndDate = c(as.Date("2008-06-04"), as.Date("2009-06-12"),
                                                as.Date("2010-06-09"), as.Date("2011-06-15"),
                                                as.Date("2012-06-11"), as.Date("2013-06-24"),
                                                as.Date("2014-06-13"), as.Date("2015-06-15"),
                                                as.Date("2016-06-12"), as.Date("2017-06-11"),
                                                as.Date("2018-06-07"), as.Date("2019-06-12"),
                                                as.Date("2020-09-28"), as.Date("2021-07-07"),
                                                as.Date("2022-06-26"), as.Date("2023-06-13"),
                                                as.Date("2024-06-24"), as.Date("2025-06-17")))

GameScore <- function(df1, df2, season_dates = NHLSeasonsStartEnd){
  df3 <- merge(df1, df2, by = "Game")
  df4 <- df3 %>%
    mutate(
      Month = substr(Game, 6, 7),
      Year = substr(Game, 1, 4),
      Month.Year = paste0(Month, ".", Year),
      Date = as.Date(substr(Game, 1, 10))
    )
  dt_games <- as.data.table(df4) %>%
    mutate(GameDate_A = Date, GameDate_B = Date) 
  dt_seasons <- as.data.table(season_dates)
  setkey(dt_seasons, StartDate, EndDate)
  
  df5_dt <- foverlaps(
    dt_games, 
    dt_seasons, 
    by.x = c("GameDate_A", "GameDate_B"),
    by.y = c("StartDate", "EndDate"), 
    type = "within", 
    nomatch = 0
  )

  original_cols <- names(df4)
  
  df5 <- df5_dt %>%
    select(all_of(original_cols), Season, StartDate, EndDate) %>% 
    as.data.frame()

  df6 <- df5 %>%
    mutate(
      Season_Rank = factor(Season, levels = unique(Season)) %>% as.numeric(),
      PlayerYear = case_when(
        Season_Rank == 1 ~ "Year1",
        Season_Rank == 2 ~ "Year2",
        TRUE ~ "Other" # Failsafe
      ),
      Month.PlayerYear = paste0(Month,".",PlayerYear),
      CareerGameNumber = row_number(),
      GameScore = ((0.75*Goals)+(0.7*First.Assists)+(0.55*Second.Assists)+
                     (0.075*Shots)+(0.05*Shots.Blocked)+((0.15*Penalties.Drawn)-(0.15*Total.Penalties))+
                     ((0.01*Faceoffs.Won)-(0.01*Faceoffs.Lost))+(0.625*xGF)+(0.625*GF)-(1.75*xGA)-(0.4375*GA))
    )
  
  return(df6)
}

RollingAverageGGPlotMaker <- function(df, player, pick_color){
  df1 <- df %>%
    mutate(
      GS5GameRollMean = rollmean(GameScore, k=5, fill=NA, align = 'right')
    ) %>%
    filter(!is.na(GS5GameRollMean))

  label_df <- df1 %>%
    group_by(Month.PlayerYear) %>%
    summarise(
      Label_Position = min(CareerGameNumber), # Game number where the month begins
      .groups = 'drop'
    )
  
  ggplot(data=df1, aes(x=CareerGameNumber)) + 
    
    geom_point(aes(y = GameScore), color = "gray50", alpha = 0.3) +
    
    geom_line(aes(y = GS5GameRollMean), color = pick_color, linewidth = 1) +
    
    geom_smooth(
      aes(y = GS5GameRollMean),
      method = "loess",
      span = 0.15, 
      color = "black", 
      linewidth = 1.5,
      se = FALSE
    ) +
    
    scale_x_continuous(
      breaks = label_df$Label_Position,
      labels = label_df$Month.PlayerYear,
      expand = expansion(mult = c(0.01, 0.05))
    ) +
    
    labs(
      title = paste0(player, ": 5-Game Rolling Game Score First Two Seasons"),
      caption = "Game Score by @domluszczyszyn, no QoC or QoT adjustments",
      x = "Game Number (Labeled by Month)",
      y = "Game Score & Rolling Mean"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

PatrickKaneGS <- as.data.frame(GameScore(PatrickKane2007_09,PatrickKane2007_09Indiv))

PatrickKaneGSSummary <- PatrickKaneGS %>%
  mutate(GS5GameRollMean = rollmean(GameScore, k=5, fill=NA, align = 'right'))

ggplot(data=PatrickKaneGSSummary, aes(x=CareerGameNumber, y=Value)) + 
  geom_line(aes(y = GS5GameRollMean), color = "red", linewidth = 1) +
  geom_smooth(
    aes(y = GS5GameRollMean),
    method = "loess",
    span = 0.15, 
    color = "darkred", 
    linewidth = 1.5,
    se = FALSE
  ) +
  geom_point(aes(y = GameScore), color = "gray50", alpha = 0.3) +
  labs(
    title = "Patrick Kane: 5-Game Rolling Game Score First Two Seasons (2007-08 and 2008-09)",
    caption = "Game Score by @domluszczyszyn, no QoC or QoT adjustments",
    x = "Game Number",
    y = "5-Game Rolling Mean Score"
  ) +
  theme_minimal()

on_ice_dataframes <- str_sub(on_ice_csv_list, end = -5)
indiv_dataframes <- str_sub(indiv_csv_list, end = -5)
devel_player_names <- str_sub(on_ice_dataframes, end = -8)

All_PlayersGS <- data.frame()
for (i in seq_along(devel_player_names)) {
  new_name <- paste0(devel_player_names[i], "GS")
  df1_name <- on_ice_dataframes[i]
  df2_name <- indiv_dataframes[i]
  df1_object <- get(df1_name)
  df2_object <- get(df2_name)
  df_result <- as.data.frame(GameScore(df1_object, df2_object))
  df_result2 <- df_result %>%
    mutate(PlayerName = devel_player_names[i])
  All_PlayersGS <- rbind(All_PlayersGS, df_result2)
  assign(new_name, df_result, envir = .GlobalEnv)
}

LeoCarlssonGS <- as.data.frame(GameScore(LeoCarlsson2023_25, LeoCarlsson2023_25Indiv))
RollingAverageGGPlotMaker(LeoCarlssonGS, "Leo Carlsson", "orange")

NathanMackinnonGS <- as.data.frame(GameScore(NathanMacKinnon2013_15, NathanMacKinnon2013_15Indiv))
RollingAverageGGPlotMaker(NathanMackinnonGS, "Nathan MacKinnon", "#6F263D")

AndreiSvechnikovGS <- as.data.frame(GameScore(AndreiSvechnikov2018_20, AndreiSvechnikov2018_20Indiv))
RollingAverageGGPlotMaker(AndreiSvechnikovGS, "Andrei Svechnikov", "#CE1126")

ConnorBedardGS <- as.data.frame(GameScore(ConnorBedard2023_25, ConnorBedard2023_25Indiv))
RollingAverageGGPlotMaker(ConnorBedardGS, "Connor Bedard", "#CF0A2C")

ConnorMcDavidGS <- as.data.frame(GameScore(ConnorMcDavid2015_17, ConnorMcDavid2015_17Indiv))
RollingAverageGGPlotMaker(ConnorMcDavidGS, "Connor McDavid", "#FF4C00")

BarkovGS <- as.data.frame(GameScore(AleksanderBarkov2013_15, AleksanderBarkov2013_15Indiv))
RollingAverageGGPlotMaker(BarkovGS, "Aleksander Barkov", "#c8102E")

MatthewTkachukGS <- as.data.frame(GameScore(MatthewTkachuk2016_18, MatthewTkachuk2016_18Indiv))
RollingAverageGGPlotMaker(MatthewTkachukGS, "Matthew Tkachuk", "#c8102E")

RollingAverageGGPlotMaker(AustonMatthewsGS, "Auston Matthews", "blue")

PlayerGSSummaryByMonth <- All_PlayersGS %>%
  filter(Month != "05") %>%
  group_by(Month.PlayerYear) %>%
  summarise(Avg_GS = mean(GameScore, na.rm = TRUE),
            GameSampleSize = n(),
            PlayerSampleSize = n_distinct(PlayerName),
            SE_GS = sd(GameScore, na.rm = TRUE)/sqrt(n()),
            .groups = "drop"
            )

PlayerGSSummaryByMonth$Month.PlayerYear <- factor(PlayerGSSummaryByMonth$Month.PlayerYear,
                                                  levels = c("10.Year1", "11.Year1", "12.Year1",
                                                             "01.Year1", "02.Year1", "03.Year1",
                                                             "04.Year1", "05.Year1",
                                                             "10.Year2", "11.Year2", "12.Year2",
                                                             "01.Year2", "02.Year2", "03.Year2",
                                                             "04.Year2", "05.Year2"))

ggplot(data = PlayerGSSummaryByMonth, aes(x = Month.PlayerYear, y = Avg_GS)) + 
  geom_point(color = "black", size = 3) + 
  geom_line(aes(y = Avg_GS, group = 1), color = "red", linewidth = 1) +
  geom_smooth(
    aes(x = as.numeric(Month.PlayerYear), y = Avg_GS, group = 1), 
    method = "loess",
    span = 0.5,
    color = "darkblue",
    linewidth = 1.5,
    se = FALSE
  ) +
  labs(
    title = "Average Game Score Trend by Month and Player Career Year",
    subtitle = "Players Who Burned First Year of ELC the Same Year as DY Only",
    caption = "Game Score by @domluszczyszyn, no QoC or QoT adjustments",
    x = "Career Month (e.g., Year1 vs. Year2)",
    y = "Average Game Score"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


PlayerGSSummaryByMonthDelta <- PlayerGSSummaryByMonth %>%
  arrange(Month.PlayerYear) %>%
  mutate(Delta = Avg_GS-lag(Avg_GS),
         TimePeriod = paste0(lag(Month.PlayerYear),"/",Month.PlayerYear)) %>%
  filter(!is.na(Delta))

PlayerGSSummaryByMonthDelta$TimePeriod <- factor(PlayerGSSummaryByMonthDelta$TimePeriod,
                                                  levels = c("10.Year1/11.Year1", "11.Year1/12.Year1", "12.Year1/01.Year1",
                                                             "01.Year1/02.Year1", "02.Year1/03.Year1", "03.Year1/04.Year1",
                                                             "04.Year1/10.Year2", "10.Year2/11.Year2", "11.Year2/12.Year2",
                                                             "12.Year2/01.Year2", "01.Year2/02.Year2", "02.Year2/03.Year2",
                                                             "03.Year2/04.Year2"))

ggplot(data = PlayerGSSummaryByMonthDelta %>% arrange(TimePeriod), aes(x = TimePeriod, y = Delta)) + 
  geom_point(color = "black", size = 3) + 
  geom_line(aes(y = Delta, group = 1), color = "red", linewidth = 1) +
  geom_smooth(
    aes(x = as.numeric(TimePeriod), y = Delta, group = 1), 
    method = "loess",
    span = 0.5,
    color = "darkblue",
    linewidth = 1.5,
    se = FALSE
  ) +
  labs(
    title = "Average Game Score Delta by Month and Player Career Year",
    subtitle = "Players Who Burned First Year of ELC the Same Year as DY Only",
    caption = "Game Score by @domluszczyszyn, no QoC or QoT adjustments",
    x = "Time Period of Change",
    y = "Average Game Score Change"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


