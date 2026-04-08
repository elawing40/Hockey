#Big Data Cup 2026
#Want to look at teammate interactions and finding the players with the best chemistry
#Event tracking data will look at both direct and indirect passes and plays to determine chemistry
#Want to give a rating and find the players with the most chemistry
#Will use the tracking data to visualize the actual play that happened, possibly rate the type of play and determine distance traveled
library(gganimate)
library(lubridate)
library(dplyr)
library(parallel)
library(doParallel)
library(foreach)
library(data.table)
library(ggplot2)
library(cluster)
library(GDAtools)
library(xgboost)
library(fastDummies)

FirstFrameTeam.A.at.Team.D.Tracking_P1.2025.10.11 <- head(Team.A.at.Team.D.Tracking_P1.2025.10.11, n=11)

nhl_rink_plot() + theme_void() + 
  geom_point(data = FirstFrameTeam.A.at.Team.D.Tracking_P1.2025.10.11, 
             aes(x=Rink.Location.X..Feet., y=Rink.Location.Y..Feet., color = Team))



#function to animate a period
AnimatePeriod <- function(df){
  df <- df %>%
    arrange(Image.Id)
  n_frames <- length(unique(df$Image.Id))
  p <- nhl_rink_plot() + 
    theme_void() + 
    geom_point(data = df,
               aes(x = Rink.Location.X..Feet., 
                   y = Rink.Location.Y..Feet., 
                   color = Team)) +
    transition_manual(Image.Id) +
    labs(title = "Frame: {current_frame}")
  animate(p, fps = 5, nframes = n_frames)
}


#looking at which players have the most plays together
TeamAAtTeamDEventsPlaySummary <- Team.A.at.Team.D.Events.2025.10.11 %>%
  group_by(Team, Player_Id, Player_Id_2) %>%
  summarise(TotalPlays = sum(Event=="Play"),
            DirectPlays = sum(Detail_1=="Direct" & Event=="Play"),
            IndirectPlays = sum(Detail_1=="Indirect" & Event=="Play"),
            .groups = 'drop') %>%
  arrange(desc(TotalPlays))

AnimatePlay <- function(df, PeriodTime){
  df <- df %>%
    arrange(Image.Id)
  df2 <- df %>%
    mutate(GameClockAsTime = ms(Game.Clock))
  if(is.character(PeriodTime)){
    PeriodTime <- ms(PeriodTime)
  }
  PeriodTimeBefore <- PeriodTime + seconds(3)
  PeriodTimeAfter <- PeriodTime - seconds(3)
  PeriodTimeBefore <- pmin(PeriodTimeBefore, ms("20:00"))
  PeriodTimeAfter <- pmax(PeriodTimeAfter, ms("0:00"))
  df3 <- df2 %>%
    filter(GameClockAsTime <= PeriodTimeBefore & GameClockAsTime >= PeriodTimeAfter)
  p <- nhl_rink_plot() + 
    theme_void() + 
    geom_point(data = df3,
               aes(x = Rink.Location.X..Feet., 
                   y = Rink.Location.Y..Feet., 
                   color = Team),
               size = 4) +
    geom_text(data = df3,
              aes(x = Rink.Location.X..Feet., 
                  y = Rink.Location.Y..Feet., 
                  label = Player.Jersey.Number),
              size = 3,
              color = "black",
              fontface = "bold") +
    transition_manual(Image.Id) +
    labs(title = "Frame: {current_frame}")
  animate(p, fps = 5)
}

#function to convert the tracking data to wide format
SpecialtyPivotWider <- function(df){
  AllImage.Ids <- unique(df$Image.Id)
  WideRows <- vector("list", length = length(AllImage.Ids))
  for (Id in seq_along(AllImage.Ids)) {
    current_id <- AllImage.Ids[Id] 
    frame_data <- df %>%
      filter(Image.Id == current_id)
    puck_data <- frame_data %>%
      filter(Player.or.Puck == "Puck")
  puck_location.x <- puck_data$Rink.Location.X..Feet.
  puck_location.y <- puck_data$Rink.Location.Y..Feet.
  home_players <- frame_data %>%
    filter(Team == "Home") %>%
    mutate(DistFromPuck = sqrt((Rink.Location.X..Feet. - puck_location.x)^2 + 
                          (Rink.Location.Y..Feet. - puck_location.y)^2)) %>%
    arrange(DistFromPuck) %>%  # NA values go to end
    select(Rink.Location.X..Feet., Rink.Location.Y..Feet.)
  away_players <- frame_data %>%
    filter(Team == "Away") %>%
    mutate(DistFromPuck = sqrt((Rink.Location.X..Feet. - puck_location.x)^2 + 
                          (Rink.Location.Y..Feet. - puck_location.y)^2))%>%
    arrange(DistFromPuck) %>%
    select(Rink.Location.X..Feet., Rink.Location.Y..Feet.)
  home_coords <- as.numeric(t(home_players))
  away_coords <- as.numeric(t(away_players))
  if (length(home_coords) < 12) {
    home_coords <- c(home_coords, rep(NA, 12 - length(home_coords)))
  }
  if (length(away_coords) < 12) {
    away_coords <- c(away_coords, rep(NA, 12 - length(away_coords)))
  }
  period <- frame_data$Period[1]
  game_clock <- frame_data$Game.Clock[1]
  wide_row <- data.frame(
    Image.Id = current_id,
    Period = period,
    Game.Clock = game_clock,
    HomeSkater1.x = home_coords[1], HomeSkater1.y = home_coords[2],
    HomeSkater2.x = home_coords[3], HomeSkater2.y = home_coords[4],
    HomeSkater3.x = home_coords[5], HomeSkater3.y = home_coords[6],
    HomeSkater4.x = home_coords[7], HomeSkater4.y = home_coords[8],
    HomeSkater5.x = home_coords[9], HomeSkater5.y = home_coords[10],
    HomeSkater6.x = home_coords[11], HomeSkater6.y = home_coords[12],
    AwaySkater1.x = away_coords[1], AwaySkater1.y = away_coords[2],
    AwaySkater2.x = away_coords[3], AwaySkater2.y = away_coords[4],
    AwaySkater3.x = away_coords[5], AwaySkater3.y = away_coords[6],
    AwaySkater4.x = away_coords[7], AwaySkater4.y = away_coords[8],
    AwaySkater5.x = away_coords[9], AwaySkater5.y = away_coords[10],
    AwaySkater6.x = away_coords[11], AwaySkater6.y = away_coords[12],
    Puck.x = puck_location.x,
    Puck.y = puck_location.y
  )
  WideRows[[Id]] <- wide_row
  }
  result <- bind_rows(WideRows)
  
  return(result)
}

PivotWider_test_data <- Team.A.at.Team.D.Tracking_P1.2025.10.11. %>% 
  filter(Image.Id %in% unique(Image.Id)[300:310])

PivotWide_test <- SpecialtyPivotWider(PivotWider_test_data)


head(PivotWide_test)
dim(PivotWide_test)

#10/11/2025 game tracking data to wide format
TeamAatTeamDTrackingFull_raw <- rbind(
  Team.A.at.Team.D.Tracking_P1.2025.10.11.,
  Team.A.at.Team.D.Tracking_P2.2025.10.11,
  Team.A.at.Team.D.Tracking_P3.2025.10.11
)

TeamAatTeamDTrackingWideFull <- SpecialtyPivotWider(TeamAatTeamDTrackingFull_raw)

#10/11/2025 Team A at Team D game tracking data to wide format
TeamAatTeamDTrackingFull_raw <- rbind(
  Team.A.at.Team.D.Tracking_P1.2025.10.11.,
  Team.A.at.Team.D.Tracking_P2.2025.10.11,
  Team.A.at.Team.D.Tracking_P3.2025.10.11
)

TeamAatTeamDTrackingWideFull <- SpecialtyPivotWider(TeamAatTeamDTrackingFull_raw)


#10/17/25 Team F at Team L game tracking data to wide format
TeamFatTeamLTrackingFull_raw <- rbind(
  Team.F.at.Team.L.Tracking_P1.2025.10.17,
  Team.F.at.Team.L.Tracking_P2.2025.10.17,
  Team.F.at.Team.L.Tracking_P3.2025.10.17
)

TeamFatTeamLTrackingWideFull <- SpecialtyPivotWider(TeamFatTeamLTrackingFull_raw)


#10/24/25 Team B at Team A game tracking data to wide format
TeamBatTeamATrackingFull_raw <- rbind(
  Team.B.at.Team.A.Tracking_P1.2025.10.24,
  Team.B.at.Team.A.Tracking_P2.2025.10.24,
  Team.B.at.Team.A.Tracking_P3.2025.10.24
)

cl <- makeCluster(3)  
registerDoParallel(cl)
clusterExport(cl, "SpecialtyPivotWider")
clusterEvalQ(cl, library(dplyr))


TeamBatTeamAall_ids <- unique(TeamBatTeamATrackingFull_raw$Image.Id)
TeamBatTeamAchunks <- split(TeamBatTeamAall_ids, ceiling(seq_along(TeamBatTeamAall_ids) / 5000))


TeamBatTeamATrackingWideFull <- foreach(
  chunk = TeamBatTeamAchunks,
  .combine = rbind,
  .packages = c('dplyr')
) %dopar% {
  chunk_data <- TeamBatTeamATrackingFull_raw %>% filter(Image.Id %in% chunk)
  SpecialtyPivotWider(chunk_data)
}

stopCluster(cl)

#10/24/25 Team E at Team D game tracking data to wide format
TeamEatTeamDTrackingFull_raw <- rbind(
  Team.E.at.Team.D.Tracking_P1.2025.10.24,
  Team.E.at.Team.D.Tracking_P2.2025.10.24,
  Team.E.at.Team.D.Tracking_P3.2025.10.24,
  Team.E.at.Team.D.Tracking_POT.2025.10.24
)

cl <- makeCluster(3)  
registerDoParallel(cl)
clusterExport(cl, "SpecialtyPivotWider")
clusterEvalQ(cl, library(dplyr))


TeamEatTeamDall_ids <- unique(TeamEatTeamDTrackingFull_raw$Image.Id)
TeamEatTeamDchunks <- split(TeamEatTeamDall_ids, ceiling(seq_along(TeamEatTeamDall_ids) / 5000))


TeamEatTeamDTrackingWideFull <- foreach(
  chunk = TeamEatTeamDchunks,
  .combine = rbind,
  .packages = c('dplyr')
) %dopar% {
  chunk_data <- TeamEatTeamDTrackingFull_raw %>% filter(Image.Id %in% chunk)
  SpecialtyPivotWider(chunk_data)
}

stopCluster(cl)

#10/28/25 Team C at Team A game tracking data to wide format
TeamCatTeamATrackingFull_raw <- rbind(
  Team.C.at.Team.A.Tracking_P1.2025.10.28,
  Team.C.at.Team.A.Tracking_P2.2025.10.28,
  Team.C.at.Team.A.Tracking_P3.2025.10.28
)

cl <- makeCluster(3)  
registerDoParallel(cl)
clusterExport(cl, "SpecialtyPivotWider")
clusterEvalQ(cl, library(dplyr))


TeamCatTeamAall_ids <- unique(TeamCatTeamATrackingFull_raw$Image.Id)
TeamCatTeamAchunks <- split(TeamCatTeamAall_ids, ceiling(seq_along(TeamCatTeamAall_ids) / 5000))


TeamCatTeamATrackingWideFull <- foreach(
  chunk = TeamCatTeamAchunks,
  .combine = rbind,
  .packages = c('dplyr')
) %dopar% {
  chunk_data <- TeamCatTeamATrackingFull_raw %>% filter(Image.Id %in% chunk)
  SpecialtyPivotWider(chunk_data)
}

stopCluster(cl)

#10/29/25 Team G at Team L game tracking data to wide format
TeamGatTeamLTrackingFull_raw <- rbind(
  Team.G.at.Team.L.Tracking_P1.2025.10.29,
  Team.G.at.Team.L.Tracking_P2.2025.10.29,
  Team.G.at.Team.L.Tracking_P3.2025.10.29
)

cl <- makeCluster(3)  
registerDoParallel(cl)
clusterExport(cl, "SpecialtyPivotWider")
clusterEvalQ(cl, library(dplyr))


TeamGatTeamLall_ids <- unique(TeamGatTeamLTrackingFull_raw$Image.Id)
TeamGatTeamLchunks <- split(TeamGatTeamLall_ids, ceiling(seq_along(TeamGatTeamLall_ids) / 5000))


TeamGatTeamLTrackingWideFull <- foreach(
  chunk = TeamGatTeamLchunks,
  .combine = rbind,
  .packages = c('dplyr')
) %dopar% {
  chunk_data <- TeamGatTeamLTrackingFull_raw %>% filter(Image.Id %in% chunk)
  SpecialtyPivotWider(chunk_data)
}

stopCluster(cl)

#11/02/25 Team D at Team A game tracking data to wide format
TeamDatTeamATrackingFull_raw <- rbind(
  Team.D.at.Team.A.Tracking_P1.2025.11.02,
  Team.D.at.Team.A.Tracking_P2.2025.11.02,
  Team.D.at.Team.A.Tracking_P3.2025.11.02
)

cl <- makeCluster(3)  
registerDoParallel(cl)
clusterExport(cl, "SpecialtyPivotWider")
clusterEvalQ(cl, library(dplyr))


TeamDatTeamAall_ids <- unique(TeamDatTeamATrackingFull_raw$Image.Id)
TeamDatTeamAchunks <- split(TeamDatTeamAall_ids, ceiling(seq_along(TeamDatTeamAall_ids) / 5000))


TeamDatTeamATrackingWideFull <- foreach(
  chunk = TeamDatTeamAchunks,
  .combine = rbind,
  .packages = c('dplyr')
) %dopar% {
  chunk_data <- TeamDatTeamATrackingFull_raw %>% filter(Image.Id %in% chunk)
  SpecialtyPivotWider(chunk_data)
}

stopCluster(cl)

#11/16/25 Team H at Team K game tracking data to wide format
TeamHatTeamKTrackingFull_raw <- rbind(
  Team.H.at.Team.K.Tracking_P1.2025.11.16,
  Team.H.at.Team.K.Tracking_P2.2025.11.16,
  Team.H.at.Team.K.Tracking_P3.2025.11.16,
  Team.H.at.Team.K.Tracking_POT.2025.11.16
)

cl <- makeCluster(3)  
registerDoParallel(cl)
clusterExport(cl, "SpecialtyPivotWider")
clusterEvalQ(cl, library(dplyr))


TeamHatTeamKall_ids <- unique(TeamHatTeamKTrackingFull_raw$Image.Id)
TeamHatTeamKchunks <- split(TeamHatTeamKall_ids, ceiling(seq_along(TeamHatTeamKall_ids) / 5000))


TeamHatTeamKTrackingWideFull <- foreach(
  chunk = TeamHatTeamKchunks,
  .combine = rbind,
  .packages = c('dplyr')
) %dopar% {
  chunk_data <- TeamHatTeamKTrackingFull_raw %>% filter(Image.Id %in% chunk)
  SpecialtyPivotWider(chunk_data)
}

stopCluster(cl)

#11/19/25 Team I at Team K game tracking data to wide format
TeamIatTeamKTrackingFull_raw <- rbind(
  Team.I.at.Team.K.Tracking_P1.2025.11.19,
  Team.I.at.Team.K.Tracking_P2.2025.11.19,
  Team.I.at.Team.K.Tracking_P3.2025.11.19
)

cl <- makeCluster(3)  
registerDoParallel(cl)
clusterExport(cl, "SpecialtyPivotWider")
clusterEvalQ(cl, library(dplyr))


TeamIatTeamKall_ids <- unique(TeamIatTeamKTrackingFull_raw$Image.Id)
TeamIatTeamKchunks <- split(TeamIatTeamKall_ids, ceiling(seq_along(TeamIatTeamKall_ids) / 5000))


TeamIatTeamKTrackingWideFull <- foreach(
  chunk = TeamIatTeamKchunks,
  .combine = rbind,
  .packages = c('dplyr')
) %dopar% {
  chunk_data <- TeamIatTeamKTrackingFull_raw %>% filter(Image.Id %in% chunk)
  SpecialtyPivotWider(chunk_data)
}

stopCluster(cl)


#11/28/25 Team J at Team K game tracking data to wide format
TeamJatTeamKTrackingFull_raw <- rbind(
  Team.J.at.Team.K.Tracking_P1.2025.11.28,
  Team.J.at.Team.K.Tracking_P2.2025.11.28,
  Team.J.at.Team.K.Tracking_P3.2025.11.28,
  Team.J.at.Team.K.Tracking_POT.2025.11.28
)

cl <- makeCluster(3)  
registerDoParallel(cl)
clusterExport(cl, "SpecialtyPivotWider")
clusterEvalQ(cl, library(dplyr))


TeamJatTeamKall_ids <- unique(TeamJatTeamKTrackingFull_raw$Image.Id)
TeamJatTeamKchunks <- split(TeamJatTeamKall_ids, ceiling(seq_along(TeamJatTeamKall_ids) / 5000))


TeamJatTeamKTrackingWideFull <- foreach(
  chunk = TeamJatTeamKchunks,
  .combine = rbind,
  .packages = c('dplyr')
) %dopar% {
  chunk_data <- TeamJatTeamKTrackingFull_raw %>% filter(Image.Id %in% chunk)
  SpecialtyPivotWider(chunk_data)
}

stopCluster(cl)


#Importing all event data and adding the start and end of the play lookup period
AddPlayWindowRange <- function(df){
  PlaysOnly <- df %>%
    filter(Event == "Play" | Event == "Incomplete Play") %>%
    mutate(Clock = ms(Clock),
           StartOfPlayRange = pmin(Clock + seconds(3), ms("20:00")),
           EndOfPlayRange = pmax(Clock - seconds(3), ms("0:00")),
           PlayID = paste0(Date,Home_Team,Away_Team,Period,".",Clock))
  return(PlaysOnly)
}



head(AddPlayWindowRange(Team.A.at.Team.D.Events.2025.10.11), 3)
head(TeamAatTeamDTrackingWideFull, 3)

AddIDToTrackingDataDT <- function(TrackingDataWide, PlayData){
  tracking_dt <- as.data.table(TrackingDataWide)
  play_dt <- as.data.table(PlayData)
  
  cat("Original tracking rows:", nrow(tracking_dt), "\n")
  
  tracking_dt[, Period := as.character(Period)]
  play_dt[, Period := as.character(Period)]
  
  tracking_dt <- tracking_dt[!is.na(Game.Clock) & Game.Clock != ""]
  
  cat("Valid tracking rows:", nrow(tracking_dt), "\n")
  cat("Play rows:", nrow(play_dt), "\n")
  
  tracking_dt[, Game.Clock.Seconds := suppressWarnings(period_to_seconds(ms(Game.Clock)))]
  
  play_dt[, `:=`(
    Start.Seconds = suppressWarnings(period_to_seconds(StartOfPlayRange)),
    End.Seconds = suppressWarnings(period_to_seconds(EndOfPlayRange)),
    Play.Clock.Seconds = suppressWarnings(period_to_seconds(Clock))
  )]
  
  tracking_dt <- tracking_dt[!is.na(Game.Clock.Seconds)]
  play_dt <- play_dt[!is.na(Start.Seconds) & !is.na(End.Seconds)]
  
  cat("After time conversion - Tracking:", nrow(tracking_dt), "| Plays:", nrow(play_dt), "\n")
  
  play_dt_subset <- play_dt[, .(PlayID, Period, Start.Seconds, End.Seconds, Play.Clock.Seconds)]
  
  result <- tracking_dt[
    play_dt_subset,
    on = .(
      Period = Period,
      Game.Clock.Seconds <= Start.Seconds,
      Game.Clock.Seconds >= End.Seconds
    ),
    nomatch = NULL,
    allow.cartesian = TRUE
  ]
  
  cat("After join:", nrow(result), "matches\n")
  
  result[, TimeDistance := abs(Game.Clock.Seconds - Play.Clock.Seconds)]
  
  indices_to_keep <- result[, .I[which.min(TimeDistance)], by = .(Image.Id)]
  result <- result[indices_to_keep$V1]
  
  cat("After keeping closest:", nrow(result), "unique frames\n")
  
  # Clean up temporary columns
  result[, c("TimeDistance", "Game.Clock.Seconds", "Play.Clock.Seconds") := NULL]
  
  # Rename PlayID
  if ("PlayID" %in% names(result)) {
    setnames(result, "PlayID", "CorrespondingPlayID")
  }
  
  return(as.data.frame(result))
}


TeamAatTeamDWithIDs <- AddPlayWindowRange(Team.A.at.Team.D.Events.2025.10.11)
TeamAatTeamDWithIDsDirect <- TeamAatTeamDWithIDs %>%
  filter(Detail_1 == "Direct")
TeamAatTeamDTrackingWithIDsDirectPlays <- AddIDToTrackingDataDT(TeamAatTeamDTrackingWideFull, TeamAatTeamDWithIDsDirect)
TeamAatTeamDWithIDsIndirect <- TeamAatTeamDWithIDs %>%
  filter(Detail_1 == "Indirect")
TeamAatTeamDTrackingWithIDsIndirectPlays <- AddIDToTrackingDataDT(TeamAatTeamDTrackingWideFull, TeamAatTeamDWithIDsIndirect)


TeamFatTeamLWithIDs <- AddPlayWindowRange(Team.F.at.Team.L.Events2025.10.17)
TeamFatTeamLWithIDsDirect <- TeamFatTeamLWithIDs %>%
  filter(Detail_1 == "Direct")
TeamFatTeamLTrackingWithIDsDirectPlays <- AddIDToTrackingDataDT(TeamFatTeamLTrackingWideFull, TeamFatTeamLWithIDsDirect)
TeamFatTeamLWithIDsIndirect <- TeamFatTeamLWithIDs %>%
  filter(Detail_1 == "Indirect")
TeamFatTeamLTrackingWithIDsIndirectPlays <- AddIDToTrackingDataDT(TeamFatTeamLTrackingWideFull, TeamFatTeamLWithIDsIndirect)


TeamBatTeamAWithIDs <- AddPlayWindowRange(Team.B.at.Team.A.Events.2025.10.24)
TeamBatTeamAWithIDsDirect <- TeamBatTeamAWithIDs %>%
  filter(Detail_1 == "Direct")
TeamBatTeamATrackingWithIDsDirectPlays <- AddIDToTrackingDataDT(TeamBatTeamATrackingWideFull, TeamBatTeamAWithIDsDirect)
TeamBatTeamAWithIDsIndirect <- TeamBatTeamAWithIDs %>%
  filter(Detail_1 == "Indirect")
TeamBatTeamATrackingWithIDsIndirectPlays <- AddIDToTrackingDataDT(TeamBatTeamATrackingWideFull, TeamBatTeamAWithIDsIndirect)


TeamEatTeamDWithIDs <- AddPlayWindowRange(Team.E.at.Team.D.Events.2025.10.24)
TeamEatTeamDWithIDsDirect <- TeamEatTeamDWithIDs %>%
  filter(Detail_1 == "Direct")
TeamEatTeamDTrackingWithIDsDirectPlays <- AddIDToTrackingDataDT(TeamEatTeamDTrackingWideFull, TeamEatTeamDWithIDsDirect)
TeamEatTeamDWithIDsIndirect <- TeamEatTeamDWithIDs %>%
  filter(Detail_1 == "Indirect")
TeamEatTeamDTrackingWithIDsIndirectPlays <- AddIDToTrackingDataDT(TeamEatTeamDTrackingWideFull, TeamEatTeamDWithIDsIndirect)


TeamCatTeamAWithIDs <- AddPlayWindowRange(Team.C.at.Team.A.Events.2025.10.28)
TeamCatTeamAWithIDsDirect <- TeamCatTeamAWithIDs %>%
  filter(Detail_1 == "Direct")
TeamCatTeamATrackingWithIDsDirectPlays <- AddIDToTrackingDataDT(TeamCatTeamATrackingWideFull, TeamCatTeamAWithIDsDirect)
TeamCatTeamAWithIDsIndirect <- TeamCatTeamAWithIDs %>%
  filter(Detail_1 == "Indirect")
TeamCatTeamATrackingWithIDsIndirectPlays <- AddIDToTrackingDataDT(TeamCatTeamATrackingWideFull, TeamCatTeamAWithIDsIndirect)


TeamGatTeamLWithIDs <- AddPlayWindowRange(Team.G.at.Team.L.Events.2025.10.29)
TeamGatTeamLWithIDsDirect <- TeamGatTeamLWithIDs %>%
  filter(Detail_1 == "Direct")
TeamGatTeamLTrackingWithIDsDirectPlays <- AddIDToTrackingDataDT(TeamGatTeamLTrackingWideFull, TeamGatTeamLWithIDsDirect)
TeamGatTeamLWithIDsIndirect <- TeamGatTeamLWithIDs %>%
  filter(Detail_1 == "Indirect")
TeamGatTeamLTrackingWithIDsIndirectPlays <- AddIDToTrackingDataDT(TeamGatTeamLTrackingWideFull, TeamGatTeamLWithIDsIndirect)


TeamDatTeamAWithIDs <- AddPlayWindowRange(Team.D.at.Team.A.Events.2025.11.02)
TeamDatTeamAWithIDsDirect <- TeamDatTeamAWithIDs %>%
  filter(Detail_1 == "Direct")
TeamDatTeamATrackingWithIDsDirectPlays <- AddIDToTrackingDataDT(TeamDatTeamATrackingWideFull, TeamDatTeamAWithIDsDirect)
TeamDatTeamAWithIDsIndirect <- TeamDatTeamAWithIDs %>%
  filter(Detail_1 == "Indirect")
TeamDatTeamATrackingWithIDsIndirectPlays <- AddIDToTrackingDataDT(TeamDatTeamATrackingWideFull, TeamDatTeamAWithIDsIndirect)


TeamHatTeamKWithIDs <- AddPlayWindowRange(Team.H.at.Team.K.Events.2025.11.16)
TeamHatTeamKWithIDsDirect <- TeamHatTeamKWithIDs %>%
  filter(Detail_1 == "Direct")
TeamHatTeamKTrackingWithIDsDirectPlays <- AddIDToTrackingDataDT(TeamHatTeamKTrackingWideFull, TeamHatTeamKWithIDsDirect)
TeamHatTeamKWithIDsIndirect <- TeamHatTeamKWithIDs %>%
  filter(Detail_1 == "Indirect")
TeamHatTeamKTrackingWithIDsIndirectPlays <- AddIDToTrackingDataDT(TeamHatTeamKTrackingWideFull, TeamHatTeamKWithIDsIndirect)

TeamIatTeamKWithIDs <- AddPlayWindowRange(Team.I.at.Team.K.Events.2025.11.19)
TeamIatTeamKWithIDsDirect <- TeamIatTeamKWithIDs %>%
  filter(Detail_1 == "Direct")
TeamIatTeamKTrackingWithIDsDirectPlays <- AddIDToTrackingDataDT(TeamIatTeamKTrackingWideFull, TeamIatTeamKWithIDsDirect)
TeamIatTeamKWithIDsIndirect <- TeamIatTeamKWithIDs %>%
  filter(Detail_1 == "Indirect")
TeamIatTeamKTrackingWithIDsIndirectPlays <- AddIDToTrackingDataDT(TeamIatTeamKTrackingWideFull, TeamIatTeamKWithIDsIndirect)

TeamJatTeamKWithIDs <- AddPlayWindowRange(Team.J.at.Team.K.Events.2025.11.28)
TeamJatTeamKWithIDsDirect <- TeamJatTeamKWithIDs %>%
  filter(Detail_1 == "Direct")
TeamJatTeamKTrackingWithIDsDirectPlays <- AddIDToTrackingDataDT(TeamJatTeamKTrackingWideFull, TeamJatTeamKWithIDsDirect)
TeamJatTeamKWithIDsIndirect <- TeamJatTeamKWithIDs %>%
  filter(Detail_1 == "Indirect")
TeamJatTeamKTrackingWithIDsIndirectPlays <- AddIDToTrackingDataDT(TeamJatTeamKTrackingWideFull, TeamJatTeamKWithIDsIndirect)


#Combine all dataframes with IDs
AllGamesTrackingDataWithIDsDirect <- rbind(TeamAatTeamDTrackingWithIDsDirectPlays,TeamFatTeamLTrackingWithIDsDirectPlays,
                                     TeamBatTeamATrackingWithIDsDirectPlays,TeamEatTeamDTrackingWithIDsDirectPlays,
                                     TeamCatTeamATrackingWithIDsDirectPlays,TeamGatTeamLTrackingWithIDsDirectPlays,
                                     TeamDatTeamATrackingWithIDsDirectPlays,TeamHatTeamKTrackingWithIDsDirectPlays,
                                     TeamIatTeamKTrackingWithIDsDirectPlays,TeamJatTeamKTrackingWithIDsDirectPlays)

AllGamesTrackingDataWithIDsIndirect <- rbind(TeamAatTeamDTrackingWithIDsIndirectPlays,TeamFatTeamLTrackingWithIDsIndirectPlays,
                                           TeamBatTeamATrackingWithIDsIndirectPlays,TeamEatTeamDTrackingWithIDsIndirectPlays,
                                           TeamCatTeamATrackingWithIDsIndirectPlays,TeamGatTeamLTrackingWithIDsIndirectPlays,
                                           TeamDatTeamATrackingWithIDsIndirectPlays,TeamHatTeamKTrackingWithIDsIndirectPlays,
                                           TeamIatTeamKTrackingWithIDsIndirectPlays,TeamJatTeamKTrackingWithIDsIndirectPlays)

#Clustering the data using dtwclust
library(dtwclust)
nrow(TeamAatTeamDTrackingWithIDs)
length(unique(TeamAatTeamDTrackingWithIDs$CorrespondingPlayID))


#Testing a smaller sample of data
TeamAatTeamDPlayIDs <- unique(TeamAatTeamDTrackingWithIDs$CorrespondingPlayID)
DTWClusterTestSampleIDs <- sample(TeamAatTeamDPlayIDs, size = 250)
DTWClusterTestSample <- as.data.frame(TeamAatTeamDTrackingWithIDs) %>%
  filter(CorrespondingPlayID %in% DTWClusterTestSampleIDs)

PrepareSequencesForDTW <- function(tracking_with_ids, 
                                   off_ice_x = -999, 
                                   off_ice_y = -999){
  
  play_ids <- unique(tracking_with_ids$CorrespondingPlayID)
  
  play_sequences <- vector("list", length = length(play_ids))
  names(play_sequences) <- play_ids
  
  feature_columns <- grep("^(HomeSkater[0-9]*|AwaySkater[0-9]*|Puck)\\.(x|y)$",
                          colnames(tracking_with_ids),
                          value = TRUE)
  
  cat("Preparing", length(play_ids), "sequences...\n")
  cat("Off-ice coordinates: (", off_ice_x, ",", off_ice_y, ")\n\n")
  
  for (i in seq_along(play_ids)) {
    if (i %% 25 == 0) {
      cat("  Processed", i, "of", length(play_ids), "plays\n")
    }
    
    current_play_id <- play_ids[i]
    
    play_data <- tracking_with_ids %>%
      filter(CorrespondingPlayID == current_play_id) %>%
      arrange(Period, desc(Game.Clock)) %>%
      select(all_of(feature_columns))
    
    play_matrix <- as.matrix(play_data)

    if (any(is.na(play_matrix))) {
      play_matrix[is.na(play_matrix)] <- ifelse(
        grepl("\\.x$", colnames(play_matrix)[col(play_matrix)[is.na(play_matrix)]]),
        off_ice_x,
        off_ice_y
      )
    }
    
    play_sequences[[i]] <- play_matrix
  }
  
  has_na <- sapply(play_sequences, function(seq) any(is.na(seq)))
  
  if (any(has_na)) {
    warning("Some NAs remain after off-ice substitution!")
  } else {
    cat("\n✓ All NAs replaced with off-ice coordinates\n")
  }
  
  seq_lengths <- sapply(play_sequences, nrow)
  
  cat("\nSequence length statistics:\n")
  cat("  Shortest:", min(seq_lengths), "frames\n")
  cat("  Longest:", max(seq_lengths), "frames\n")
  cat("  Average:", round(mean(seq_lengths), 1), "frames\n")
  cat("  Median:", median(seq_lengths), "frames\n")
  
  return(play_sequences)
}
test_sequences <- PrepareSequencesForDTW(DTWClusterTestSample)
AllGamesForClusteringDirect <- PrepareSequencesForDTW(AllGamesTrackingDataWithIDsDirect)
AllGamesForClusteringIndirect <- PrepareSequencesForDTW(AllGamesTrackingDataWithIDsIndirect)

#taking out all plays with less than 30 frames
AllGamesForClusteringDirectFiltered <- AllGamesForClusteringDirect %>%
  keep(~ nrow(.x) >= 30)
AllGamesForClusteringIndirectFiltered <- AllGamesForClusteringIndirect %>%
  keep(~ nrow(.x) >= 30)


k_values <- 4:12

dist_direct <- proxy::dist(
  AllGamesForClusteringDirectFiltered,
  method = "dtw2",
  window.size = 10
)

dist_direct_sym <- (dist_direct + t(dist_direct)) / 2

hc_direct_fit <- hclust(as.dist(dist_direct_sym), method = "ward.D2")

direct_sil_scores <- numeric(length(k_values))

for (i in seq_along(k_values)) {
  clusters <- cutree(hc_direct_fit, k = k_values[i])
  sil <- silhouette(clusters, as.dist(dist_direct_sym))
  direct_sil_scores[i] <- mean(sil[, 3])
}

names(direct_sil_scores) <- k_values
direct_sil_scores

best_k_direct <- k_values[which.max(direct_sil_scores)]
best_clusters_direct <- cutree(hc_direct_fit, k = best_k)
#best k is 4

dist_indirect <- proxy::dist(
  AllGamesForClusteringIndirectFiltered,
  method = "dtw2",
  window.size = 10
)

dist_indirect_sym <- (dist_indirect + t(dist_indirect)) / 2

hc_indirect_fit <- hclust(as.dist(dist_indirect_sym), method = "ward.D2")

indirect_sil_scores <- numeric(length(k_values))

for (i in seq_along(k_values)) {
  clusters <- cutree(hc_indirect_fit, k = k_values[i])
  sil <- silhouette(clusters, as.dist(dist_indirect_sym))
  indirect_sil_scores[i] <- mean(sil[, 3])
}

names(indirect_sil_scores) <- k_values
indirect_sil_scores

best_k_indirect <- k_values[which.max(indirect_sil_scores)]
best_clusters_indirect <- cutree(hc_indirect_fit, k = best_k)
#best k is 4 

#Get Medoids
direct_medoids <- GDAtools::medoids(as.dist(dist_direct_sym), best_clusters_direct)
indirect_medoids <- GDAtools::medoids(as.dist(dist_indirect_sym), best_clusters_indirect)

#Direct Medoids
names(AllGamesForClusteringDirectFiltered)[direct_medoids]
#Direct Cluster 1 is short pass, primarily east-west, primarily zone entries
cluster1direct_members <- which(best_clusters_direct == 1)
cluster1direct_avg_dists <- rowMeans(dist_direct_sym[cluster1direct_members, cluster1direct_members])
sort(cluster1direct_avg_dists)[1:3]
AnimatePlay(Team.I.at.Team.K.Tracking_P2.2025.11.19,"18:42")
AnimatePlay(Team.G.at.Team.L.Tracking_P3.2025.10.29,"7:45")
AnimatePlay(Team.I.at.Team.K.Tracking_P2.2025.11.19,"6:37")
#Direct Cluster 2 is pass from point to wing, possibly high danger pass
cluster2direct_members <- which(best_clusters_direct == 2)
cluster2direct_avg_dists <- rowMeans(dist_direct_sym[cluster2direct_members, cluster2direct_members])
sort(cluster2direct_avg_dists)[1:3]
AnimatePlay(Team.B.at.Team.A.Tracking_P1.2025.10.24,"00:15")
AnimatePlay(Team.C.at.Team.A.Tracking_P2.2025.10.28,"5:35")
AnimatePlay(Team.F.at.Team.L.Tracking_P2.2025.10.17,"2:55")
#Direct Cluster 3 is a vertical pass near zone entry
cluster3direct_members <- which(best_clusters_direct == 3)
cluster3direct_avg_dists <- rowMeans(dist_direct_sym[cluster3direct_members, cluster3direct_members])
sort(cluster3direct_avg_dists)[1:3]
AnimatePlay(Team.A.at.Team.D.Tracking_P2.2025.10.11,"4:25")
AnimatePlay(Team.J.at.Team.K.Tracking_P2.2025.11.28,"10:24")
AnimatePlay(Team.H.at.Team.K.Tracking_P2.2025.11.16,"2:57")
#Direct Cluster 4 is a vertical pass out of defensive zone or pass leading to exit
cluster4direct_members <- which(best_clusters_direct == 4)
cluster4direct_avg_dists <- rowMeans(dist_direct_sym[cluster4direct_members, cluster4direct_members])
sort(cluster4direct_avg_dists)[1:3]
AnimatePlay(Team.E.at.Team.D.Tracking_P2.2025.10.24,"2:40")
AnimatePlay(Team.E.at.Team.D.Tracking_P2.2025.10.24,"2:23")
AnimatePlay(Team.A.at.Team.D.Tracking_P2.2025.10.11,"7:42")

#Indirect Medoids
names(AllGamesForClusteringIndirectFiltered)[indirect_medoids]
#Indirect Cluster 1 is area pass/poke pass into open space for teammate
cluster1indirect_members <- which(best_clusters_indirect == 1)
cluster1indirect_avg_dists <- rowMeans(dist_indirect_sym[cluster1indirect_members, cluster1indirect_members])
sort(cluster1indirect_avg_dists)[1:3]
AnimatePlay(Team.G.at.Team.L.Tracking_P3.2025.10.29,"7:49")
AnimatePlay(Team.A.at.Team.D.Tracking_P3.2025.10.11,"16:21")
AnimatePlay(Team.E.at.Team.D.Tracking_P1.2025.10.24,"2:18")
#Indirect Cluster 2 is a rim around up the boards
cluster2indirect_members <- which(best_clusters_indirect == 2)
cluster2indirect_avg_dists <- rowMeans(dist_indirect_sym[cluster2indirect_members, cluster2indirect_members])
sort(cluster2indirect_avg_dists)[1:3]
AnimatePlay(Team.I.at.Team.K.Tracking_P2.2025.11.19,"11:15")
AnimatePlay(Team.I.at.Team.K.Tracking_P3.2025.11.19,"3:23")
AnimatePlay(Team.D.at.Team.A.Tracking_P2.2025.11.02,"14:42")
#Indirect Cluster 3 is an area pass closer to the boards
cluster3indirect_members <- which(best_clusters_indirect == 3)
cluster3indirect_avg_dists <- rowMeans(dist_indirect_sym[cluster3indirect_members, cluster3indirect_members])
sort(cluster3indirect_avg_dists)[1:3]
AnimatePlay(Team.F.at.Team.L.Tracking_P3.2025.10.17,"3:33")
AnimatePlay(Team.G.at.Team.L.Tracking_P1.2025.10.29,"3:34")
AnimatePlay(Team.H.at.Team.K.Tracking_P1.2025.11.16,"18:25")
#Indirect Cluster 4 is throw out of the zone off the boards
cluster4indirect_members <- which(best_clusters_indirect == 4)
cluster4indirect_avg_dists <- rowMeans(dist_indirect_sym[cluster4indirect_members, cluster4indirect_members])
sort(cluster4indirect_avg_dists)[1:3]
AnimatePlay(Team.G.at.Team.L.Tracking_P3.2025.10.29,"14:06")
AnimatePlay(Team.D.at.Team.A.Tracking_P1.2025.11.02,"12:35")
AnimatePlay(Team.I.at.Team.K.Tracking_P1.2025.11.19,"18:17")



#Make full dataframes with all events
AllBigDataCupEvents <- rbind(Team.E.at.Team.D.Events.2025.10.24, Team.A.at.Team.D.Events.2025.10.11,
                             Team.B.at.Team.A.Events.2025.10.24, Team.C.at.Team.A.Events.2025.10.28,
                             Team.D.at.Team.A.Events.2025.11.02, Team.F.at.Team.L.Events2025.10.17,
                             Team.G.at.Team.L.Events.2025.10.29, Team.H.at.Team.K.Events.2025.11.16,
                             Team.I.at.Team.K.Events.2025.11.19, Team.J.at.Team.K.Events.2025.11.28)

#Calculate Euclidian Distance between players on each play
AllBigDataCupEventsPlayDistances <- AllBigDataCupEvents %>%
  filter(Event == "Play" | Event == "Incomplete Play") %>%
  mutate(DistanceBetweenPlayers = sqrt((X_Coordinate-X_Coordinate_2)^2+(Y_Coordinate-Y_Coordinate_2)^2),
         CorrespondingPlayID = paste0(Date, Home_Team, Away_Team, Period, ".", ms(Clock)))

AllBigDataCupEventsPlayDistances_Clean <- AllBigDataCupEventsPlayDistances %>%
  group_by(CorrespondingPlayID) %>%
  filter(n() == 1) %>%
  ungroup()

BigDataCupduplicate_plays <- AllBigDataCupEventsPlayDistances %>%
  group_by(CorrespondingPlayID) %>%
  filter(n() > 1) %>%
  ungroup()

#Use the Tracking data to determine distances from each defender and speed
DirectTrackingDataWithEventLocation <- merge(
  AllGamesTrackingDataWithIDsDirect, 
  AllBigDataCupEventsPlayDistances_Clean,
  by = "CorrespondingPlayID") %>%
  arrange(Image.Id)

View(DirectTrackingDataWithEventLocation)

IndirectTrackingDataWithEventLocation <- merge(
  AllGamesTrackingDataWithIDsIndirect, 
  AllBigDataCupEventsPlayDistances_Clean,
  by = "CorrespondingPlayID") %>%
  arrange(Image.Id)

View(IndirectTrackingDataWithEventLocation)

  
DirectTrackingDataWithEventLocationPuckDistVelo <- DirectTrackingDataWithEventLocationPuckDist %>%
  group_by(CorrespondingPlayID) %>%
  arrange(CorrespondingPlayID, Image.Id) %>%
  mutate(
    HomeSkater1.Vx = HomeSkater1.x - lag(HomeSkater1.x),
    HomeSkater1.Vy = HomeSkater1.y - lag(HomeSkater1.y),
    HomeSkater1.Speed = sqrt(HomeSkater1.Vx^2 + HomeSkater1.Vy^2),
    HomeSkater2.Vx = HomeSkater2.x - lag(HomeSkater2.x),
    HomeSkater2.Vy = HomeSkater2.y - lag(HomeSkater2.y),
    HomeSkater2.Speed = sqrt(HomeSkater2.Vx^2 + HomeSkater2.Vy^2),
    HomeSkater3.Vx = HomeSkater3.x - lag(HomeSkater3.x),
    HomeSkater3.Vy = HomeSkater3.y - lag(HomeSkater3.y),
    HomeSkater3.Speed = sqrt(HomeSkater3.Vx^2 + HomeSkater3.Vy^2),
    HomeSkater4.Vx = HomeSkater4.x - lag(HomeSkater4.x),
    HomeSkater4.Vy = HomeSkater4.y - lag(HomeSkater4.y),
    HomeSkater4.Speed = sqrt(HomeSkater4.Vx^2 + HomeSkater4.Vy^2),
    HomeSkater5.Vx = HomeSkater5.x - lag(HomeSkater5.x),
    HomeSkater5.Vy = HomeSkater5.y - lag(HomeSkater5.y),
    HomeSkater5.Speed = sqrt(HomeSkater5.Vx^2 + HomeSkater5.Vy^2),
    HomeSkater6.Vx = HomeSkater6.x - lag(HomeSkater6.x),
    HomeSkater6.Vy = HomeSkater6.y - lag(HomeSkater6.y),
    HomeSkater6.Speed = sqrt(HomeSkater6.Vx^2 + HomeSkater6.Vy^2),
    AwaySkater1.Vx = AwaySkater1.x - lag(AwaySkater1.x),
    AwaySkater1.Vy = AwaySkater1.y - lag(AwaySkater1.y),
    AwaySkater1.Speed = sqrt(AwaySkater1.Vx^2 + AwaySkater1.Vy^2),
    AwaySkater2.Vx = AwaySkater2.x - lag(AwaySkater2.x),
    AwaySkater2.Vy = AwaySkater2.y - lag(AwaySkater2.y),
    AwaySkater2.Speed = sqrt(AwaySkater2.Vx^2 + AwaySkater2.Vy^2),
    AwaySkater3.Vx = AwaySkater3.x - lag(AwaySkater3.x),
    AwaySkater3.Vy = AwaySkater3.y - lag(AwaySkater3.y),
    AwaySkater3.Speed = sqrt(AwaySkater3.Vx^2 + AwaySkater3.Vy^2),
    AwaySkater4.Vx = AwaySkater4.x - lag(AwaySkater4.x),
    AwaySkater4.Vy = AwaySkater4.y - lag(AwaySkater4.y),
    AwaySkater4.Speed = sqrt(AwaySkater4.Vx^2 + AwaySkater4.Vy^2),
    AwaySkater5.Vx = AwaySkater5.x - lag(AwaySkater5.x),
    AwaySkater5.Vy = AwaySkater5.y - lag(AwaySkater5.y),
    AwaySkater5.Speed = sqrt(AwaySkater5.Vx^2 + AwaySkater5.Vy^2),
    AwaySkater6.Vx = AwaySkater6.x - lag(AwaySkater6.x),
    AwaySkater6.Vy = AwaySkater6.y - lag(AwaySkater6.y),
    AwaySkater6.Speed = sqrt(AwaySkater6.Vx^2 + AwaySkater6.Vy^2)
  ) %>%
  ungroup()

DirectTrackingDataWithEventLocationPuckDistVeloBestFrame <- DirectTrackingDataWithEventLocationPuckDistVelo %>%
  group_by(CorrespondingPlayID) %>%
  slice_min(PuckToEventDist, n=1, with_ties = FALSE) %>%
  ungroup()

AllTrackingDataframes <- rbind(TeamAatTeamDTrackingFull_raw, TeamBatTeamATrackingFull_raw,
                               TeamCatTeamATrackingFull_raw, TeamDatTeamATrackingFull_raw,
                               TeamEatTeamDTrackingFull_raw, TeamFatTeamLTrackingFull_raw,
                               TeamGatTeamLTrackingFull_raw, TeamHatTeamKTrackingFull_raw,
                               TeamIatTeamKTrackingFull_raw, TeamJatTeamKTrackingFull_raw)

AllTrackingDataWithInstantVelo <- AllTrackingDataframes %>%
  filter(Player.or.Puck=="Player") %>%
  arrange(Player.Id, Image.Id) %>%
  group_by(Player.Id) %>%
  mutate(
    Vx = Rink.Location.X..Feet. - lag(Rink.Location.X..Feet.),
    Vy = Rink.Location.Y..Feet. - lag(Rink.Location.Y..Feet.),
    Speed = sqrt(Vx^2+Vy^2)
  ) %>%
  ungroup()

BestFrameTrackingIDs <- DirectTrackingDataWithEventLocationPuckDistVeloBestFrame %>%
  select(CorrespondingPlayID, Image.Id, Period.x, Game.Clock, Home_Team, Away_Team,
         Team, Event, Detail_1, Player_Id, X_Coordinate, Y_Coordinate, Player_Id_2, X_Coordinate_2, Y_Coordinate_2,
         DistanceBetweenPlayers)

BestFrameTrackingLong <- BestFrameTrackingIDs %>%
  left_join(AllTrackingDataWithInstantVelo, by="Image.Id") %>%
  filter(Player.or.Puck == "Player") %>%
  mutate(DistToEvent     = sqrt((Rink.Location.X..Feet. - X_Coordinate)^2 + 
                                  (Rink.Location.Y..Feet. - Y_Coordinate)^2),
         DistToReception = sqrt((Rink.Location.X..Feet. - X_Coordinate_2)^2 + 
                                  (Rink.Location.Y..Feet. - Y_Coordinate_2)^2),
         TeamName = ifelse(Team.y=="Home", Home_Team, Away_Team),
         EventPasserID = paste0(Team.x, Player_Id),
         EventRecieverID = paste0(Team.x, Player_Id_2),
         TrackingPlayerID = paste0(TeamName, Player.Jersey.Number))

DirectFramesTrackingWithEvents <- BestFrameTrackingLong %>%
  group_by(CorrespondingPlayID) %>%
  summarise(
    Image.Id = first(Image.Id),
    Period = first(Period.x),
    Game.Clock = first(Game.Clock.x),
    Home_Team = first(Home_Team),
    Away_Team = first(Away_Team),
    Event = first(Event),
    Detail_1 = first(Detail_1),
    X_Coordinate = first(X_Coordinate),
    Y_Coordinate = first(Y_Coordinate),
    X_Coordinate_2 = first(X_Coordinate_2),
    Y_Coordinate_2 = first(Y_Coordinate_2),
    DistanceBetweenPlayers = first(DistanceBetweenPlayers),
    EventPasserID = first(EventPasserID),
    EventReceiverID = first(EventRecieverID),
    ClosestDefenderToPasserDist = min(DistToEvent[TeamName != first(Team.x)], 
                                      na.rm = TRUE),
    ClosestDefenderToReceiverDist = min(DistToReception[TeamName != first(Team.x)], 
                                        na.rm = TRUE),
    DefendersNearReceiver = sum(DistToReception[TeamName != first(Team.x)] < 10, 
                                na.rm = TRUE),
    PasserSpeed = Speed[TrackingPlayerID == first(EventPasserID)][1],
    ReceiverSpeed = Speed[TrackingPlayerID == first(EventRecieverID)][1],
    ClosestDefenderToPasserSpeed = Speed[
      TeamName != first(Team.x) & 
        DistToEvent == min(DistToEvent[TeamName != first(Team.x)])][1],
    ClosestDefenderToReceiverSpeed = Speed[
      TeamName != first(Team.x) & 
        DistToReception == min(DistToReception[TeamName != first(Team.x)])][1],
    PasserJersey = Player.Jersey.Number[TrackingPlayerID == first(EventPasserID)][1],
    ReceiverJersey = Player.Jersey.Number[TrackingPlayerID == first(EventRecieverID)][1],
    ClosestDefenderToPasserJersey = Player.Jersey.Number[TeamName != first(Team.x) & 
                                                           DistToEvent == min(DistToEvent[TeamName != first(Team.x)])][1],
    ClosestDefenderToReceiverJersey = Player.Jersey.Number[TeamName != first(Team.x) & 
        DistToReception == min(DistToReception[TeamName != first(Team.x)])][1]
  ) %>%
  ungroup()

#Replace Inf values with NA
DirectFramesTrackingWithEvents2 <- DirectFramesTrackingWithEvents %>%
  mutate(across(c(ClosestDefenderToPasserDist, ClosestDefenderToReceiverDist,
                  ClosestDefenderToPasserSpeed, ClosestDefenderToReceiverSpeed),
                ~ na_if(., Inf))) %>%
  mutate(DefendersNearReceiver = na_if(as.double(DefendersNearReceiver), Inf)) %>%
  mutate(across(c(ClosestDefenderToPasserJersey, ClosestDefenderToReceiverJersey),
                ~ na_if(., "Inf")))

#With Indirect Plays as well
IndirectTrackingDataWithEventLocationPuckDist <- IndirectTrackingDataWithEventLocation %>%
  mutate(PuckToEventDist = sqrt((Puck.x-X_Coordinate)^2+(Puck.y-Y_Coordinate)^2))

IndirectTrackingDataWithEventBestFrame <- IndirectTrackingDataWithEventLocationPuckDist %>%
  group_by(CorrespondingPlayID) %>%
  slice_min(PuckToEventDist, n=1, with_ties = FALSE) %>%
  ungroup()

IndirectBestFrameTrackingIDs <- IndirectTrackingDataWithEventBestFrame %>%
  select(CorrespondingPlayID, Image.Id, Period.x, Game.Clock, Home_Team, Away_Team,
         Team, Event, Detail_1, Player_Id, X_Coordinate, Y_Coordinate, Player_Id_2, X_Coordinate_2, Y_Coordinate_2,
         DistanceBetweenPlayers)

IndirectBestFrameTrackingLong <- IndirectBestFrameTrackingIDs %>%
  left_join(AllTrackingDataWithInstantVelo, by="Image.Id") %>%
  filter(Player.or.Puck == "Player") %>%
  mutate(DistToEvent     = sqrt((Rink.Location.X..Feet. - X_Coordinate)^2 + 
                                  (Rink.Location.Y..Feet. - Y_Coordinate)^2),
         DistToReception = sqrt((Rink.Location.X..Feet. - X_Coordinate_2)^2 + 
                                  (Rink.Location.Y..Feet. - Y_Coordinate_2)^2),
         TeamName = ifelse(Team.y=="Home", Home_Team, Away_Team),
         EventPasserID = paste0(Team.x, Player_Id),
         EventRecieverID = paste0(Team.x, Player_Id_2),
         TrackingPlayerID = paste0(TeamName, Player.Jersey.Number))

IndirectFramesTrackingWithEvents <- IndirectBestFrameTrackingLong %>%
  group_by(CorrespondingPlayID) %>%
  summarise(
    Image.Id = first(Image.Id),
    Period = first(Period.x),
    Game.Clock = first(Game.Clock.x),
    Home_Team = first(Home_Team),
    Away_Team = first(Away_Team),
    Event = first(Event),
    Detail_1 = first(Detail_1),
    X_Coordinate = first(X_Coordinate),
    Y_Coordinate = first(Y_Coordinate),
    X_Coordinate_2 = first(X_Coordinate_2),
    Y_Coordinate_2 = first(Y_Coordinate_2),
    DistanceBetweenPlayers = first(DistanceBetweenPlayers),
    EventPasserID = first(EventPasserID),
    EventReceiverID = first(EventRecieverID),
    ClosestDefenderToPasserDist = min(DistToEvent[TeamName != first(Team.x)], 
                                      na.rm = TRUE),
    ClosestDefenderToReceiverDist = min(DistToReception[TeamName != first(Team.x)], 
                                        na.rm = TRUE),
    DefendersNearReceiver = sum(DistToReception[TeamName != first(Team.x)] < 10, 
                                na.rm = TRUE),
    PasserSpeed = Speed[TrackingPlayerID == first(EventPasserID)][1],
    ReceiverSpeed = Speed[TrackingPlayerID == first(EventRecieverID)][1],
    ClosestDefenderToPasserSpeed = Speed[
      TeamName != first(Team.x) & 
        DistToEvent == min(DistToEvent[TeamName != first(Team.x)])][1],
    ClosestDefenderToReceiverSpeed = Speed[
      TeamName != first(Team.x) & 
        DistToReception == min(DistToReception[TeamName != first(Team.x)])][1],
    PasserJersey = Player.Jersey.Number[TrackingPlayerID == first(EventPasserID)][1],
    ReceiverJersey = Player.Jersey.Number[TrackingPlayerID == first(EventRecieverID)][1],
    ClosestDefenderToPasserJersey = Player.Jersey.Number[TeamName != first(Team.x) & 
                                                           DistToEvent == min(DistToEvent[TeamName != first(Team.x)])][1],
    ClosestDefenderToReceiverJersey = Player.Jersey.Number[TeamName != first(Team.x) & 
                                                             DistToReception == min(DistToReception[TeamName != first(Team.x)])][1]
  ) %>%
  ungroup()

#Replace Inf values with NA
IndirectFramesTrackingWithEvents2 <- IndirectFramesTrackingWithEvents %>%
  mutate(across(c(ClosestDefenderToPasserDist, ClosestDefenderToReceiverDist,
                  ClosestDefenderToPasserSpeed, ClosestDefenderToReceiverSpeed),
                ~ na_if(., Inf))) %>%
  mutate(DefendersNearReceiver = na_if(as.double(DefendersNearReceiver), Inf)) %>%
  mutate(across(c(ClosestDefenderToPasserJersey, ClosestDefenderToReceiverJersey),
                ~ na_if(., "Inf")))


#Create an XGBoost model
#Target will be "play", so all of those will be 1, all labeled "incomplete play" will be zero
DirectFramesTrackingWithEvents3 <- DirectFramesTrackingWithEvents2 %>%
  mutate(CompletedPlay = ifelse(Event=="Play",1,0))
IndirectFramesTrackingWithEvents3 <- IndirectFramesTrackingWithEvents2 %>%
  mutate(CompletedPlay = ifelse(Event=="Play",1,0))

#confirm no duplicate rows
sum(duplicated(DirectFramesTrackingWithEvents3 %>% select(-Image.Id))) #sums to 0
sum(duplicated(IndirectFramesTrackingWithEvents3 %>% select(-Image.Id))) #sums to 0

#Bring over clusters
direct_cluster_df <- data.frame(
  CorrespondingPlayID = names(best_clusters_direct),
  Cluster = best_clusters_direct
)

DirectFramesTrackingWithEvents4 <- DirectFramesTrackingWithEvents3 %>%
  left_join(direct_cluster_df, by = "CorrespondingPlayID") %>%
  filter(!is.na(Cluster)) %>%
  dummy_cols("Cluster",
             remove_first_dummy = TRUE,
             remove_selected_columns = TRUE)

indirect_cluster_df <- data.frame(
  CorrespondingPlayID = names(best_clusters_indirect),
  Cluster = best_clusters_indirect
)

IndirectFramesTrackingWithEvents4 <- IndirectFramesTrackingWithEvents3 %>%
  left_join(indirect_cluster_df, by = "CorrespondingPlayID") %>%
  filter(!is.na(Cluster)) %>%
  dummy_cols("Cluster",
             remove_first_dummy = TRUE,
             remove_selected_columns = TRUE)

DirectParams <- DirectFramesTrackingWithEvents4 %>%
  select(DistanceBetweenPlayers, ClosestDefenderToPasserDist, ClosestDefenderToReceiverDist,
         DefendersNearReceiver, PasserSpeed, ReceiverSpeed, ClosestDefenderToPasserSpeed,
         ClosestDefenderToReceiverSpeed, Cluster_2, Cluster_3, Cluster_4)
DirectParamsMatrix <- as.matrix(DirectParams)
DirectLabel <- DirectFramesTrackingWithEvents4$CompletedPlay

DirectTrain <- xgb.DMatrix(data = DirectParamsMatrix, label = DirectLabel)
set.seed(03032002)

direct_params <- list(
  objective = "binary:logistic",
  eval_metric = "aucpr",
  max_depth = 4,
  eta = 0.03,
  min_child_weight = 5,
  scale_pos_weight = sum(DirectLabel==0)/sum(DirectLabel==1)
)

direct_cv_results <- xgb.cv(
  params = direct_params,
  data = DirectTrain,
  nfold = 5,
  nrounds = 1000,
  early_stopping_rounds = 50,
  verbose = TRUE
)

direct_best_round <- direct_cv_results$niter

direct_final_model <- xgb.train(
  params = direct_params,
  data = DirectTrain,
  nrounds = direct_best_round
)

direct_importance <- xgb.importance(model = direct_final_model)
xgb.plot.importance(direct_importance, title = "Direct Model Feature Importance")

IndirectParams <- IndirectFramesTrackingWithEvents4 %>%
  select(DistanceBetweenPlayers, ClosestDefenderToPasserDist, ClosestDefenderToReceiverDist,
         DefendersNearReceiver, PasserSpeed, ReceiverSpeed, ClosestDefenderToPasserSpeed,
         ClosestDefenderToReceiverSpeed, Cluster_2, Cluster_3, Cluster_4)
IndirectParamsMatrix <- as.matrix(IndirectParams)
IndirectLabel <- IndirectFramesTrackingWithEvents4$CompletedPlay

IndirectTrain <- xgb.DMatrix(data = IndirectParamsMatrix, label = IndirectLabel)

indirect_params <- list(
  objective = "binary:logistic",
  eval_metric = "aucpr",
  max_depth = 4,
  eta = 0.01,
  min_child_weight = 5,
  gamma = 0.1,
  alpha = 0.05,
  scale_pos_weight = sum(IndirectLabel==0)/sum(IndirectLabel==1)
)

indirect_cv_results <- xgb.cv(
  params = indirect_params,
  data = IndirectTrain,
  nfold = 5,
  nrounds = 1000,
  early_stopping_rounds = 50,
  verbose = TRUE
)

indirect_best_round <- indirect_cv_results$niter

indirect_final_model <- xgb.train(
  params = indirect_params,
  data = IndirectTrain,
  nrounds = indirect_best_round
)

indirect_importance <- xgb.importance(model = indirect_final_model)
xgb.plot.importance(indirect_importance, title = "Indirect Model Feature Importance")

direct_preds   <- predict(direct_final_model, DirectTrain)
indirect_preds <- predict(indirect_final_model, IndirectTrain)
par(mfrow = c(1,2))
hist(direct_preds,   breaks = 20, main = "Direct Predicted Probabilities",   xlab = "P(Complete)")
hist(indirect_preds, breaks = 20, main = "Indirect Predicted Probabilities", xlab = "P(Complete)")
mean(direct_preds);   mean(DirectLabel)
mean(indirect_preds); mean(IndirectLabel)
#means were far apart, need to do Platt scaling
#need to redo the training of the model to do a test and train subset
library(caTools)
set.seed(03032002)
DirectSample <- sample.split(DirectFramesTrackingWithEvents4$CompletedPlay, SplitRatio = 0.7)
DirectTrainingSet  <- DirectFramesTrackingWithEvents4[DirectSample, ]
DirectRemaining    <- DirectFramesTrackingWithEvents4[!DirectSample, ]
DirectTestSplit      <- sample.split(DirectRemaining$CompletedPlay, SplitRatio = 0.5)
DirectCalibrationSet <- DirectRemaining[DirectTestSplit, ]
DirectFinalTestSet   <- DirectRemaining[!DirectTestSplit, ]

DirectTrainDMatrix <- xgb.DMatrix(
  data  = as.matrix(DirectTrainingSet %>% 
                      select(DistanceBetweenPlayers, ClosestDefenderToPasserDist,
                             ClosestDefenderToReceiverDist, DefendersNearReceiver,
                             PasserSpeed, ReceiverSpeed, ClosestDefenderToPasserSpeed,
                             ClosestDefenderToReceiverSpeed, Cluster_2, Cluster_3, Cluster_4)),
  label = DirectTrainingSet$CompletedPlay
)

DirectCalibrationDMatrix <- xgb.DMatrix(
  data  = as.matrix(DirectCalibrationSet %>%
                      select(DistanceBetweenPlayers, ClosestDefenderToPasserDist,
                             ClosestDefenderToReceiverDist, DefendersNearReceiver,
                             PasserSpeed, ReceiverSpeed, ClosestDefenderToPasserSpeed,
                             ClosestDefenderToReceiverSpeed, Cluster_2, Cluster_3, Cluster_4)),
  label = DirectCalibrationSet$CompletedPlay
)

DirectTestDMatrix <- xgb.DMatrix(
  data  = as.matrix(DirectFinalTestSet %>%
                      select(DistanceBetweenPlayers, ClosestDefenderToPasserDist,
                             ClosestDefenderToReceiverDist, DefendersNearReceiver,
                             PasserSpeed, ReceiverSpeed, ClosestDefenderToPasserSpeed,
                             ClosestDefenderToReceiverSpeed, Cluster_2, Cluster_3, Cluster_4)),
  label = DirectFinalTestSet$CompletedPlay
)
direct_params2 <- list(
  objective        = "binary:logistic",
  eval_metric      = "aucpr",
  max_depth        = 5,
  eta              = 0.04,
  min_child_weight = 5,
  gamma = 0.2,
  alpha = 1,
  scale_pos_weight = sum(DirectTrainingSet$CompletedPlay == 0) / 
    sum(DirectTrainingSet$CompletedPlay == 1)  
)

direct_cv_results2 <- xgb.cv(
  params                = direct_params2,
  data                  = DirectTrainDMatrix,
  nfold                 = 5,
  nrounds               = 1000,
  early_stopping_rounds = 50,
  verbose               = TRUE,
  print_every_n         = 10
)

direct_best_round2 <- direct_cv_results2$niter

direct_final_model2 <- xgb.train(
  params  = direct_params2,
  data    = DirectTrainDMatrix,
  nrounds = direct_best_round2
)

cal_preds_direct <- predict(direct_final_model2, DirectCalibrationDMatrix)

platt_direct <- glm(actual ~ pred,
                    data   = data.frame(pred   = cal_preds_direct,
                                        actual = DirectCalibrationSet$CompletedPlay),
                    family = binomial)
test_preds_raw_direct <- predict(direct_final_model2, DirectTestDMatrix)

test_preds_calibrated_direct <- predict(platt_direct,
                                        newdata = data.frame(pred = test_preds_raw_direct),
                                        type    = "response")
mean(test_preds_calibrated_direct)
mean(DirectFinalTestSet$CompletedPlay)

data.frame(pred   = test_preds_calibrated_direct,
           actual = DirectFinalTestSet$CompletedPlay) %>%
  mutate(bucket = ntile(pred, 10)) %>%
  group_by(bucket) %>%
  summarise(mean_pred   = mean(pred),
            mean_actual = mean(actual)) %>%
  ggplot(aes(x = mean_pred, y = mean_actual)) +
  geom_point(size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Direct Model Calibration — Test Set",
       x = "Mean Predicted Probability",
       y = "Mean Actual Completion Rate") +
  theme_minimal()

direct_importance2 <- xgb.importance(model = direct_final_model2)

#need to do the same process with indirect
IndirectSample <- sample.split(IndirectFramesTrackingWithEvents4$CompletedPlay, SplitRatio = 0.7)
IndirectTrainingSet  <- IndirectFramesTrackingWithEvents4[IndirectSample, ]
IndirectRemaining    <- IndirectFramesTrackingWithEvents4[!IndirectSample, ]
IndirectTestSplit      <- sample.split(IndirectRemaining$CompletedPlay, SplitRatio = 0.5)
IndirectCalibrationSet <- IndirectRemaining[IndirectTestSplit, ]
IndirectFinalTestSet   <- IndirectRemaining[!IndirectTestSplit, ]

IndirectTrainDMatrix <- xgb.DMatrix(
  data  = as.matrix(IndirectTrainingSet %>% 
                      select(DistanceBetweenPlayers, ClosestDefenderToPasserDist,
                             ClosestDefenderToReceiverDist, DefendersNearReceiver,
                             PasserSpeed, ReceiverSpeed, ClosestDefenderToPasserSpeed,
                             ClosestDefenderToReceiverSpeed, Cluster_2, Cluster_3, Cluster_4)),
  label = IndirectTrainingSet$CompletedPlay
)

IndirectCalibrationDMatrix <- xgb.DMatrix(
  data  = as.matrix(IndirectCalibrationSet %>%
                      select(DistanceBetweenPlayers, ClosestDefenderToPasserDist,
                             ClosestDefenderToReceiverDist, DefendersNearReceiver,
                             PasserSpeed, ReceiverSpeed, ClosestDefenderToPasserSpeed,
                             ClosestDefenderToReceiverSpeed, Cluster_2, Cluster_3, Cluster_4)),
  label = IndirectCalibrationSet$CompletedPlay
)

IndirectTestDMatrix <- xgb.DMatrix(
  data  = as.matrix(IndirectFinalTestSet %>%
                      select(DistanceBetweenPlayers, ClosestDefenderToPasserDist,
                             ClosestDefenderToReceiverDist, DefendersNearReceiver,
                             PasserSpeed, ReceiverSpeed, ClosestDefenderToPasserSpeed,
                             ClosestDefenderToReceiverSpeed, Cluster_2, Cluster_3, Cluster_4)),
  label = IndirectFinalTestSet$CompletedPlay
)
indirect_params2 <- list(
  objective        = "binary:logistic",
  eval_metric      = "aucpr",
  max_depth        = 3,
  eta              = 0.03,
  min_child_weight = 10,
  lambda = 2.5,
  alpha = 0.1,
  gamma = 0.2,
  subsample = 0.8,
  colsample_bytree = 0.8,
  scale_pos_weight = sum(IndirectTrainingSet$CompletedPlay == 0) / 
    sum(IndirectTrainingSet$CompletedPlay == 1)  
)

indirect_cv_results2 <- xgb.cv(
  params                = indirect_params2,
  data                  = IndirectTrainDMatrix,
  nfold                 = 5,
  nrounds               = 1000,
  early_stopping_rounds = 50,
  verbose               = TRUE,
  print_every_n         = 10
)

indirect_best_round2 <- indirect_cv_results2$niter

indirect_final_model2 <- xgb.train(
  params  = indirect_params2,
  data    = IndirectTrainDMatrix,
  nrounds = indirect_best_round2
)

cal_preds_indirect <- predict(indirect_final_model2, IndirectCalibrationDMatrix)

platt_indirect <- glm(actual ~ pred,
                    data   = data.frame(pred   = cal_preds_indirect,
                                        actual = IndirectCalibrationSet$CompletedPlay),
                    family = binomial)
test_preds_raw_indirect <- predict(indirect_final_model2, IndirectTestDMatrix)

test_preds_calibrated_indirect <- predict(platt_indirect,
                                        newdata = data.frame(pred = test_preds_raw_indirect),
                                        type    = "response")
mean(test_preds_calibrated_indirect)
mean(IndirectFinalTestSet$CompletedPlay)

data.frame(pred   = test_preds_calibrated_indirect,
           actual = IndirectFinalTestSet$CompletedPlay) %>%
  mutate(bucket = ntile(pred, 10)) %>%
  group_by(bucket) %>%
  summarise(mean_pred   = mean(pred),
            mean_actual = mean(actual)) %>%
  ggplot(aes(x = mean_pred, y = mean_actual)) +
  geom_point(size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Indirect Model Calibration — Test Set",
       x = "Mean Predicted Probability",
       y = "Mean Actual Completion Rate") +
  theme_minimal()

indirect_importance2 <- xgb.importance(model = indirect_final_model2)

#Create predications on all plays with data
DirectParams <- DirectFramesTrackingWithEvents4 %>%
  select(DistanceBetweenPlayers, ClosestDefenderToPasserDist, ClosestDefenderToReceiverDist,
         DefendersNearReceiver, PasserSpeed, ReceiverSpeed, ClosestDefenderToPasserSpeed,
         ClosestDefenderToReceiverSpeed, Cluster_2, Cluster_3, Cluster_4)
DirectParamsMatrix <- as.matrix(DirectParams)
DirectLabel <- DirectFramesTrackingWithEvents4$CompletedPlay

DirectTrain <- xgb.DMatrix(data = DirectParamsMatrix, label = DirectLabel)

IndirectParams <- IndirectFramesTrackingWithEvents4 %>%
  select(DistanceBetweenPlayers, ClosestDefenderToPasserDist, ClosestDefenderToReceiverDist,
         DefendersNearReceiver, PasserSpeed, ReceiverSpeed, ClosestDefenderToPasserSpeed,
         ClosestDefenderToReceiverSpeed, Cluster_2, Cluster_3, Cluster_4)
IndirectParamsMatrix <- as.matrix(IndirectParams)
IndirectLabel <- IndirectFramesTrackingWithEvents4$CompletedPlay

IndirectTrain <- xgb.DMatrix(data = IndirectParamsMatrix, label = IndirectLabel)

DirectFramesTrackingWithEvents5 <- DirectFramesTrackingWithEvents4 %>%
  mutate(ExpectedCompletionProb = predict(platt_direct,
                                          newdata = data.frame(
                                            pred = predict(direct_final_model2, DirectTrain)),
                                          type = "response"))

IndirectFramesTrackingWithEvents5 <- IndirectFramesTrackingWithEvents4 %>%
  mutate(ExpectedCompletionProb = predict(platt_indirect,
                                          newdata = data.frame(
                                            pred = predict(indirect_final_model2, IndirectTrain)),
                                          type = "response"))
mean(DirectFramesTrackingWithEvents5$ExpectedCompletionProb)
mean(DirectFramesTrackingWithEvents5$CompletedPlay)
mean(IndirectFramesTrackingWithEvents5$ExpectedCompletionProb)
mean(IndirectFramesTrackingWithEvents5$CompletedPlay)

AllPlaysExpectedProbs <- rbind(DirectFramesTrackingWithEvents5 %>% mutate(PlayType = "Direct"),
                               IndirectFramesTrackingWithEvents5 %>% mutate(PlayType = "Indirect"))

AllPlaysExpectedProbsSummary <- AllPlaysExpectedProbs %>%
  group_by(EventPasserID, EventReceiverID) %>%
  summarise(TotalPlays = n(),
            TotalDirect = sum(PlayType=="Direct"),
            TotalIndirect = sum(PlayType=="Indirect"),
            ActualCompletions = sum(CompletedPlay),
            ExpectedCompletions = sum(ExpectedCompletionProb),
            CompletionsAboveExpected = ActualCompletions-ExpectedCompletions,
            ChemistryRate = CompletionsAboveExpected/TotalPlays,
            ActualCompletionRate = ActualCompletions / TotalPlays,
            ExpectedCompletionRate = ExpectedCompletions / TotalPlays,
            DirectActualCompletions = sum(CompletedPlay[PlayType=="Direct"]),
            DirectExpectedCompletions = sum(ExpectedCompletionProb[PlayType=="Direct"]),
            DirectCompletionsAboveExpected = DirectActualCompletions-DirectExpectedCompletions,
            DirectChemistryRate = DirectCompletionsAboveExpected/TotalDirect,
            DirectActualCompRate = DirectActualCompletions/TotalDirect,
            DirectExpectedCompRate = DirectExpectedCompletions/TotalDirect,
            IndirectActualCompletions = sum(CompletedPlay[PlayType=="Indirect"]),
            IndirectExpectedCompletions = sum(ExpectedCompletionProb[PlayType=="Indirect"]),
            IndirectCompletionsAboveExpected = IndirectActualCompletions-IndirectExpectedCompletions,
            IndirectChemistryRate = IndirectCompletionsAboveExpected/TotalIndirect,
            IndirectActualCompRate = IndirectActualCompletions/TotalIndirect,
            IndirectExpectedCompRate = IndirectExpectedCompletions/TotalIndirect,
            .groups = "drop"
  ) %>%
  filter(TotalPlays >= 7) %>%   
  arrange(desc(CompletionsAboveExpected))

write.csv(AllPlaysExpectedProbsSummary, "~/Desktop/AllPlaysExpectedProbsSummary.csv", row.names = FALSE)

library(gt)
AllPlaysExpectedProbsSummary %>%
  select(EventPasserID, EventReceiverID, TotalPlays, ActualCompletions, ExpectedCompletions,
         CompletionsAboveExpected, ChemistryRate) %>%
  gt() |>
  cols_label(
    EventPasserID = "Passer",
    EventReceiverID = "Receiver",
    TotalPlays = "Total Plays",
    ActualCompletions = "Actual Completions",
    ExpectedCompletions = "Expected Completions",
    CompletionsAboveExpected = "Completions Above Expected (CAE)",
    ChemistryRate = "CAE Per Play"
  ) |>
  fmt_number(
    columns = c(ExpectedCompletions, CompletionsAboveExpected, ChemistryRate),
    decimals = 3 
  ) |>
  tab_header(
    title = md("**Passer and Reciever Chemistry**")
  ) |>
  cols_align(
    align = "center",
    columns = everything()
  )

AllPlaysExpectedProbsSummary %>%
  select(EventPasserID, EventReceiverID, TotalDirect, DirectActualCompletions, DirectExpectedCompletions,
         DirectCompletionsAboveExpected, DirectChemistryRate) %>%
  arrange(desc(DirectCompletionsAboveExpected)) %>%
  gt() |>
  cols_label(
    EventPasserID = "Passer",
    EventReceiverID = "Receiver",
    TotalDirect = "Total Plays",
    DirectActualCompletions = "Actual Completions",
    DirectExpectedCompletions = "Expected Completions",
    DirectCompletionsAboveExpected = "Completions Above Expected (CAE)",
    DirectChemistryRate = "CAE Per Play"
  ) |>
  fmt_number(
    columns = c(DirectExpectedCompletions, DirectCompletionsAboveExpected, DirectChemistryRate),
    decimals = 3 
  ) |>
  tab_header(
    title = md("**Passer and Reciever Chemistry**"),
    subtitle = ("Direct Plays Only")
  ) |>
  cols_align(
    align = "center",
    columns = everything()
  )

AllPlaysExpectedProbsSummary %>%
  select(EventPasserID, EventReceiverID, TotalIndirect, IndirectActualCompletions, IndirectExpectedCompletions,
         IndirectCompletionsAboveExpected, IndirectChemistryRate) %>%
  arrange(desc(IndirectCompletionsAboveExpected)) %>%
  gt() |>
  cols_label(
    EventPasserID = "Passer",
    EventReceiverID = "Receiver",
    TotalIndirect = "Total Plays",
    IndirectActualCompletions = "Actual Completions",
    IndirectExpectedCompletions = "Expected Completions",
    IndirectCompletionsAboveExpected = "Completions Above Expected (CAE)",
    IndirectChemistryRate = "CAE Per Play"
  ) |>
  fmt_number(
    columns = c(IndirectExpectedCompletions, IndirectCompletionsAboveExpected, IndirectChemistryRate),
    decimals = 3 
  ) |>
  tab_header(
    title = md("**Passer and Reciever Chemistry**"),
    subtitle = ("Indirect Plays Only")
  ) |>
  cols_align(
    align = "center",
    columns = everything()
  )
