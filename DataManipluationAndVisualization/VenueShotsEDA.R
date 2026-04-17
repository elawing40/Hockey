#Venue Shots EDA
library(dplyr)
library(ggplot2)
library(ggrepel)
library(patchwork)

ShotsColNamesDifference <- setdiff(colnames(shots_2024),colnames(shots_2023))

shots_2024_v2 <- shots_2024 %>%
  select(-all_of(ShotsColNamesDifference))

shots2023_3.30.2026 <- rbind(shots_2023, shots_2024_v2, shots_3.30.2026) %>%
  mutate(shotType = ifelse(shotType == "" | is.na(shotType), "UNKNOWN", shotType))

shots_2023_shot_distrib <- shots_2023 %>%
  mutate(shotType = ifelse(shotType == "" | is.na(shotType), "UNKNOWN", shotType)) %>%
  group_by(shotType) %>%
  summarise(ShotAttempts = n(), .groups = "drop") %>%
  mutate(PercentOfAttempts = round((ShotAttempts/sum(ShotAttempts))*100,3))

league_shot_distrib2023_24 <- ggplot(shots_2023_shot_distrib, aes(x="", y=PercentOfAttempts, fill=shotType))+
  geom_col()+
  coord_polar(theta = "y")+
  geom_label_repel(aes(label = paste0(shotType,", ",PercentOfAttempts,"%")),
                   position=position_stack(vjust=0.5),
                   show.legend=FALSE,
                   size=3,
                   segment.size=0.3) +
  labs(title = "NHL Shot Type Distriubtion 2023-2024") +
  theme_void()

shots_2024_26_shot_distrib <- rbind(shots_2024_v2,shots_3.30.2026) %>%
  mutate(shotType = ifelse(shotType == "" | is.na(shotType), "UNKNOWN", shotType)) %>%
  group_by(shotType) %>%
  summarise(ShotAttempts = n(), .groups = "drop") %>%
  mutate(PercentOfAttempts = round((ShotAttempts/sum(ShotAttempts))*100,3))

league_shot_distrib2024_26 <- ggplot(shots_2024_26_shot_distrib, aes(x="", y=PercentOfAttempts, fill=shotType))+
                              geom_col()+
                              coord_polar(theta = "y")+
                  geom_label_repel(aes(label = paste0(shotType,", ",PercentOfAttempts,"%")),
                   position=position_stack(vjust=0.5),
                   show.legend=FALSE,
                   size=3,
                   segment.size=0.3) +
  labs(title = "NHL Shot Type Distriubtion 2024-2026") +
  theme_void()

league_shot_distrib2023_24/league_shot_distrib2024_26


shots2023_3.30.2026_shot_type_distrib <- shots2023_3.30.2026 %>%
  group_by(shotType) %>%
  summarise(ShotAttempts = n(), .groups = "drop") %>%
  mutate(PercentOfAttempts = round((ShotAttempts/sum(ShotAttempts))*100,3))

shots2023_3.30.2026_venue_shot_type_distrib <- shots2023_3.30.2026 %>%
  group_by(homeTeamCode, shotType) %>%
  summarise(ShotAttempts = n(), .groups = "drop") %>%
  group_by(homeTeamCode) %>%
  mutate(PercentOfVenueAttempts = round((ShotAttempts/sum(ShotAttempts))*100,3))

pie(shots2023_3.30.2026_shot_type_distrib$PercentOfAttempts,
    labels = paste0(shots2023_3.30.2026_shot_type_distrib$shotType,", ", shots2023_3.30.2026_shot_type_distrib$PercentOfAttempts,"%"), 
    main = "NHL Overall Shot Distribution")

ggplot(shots2023_3.30.2026_shot_type_distrib, aes(x="", y=PercentOfAttempts, fill=shotType))+
  geom_col()+
  coord_polar(theta = "y")+
  geom_label_repel(aes(label = paste0(shotType,", ",PercentOfAttempts,"%")),
                   position=position_stack(vjust=0.5),
                   show.legend=FALSE,
                   size=3,
                   segment.size=0.3) +
  labs(title = "NHL Shot Type Distriubtion 2023-2026") +
  theme_void()

teams_for_shot_distribs <- unique(shots2023_3.30.2026_venue_shot_type_distrib$homeTeamCode)

for (team in teams_for_shot_distribs) {
  teamData <- shots2023_3.30.2026_venue_shot_type_distrib %>%
    filter(homeTeamCode == team) %>%
    arrange(desc(shotType)) %>% 
    mutate(labelPos = cumsum(PercentOfVenueAttempts) - PercentOfVenueAttempts / 2)
  
  teamPieChart <- ggplot(teamData, aes(x="", y=PercentOfVenueAttempts, fill=shotType))+
    geom_col()+
    coord_polar(theta = "y") +
    geom_label_repel(aes(label = paste0(shotType,", ",PercentOfVenueAttempts,"%")),
                     position=position_stack(vjust=0.75),
                     show.legend=FALSE,
                     size=2,
                     segment.size=0.3) +
    labs(title = paste("Shot Distirubtion at", team, "Home Venue")) +
    theme_void()
  
  assign(paste0(team, "HomeVenueShotDistribPie"), teamPieChart)
}

UTAHomeShootingDistrib <- rbind(shots_2024_v2,shots_3.30.2026) %>%
  mutate(shotType = ifelse(shotType == "" | is.na(shotType), "UNKNOWN", shotType)) %>%
  filter(teamCode == "UTA", homeTeamCode == "UTA") %>%
  group_by(shotType) %>%
  summarise(ShotAttempts = n(), .groups = "drop") %>%
  group_by(homeTeamCode) %>%
  mutate(PercentOfVenueAttempts = round((ShotAttempts/sum(ShotAttempts))*100,3))

