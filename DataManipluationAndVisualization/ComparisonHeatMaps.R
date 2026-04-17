library(ggplot2)
library(dplyr)
library(nhlapi)
library(patchwork)

#Steven Stamkos Heat Map Comparison
StevenStamkos2025_26Shots <- shots_2025_1.6.25 %>%
  filter(shooterName == "Steven Stamkos")


StevenStamkosGames <- unique(StevenStamkos2025_26Shots$game_id)

StevenStamkosLast8Games <- tail(StevenStamkosGames, n=8)

StevenStamkosLast8GamesShots <- StevenStamkos2025_26Shots %>%
  filter(game_id %in% StevenStamkosLast8Games)

nhl_rink_plot() + theme_void() + stat_density_2d(data = StevenStamkosLast8GamesShots,
                                                 mapping = aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(nlevel)),
                                                 geom = "polygon", alpha = 0.3) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,1), name = "Relative Density") +
  geom_point(data = StevenStamkosLast8GamesShots, aes(x=arenaAdjustedXCord,y=arenaAdjustedYCord), alpha = 0.5) +
  scale_x_continuous(limits = c(-100,100)) + scale_y_continuous(limits = c(-100,100)) +
  coord_fixed(xlim = c(-95,95), ylim = c(-38.7,38.7)) + labs(title = "Steven Stamkos Shot Heat Map Last 8 Games",
                                                             subtitle = "Normalized Shot Volume")

StevenStamkosExceptLast8GamesShots <- StevenStamkos2025_26Shots %>%
  filter(!(game_id %in% StevenStamkosLast8Games))

nhl_rink_plot() + theme_void() + stat_density_2d(data = StevenStamkosExceptLast8GamesShots,
                                                 mapping = aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(nlevel)),
                                                 geom = "polygon", alpha = 0.3) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,1), name = "Relative Density") +
  geom_point(data = StevenStamkosExceptLast8GamesShots, aes(x=arenaAdjustedXCord,y=arenaAdjustedYCord), alpha = 0.5) +
  scale_x_continuous(limits = c(-100,100)) + scale_y_continuous(limits = c(-100,100)) +
  coord_fixed(xlim = c(-95,95), ylim = c(-38.7,38.7)) + labs(title = "Steven Stamkos Shot Heat Map Except Last 8 Games",
                                                           subtitle = "Normalized Shot Volume")


#Auston Matthews Heat Map Comparison
AustonMatthews2025_26Shots <- shots_2025_1.6.25 %>%
  filter(shooterName == "Auston Matthews")

get_max_density <- function(data, x_col, y_col) {
  density_data <- MASS::kde2d(data[[x_col]], data[[y_col]], n = 100)
  return(max(density_data$z))
}

# Calculate max density for each dataset
AMmax_density_last5 <- get_max_density(AustonMatthewsLast5GamesShots, 
                                     "arenaAdjustedXCord", "arenaAdjustedYCord")
AMmax_density_except <- get_max_density(AustonMatthewsExceptLast5GamesShots, 
                                      "arenaAdjustedXCord", "arenaAdjustedYCord")
AMmax_density_full <- get_max_density(AustonMatthews2025_26Shots, 
                                    "arenaAdjustedXCord", "arenaAdjustedYCord")

# Use the maximum across all datasets
AMmax_density <- max(AMmax_density_last5, AMmax_density_except, AMmax_density_full)

AustonMatthewsGames <- unique(AustonMatthews2025_26Shots$game_id)

AustonMatthewsLast5Games <- tail(AustonMatthewsGames, n=5)

AustonMatthewsLast5GamesShots <- AustonMatthews2025_26Shots %>%
  filter(game_id %in% AustonMatthewsLast5Games)

MatthewsHeatMapLast51.6 <- nhl_rink_plot() + theme_void() + stat_density_2d(data = AustonMatthewsLast5GamesShots,
                                                                            mapping = aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(nlevel)),
                                                                            geom = "polygon", alpha = 0.3) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,1), name = "Relative Density") +
  geom_point(data = AustonMatthewsLast5GamesShots, aes(x=arenaAdjustedXCord,y=arenaAdjustedYCord), alpha = 0.5) +
  scale_x_continuous(limits = c(-100,100)) + scale_y_continuous(limits = c(-100,100)) +
  coord_fixed(xlim = c(-95,95), ylim = c(-38.7,38.7)) + labs(subtitle = "Auston Matthews Shot Heat Map Last 5 Games\nNormalized Shot Volume")

AustonMatthewsExceptLast5GamesShots <- AustonMatthews2025_26Shots %>%
  filter(!(game_id %in% AustonMatthewsLast5Games))

MatthewsHeatMapExceptLast51.6 <- nhl_rink_plot() + theme_void() + stat_density_2d(data = AustonMatthewsExceptLast5GamesShots,
                                                 mapping = aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(nlevel)),
                                                 geom = "polygon", alpha = 0.3) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,1), name = "Relative Density") +
  geom_point(data = AustonMatthewsExceptLast5GamesShots, aes(x=arenaAdjustedXCord,y=arenaAdjustedYCord), alpha = 0.5) +
  scale_x_continuous(limits = c(-100,100)) + scale_y_continuous(limits = c(-100,100)) +
  coord_fixed(xlim = c(-95,95), ylim = c(-38.7,38.7)) + labs(subtitle = "Auston Matthews Shot Heat Map Except Last 5 Games\nNormalized Shot Volume")

MatthewsHeatMap1.6 <- nhl_rink_plot() + theme_void() + stat_density_2d(data = AustonMatthews2025_26Shots,
                                                                                  mapping = aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(nlevel)),
                                                                                  geom = "polygon", alpha = 0.3) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,1), name = "Relative Density") +
  geom_point(data = AustonMatthews2025_26Shots, aes(x=arenaAdjustedXCord,y=arenaAdjustedYCord), alpha = 0.5) +
  scale_x_continuous(limits = c(-100,100)) + scale_y_continuous(limits = c(-100,100)) +
  coord_fixed(xlim = c(-95,95), ylim = c(-38.7,38.7)) + labs(subtitle = "Auston Matthews Shot Heat Map Full Season\nNormalized Shot Volume")

(MatthewsHeatMapLast51.6 / MatthewsHeatMapExceptLast51.6 / MatthewsHeatMap1.6) +
  plot_annotation(title = "Comparing Auston Matthews Shot Volume",
                  subtitle = "Each Plot is Independent of Each Other")


MatthewsHeatMapLast51.6Pt2 <- nhl_rink_plot() + theme_void() + stat_density_2d(data = AustonMatthewsLast5GamesShots,
                                                                            mapping = aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(level)),
                                                                            geom = "polygon", alpha = 0.3) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,AMmax_density), name = "Shot Density") +
  geom_point(data = AustonMatthewsLast5GamesShots, aes(x=arenaAdjustedXCord,y=arenaAdjustedYCord), alpha = 0.5) +
  scale_x_continuous(limits = c(-100,100)) + scale_y_continuous(limits = c(-100,100)) +
  coord_fixed(xlim = c(-95,95), ylim = c(-38.7,38.7)) + labs(subtitle = "Auston Matthews Shot Heat Map Last 5 Games")

MatthewsHeatMapExceptLast51.6Pt2 <- nhl_rink_plot() + theme_void() + stat_density_2d(data = AustonMatthewsExceptLast5GamesShots,
                                                                                  mapping = aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(level)),
                                                                                  geom = "polygon", alpha = 0.3) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,AMmax_density), name = "Shot Density") +
  geom_point(data = AustonMatthewsExceptLast5GamesShots, aes(x=arenaAdjustedXCord,y=arenaAdjustedYCord), alpha = 0.5) +
  scale_x_continuous(limits = c(-100,100)) + scale_y_continuous(limits = c(-100,100)) +
  coord_fixed(xlim = c(-95,95), ylim = c(-38.7,38.7)) + labs(subtitle = "Auston Matthews Shot Heat Map Except Last 5 Games")

MatthewsHeatMap1.6Pt2 <- nhl_rink_plot() + theme_void() + stat_density_2d(data = AustonMatthews2025_26Shots,
                                                                       mapping = aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(level)),
                                                                       geom = "polygon", alpha = 0.3) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,AMmax_density), name = "Shot Density") +
  geom_point(data = AustonMatthews2025_26Shots, aes(x=arenaAdjustedXCord,y=arenaAdjustedYCord), alpha = 0.5) +
  scale_x_continuous(limits = c(-100,100)) + scale_y_continuous(limits = c(-100,100)) +
  coord_fixed(xlim = c(-95,95), ylim = c(-38.7,38.7)) + labs(subtitle = "Auston Matthews Shot Heat Map Full Season")

(MatthewsHeatMapLast51.6Pt2 / MatthewsHeatMapExceptLast51.6Pt2 / MatthewsHeatMap1.6Pt2) +
  plot_annotation(title = "Comparing Auston Matthews Shot Volume",
                  subtitle = "Density Shot Volumes Relative to the Full Season Maximum")


#Morgan Geekie Heat Map Comparison
MorganGeekie2025_26Shots <- shots_1.23.2026 %>%
  filter(shooterName == "Morgan Geekie")

MorganGeekieGames <- unique(MorganGeekie2025_26Shots$game_id)

MorganGeekieLast14Games <- tail(MorganGeekieGames, n=14)

MorganGeekieLast14GamesShots <- MorganGeekie2025_26Shots %>%
  filter(game_id %in% MorganGeekieLast14Games)

MorganGeekieExceptLast14GamesShots <- MorganGeekie2025_26Shots %>%
  filter(!(game_id %in% MorganGeekieLast14Games))

MGmax_density_last14 <- get_max_density(MorganGeekieLast14GamesShots, 
                                       "arenaAdjustedXCord", "arenaAdjustedYCord")
MGmax_density_except <- get_max_density(MorganGeekieExceptLast14GamesShots, 
                                        "arenaAdjustedXCord", "arenaAdjustedYCord")
MGmax_density_full <- get_max_density(MorganGeekie2025_26Shots, 
                                      "arenaAdjustedXCord", "arenaAdjustedYCord")

MGmax_density <- max(MGmax_density_last14, MGmax_density_except, MGmax_density_full)

GeekieHeatMapLast14.1.23 <- nhl_rink_plot() + theme_void() + stat_density_2d(data = MorganGeekieLast14GamesShots,
                                                                            mapping = aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(nlevel)),
                                                                            geom = "polygon", alpha = 0.3) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,1), name = "Relative Density") +
  geom_point(data = MorganGeekieLast14GamesShots, aes(x=arenaAdjustedXCord,y=arenaAdjustedYCord, color = goal), alpha = 0.5, show.legend = FALSE) +
  scale_x_continuous(limits = c(-100,100)) + scale_y_continuous(limits = c(-100,100)) +
  coord_fixed(xlim = c(-93,93), ylim = c(-38.7,38.7)) + labs(subtitle = "Morgan Geekie Shot Heat Map Last 14 Games\nNormalized Shot Volume") +
  guides(color = "none")

GeekieHeatMapExceptLast14.1.23 <- nhl_rink_plot() + theme_void() + stat_density_2d(data = MorganGeekieExceptLast14GamesShots,
                                                                                  mapping = aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(nlevel)),
                                                                                  geom = "polygon", alpha = 0.3) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,1), name = "Relative Density") +
  geom_point(data = MorganGeekieExceptLast14GamesShots, aes(x=arenaAdjustedXCord,y=arenaAdjustedYCord, color = goal), alpha = 0.5, show.legend = FALSE) +
  scale_x_continuous(limits = c(-100,100)) + scale_y_continuous(limits = c(-100,100)) +
  coord_fixed(xlim = c(-95,95), ylim = c(-38.7,38.7)) + labs(subtitle = "Morgan Geekie Shot Heat Map Except Last 14 Games\nNormalized Shot Volume") +
  guides(color = "none")

GeekieHeatMap1.23 <- nhl_rink_plot() + theme_void() + stat_density_2d(data = MorganGeekie2025_26Shots,
                                                                       mapping = aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(nlevel)),
                                                                       geom = "polygon", alpha = 0.3) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,1), name = "Relative Density") +
  geom_point(data = MorganGeekie2025_26Shots, aes(x=arenaAdjustedXCord,y=arenaAdjustedYCord, color = goal), alpha = 0.5, show.legend = FALSE) +
  scale_x_continuous(limits = c(-100,100)) + scale_y_continuous(limits = c(-100,100)) +
  coord_fixed(xlim = c(-95,95), ylim = c(-38.7,38.7)) + labs(subtitle = "Morgan Geekie Shot Heat Map Full Season\nNormalized Shot Volume") +
  guides(color = "none")

(GeekieHeatMapLast14.1.23 / GeekieHeatMapExceptLast14.1.23 / GeekieHeatMap1.23) +
  plot_annotation(title = "Comparing Morgan Geekie Shot Volume",
                  subtitle = "Each Plot is Independent of Each Other")


GeekieHeatMapLast14.1.23.Pt2 <- nhl_rink_plot() + theme_void() + stat_density_2d(data = MorganGeekieLast14GamesShots,
                                                                               mapping = aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(level)),
                                                                               geom = "polygon", alpha = 0.3) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,MGmax_density), name = "Shot Density") +
  geom_point(data = MorganGeekieLast14GamesShots, aes(x=arenaAdjustedXCord,y=arenaAdjustedYCord, color = goal), alpha = 0.5, show.legend = FALSE) +
  scale_x_continuous(limits = c(-100,100)) + scale_y_continuous(limits = c(-100,100)) +
  coord_fixed(xlim = c(-95,95), ylim = c(-38.7,38.7)) + labs(subtitle = "Morgan Geekie Shot Heat Map Last 14 Games") +
  guides(color = "none")

GeekieHeatMapExceptLast14.1.23.Pt2 <- nhl_rink_plot() + theme_void() + stat_density_2d(data = MorganGeekieExceptLast14GamesShots,
                                                                                     mapping = aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(level)),
                                                                                     geom = "polygon", alpha = 0.3) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,MGmax_density), name = "Shot Density") +
  geom_point(data = MorganGeekieExceptLast14GamesShots, aes(x=arenaAdjustedXCord,y=arenaAdjustedYCord, color = goal), alpha = 0.5, show.legend = FALSE) +
  scale_x_continuous(limits = c(-100,100)) + scale_y_continuous(limits = c(-100,100)) +
  coord_fixed(xlim = c(-95,95), ylim = c(-38.7,38.7)) + labs(subtitle = "Morgan Geekie Shot Heat Map Except Last 14 Games") +
  guides(color = "none")

GeekieHeatMap1.23Pt2 <- nhl_rink_plot() + theme_void() + stat_density_2d(data = MorganGeekie2025_26Shots,
                                                                          mapping = aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(level)),
                                                                          geom = "polygon", alpha = 0.3) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,MGmax_density), name = "Shot Density") +
  geom_point(data = MorganGeekie2025_26Shots, aes(x=arenaAdjustedXCord,y=arenaAdjustedYCord, color = goal), alpha = 0.5, show.legend = FALSE) +
  scale_x_continuous(limits = c(-100,100)) + scale_y_continuous(limits = c(-100,100)) +
  coord_fixed(xlim = c(-95,95), ylim = c(-38.7,38.7)) + labs(subtitle = "Morgan Geekie Shot Heat Map Full Season") +
  guides(color = "none")

(GeekieHeatMapLast14.1.23.Pt2 / GeekieHeatMapExceptLast14.1.23.Pt2 / GeekieHeatMap1.23Pt2) +
  plot_annotation(title = "Comparing Morgan Geekie Shot Volume",
                  subtitle = "Density Shot Volumes Relative to the Full Season Maximum")



#Morgan Geekie Heat Map Comparison 2
MorganGeekie2025_26Shots <- shots_1.28.2026 %>%
  filter(shooterName == "Morgan Geekie")

MorganGeekieGames <- unique(MorganGeekie2025_26Shots$game_id)

MorganGeekieLast5Games <- tail(MorganGeekieGames, n=5)

MorganGeekieSlumpGames <- MorganGeekieGames[36:47]

MorganGeekieLast5GamesShots <- MorganGeekie2025_26Shots %>%
  filter(game_id %in% MorganGeekieLast5Games)

MorganGeekieSlumpGamesShots <- MorganGeekie2025_26Shots %>%
  filter(game_id %in% MorganGeekieSlumpGames)

MGmax_density_last5 <- get_max_density(MorganGeekieLast5GamesShots, 
                                        "arenaAdjustedXCord", "arenaAdjustedYCord")
MGmax_density_slump <- get_max_density(MorganGeekieSlumpGamesShots, 
                                        "arenaAdjustedXCord", "arenaAdjustedYCord")
MGmax_density_full <- get_max_density(MorganGeekie2025_26Shots, 
                                      "arenaAdjustedXCord", "arenaAdjustedYCord")

MGmax_density2 <- max(MGmax_density_last5, MGmax_density_slump, MGmax_density_full)

GeekieHeatMapLast5.1.28 <- nhl_rink_plot() + theme_void() + stat_density_2d(data = MorganGeekieLast5GamesShots,
                                                                             mapping = aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(nlevel)),
                                                                             geom = "polygon", alpha = 0.3) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,1), name = "Relative Density") +
  geom_point(data = MorganGeekieLast5GamesShots, aes(x=arenaAdjustedXCord,y=arenaAdjustedYCord, color = goal), alpha = 0.5, show.legend = FALSE) +
  scale_x_continuous(limits = c(-100,100)) + scale_y_continuous(limits = c(-100,100)) +
  coord_fixed(xlim = c(-93,93), ylim = c(-38.7,38.7)) + labs(subtitle = "Morgan Geekie Shot Heat Map Last 5 Games\nNormalized Shot Volume") +
  guides(color = "none")

GeekieHeatMapSlump.1.28 <- nhl_rink_plot() + theme_void() + stat_density_2d(data = MorganGeekieSlumpGamesShots,
                                                                                   mapping = aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(nlevel)),
                                                                                   geom = "polygon", alpha = 0.3) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,1), name = "Relative Density") +
  geom_point(data = MorganGeekieSlumpGamesShots, aes(x=arenaAdjustedXCord,y=arenaAdjustedYCord, color = goal), alpha = 0.5, show.legend = FALSE) +
  scale_x_continuous(limits = c(-100,100)) + scale_y_continuous(limits = c(-100,100)) +
  coord_fixed(xlim = c(-95,95), ylim = c(-38.7,38.7)) + labs(subtitle = "Morgan Geekie Shot Heat Map 12/20 to 1/17 Games\nNormalized Shot Volume") +
  guides(color = "none")

GeekieHeatMap1.28 <- nhl_rink_plot() + theme_void() + stat_density_2d(data = MorganGeekie2025_26Shots,
                                                                      mapping = aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(nlevel)),
                                                                      geom = "polygon", alpha = 0.3) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,1), name = "Relative Density") +
  geom_point(data = MorganGeekie2025_26Shots, aes(x=arenaAdjustedXCord,y=arenaAdjustedYCord, color = goal), alpha = 0.5, show.legend = FALSE) +
  scale_x_continuous(limits = c(-100,100)) + scale_y_continuous(limits = c(-100,100)) +
  coord_fixed(xlim = c(-95,95), ylim = c(-38.7,38.7)) + labs(subtitle = "Morgan Geekie Shot Heat Map Full Season\nNormalized Shot Volume") +
  guides(color = "none")

(GeekieHeatMapLast5.1.28 / GeekieHeatMapSlump.1.28 / GeekieHeatMap1.28) +
  plot_annotation(title = "Comparing Morgan Geekie Shot Volume as of 1/28",
                  subtitle = "Each Plot is Independent of Each Other")


GeekieHeatMapLast5.1.28.Pt2 <- nhl_rink_plot() + theme_void() + stat_density_2d(data = MorganGeekieLast5GamesShots,
                                                                                 mapping = aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(level)),
                                                                                 geom = "polygon", alpha = 0.3) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,MGmax_density2), name = "Shot Density") +
  geom_point(data = MorganGeekieLast5GamesShots, aes(x=arenaAdjustedXCord,y=arenaAdjustedYCord, color = goal), alpha = 0.5, show.legend = FALSE) +
  scale_x_continuous(limits = c(-100,100)) + scale_y_continuous(limits = c(-100,100)) +
  coord_fixed(xlim = c(-95,95), ylim = c(-38.7,38.7)) + labs(subtitle = "Morgan Geekie Shot Heat Map Last 5 Games") +
  guides(color = "none")

GeekieHeatMapSlump.1.28.Pt2 <- nhl_rink_plot() + theme_void() + stat_density_2d(data = MorganGeekieSlumpGamesShots,
                                                                                       mapping = aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(level)),
                                                                                       geom = "polygon", alpha = 0.3) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,MGmax_density2), name = "Shot Density") +
  geom_point(data = MorganGeekieSlumpGamesShots, aes(x=arenaAdjustedXCord,y=arenaAdjustedYCord, color = goal), alpha = 0.5, show.legend = FALSE) +
  scale_x_continuous(limits = c(-100,100)) + scale_y_continuous(limits = c(-100,100)) +
  coord_fixed(xlim = c(-95,95), ylim = c(-38.7,38.7)) + labs(subtitle = "Morgan Geekie Shot Heat Map 12/20 to 1/17") +
  guides(color = "none")

GeekieHeatMap1.28Pt2 <- nhl_rink_plot() + theme_void() + stat_density_2d(data = MorganGeekie2025_26Shots,
                                                                         mapping = aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(level)),
                                                                         geom = "polygon", alpha = 0.3) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,MGmax_density2), name = "Shot Density") +
  geom_point(data = MorganGeekie2025_26Shots, aes(x=arenaAdjustedXCord,y=arenaAdjustedYCord, color = goal), alpha = 0.5, show.legend = FALSE) +
  scale_x_continuous(limits = c(-100,100)) + scale_y_continuous(limits = c(-100,100)) +
  coord_fixed(xlim = c(-95,95), ylim = c(-38.7,38.7)) + labs(subtitle = "Morgan Geekie Shot Heat Map Full Season") +
  guides(color = "none")

(GeekieHeatMapLast5.1.28.Pt2 / GeekieHeatMapSlump.1.28.Pt2 / GeekieHeatMap1.28Pt2) +
  plot_annotation(title = "Comparing Morgan Geekie Shot Volume as of 1/28",
                  subtitle = "Density Shot Volumes Relative to the Full Season Maximum")

#Jack Hughes Heat Map
JackHughes2025_26Shots <- shots_2.23.2026 %>%
  filter(shooterName == "Jack Hughes") %>%
  filter(shotOnEmptyNet == 0)

JackHughesHeatMap2.23 <- nhl_rink_plot() + theme_void() + stat_density_2d(data = JackHughes2025_26Shots,
                                                                      mapping = aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(nlevel)),
                                                                      geom = "polygon", alpha = 0.3) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,1)) +
  geom_point(data = JackHughes2025_26Shots, aes(x=arenaAdjustedXCord,y=arenaAdjustedYCord, color = goal), alpha = 0.5, show.legend = FALSE) +
  scale_x_continuous(limits = c(-100,100)) + scale_y_continuous(limits = c(-100,100)) +
  coord_fixed(xlim = c(-95,95), ylim = c(-38.7,38.7)) + labs(title = "Jack Hughes Shot Heat Map 2025-26 Season") +
  guides(color = "none")

#Matvei Michkov Heatmap Comparison
MatveiMichkov2025_26Shots <- shots_2.23.2026 %>%
  filter(shooterName == "Matvei Michkov") %>%
  filter(shotOnEmptyNet == 0)

MatveiMichkov2024_25Shots <- shots_2024_25_FullSeason %>%
  filter(shooterName == "Matvei Michkov") %>%
  filter(shotOnEmptyNet == 0)

MMmax_density_2025_26 <- get_max_density(MatveiMichkov2025_26Shots, 
                                       "arenaAdjustedXCord", "arenaAdjustedYCord")
MMmax_density_2024_25 <- get_max_density(MatveiMichkov2024_25Shots, 
                                       "arenaAdjustedXCord", "arenaAdjustedYCord")
MMmax_density <- max(MMmax_density_2025_26, MMmax_density_2024_25)

MatveiMichkov2025_26HeatMap <- nhl_rink_plot() + theme_void() + stat_density_2d(data = MatveiMichkov2025_26Shots,
                                                                            mapping = aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(nlevel)),
                                                                            geom = "polygon", alpha = 0.3) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,1), name = "Relative Density") +
  geom_point(data = MatveiMichkov2025_26Shots, aes(x=arenaAdjustedXCord,y=arenaAdjustedYCord, color = goal), alpha = 0.5, show.legend = FALSE) +
  scale_x_continuous(limits = c(-100,100)) + scale_y_continuous(limits = c(-100,100)) +
  coord_fixed(xlim = c(-93,93), ylim = c(-38.7,38.7)) + labs(subtitle = "Matvei Michkov 2025-26 Shooting Heat Map") +
  guides(color = "none")

MatveiMichkov2024_25HeatMap <- nhl_rink_plot() + theme_void() + stat_density_2d(data = MatveiMichkov2024_25Shots,
                                                                                mapping = aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(nlevel)),
                                                                                geom = "polygon", alpha = 0.3) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,1), name = "Relative Density") +
  geom_point(data = MatveiMichkov2024_25Shots, aes(x=arenaAdjustedXCord,y=arenaAdjustedYCord, color = goal), alpha = 0.5, show.legend = FALSE) +
  scale_x_continuous(limits = c(-100,100)) + scale_y_continuous(limits = c(-100,100)) +
  coord_fixed(xlim = c(-93,93), ylim = c(-38.7,38.7)) + labs(subtitle = "Matvei Michkov 2024-25 Shooting Heat Map") +
  guides(color = "none")


(MatveiMichkov2025_26HeatMap / MatveiMichkov2024_25HeatMap) +
  plot_annotation(title = "Comparing Matvei Michkov Shot Volume Past Two Seasons",
                  subtitle = "Each Plot is Independent of Each Other")


MatveiMichkov2025_26HeatMap2 <- nhl_rink_plot() + theme_void() + stat_density_2d(data = MatveiMichkov2025_26Shots,
                                                                                mapping = aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(level)),
                                                                                geom = "polygon", alpha = 0.3) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,MMmax_density), name = "Shot Density") +
  geom_point(data = MatveiMichkov2025_26Shots, aes(x=arenaAdjustedXCord,y=arenaAdjustedYCord, color = goal), alpha = 0.5, show.legend = FALSE) +
  scale_x_continuous(limits = c(-100,100)) + scale_y_continuous(limits = c(-100,100)) +
  coord_fixed(xlim = c(-95,95), ylim = c(-38.7,38.7)) + labs(subtitle = "Matvei Michkov 2025-26 Shooting Heat Map") +
  guides(color = "none")

MatveiMichkov2024_25HeatMap2 <- nhl_rink_plot() + theme_void() + stat_density_2d(data = MatveiMichkov2024_25Shots,
                                                                                mapping = aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(level)),
                                                                                geom = "polygon", alpha = 0.3) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,MMmax_density), name = "Shot Density") +
  geom_point(data = MatveiMichkov2024_25Shots, aes(x=arenaAdjustedXCord,y=arenaAdjustedYCord, color = goal), alpha = 0.5, show.legend = FALSE) +
  scale_x_continuous(limits = c(-100,100)) + scale_y_continuous(limits = c(-100,100)) +
  coord_fixed(xlim = c(-95,95), ylim = c(-38.7,38.7)) + labs(subtitle = "Matvei Michkov 2024-25 Shooting Heat Map") +
  guides(color = "none")


(MatveiMichkov2025_26HeatMap2 / MatveiMichkov2024_25HeatMap2) +
  plot_annotation(title = "Comparing Matvei Michkov Shooting Volume",
                  subtitle = "Density Shot Volumes Relative to the Maximum")

#Matvei Michkov Power Play
MatveiMichkov2025_26PPShots <- MatveiMichkov2025_26Shots %>%
  mutate(FlyersSkaters = ifelse(team=="AWAY", awaySkatersOnIce, homeSkatersOnIce),
         OppSkaters = ifelse(team=="AWAY", homeSkatersOnIce, awaySkatersOnIce),
         OnPP = ifelse(FlyersSkaters > OppSkaters & OppSkaters < 5, 1, 0)) %>%
  filter(OnPP == 1)

MatveiMichkov2024_25PPShots <- MatveiMichkov2024_25Shots %>%
  mutate(FlyersSkaters = ifelse(team=="AWAY", awaySkatersOnIce, homeSkatersOnIce),
         OppSkaters = ifelse(team=="AWAY", homeSkatersOnIce, awaySkatersOnIce),
         OnPP = ifelse(FlyersSkaters > OppSkaters & OppSkaters < 5, 1, 0)) %>%
  filter(OnPP == 1)


MMPPmax_density_2025_26 <- get_max_density(MatveiMichkov2025_26PPShots, 
                                         "arenaAdjustedXCord", "arenaAdjustedYCord")
MMPPmax_density_2024_25 <- get_max_density(MatveiMichkov2024_25PPShots, 
                                         "arenaAdjustedXCord", "arenaAdjustedYCord")
MMPPmax_density <- max(MMPPmax_density_2025_26, MMPPmax_density_2024_25)

MatveiMichkov2025_26PPHeatMap <- nhl_rink_plot() + theme_void() + stat_density_2d(data = MatveiMichkov2025_26PPShots,
                                                                                mapping = aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(nlevel)),
                                                                                geom = "polygon", alpha = 0.3) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,1), name = "Relative Density") +
  geom_point(data = MatveiMichkov2025_26PPShots, aes(x=arenaAdjustedXCord,y=arenaAdjustedYCord, color = goal), alpha = 0.5, show.legend = FALSE) +
  scale_x_continuous(limits = c(-100,100)) + scale_y_continuous(limits = c(-100,100)) +
  coord_fixed(xlim = c(-93,93), ylim = c(-38.7,38.7)) + labs(title = "Matvei Michkov 2025-26 Power Play Shooting Heat Map",
                                                             subtitle = "Total PP TOI 113:37, 42.43% of Flyers PP Time") +
  guides(color = "none")

MatveiMichkov2024_25PPHeatMap <- nhl_rink_plot() + theme_void() + stat_density_2d(data = MatveiMichkov2024_25PPShots,
                                                                                mapping = aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(nlevel)),
                                                                                geom = "polygon", alpha = 0.3) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,1), name = "Relative Density") +
  geom_point(data = MatveiMichkov2024_25PPShots, aes(x=arenaAdjustedXCord,y=arenaAdjustedYCord, color = goal), alpha = 0.5, show.legend = FALSE) +
  scale_x_continuous(limits = c(-100,100)) + scale_y_continuous(limits = c(-100,100)) +
  coord_fixed(xlim = c(-93,93), ylim = c(-38.7,38.7)) + labs(title = "Matvei Michkov 2024-25 Power Play Shooting Heat Map",
                                                             subtitle = "Total PP TOI 225:22, 61.99% of Flyers PP Time") +
  guides(color = "none")


(MatveiMichkov2025_26PPHeatMap / MatveiMichkov2024_25PPHeatMap) +
  plot_annotation(title = "Comparing Matvei Michkov Power Play Shot Volume Past Two Seasons",
                  subtitle = "Each Plot is Independent of Each Other")


MatveiMichkov2025_26PPHeatMap2 <- nhl_rink_plot() + theme_void() + stat_density_2d(data = MatveiMichkov2025_26PPShots,
                                                                                 mapping = aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(level)),
                                                                                 geom = "polygon", alpha = 0.3) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,MMPPmax_density), name = "Shot Density") +
  geom_point(data = MatveiMichkov2025_26PPShots, aes(x=arenaAdjustedXCord,y=arenaAdjustedYCord, color = goal), alpha = 0.5, show.legend = FALSE) +
  scale_x_continuous(limits = c(-100,100)) + scale_y_continuous(limits = c(-100,100)) +
  coord_fixed(xlim = c(-95,95), ylim = c(-38.7,38.7)) + labs(title = "Matvei Michkov 2025-26 Power Play Shooting Heat Map",
                                                             subtitle = "Total PP TOI 113:37, 42.43% of Flyers PP Time") +
  guides(color = "none")

MatveiMichkov2024_25PPHeatMap2 <- nhl_rink_plot() + theme_void() + stat_density_2d(data = MatveiMichkov2024_25PPShots,
                                                                                 mapping = aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(level)),
                                                                                 geom = "polygon", alpha = 0.3) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,MMmax_density), name = "Shot Density") +
  geom_point(data = MatveiMichkov2024_25PPShots, aes(x=arenaAdjustedXCord,y=arenaAdjustedYCord, color = goal), alpha = 0.5, show.legend = FALSE) +
  scale_x_continuous(limits = c(-100,100)) + scale_y_continuous(limits = c(-100,100)) +
  coord_fixed(xlim = c(-95,95), ylim = c(-38.7,38.7)) + labs(title = "Matvei Michkov 2024-25 Power Play Shooting Heat Map",
                                                             subtitle = "Total PP TOI 225:22, 61.99% of Flyers PP Time") +
  guides(color = "none")


(MatveiMichkov2025_26PPHeatMap2 / MatveiMichkov2024_25PPHeatMap2) +
  plot_annotation(title = "Comparing Matvei Michkov Power Play Shooting Volume",
                  subtitle = "Density Shot Volumes Relative to the Maximum")



#Ilya Sorokin Save Heat Map
IlyaSorokinSOGAgainst <- shots_3.2.2026 %>%
  filter(goalieIdForShot=="8478009", shotWasOnGoal==1) 


IlyaSorokinSavesHeatMap <- nhl_rink_plot() + theme_void() + stat_density_2d(data = IlyaSorokinSOGAgainst %>% filter(goal==0),
                                                                                mapping = aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(nlevel)),
                                                                                geom = "polygon", alpha = 0.3) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,1), name = "Density") +
  geom_point(data = IlyaSorokinSOGAgainst %>% filter(goal==0), aes(x=arenaAdjustedXCord,y=arenaAdjustedYCord), alpha = 0.2, show.legend = FALSE) +
  scale_x_continuous(limits = c(-100,100)) + scale_y_continuous(limits = c(-100,100)) +
  coord_fixed(xlim = c(-93,93), ylim = c(-38.7,38.7)) + labs(title = "Ilya Sorokin 2025-26 All Saves Heat Map") +
  guides(color = "none")

sorokin_svp <- mean(1 - IlyaSorokinSOGAgainst$goal)

IlyaSorokinSavePercentHeatMap <- nhl_rink_plot() + theme_void() +
  stat_summary_hex(
    data = IlyaSorokinSOGAgainst,
    aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, z = 1 - goal),
    fun = function(z) ifelse(length(z) >= 5, mean(z), NA),
    binwidth = c(8, 8),
    alpha = 0.75
  ) +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = sorokin_svp,
    limits = c(0.7, 1),
    name = "SV%",
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_x_continuous(limits = c(-100, 100)) +
  scale_y_continuous(limits = c(-100, 100)) +
  coord_fixed(xlim = c(-93, 93), ylim = c(-38.7, 38.7)) +
  labs(
    title = paste0("Ilya Sorokin 2025-26 Save % Heat Map | Overall SV%: ", 
                      scales::percent(sorokin_svp, accuracy = 0.1)),
    subtitle = "Tiles relative to the season long save percentage"
  ) +
  guides(color = "none")

(IlyaSorokinSavesHeatMap/IlyaSorokinSavePercentHeatMap)


#Comparison of Vezina Favorites
library(hexbin)
SOG2025_26 <- shots_3.4.2026 %>%
  filter(shotWasOnGoal==1)

SOG2025_26HexBinManual <- SOG2025_26 %>%
  mutate(
    xHex = round(arenaAdjustedXCord / 12) * 12,
    yHex = round(arenaAdjustedYCord / 12) * 12
  )

AverageGoalieByHexBin <- SOG2025_26HexBinManual %>%
  group_by(xHex, yHex) %>%
  filter(n() >= 5) %>%
  summarise(Saves = sum(goal==0),
         Goals = sum(goal==1),
         LeagueAverageSavePercent = (Saves/n()),
         .groups = "drop")

IlyaSorokinHexBinManual <- SOG2025_26 %>%
  filter(goalieIdForShot == "8478009") %>%
  mutate(
    xHex = round(arenaAdjustedXCord / 8) * 8,
    yHex = round(arenaAdjustedYCord / 8) * 8
  ) %>%
  group_by(xHex, yHex) %>%
  filter(n() > 10) %>%
  summarise(Saves = sum(goal==0),
            Goals = sum(goal==1),
            SOGs = n(),
            HexSavePercent = (Saves/n()),
            .groups = "drop") %>%
  left_join(AverageGoalieByHexBin %>% select(xHex, yHex, LeagueAverageSavePercent),
            by = c("xHex", "yHex")) %>%
  mutate(DifferenceInHexSavePercent = HexSavePercent - LeagueAverageSavePercent)

AndreiVasilevskiyHexBinManual <- SOG2025_26 %>%
  filter(goalieIdForShot == "8476883") %>%
  mutate(
    xHex = round(arenaAdjustedXCord / 8) * 8,
    yHex = round(arenaAdjustedYCord / 8) * 8
  ) %>%
  group_by(xHex, yHex) %>%
  filter(n() > 10) %>%
  summarise(Saves = sum(goal==0),
            Goals = sum(goal==1),
            SOGs = n(),
            HexSavePercent = (Saves/n()),
            .groups = "drop") %>%
  left_join(AverageGoalieByHexBin %>% select(xHex, yHex, LeagueAverageSavePercent),
            by = c("xHex", "yHex")) %>%
  mutate(DifferenceInHexSavePercent = HexSavePercent - LeagueAverageSavePercent)


IlyaSorokinSavePercentHeatMap <- nhl_rink_plot() + theme_void() +
  geom_tile(
    data = IlyaSorokinHexBinManual,
    aes(x = xHex, y = yHex, fill = DifferenceInHexSavePercent),
    width = 8, height = 8,
    alpha = 0.75
  ) +
  geom_text(
    data = IlyaSorokinHexBinManual,
    aes(x = xHex, y = yHex, label = round(DifferenceInHexSavePercent,3)),
    size = 2.5,
    color = "black"
  ) +
  scale_fill_gradient2(
    low = "blue", mid = "gray", high = "red",
    midpoint = 0,
    limits = c(-0.2, 0.2),
    name = "Difference from\nLeague Average SV%",
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_x_continuous(limits = c(-100, 100)) +
  scale_y_continuous(limits = c(-100, 100)) +
  coord_fixed(xlim = c(-93, 93), ylim = c(-38.7, 38.7)) +
  labs(
    subtitle = "Ilya Sorokin 2025-26 Binned Save Percentage Above League Average"
  ) +
  guides(color = "none")

AndreiVasilevskiySavePercentHeatMap <- nhl_rink_plot() + theme_void() +
  geom_tile(
    data = AndreiVasilevskiyHexBinManual,
    aes(x = xHex, y = yHex, fill = DifferenceInHexSavePercent),
    width = 8, height = 8,
    alpha = 0.75
  ) +
  geom_text(
    data = AndreiVasilevskiyHexBinManual,
    aes(x = xHex, y = yHex, label = round(DifferenceInHexSavePercent, 3)),
    size = 2.5,
    color = "black"
  ) +
  scale_fill_gradient2(
    low = "blue", mid = "gray", high = "red",
    midpoint = 0,
    limits = c(-0.2, 0.2),
    name = "Difference from\nLeague Average SV%",
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_x_continuous(limits = c(-100, 100)) +
  scale_y_continuous(limits = c(-100, 100)) +
  coord_fixed(xlim = c(-93, 93), ylim = c(-38.7, 38.7)) +
  labs(
    subtitle = "Andrei Vasilevskiy 2025-26 Binned Save Percentage Above League Average"
  ) +
  guides(color = "none")


library(patchwork)
(IlyaSorokinSavePercentHeatMap/AndreiVasilevskiySavePercentHeatMap) + 
  plot_annotation(title="Comparing Vezina Favorites Ilya Sorokin and Andrei Vasilevskiy",
                  subtitle = "Difference in SV% from league average in each tile")


#Tristan Jarry
TristanJarryHexBinManual <- SOG2025_26 %>%
  filter(goalieIdForShot == "8477465") %>%
  mutate(
    xHex = round(arenaAdjustedXCord / 12) * 12,
    yHex = round(arenaAdjustedYCord / 12) * 12
  ) %>%
  group_by(xHex, yHex) %>%
  filter(n() > 5) %>%
  summarise(Saves = sum(goal==0),
            Goals = sum(goal==1),
            SOGs = n(),
            HexSavePercent = (Saves/n()),
            .groups = "drop") %>%
  left_join(AverageGoalieByHexBin %>% select(xHex, yHex, LeagueAverageSavePercent),
            by = c("xHex", "yHex")) %>%
  mutate(DifferenceInHexSavePercent = HexSavePercent - LeagueAverageSavePercent)

TristanJarrySavePercentHeatMap <- nhl_rink_plot() + theme_void() +
  geom_tile(
    data = TristanJarryHexBinManual,
    aes(x = xHex, y = yHex, fill = DifferenceInHexSavePercent),
    width = 12, height = 12,
    alpha = 0.75
  ) +
  geom_text(
    data = TristanJarryHexBinManual,
    aes(x = xHex, y = yHex, label = round(DifferenceInHexSavePercent, 3)),
    size = 2.5,
    color = "black"
  ) +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 0,
    limits = c(-0.25, 0.25),
    name = "Difference from\nLeague Average SV%",
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_x_continuous(limits = c(-100, 100)) +
  scale_y_continuous(limits = c(-100, 100)) +
  coord_fixed(xlim = c(-93, 93), ylim = c(-38.7, 38.7)) +
  labs(
    title = "Tristan Jarry 2025-26 Binned Save Percentage Above League Average"
  ) +
  guides(color = "none")

#Jordan Binnington
JordanBinningtonHexBinManual <- SOG2025_26 %>%
  filter(goalieIdForShot == "8476412") %>%
  mutate(
    xHex = round(arenaAdjustedXCord / 12) * 12,
    yHex = round(arenaAdjustedYCord / 12) * 12
  ) %>%
  group_by(xHex, yHex) %>%
  filter(n() > 5) %>%
  summarise(Saves = sum(goal==0),
            Goals = sum(goal==1),
            SOGs = n(),
            HexSavePercent = (Saves/n()),
            .groups = "drop") %>%
  left_join(AverageGoalieByHexBin %>% select(xHex, yHex, LeagueAverageSavePercent),
            by = c("xHex", "yHex")) %>%
  mutate(DifferenceInHexSavePercent = HexSavePercent - LeagueAverageSavePercent)

JordanBinningtonSavePercentHeatMap <- nhl_rink_plot() + theme_void() +
  geom_tile(
    data = JordanBinningtonHexBinManual,
    aes(x = xHex, y = yHex, fill = DifferenceInHexSavePercent),
    width = 12, height = 12,
    alpha = 0.75
  ) +
  geom_text(
    data = JordanBinningtonHexBinManual,
    aes(x = xHex, y = yHex, label = round(DifferenceInHexSavePercent, 3)),
    size = 2.5,
    color = "black"
  ) +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 0,
    limits = c(-0.27, 0.27),
    name = "Difference from\nLeague Average SV%",
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_x_continuous(limits = c(-100, 100)) +
  scale_y_continuous(limits = c(-100, 100)) +
  coord_fixed(xlim = c(-93, 93), ylim = c(-38.7, 38.7)) +
  labs(
    title = "Jordan Binnington 2025-26 Binned Save Percentage Above League Average"
  ) +
  guides(color = "none")

#Sergei Bobrovsky
SergeiBobrovskyHexBinManual <- SOG2025_26 %>%
  filter(goalieIdForShot == "8475683") %>%
  mutate(
    xHex = round(arenaAdjustedXCord / 12) * 12,
    yHex = round(arenaAdjustedYCord / 12) * 12
  ) %>%
  group_by(xHex, yHex) %>%
  filter(n() > 5) %>%
  summarise(Saves = sum(goal==0),
            Goals = sum(goal==1),
            SOGs = n(),
            HexSavePercent = (Saves/n()),
            .groups = "drop") %>%
  left_join(AverageGoalieByHexBin %>% select(xHex, yHex, LeagueAverageSavePercent),
            by = c("xHex", "yHex")) %>%
  mutate(DifferenceInHexSavePercent = HexSavePercent - LeagueAverageSavePercent)

SergeiBobrovskySavePercentHeatMap <- nhl_rink_plot() + theme_void() +
  geom_tile(
    data = SergeiBobrovskyHexBinManual,
    aes(x = xHex, y = yHex, fill = DifferenceInHexSavePercent),
    width = 12, height = 12,
    alpha = 0.75
  ) +
  geom_text(
    data = SergeiBobrovskyHexBinManual,
    aes(x = xHex, y = yHex, label = round(DifferenceInHexSavePercent, 3)),
    size = 2.5,
    color = "black"
  ) +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 0,
    limits = c(-0.27, 0.27),
    name = "Difference from\nLeague Average SV%",
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_x_continuous(limits = c(-100, 100)) +
  scale_y_continuous(limits = c(-100, 100)) +
  coord_fixed(xlim = c(-93, 93), ylim = c(-38.7, 38.7)) +
  labs(
    title = "Sergei Bobrovsky 2025-26 Binned Save Percentage Above League Average"
  ) +
  guides(color = "none")

#OT Even Strength Shots and Goals
unique(shots_3.10.2026$period)

OTESShots <- shots_3.10.2026 %>%
  filter(period == 4 & homeSkatersOnIce == 3 & awaySkatersOnIce == 3)

OTESShotHeatmap <- nhl_rink_plot() + theme_void() + stat_density_2d(data = OTESShots,
                                                                          mapping = aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(nlevel)),
                                                                          geom = "polygon", alpha = 0.3) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,1)) +
  geom_point(data = OTESShots, aes(x=arenaAdjustedXCord,y=arenaAdjustedYCord, color = goal), alpha = 0.5, show.legend = FALSE) +
  scale_x_continuous(limits = c(-100,100)) + scale_y_continuous(limits = c(-100,100)) +
  coord_fixed(xlim = c(-95,95), ylim = c(-38.7,38.7)) + labs(title = "Overtime 3 vs. 3 Shots Heat Map") +
  guides(color = "none")

OTESGoalHeatmap <- nhl_rink_plot() + theme_void() + stat_density_2d(data = OTESShots %>%
                                                                      filter(goal==1),
                                                                    mapping = aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(nlevel)),
                                                                    geom = "polygon", alpha = 0.3) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,1)) +
  geom_point(data = OTESShots %>%
               filter(goal==1), aes(x=arenaAdjustedXCord,y=arenaAdjustedYCord, color = goal), alpha = 0.5, show.legend = FALSE) +
  scale_x_continuous(limits = c(-100,100)) + scale_y_continuous(limits = c(-100,100)) +
  coord_fixed(xlim = c(-95,95), ylim = c(-38.7,38.7)) + labs(title = "Overtime 3 vs. 3 Shots Heat Map") +
  guides(color = "none")


#Andrew Hammond Hamburglar Run
SOG_2014_15NoHammond <- shots_2014_15 %>%
  filter(goalieIdForShot != "8477202", shotWasOnGoal == 1, isPlayoffGame==0)

SOG2014_25HexBinManual <- SOG_2014_15NoHammond %>%
  mutate(
    xHex = round(arenaAdjustedXCord / 12) * 12,
    yHex = round(arenaAdjustedYCord / 12) * 12
  )

AverageGoalieByHexBin <- SOG2014_25HexBinManual %>%
  group_by(xHex, yHex) %>%
  filter(n() >= 5) %>%
  summarise(Saves = sum(goal==0),
            Goals = sum(goal==1),
            LeagueAverageSavePercent = (Saves/n()),
            .groups = "drop")

AndrewHammondHexBinManual <- shots_2014_15 %>%
  filter(shotWasOnGoal == 1) %>%
  filter(goalieIdForShot == "8477202", game_id != "20839", isPlayoffGame==0) %>%
  mutate(
    xHex = round(arenaAdjustedXCord / 12) * 12,
    yHex = round(arenaAdjustedYCord / 12) * 12
  ) %>%
  group_by(xHex, yHex) %>%
  filter(n() > 5) %>%
  summarise(Saves = sum(goal==0),
            Goals = sum(goal==1),
            SOGs = n(),
            HexSavePercent = (Saves/n()),
            .groups = "drop") %>%
  left_join(AverageGoalieByHexBin %>% select(xHex, yHex, LeagueAverageSavePercent),
            by = c("xHex", "yHex")) %>%
  mutate(DifferenceInHexSavePercent = HexSavePercent - LeagueAverageSavePercent)


AndrewHammondSavePercentHeatMap <- nhl_rink_plot() + theme_void() +
  geom_tile(
    data = AndrewHammondHexBinManual,
    aes(x = xHex, y = yHex, fill = DifferenceInHexSavePercent),
    width = 12, height = 12,
    alpha = 0.75
  ) +
  geom_text(
    data = AndrewHammondHexBinManual,
    aes(x = xHex, y = yHex, label = round(DifferenceInHexSavePercent, 3)),
    size = 2.5,
    color = "black"
  ) +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 0,
    limits = c(-0.25, 0.25),
    name = "Difference from\nLeague Average SV%",
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_x_continuous(limits = c(-100, 100)) +
  scale_y_continuous(limits = c(-100, 100)) +
  coord_fixed(xlim = c(-93, 93), ylim = c(-38.7, 38.7)) +
  labs(
    title = "Andrew Hammond 2015 Hamburglar Run Binned Save Percentage Above League Average"
  ) +
  guides(color = "none")


AndrewHammondHamburglarRunStats <- shots_2014_15 %>%
  filter(goalieIdForShot == "8477202", game_id != "20839", isPlayoffGame==0)
sum(AndrewHammondHamburglarRunStats$xGoal)-sum(AndrewHammondHamburglarRunStats$goal)


#Before and After Rick Bowness hiring
CBJShotAttempts2025_26 <- shots_3.18.26 %>%
  filter(teamCode == "CBJ", shotOnEmptyNet == 0)


CBJGameIDs <- unique(CBJShotAttempts2025_26$game_id)

EvasonGameIDs <- CBJGameIDs[1:45]
BownessGameIDs <- CBJGameIDs[46:length(CBJGameIDs)]

EvasonGameShots <- CBJShotAttempts2025_26 %>%
  filter(game_id %in% EvasonGameIDs)
BownessGameShots <- CBJShotAttempts2025_26 %>%
  filter(game_id %in% BownessGameIDs)

EvasonShotHeatMap <- nhl_rink_plot() + theme_void() + stat_density_2d(data = EvasonGameShots,
                                                                      mapping = aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(nlevel)),
                                                                      geom = "polygon", alpha = 0.3) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,1), name = "Relative Density") +
  scale_x_continuous(limits = c(-100,100)) + scale_y_continuous(limits = c(-100,100)) +
  coord_fixed(xlim = c(-95,95), ylim = c(-38.7,38.7)) + labs(subtitle = "Columbus Blue Jackets \nShot Attempt Heat Map under Dean Evason") +
  guides(color = "none")

BownessShotHeatMap <- nhl_rink_plot() + theme_void() + stat_density_2d(data = BownessGameShots,
                                                                      mapping = aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(nlevel)),
                                                                      geom = "polygon", alpha = 0.3) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,1), name = "Relative Density") +
  scale_x_continuous(limits = c(-100,100)) + scale_y_continuous(limits = c(-100,100)) +
  coord_fixed(xlim = c(-95,95), ylim = c(-38.7,38.7)) + labs(subtitle = "Columbus Blue Jackets \nShot Attempt Heat Map under Rick Bowness") +
  guides(color = "none")

(EvasonShotHeatMap/BownessShotHeatMap) +
  plot_annotation(title = "Comparing Colubmus Blue Jackets Shooting Patterns\nwith Dean Evason vs. Rick Bowness")

#On Power Play
EvasonGamePPShots <- EvasonGameShots %>%
mutate(BlueJacketsSkaters = ifelse(team=="AWAY", awaySkatersOnIce, homeSkatersOnIce),
       OppSkaters = ifelse(team=="AWAY", homeSkatersOnIce, awaySkatersOnIce),
       OnPP = ifelse(BlueJacketsSkaters > OppSkaters & OppSkaters < 5, 1, 0)) %>%
  filter(OnPP == 1)

BownessGamePPShots <- BownessGameShots %>%
  mutate(BlueJacketsSkaters = ifelse(team=="AWAY", awaySkatersOnIce, homeSkatersOnIce),
         OppSkaters = ifelse(team=="AWAY", homeSkatersOnIce, awaySkatersOnIce),
         OnPP = ifelse(BlueJacketsSkaters > OppSkaters & OppSkaters < 5, 1, 0)) %>%
  filter(OnPP == 1)

EvasonPPShotHeatMap <- nhl_rink_plot() + theme_void() + stat_density_2d(data = EvasonGamePPShots,
                                                                      mapping = aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(nlevel)),
                                                                      geom = "polygon", alpha = 0.3) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,1), name = "Relative Density") +
  geom_point(data = EvasonGamePPShots, aes(x=arenaAdjustedXCord,y=arenaAdjustedYCord, color = goal), alpha = 0.5) +
  scale_x_continuous(limits = c(-100,100)) + scale_y_continuous(limits = c(-100,100)) +
  coord_fixed(xlim = c(-95,95), ylim = c(-38.7,38.7)) + labs(subtitle = "Columbus Blue Jackets Power Play\n Shot Attempt Heat Map under Dean Evason") +
  guides(color = "none")

BownessPPShotHeatMap <- nhl_rink_plot() + theme_void() + stat_density_2d(data = BownessGamePPShots,
                                                                       mapping = aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(nlevel)),
                                                                       geom = "polygon", alpha = 0.3) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,1), name = "Relative Density") +
  geom_point(data = BownessGamePPShots, aes(x=arenaAdjustedXCord,y=arenaAdjustedYCord, color = goal), alpha = 0.5) +
  scale_x_continuous(limits = c(-100,100)) + scale_y_continuous(limits = c(-100,100)) +
  coord_fixed(xlim = c(-95,95), ylim = c(-38.7,38.7)) + labs(subtitle = "Columbus Blue Jackets Power Play\n Shot Attempt Heat Map under Rick Bowness") +
  guides(color = "none")

(EvasonPPShotHeatMap/BownessPPShotHeatMap) +
  plot_annotation(title = "Comparing Colubmus Blue Jackets Power Play Shooting Patterns\nwith Dean Evason vs. Rick Bowness")

#Shots Against under Bowness
CBJShotsAgainst <- shots_3.18.26 %>%
  mutate(defendingTeam = ifelse(teamCode==homeTeamCode, awayTeamCode, homeTeamCode)) %>%
  filter(defendingTeam == "CBJ", period != 4)

EvasonGameShotsAgainst <- CBJShotsAgainst %>%
  filter(game_id %in% EvasonGameIDs)
BownessGameShotsAgainst <- CBJShotsAgainst %>%
  filter(game_id %in% BownessGameIDs)

EvasonShotAgainstHeatMap <- nhl_rink_plot() + theme_void() + stat_density_2d(data = EvasonGameShotsAgainst,
                                                                      mapping = aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(nlevel)),
                                                                      geom = "polygon", alpha = 0.3) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,1), name = "Relative Density") +
  scale_x_continuous(limits = c(-100,100)) + scale_y_continuous(limits = c(-100,100)) +
  coord_fixed(xlim = c(-95,95), ylim = c(-38.7,38.7)) + labs(subtitle = "Columbus Blue Jackets Shot Attempts Against\n Heat Map under Dean Evason") +
  guides(color = "none")

BownessShotsAgainstHeatMap <- nhl_rink_plot() + theme_void() + stat_density_2d(data = BownessGameShotsAgainst,
                                                                       mapping = aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(nlevel)),
                                                                       geom = "polygon", alpha = 0.3) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,1), name = "Relative Density") +
  scale_x_continuous(limits = c(-100,100)) + scale_y_continuous(limits = c(-100,100)) +
  coord_fixed(xlim = c(-95,95), ylim = c(-38.7,38.7)) + labs(subtitle = "Columbus Blue Jackets Shot Attempts Against\n Heat Map under Rick Bowness") +
  guides(color = "none")

(EvasonShotAgainstHeatMap/BownessShotsAgainstHeatMap) +
  plot_annotation(title = "Comparing Colubmus Blue Jackets Shots Against Patterns\nwith Dean Evason vs. Rick Bowness")


#PK Shots Against
EvasonPKShotsAgainst <- EvasonGameShotsAgainst %>%
  mutate(BlueJacketsSkaters = ifelse(team=="AWAY", homeSkatersOnIce, awaySkatersOnIce),
         OppSkaters = ifelse(team=="AWAY", awaySkatersOnIce, homeSkatersOnIce),
         OnPK = ifelse(OppSkaters > BlueJacketsSkaters & BlueJacketsSkaters < 5, 1, 0)) %>%
  filter(OnPK == 1, period != 4)

BownessPKShotsAgainst <- BownessGameShotsAgainst %>%
  mutate(BlueJacketsSkaters = ifelse(team=="AWAY", homeSkatersOnIce, awaySkatersOnIce),
         OppSkaters = ifelse(team=="AWAY", awaySkatersOnIce, homeSkatersOnIce),
         OnPK = ifelse(OppSkaters > BlueJacketsSkaters & BlueJacketsSkaters < 5, 1, 0)) %>%
  filter(OnPK == 1, period != 4)

EvasonPKShotAgainstHeatMap <- nhl_rink_plot() + theme_void() + stat_density_2d(data = EvasonPKShotsAgainst,
                                                                             mapping = aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(nlevel)),
                                                                             geom = "polygon", alpha = 0.3) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,1), name = "Relative Density") +
  geom_point(data = EvasonPKShotsAgainst, aes(x=arenaAdjustedXCord,y=arenaAdjustedYCord, color = goal), alpha = 0.5, show.legend = FALSE) +
  scale_x_continuous(limits = c(-100,100)) + scale_y_continuous(limits = c(-100,100)) +
  coord_fixed(xlim = c(-95,95), ylim = c(-38.7,38.7)) + labs(subtitle = "Columbus Blue Jackets Penalty Kill\nShot Attempts Against Heat Map under Dean Evason") +
  guides(color = "none")

BownessPKShotsAgainstHeatMap <- nhl_rink_plot() + theme_void() + stat_density_2d(data = BownessPKShotsAgainst,
                                                                               mapping = aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(nlevel)),
                                                                               geom = "polygon", alpha = 0.3) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,1), name = "Relative Density") +
  geom_point(data = BownessPKShotsAgainst, aes(x=arenaAdjustedXCord,y=arenaAdjustedYCord, color = goal), alpha = 0.5, show.legend = FALSE) +
  scale_x_continuous(limits = c(-100,100)) + scale_y_continuous(limits = c(-100,100)) +
  coord_fixed(xlim = c(-95,95), ylim = c(-38.7,38.7)) + labs(subtitle = "Columbus Blue Jackets Penalty Kill\nShot Attempts Against Heat Map under Rick Bowness") +
  guides(color = "none")

(EvasonPKShotAgainstHeatMap/BownessPKShotsAgainstHeatMap) +
  plot_annotation(title = "Comparing Colubmus Blue Jackets Penalty Kill Shots Against Patterns\nwith Dean Evason vs. Rick Bowness")


