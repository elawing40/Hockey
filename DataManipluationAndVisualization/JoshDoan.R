#Josh Doan Past Two Years Comparison

GameScoreOnly <- function(df1, df2, season_dates = NHLSeasonsStartEnd){
  df3 <- merge(df1, df2, by = "Game")
  df4 <- df3 %>%
    mutate(
      GameNumber = row_number(),
      GameScore = ((0.75*Goals)+(0.7*First.Assists)+(0.55*Second.Assists)+
                     (0.075*Shots)+(0.05*Shots.Blocked)+((0.15*Penalties.Drawn)-(0.15*Total.Penalties))+
                     ((0.01*Faceoffs.Won)-(0.01*Faceoffs.Lost))+(0.625*xGF)+(0.625*GF)-(1.75*xGA)-(0.4375*GA))
    )
  
  return(df4)
}

JoshDoan2024_25GS <- GameScoreOnly(JoshDoanIndiv2024_25, JoshDoanOnIce2024_25)
JoshDoan2025_26GS <- GameScoreOnly(JoshDoan2025_26Indiv, JoshDoanOnIce2025_26)

RollingAverageGGPlotMakerGameNumber <- function(df, player, pick_color){
  df1 <- df %>%
    mutate(
      GS5GameRollMean = rollmean(GameScore, k=5, fill=NA, align = 'right')
    ) 
  
  ggplot(data=df1, aes(x=GameNumber)) + 
    
    geom_point(aes(y = GameScore), color = "gray50", alpha = 0.3) +
    
    geom_line(data = df1 %>% filter(!is.na(GS5GameRollMean)), 
              aes(y = GS5GameRollMean), color = pick_color, linewidth = 1) +
    
    geom_smooth(
      aes(y = GS5GameRollMean),
      method = "loess",
      span = 0.3,  # or 0.5 - increased from 0.15
      color = "black", 
      linewidth = 1.5,
      se = FALSE
    ) +
    
    labs(
      title = paste0(player, ": 5-Game Rolling Game Score"),
      caption = "Game Score by @domluszczyszyn, no QoC or QoT adjustments",
      x = "Game Number",
      y = "Game Score & Rolling Mean"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

#Game Score
RollingAverageGGPlotMakerGameNumber(JoshDoan2024_25GS, "Josh Doan, 2024-25", "#6CACE3")
RollingAverageGGPlotMakerGameNumber(JoshDoan2025_26GS, "Josh Doan, 2025-26", "#003087")


JoshDoanPast2Szn <- rbind(JoshDoan2024_25Woodmoney, JoshDoan2025_26Woodmoney)
JoshDoanPast2SznFiltered <- JoshDoanPast2Szn %>%
  filter(WMTier != "All")

#Play against Competition
ggplot(data = JoshDoanPast2SznFiltered, aes(x=WMTier, y=DFF., fill = as.factor(Season))) + 
  geom_bar(stat="identity", aes(fill = as.factor(Season)),
           position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("#6CACE3", "#003087")) +
  labs(title = "Josh Doan Dangerous Fenwick For %",
       subtitle = "Past Two Seasons",
       x = "Competition Tier",
       y = "Dangerous Fenwick For %",
       caption = "Data via PuckIQ",
       fill = "Season") + 
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60)) 

ggplot(data = JoshDoanPast2SznFiltered, aes(x=WMTier, y=CTOI., fill = as.factor(Season))) + 
  geom_bar(stat="identity", aes(fill = as.factor(Season)),
           position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("#6CACE3", "#003087")) +
  labs(title = "Josh Doan Percent of TOI vs. Competition Level",
       subtitle = "Past Two Seasons",
       x = "Competition Tier",
       y = "Percent of TOI vs. Competition Level",
       caption = "Data via PuckIQ",
       fill = "Player") + 
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40)) 

ggplot(data = JoshDoanPast2SznFiltered, aes(x=WMTier, y=FO.60, fill = as.factor(Season))) + 
  geom_bar(stat="identity", aes(fill = as.factor(Season)),
           position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("#6CACE3", "#003087")) +
  labs(title = "Josh Doan Percent Faceoffs Per 60 vs. Competition Level",
       subtitle = "Past Two Seasons",
       x = "Competition Tier",
       y = "Faceoffs Per 60 vs. Competition Level",
       caption = "Data via PuckIQ",
       fill = "Player") + 
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60)) 




