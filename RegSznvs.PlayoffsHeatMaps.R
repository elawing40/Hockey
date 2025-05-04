#Regular Season vs. Playoffs Heat Maps
library(ggplot2)
library(ggplotify)
library(dplyr)
library(sportyR)
library(ks)
library(patchwork)
library(transport)
library(gt)

team_codes <- c(
  "ANA", "BOS", "BUF", "CAR", "CBJ", "CGY", "CHI", "COL", "DAL", "DET", "EDM", "FLA",
  "LAK", "MIN", "MTL", "NJD", "NSH", "NYI", "NYR", "OTT", "PHI", "PIT", "SJS", "SEA",
  "STL", "TBL", "TOR", "UTA", "VAN", "VGK", "WPG", "WSH"
)

team_names <- c(
  "Anaheim Ducks", "Boston Bruins", "Buffalo Sabres", "Carolina Hurricanes", 
  "Columbus Blue Jackets", "Calgary Flames", "Chicago Blackhawks", "Colorado Avalanche", 
  "Dallas Stars", "Detroit Red Wings", "Edmonton Oilers", "Florida Panthers", 
  "Los Angeles Kings", "Minnesota Wild", "Montreal Canadiens", "New Jersey Devils", 
  "Nashville Predators", "New York Islanders", "New York Rangers", "Ottawa Senators", 
  "Philadelphia Flyers", "Pittsburgh Penguins", "San Jose Sharks", "Seattle Kraken", 
  "St Louis Blues", "Tampa Bay Lightning", "Toronto Maple Leafs", "Utah Hockey Club", 
  "Vancouver Canucks", "Vegas Golden Knights", "Winnipeg Jets", "Washington Capitals"
)

team_map <- data.frame(
  teamCode = team_codes,
  teamName = team_names,
  stringsAsFactors = FALSE
)

shots_merged <- shots_2024.5.4 %>%
  left_join(team_map, by = "teamCode")

shots_merged$OppTeamCode <- ifelse(
  shots_merged$teamCode == shots_merged$homeTeamCode,
  shots_merged$awayTeamCode,
  shots_merged$homeTeamCode
)

RegSznTeamStats2 <- RegSznTeamStats %>%
  left_join(team_map, by = c("Team" = "teamName"))

PlayoffsTeamStats5.4.2 <- PlayoffsTeamStats5.4 %>%
  left_join(team_map, by = c("Team" = "teamName"))


# Regular Season
for (team1 in Teams) {
  RS_TOI <- RegSznTeamStats2[RegSznTeamStats2$teamCode == team1, 4][[1]]
  teamShotsRS <- shots_merged %>%
    filter(teamCode == team1 & isPlayoffGame == 0) %>%
    slice(rep(1:n(), each = round((1 / RS_TOI) * 60 * 100)))
  HeatMap1 <- geom_hockey(league = "NHL") +
    stat_density_2d(
      data = teamShotsRS,
      aes(
        x = arenaAdjustedXCord,
        y = arenaAdjustedYCord,
        fill = after_stat(level)
      ),
      geom = "polygon",
      alpha = 0.3
    ) +
    scale_fill_gradient(low = "blue", high = "red") +
    labs(title = paste(team1, "Regular Season Shot Density (Per 60)")) +
    theme(
      plot.title = element_text(vjust = -0.5, hjust = 0.5),
      legend.position = "none"
    )
  assign(paste0(team1, "RegSznHeatMap"), HeatMap1)
}



#Playoffs
PlayoffTeams <- c(PlayoffsTeamStats5.4.2$teamCode)
for (team1 in PlayoffTeams) {
  PO_TOI <- PlayoffsTeamStats5.4.2[PlayoffsTeamStats5.4.2$teamCode == team1, 4][[1]]
  teamShotsPO <- shots_merged %>%
    filter(teamCode == team1 & isPlayoffGame == 1) %>%
    slice(rep(1:n(), each = round((1 / PO_TOI) * 60 * 100)))
  HeatMap2 <- geom_hockey(league = "NHL") +
    stat_density_2d(
      data = teamShotsPO,
      aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(level)),
      geom = "polygon",
      alpha = 0.3
    ) +
    scale_fill_gradient(low = "blue", high = "red") +
    labs(title = paste(team1, "Playoffs Shot Density (Per 60)")) +
    theme(
      plot.title = element_text(vjust = -0.5, hjust = 0.5),
      legend.position = "none"
    )
  assign(paste0(team1,"PlayoffsHeatMap"), HeatMap2)
}



#Goals Against
for (team1 in Teams) {
  RS_TOI <- RegSznTeamStats2[RegSznTeamStats2$teamCode == team1, 4][[1]]
  teamGoals <- shots_merged %>%
    filter(OppTeamCode == team1 & isPlayoffGame == 0 & goal == 1) %>%
    slice(rep(1:n(), each = round((1 / RS_TOI) * 60 * 100)))
  HeatMap3 <- geom_hockey(league = "NHL") +
    stat_density_2d(
      data = teamGoals,
      aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(level)),
      geom = "polygon",
      alpha = 0.3
    ) +
    scale_fill_gradient(low = "blue", high = "red") +
    labs(title = paste(team1, "Regular Season Goals Against Density")) +
    theme(
      plot.title = element_text(vjust = -0.5, hjust = 0.5),
      legend.position = "none"
    )
  assign(paste0(team1,"GoalsAgainstHeatMap"),HeatMap3)
}


#Wasserstein Distances
WassResults <- data.frame()
for (team1 in PlayoffTeams) {
  RS_TOI <- RegSznTeamStats2[RegSznTeamStats2$teamCode == team1, 4][[1]]
  PO_TOI <- PlayoffsTeamStats5.4.2[PlayoffsTeamStats5.4.2$teamCode == team1, 4][[1]]
  teamShotsRS <- shots_merged %>%
    filter(teamCode == team1 & isPlayoffGame == 0) %>%
    select(x = arenaAdjustedXCord, y = arenaAdjustedYCord)
  teamShotsPO <- shots_merged %>%
    filter(teamCode == team1 & isPlayoffGame == 1) %>%
    select(x = arenaAdjustedXCord, y = arenaAdjustedYCord)
  if (nrow(teamShotsRS) == 0 | nrow(teamShotsPO) == 0) next
  teamShotsRS$weight <- rep(60 / RS_TOI, nrow(teamShotsRS))
  teamShotsPO$weight <- rep(60 / PO_TOI, nrow(teamShotsPO))
  rs_coords <- as.matrix(teamShotsRS[, c("x", "y")])
  po_coords <- as.matrix(teamShotsPO[, c("x", "y")])
  rs_weights <- teamShotsRS$weight / sum(teamShotsRS$weight)
  po_weights <- teamShotsPO$weight / sum(teamShotsPO$weight)
  cost_matrix <- as.matrix(dist(rbind(rs_coords, po_coords)))
  cost_rs_po <- cost_matrix[1:nrow(rs_coords), (nrow(rs_coords) + 1):(nrow(rs_coords) + nrow(po_coords))]
  wass_dist <- wasserstein(a = rs_weights,
                           b = po_weights,
                           p = 2,
                           costm = cost_rs_po) # Add the cost matrix
  WassResults <- rbind(WassResults, data.frame(team = team1, wasserstein_distance = wass_dist))
}
WassResults

WassResultsWithLogo <- WassResults %>%
  mutate(
    img = case_when(
      team == "DAL" ~
        "https://upload.wikimedia.org/wikipedia/en/c/ce/Dallas_Stars_logo_%282013%29.svg",
      team == "CGY" ~
        "https://upload.wikimedia.org/wikipedia/en/6/61/Calgary_Flames_logo.svg",
      team == "BOS" ~
        "https://upload.wikimedia.org/wikipedia/commons/1/12/Boston_Bruins.svg",
      team == "NYI" ~
        "https://upload.wikimedia.org/wikipedia/en/4/42/Logo_New_York_Islanders.svg",
      team == "OTT" ~
        "https://upload.wikimedia.org/wikipedia/en/b/b2/Ottawa_Senators_2020-2021_logo.svg",
      team == "CBJ" ~
        "https://upload.wikimedia.org/wikipedia/en/5/5d/Columbus_Blue_Jackets_logo.svg",
      team == "SJS" ~
        "https://upload.wikimedia.org/wikipedia/en/3/37/SanJoseSharksLogo.svg",
      team == "VGK" ~
        "https://upload.wikimedia.org/wikipedia/en/a/ac/Vegas_Golden_Knights_logo.svg",
      team == "UTA" ~
        "https://upload.wikimedia.org/wikipedia/commons/9/95/Utah_Hockey_Club_2024-25_Logo.svg",
      team == "SEA" ~
        "https://upload.wikimedia.org/wikipedia/en/4/48/Seattle_Kraken_official_logo.svg",
      team == "ANA" ~
        "https://upload.wikimedia.org/wikipedia/en/9/95/Anaheim_Ducks_logo_2024.svg",
      team == "STL" ~
        "https://upload.wikimedia.org/wikipedia/en/e/ed/St._Louis_Blues_logo.svg",
      team == "TOR" ~
        "https://upload.wikimedia.org/wikipedia/en/b/b6/Toronto_Maple_Leafs_2016_logo.svg",
      team == "BUF" ~
        "https://upload.wikimedia.org/wikipedia/en/9/9e/Buffalo_Sabres_Logo.svg",
      team == "NJD" ~
        "https://upload.wikimedia.org/wikipedia/en/9/9f/New_Jersey_Devils_logo.svg",
      team == "TBL" ~
        "https://upload.wikimedia.org/wikipedia/commons/3/31/Tampa_Bay_Lightning_2011.svg",
      team == "COL" ~
        "https://upload.wikimedia.org/wikipedia/en/4/45/Colorado_Avalanche_logo.svg",
      team == "CHI" ~
        "https://upload.wikimedia.org/wikipedia/en/2/29/Chicago_Blackhawks_logo.svg",
      team == "NYR" ~
        "https://upload.wikimedia.org/wikipedia/commons/a/ae/New_York_Rangers.svg",
      team == "MTL" ~
        "https://upload.wikimedia.org/wikipedia/commons/6/69/Montreal_Canadiens.svg",
      team == "WSH" ~
        "https://upload.wikimedia.org/wikipedia/commons/2/2d/Washington_Capitals.svg",
      team == "PHI" ~
        "https://upload.wikimedia.org/wikipedia/en/d/dc/Philadelphia_Flyers.svg",
      team == "NSH" ~
        "https://upload.wikimedia.org/wikipedia/en/9/9c/Nashville_Predators_Logo_%282011%29.svg",
      team == "VAN" ~
        "https://upload.wikimedia.org/wikipedia/en/3/3a/Vancouver_Canucks_logo.svg",
      team == "CAR" ~
        "https://upload.wikimedia.org/wikipedia/en/3/32/Carolina_Hurricanes.svg",
      team == "EDM" ~
        "https://upload.wikimedia.org/wikipedia/en/4/4d/Logo_Edmonton_Oilers.svg",
      team == "PIT" ~
        "https://upload.wikimedia.org/wikipedia/en/c/c0/Pittsburgh_Penguins_logo_%282016%29.svg",
      team == "MIN" ~
        "https://upload.wikimedia.org/wikipedia/en/1/1b/Minnesota_Wild.svg",
      team == "WPG" ~
        "https://upload.wikimedia.org/wikipedia/en/9/93/Winnipeg_Jets_Logo_2011.svg",
      team == "DET" ~
        "https://upload.wikimedia.org/wikipedia/en/e/e0/Detroit_Red_Wings_logo.svg",
      team == "LAK" ~
        "https://upload.wikimedia.org/wikipedia/en/c/c4/Los_Angeles_Kings_2024_Logo.svg",
      team == "FLA" ~
        "https://upload.wikimedia.org/wikipedia/en/4/43/Florida_Panthers_2016_logo.svg",
      TRUE ~ "NA")) %>%
  arrange(desc(wasserstein_distance))

WassResultsWithLogo %>%
  select(img, team, wasserstein_distance) %>% # Reorder columns
  gt() |>
  text_transform(
    locations = cells_body(columns = img),
    fn = function(x) web_image(url = x, height = 30) # Display team logos
  ) |>
  cols_label(
    img = "Team",
    team = "Team Code",
    wasserstein_distance = "Wasserstein Distance"
  ) |>
  tab_header(
    title = md("**Wasserstein Distance Between Regular Season\n and First Round of Playoffs**")
  ) |>
  cols_align(
    align = "center",
    columns = c(img, team, wasserstein_distance)
  )


WassResultsWithLogo %>%
  slice(1:8) %>%
  select(img, team, wasserstein_distance) %>%
  gt() |>
  text_transform(
    locations = cells_body(columns = img),
    fn = function(x) web_image(url = x, height = 30)
  ) |>
  cols_label(
    img = "Team",
    team = "Team Code",
    wasserstein_distance = "Wasserstein Distance"
  ) |>
  tab_header(
    title = md("**Wasserstein Distance: Regular Season vs Playoffs (Teams 1–8)**")
  ) |>
  cols_align(
    align = "center",
    columns = c(img, team, wasserstein_distance)
  )

WassResultsWithLogo %>%
  slice(9:16) %>%
  select(img, team, wasserstein_distance) %>%
  gt() |>
  text_transform(
    locations = cells_body(columns = img),
    fn = function(x) web_image(url = x, height = 30)
  ) |>
  cols_label(
    img = "Team",
    team = "Team Code",
    wasserstein_distance = "Wasserstein Distance"
  ) |>
  tab_header(
    title = md("**Wasserstein Distance: Regular Season vs Playoffs (Teams 9–16)**")
  ) |>
  cols_align(
    align = "center",
    columns = c(img, team, wasserstein_distance)
  )
