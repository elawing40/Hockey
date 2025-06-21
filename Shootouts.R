#Shootout Prediction and Optimization Project

library(dplyr)
library(gt)

ShootoutPlayerData2021_25 <- bind_rows(ShootoutPlayerData2021_22,
                                       ShootoutPlayerData2022_23,
                                       ShootoutPlayerData2023_24,
                                       ShootoutPlayerData2024_25)

ShootoutPlayerData2021_25Summed <- ShootoutPlayerData2021_25 %>%
  group_by(playerId, Player) %>%
  summarize(
    total_attempts = sum(Att., na.rm = TRUE),
    total_goals = sum(Made, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  mutate(
    shootout_percentage = ifelse(total_attempts > 0, total_goals / total_attempts, 0)
  ) %>%
  arrange(desc(shootout_percentage))


ShootoutPlayerData2021_25Summed %>%
  filter(total_attempts >= 10) %>%
  select(Player, total_attempts, total_goals, shootout_percentage) %>%
  gt() |>
  cols_label(
    total_attempts = "Shootout Attempts",
    total_goals = "Shootout Goals",
    shootout_percentage = "Shootout Percentage"
  ) |>
  # Apply formatting here
  fmt_number(
    columns = c(shootout_percentage),
    decimals = 3
  ) |>
  tab_header(
    title = md("**Shootout Percentage Leaders 2021-2025**"),
    subtitle = md("**Minimum 10 Attempts**")
  ) |>
  cols_align(
    align = "center",
    columns = c(Player, total_attempts, total_goals, shootout_percentage)
  )


ShootoutGoalieData2021_25 <- bind_rows(ShootoutGoalieData2021_22,
                                       ShootoutGoalieData2022_23,
                                       ShootoutGoalieData2023_24,
                                       ShootoutGoalieData2024_25)

ShootoutGoalieData2021_25Summed <- ShootoutGoalieData2021_25 %>%
  group_by(playerId, Player) %>%
  summarize(
    total_shots = sum(Att., na.rm = TRUE),
    total_saves = sum(Miss, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  mutate(
    save_percentage = ifelse(total_shots > 0, total_saves / total_shots, 0)
  ) %>%
  arrange(desc(save_percentage))


ShootoutGoalieData2021_25Summed %>%
  filter(total_shots >= 20) %>%
  select(Player, total_saves, total_shots, save_percentage) %>%
  gt() |>
  cols_label(
    total_shots = "Total Attempts Against",
    total_saves = "Total Shootout Saves",
    save_percentage = "Shootout Save Percentage"
  ) |>
  # Apply formatting here
  fmt_number(
    columns = c(save_percentage),
    decimals = 3
  ) |>
  tab_header(
    title = md("**Shootout Save Percentage Leaders 2021-2025**"),
    subtitle = md("**Minimum 20 Attempts**")
  ) |>
  cols_align(
    align = "center",
    columns = c(Player, total_shots, total_saves, save_percentage)
  )


#manipulate other statisitcs dataframes

GoalieData2021_25 <- bind_rows(goalies_2021_22,
                                goalies_2022_23,
                                goalies_2023_24,
                                goalies_2024_25)

GoalieData2021_25Summed <- GoalieData2021_25 %>%
  filter(situation == "all") %>%
  group_by(playerId, name) %>%
  summarize(
    total_GamesPlayed = sum(games_played, na.rm = TRUE),
    total_shots = sum(unblocked_shot_attempts, na.rm = TRUE),
    total_goals = sum(goals, na.rm = TRUE),
    total_TOI = sum(icetime, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    save_percentage = ifelse(total_shots > 0, total_goals / total_shots, 0)
  )

ShooterData2021_25 <- bind_rows(skaters_2021_22,
                               skaters_2022_23,
                               skaters_2023_24,
                               skaters_2024_25)

ShooterData2021_25Summed <- ShooterData2021_25 %>%
  filter(situation == "all") %>%
  group_by(playerId) %>%
  summarize(
    total_GamesPlayed = sum(games_played, na.rm = TRUE),
    total_ShotAttempts = sum(I_F_shotAttempts, na.rm = TRUE),
    total_goals = sum(I_F_goals, na.rm = TRUE),
    total_TOI = sum(icetime, na.rm = TRUE),
    total_shifts = sum(shifts, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    shot_percentage = ifelse(total_ShotAttempts > 0, total_goals / total_ShotAttempts, 0)
  )

#merge basic statistics with the shootout statistics and the player information dataframes

ShootersMergeDataFrame1 <- merge(x = ShootoutPlayerData2021_25Summed, y = ShooterData2021_25Summed, by = "playerId", all.x = TRUE)
ShootersFullDataframe <- merge(x = ShootersMergeDataFrame1, y = allPlayersLookup, by = "playerId", all.x = TRUE)

GoaliesMergeDataFrame1 <- merge(x = ShootoutGoalieData2021_25Summed, y = GoalieData2021_25Summed, by = "playerId", all.x = TRUE)
GoaliesFullDataframe <- merge(x = GoaliesMergeDataFrame1, y = allPlayersLookup, by = "playerId", all.x = TRUE)

ShootersFullDataframe$Age <- as.numeric(difftime(as.Date("2025-10-01"), as.Date(ShootersFullDataframe$birthDate),unit="weeks"))/52.25
GoaliesFullDataframe$Age <- as.numeric(difftime(as.Date("2025-10-01"), as.Date(GoaliesFullDataframe$birthDate),unit="weeks"))/52.25

ShootersFullDataframe$shootsCatches <- as.factor(ShootersFullDataframe$shootsCatches)

#Create Priors for Forwards and Defensemen
ForwardPriorMean <- sum(ShootersFullDataframe[ShootersFullDataframe$primaryPosition %in% c("C", "L", "R"), ]$total_goals.x)/sum(ShootersFullDataframe[ShootersFullDataframe$primaryPosition %in% c("C", "L", "R"), ]$total_attempts)
ForwardPriorLogit <- log(ForwardPriorMean / (1 - ForwardPriorMean))
DefensemenPriorMean <- sum(ShootersFullDataframe[ShootersFullDataframe$primaryPosition == "D", ]$total_goals.x)/sum(ShootersFullDataframe[ShootersFullDataframe$primaryPosition == "D", ]$total_attempts)
DefensemenPriorLogit <- log(DefensemenPriorMean / (1 - DefensemenPriorMean))

#create Logistic Bayesian regression model to predict actual shootout make percentage
library(brms)
ForwardsShootoutData$shootout_pct <- ForwardsShootoutData$total_goals.x / ForwardsShootoutData$total_attempts
mean_p <- mean(ForwardsShootoutData$shootout_pct)
binom_var <- mean_p * (1 - mean_p) / mean(ForwardsShootoutData$total_attempts)
empirical_var <- var(ForwardsShootoutData$shootout_pct)
overdispersion_ratio <- empirical_var / binom_var
ForwardsPriorFull <- c(set_prior(paste0("normal(", ForwardPriorLogit, ", 1)"), class = "Intercept"))
ForwardsShootoutData <- ShootersFullDataframe %>%
  select(-name) %>%
  filter(primaryPosition %in% c("C", "L", "R"))
ForwardsShootoutData_scaled <- ForwardsShootoutData %>%
  mutate(
    total_ShotAttempts = as.numeric(scale(total_ShotAttempts)),
    total_goals.y = as.numeric(scale(total_goals.y)),
    total_shifts = as.numeric(scale(total_shifts)),
    shot_percentage = as.numeric(scale(shot_percentage)),
    Age = as.numeric(scale(Age))
  )
ForwardsPriorFull <- c(
  set_prior(paste0("normal(", ForwardPriorLogit, ", 1)"), class = "Intercept"))

ForwardsModel <- brm(
  total_goals.x | trials(total_attempts) ~ total_ShotAttempts + total_goals.y +
    total_shifts + shot_percentage + Age + (1 | playerId),
  data = ForwardsShootoutData_scaled,
  family = binomial(link = "logit"),
  prior = ForwardsPriorFull,
  init = "0",
  chains = 4,
  iter = 3000,
  warmup = 1500,
  control = list(adapt_delta = 0.9)
)
posterior_summary(ForwardsModel)
ForwardsShootoutData$predicted_success <- fitted(ForwardsModel, scale = "response")[, "Estimate"]


DefensemenPriorFull <- c(
  set_prior(paste0("normal(", DefensemenPriorLogit, ", 1)"), class = "Intercept"),
  set_prior("normal(0, 1)", class = "b")
)
DefensemenShootoutData <- ShootersFullDataframe %>%
  select(-name) %>%
  filter(primaryPosition == "D")
DefensemenShootoutData_scaled <- DefensemenShootoutData %>%
  mutate(
    total_ShotAttempts = as.numeric(scale(total_ShotAttempts)),
    total_goals.y = as.numeric(scale(total_goals.y)),
    total_shifts = as.numeric(scale(total_shifts)),
    shot_percentage = as.numeric(scale(shot_percentage)),
    Age = as.numeric(scale(Age))
  )
DefensemenModel <- brm(
  total_goals.x | trials(total_attempts) ~ total_GamesPlayed + total_ShotAttempts + total_goals.y +
    total_shifts + shot_percentage + Age + (1 | playerId),
  data = DefensemenShootoutData,
  family = binomial(link = "logit"),
  prior = DefensemenPriorFull
)


#using rstanarm for the model
library(rstanarm)
forwards_prior_intercept = normal(location = ForwardPriorLogit, scale = 1) 
ForwardsModel2 <- stan_glmer(cbind(total_goals.x, total_attempts - total_goals.x) ~ total_ShotAttempts + total_goals.y +
                   total_shifts + shot_percentage + shootsCatches +(1 | playerId), 
                 data = ForwardsShootoutData_scaled, 
                 family = binomial(link = "logit"), 
                 prior = normal(0, 1),
                 prior_intercept = normal(ForwardPriorLogit, 1),
                 chains = 4,
                 cores = 2,
                 seed = 03032002)
summary(ForwardsModel2)
ForwardsShootoutData$predicted_success <- fitted(ForwardsModel2, type = "response")


DefensemenModel2 <- stan_glmer(cbind(total_goals.x, total_attempts - total_goals.x) ~ total_ShotAttempts + total_goals.y +
                               total_shifts + shot_percentage + shootsCatches + (1 | playerId), 
                             data = DefensemenShootoutData_scaled, 
                             family = binomial(link = "logit"), 
                             prior = normal(0, 1),
                             prior_intercept = normal(DefensemenPriorLogit, 1),
                             chains = 4,
                             cores = 2,
                             seed = 03032002)
summary(DefensemenModel2)
DefensemenShootoutData$predicted_success <- fitted(DefensemenModel2, type = "response")



#combine dataframes with predicted make rates
AllPlayersShootoutData <- rbind(ForwardsShootoutData, DefensemenShootoutData) %>%
  arrange(desc(predicted_success))


#create shootout simulation for Monte Carlo simualation, assuming team of interest is going second
simulate_shootout <- function(team_lineup, opp_team_rate = 0.33){
  team_goals <- 0
  opp_team_goals <- 0
  i <- 1
  for (round in 1:3) {
    opp_shot <- rbinom(1, 1, opp_team_rate)
    opp_team_goals <- opp_shot + opp_team_goals
    
    remaining_team_shots <- 3 - round + 1
    if(opp_team_goals > team_goals + remaining_team_shots){
      return(0)
    }
    team_shot <- rbinom(1, 1, team_lineup[i])
    team_goals <- team_shot + team_goals
    i <- i + 1
    
    remaining_opp_shots <- 3 - round
    if(team_goals > opp_team_goals + remaining_opp_shots){
        return(1)      
    }
  }
  while (TRUE) {
    if (i > length(team_lineup)) {
      team_sd <- rbinom(1, 1, sample(team_lineup, 1))
    } else {
      team_sd <- rbinom(1, 1, team_lineup[i])
      i <- i + 1
    }
    opp_sd <- rbinom(1, 1, opp_team_rate)
    if (team_sd > opp_sd) return(1)
    if (team_sd < opp_sd) return(0)
  }
}


#get players and teams for 2024-25 season only
ShootoutEligible2024_25 <- AllPlayersShootoutData %>%
  filter(playerId %in% skaters_2024_25$playerId)
View(ShootoutEligible2024_25)

ShootoutPlayersSelectCol <- ShootoutEligible2024_25 %>%
  select(playerId, Player, team, predicted_success)

ShootoutPlayersSelectColWithLogo <- ShootoutPlayersSelectCol %>%
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
      TRUE ~ "NA"))

library(gt)
ShootoutPlayersSelectColWithLogo %>%
  select(img, Player, predicted_success) %>%
  gt() |>
  text_transform(
    locations = cells_body(columns = img),
    fn = function(x) web_image(url = x, height = 30) # Display team logos
  ) |>
  cols_label(
    img = "Team",
    predicted_success = "Bayesian Prediction Make Rate"
  ) |>
  # Apply formatting here
  fmt_number(
    columns = c(predicted_success),
    decimals = 4 # Rounds to 2 decimal places for display
  ) |>
  tab_header(
    title = md("**Predicted Make Rate**")
  ) |>
  cols_align(
    align = "center",
    columns = c(img, Player, predicted_success)
  )
  
head(ShootoutPlayersSelectCol)
EDMShootout <- ShootoutPlayersSelectCol %>%
  filter(team == "EDM")
FLAShootout <- ShootoutPlayersSelectCol %>%
  filter(team == "FLA")
MTLShootout <- ShootoutPlayersSelectCol %>%
  filter(team == "MTL")
CHIShootout <- ShootoutPlayersSelectCol %>%
  filter(team == "CHI")
UTAShootout <- ShootoutPlayersSelectCol %>%
  filter(team == "UTA")

#create function to simulate all permutations for each team
library(purrr)
library(tibble)
library(combinat)
library(gtools)
get_optimal_lineup <- function(team_df, team_name, n_sim = 1000, opp_rate = 0.33){
  if (nrow(team_df) < 3) {
    warning(paste("Not enough players to create a 3-player lineup for", team_name))
    return(NULL)
  }
  
  idx_perms <- permutations(n = nrow(team_df), r = 3)
  
  results <- purrr::map_dfr(1:nrow(idx_perms), function(i) {
    idx <- idx_perms[i, ]
    probs <- team_df$predicted_success[idx]
    players <- team_df$Player[idx]
    
    wins <- sum(replicate(n_sim, simulate_shootout(probs, opp_rate)))
    
    tibble(
      team = team_name,
      lineup = paste(players, collapse = ", "),
      win_rate = wins / n_sim
    )
  })
  
  results %>% arrange(desc(win_rate)) %>% slice(1:3)
}
get_optimal_lineup(FLAShootout,10000,0.33)
get_optimal_lineup(EDMShootout,10000,0.33)
get_optimal_lineup(MTLShootout,10000,0.33)
get_optimal_lineup(CHIShootout,10000,0.33)
get_optimal_lineup(UTAShootout,10000,0.33)

ActualShootoutDensity <- ShootoutEligible2024_25 %>%
  filter(total_attempts > 5) %>%
  pull(shootout_percentage) %>%
  density()
plot(
  ActualShootoutDensity,
  main = "Actual Shootout Percentage Density (Min 5 Attempts)",
  xlab = "Shootout Success Rate",
  ylab = "Density",
  yaxt = "l"
)


PrecitedShootoutDensity <- ShootoutEligible2024_25 %>%
  pull(predicted_success) %>%
  density()
plot(
  PrecitedShootoutDensity,
  main = "Predicted Shootout Percentage Density",
  xlab = "Shootout Success Rate",
  ylab = "Density",
  yaxt = "l"
)

#function to cycle through all teams and return the top 3 lineups for each team
all_teams_shootouts <- function(players_df, n_sim = 1000, opp_rate = 0.33){
  all_teams <- unique(players_df$team)
  
  purrr::map_dfr(all_teams, function(team1) {
    team_shooters <- players_df %>% filter(team == team1)
    get_optimal_lineup(team_shooters, team1, n_sim, opp_rate)
  })
}
TopShootoutPerms <- all_teams_shootouts(ShootoutPlayersSelectCol, 10000, 0.33)

write.csv(TopShootoutPerms, "~/Desktop/SC Stingrays Game Data/PK Data/PK Analysis Output/PKSummaries12.22To1.5.csv", row.names = FALSE)

TopShootoutPermsWithLogo <- TopShootoutPerms %>%
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
      TRUE ~ "NA"))

TopShootoutPermsWithLogo %>%
  select(img, lineup, win_rate) %>%
  gt() |>
  cols_label(
    img = "Team",
    lineup = "Shootout Lineup",
    win_rate = "Predicted Win Rate"
  ) |>
  cols_align(
    align = "center",
    columns = everything()
  ) |>
  tab_header(
    title = md("Top Shootout Lineup Permutations and Predicted Win Rate")
  ) |>
  fmt_percent(columns = win_rate) |>
  text_transform(
    locations = cells_body(columns = img),
    fn = function(x) web_image(url = x, height = 30))
  

BestShootoutPermByTeam <- TopShootoutPerms %>%
  group_by(team) %>%
  slice_max(order_by = win_rate, n = 1, with_ties = FALSE) %>%
  ungroup() 

BestShootoutPermByTeamWithLogo <- BestShootoutPermByTeam %>%
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
      TRUE ~ "NA"))


BestShootoutPermByTeamWithLogo %>%
  select(img, lineup, win_rate) %>%
  gt() |>
  cols_label(
    img = "Team",
    lineup = "Shootout Lineup",
    win_rate = "Predicted Win Rate"
  ) |>
  cols_align(
    align = "center",
    columns = everything()
  ) |>
  tab_header(
    title = md("Predicted Win Rate for Optimal Shootout Lineup")
  ) |>
  text_transform(
    locations = cells_body(columns = img),
    fn = function(x) web_image(url = x, height = 30))
  

ShootoutTable <- BestShootoutPermByTeamWithLogo %>%
  select(img, lineup, win_rate) %>%
  gt() |>
  cols_label(
    img = "Team",
    lineup = "Shootout Lineup",
    win_rate = "Predicted Win Rate"
  ) |>
  cols_align(
    align = "center",
    columns = everything()
  ) |>
  fmt_percent(columns = win_rate, decimals = 2) |>
  tab_header(
    title = md("**Predicted Win Rate for Optimal Shootout Lineup**")
  ) |>
  text_transform(
    locations = cells_body(columns = img),
    fn = function(x) web_image(url = x, height = 30)
  )


library(gt)
library(dplyr)
library(htmltools)

# Step 1: Split your data into 4 groups of 8 teams each
SplitShootoutTables <- BestShootoutPermByTeamWithLogo %>%
  arrange(team) %>%
  mutate(group = rep(1:4, each = 8)) %>%
  group_split(group)

# Step 2: Create a reusable function to generate gt tables
make_gt_table <- function(df) {
  df %>%
    select(img, lineup, win_rate) %>%
    gt() |>
    cols_label(
      img = "Team",
      lineup = "Shootout Lineup",
      win_rate = "Predicted Win Rate"
    ) |>
    cols_align(
      align = "center",
      columns = everything()
    ) |>
    fmt_percent(columns = win_rate, decimals = 2) |>
    tab_options(
      table.width = pct(100)
    ) |>
    text_transform(
      locations = cells_body(columns = img),
      fn = function(x) web_image(url = x, height = 30)
    )
}

# Step 3: Create all four gt tables
gt1 <- make_gt_table(SplitShootoutTables[[1]])
gt2 <- make_gt_table(SplitShootoutTables[[2]])
gt3 <- make_gt_table(SplitShootoutTables[[3]])
gt4 <- make_gt_table(SplitShootoutTables[[4]])

# Step 4: Display all 4 side by side using htmltools
browsable(
  tagList(
    div(style = "display: flex; gap: 20px; justify-content: space-between;",
        div(gt1),
        div(gt2),
        div(gt3),
        div(gt4)
    )
  )
)


  