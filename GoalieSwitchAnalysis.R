#looking at goalies starting hot or cold
library(dplyr)
library(tidyr)
library(gt)
library(ggplot2)
library(sportyR)


shots_2024_25_FINALRegSzn <- shots_2024_25_FINAL %>%
  filter(isPlayoffGame == 0) %>%
  mutate(SOG = ifelse((event=="SHOT"|event=="GOAL"),1,0)) 

GoalieGames <- shots_2024_25_FINALRegSzn %>%
  filter(goalieNameForShot != "") %>%
  arrange(game_id, shotID) %>%
  group_by(game_id, goalieIdForShot) %>%
  group_by(game_id, goalieNameForShot) %>%
  mutate(
    ShotsInGame = cumsum(SOG),
    GoalsInGame = cumsum(goal),
    TempFirstGoalFlag = (ShotsInGame == 1 & GoalsInGame == 1),
    GoalOnFirstShot = max(.data$`TempFirstGoalFlag`),
    RollingSavePercentage = (ShotsInGame - GoalsInGame) / ShotsInGame
  ) %>%
  select(-TempFirstGoalFlag) %>%
  ungroup()

GoalConcededFirstShotV2 <- GoalieGames %>%
  filter(GoalOnFirstShot == 1)

#first shot of game
GoalConcededFirstShot <- GoalieGames %>%
  filter(ShotsInGame == 1 & GoalsInGame == 1 & goal == 1)


geom_hockey(league = "NHL", display_range = "full") +geom_point(data = GoalConcededFirstShot,aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord)) +
  labs(title = "Location of Goals on First SOG of Game", subtitle = "2024-25 Regular Season") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5))

GoalConcededFirstShotSummarized <- GoalConcededFirstShot %>%
  mutate(GoalieTeam = ifelse(teamCode==homeTeamCode, awayTeamCode, homeTeamCode)) %>%
  group_by(goalieNameForShot) %>%
  summarise(GoalOnFirstSOGCount = n(),
            Team = last(GoalieTeam)) %>%
  ungroup() %>%
  arrange(desc(GoalOnFirstSOGCount))

SaveonFirstShot <- GoalieGames %>%
  filter(GoalOnFirstShot == 0)

GameSavePercentFirstSOGSave <- SaveonFirstShot %>%
  mutate(GoalieTeam = ifelse(teamCode==homeTeamCode, awayTeamCode, homeTeamCode),
         Opponent = teamCode) %>%
  arrange(game_id, shotID) %>%
  group_by(goalieNameForShot, goalieIdForShot ,game_id) %>%
  summarise(GameSavePercent = last(RollingSavePercentage),
            ShotsFaced = last(ShotsInGame),
            GoalsAllowed = last(GoalsInGame),
            Team = last(GoalieTeam),
            Opponent = last(Opponent)) %>%
  ungroup()

ggplot(data = GameSavePercentFirstSOGSave, aes(x=GameSavePercent)) + geom_density() +
  labs(title = "Save Percentage When Goalie Saves First SOG of Game",
       subtitle = "2024-25 Regular Season")

#Save Percentage in Game when goal on first shot
FinalSavePercentAfterGoalOnFirstSOG <- GoalConcededFirstShotV2 %>%
  mutate(GoalieTeam = ifelse(teamCode==homeTeamCode, awayTeamCode, homeTeamCode),
         Opponent = teamCode) %>%
  arrange(game_id, shotID) %>%
  group_by(goalieNameForShot, goalieIdForShot ,game_id) %>%
  summarise(GameSavePercent = last(RollingSavePercentage),
            ShotsFaced = last(ShotsInGame),
            GoalsAllowed = last(GoalsInGame),
            Team = last(GoalieTeam),
            Opponent = last(Opponent)) %>%
  ungroup()

ggplot(data = FinalSavePercentAfterGoalOnFirstSOG, aes(x=GameSavePercent)) + geom_density() +
  labs(title = "Save Percentage When Goalie Allows Goal on First SOG of Game",
       subtitle = "2024-25 Regular Season")

goalie_appearances <- GoalieGames %>%
  group_by(goalieIdForShot, goalieNameForShot, game_id) %>%
  summarise(NumberOfShotForGoalie = last(ShotsInGame)) %>%
  summarise(Appearances = n()) %>%
  ungroup() %>%
  arrange(desc(Appearances))

GoaliesSummarized <- GoalieGames %>%
  group_by(game_id, goalieIdForShot, goalieNameForShot) %>%
  summarise(GameFinishShots = last(ShotsInGame),
            GameFinishGoals = last(GoalsInGame)) %>%
  ungroup() %>%
  group_by(goalieIdForShot, goalieNameForShot) %>%
  summarise(SeasonSOGs = sum(GameFinishShots),
            SeasonGoals = sum(GameFinishGoals)) %>%
  ungroup() %>%
  mutate(SavePercent = 1-(SeasonGoals/SeasonSOGs)) %>%
  arrange(desc(SavePercent))

ggplot(data = GoaliesSummarized, aes(x=SavePercent)) + geom_density() +
  labs(title = "Density of Save Percentages of Goalies",
       subtitle = "2024-25 Regular Season\nAll Goalies, Inclusive of Non-Qualifiers")

GoaliesSummarizedFiltered <- GoalieGames %>%
  group_by(game_id, goalieIdForShot, goalieNameForShot) %>%
  summarise(GameFinishShots = last(ShotsInGame),
            GameFinishGoals = last(GoalsInGame)) %>%
  ungroup() %>%
  group_by(goalieIdForShot, goalieNameForShot) %>%
  summarise(SeasonSOGs = sum(GameFinishShots),
            SeasonGoals = sum(GameFinishGoals)) %>%
  ungroup() %>%
  mutate(SavePercent = 1-(SeasonGoals/SeasonSOGs)) %>%
  arrange(desc(SavePercent)) %>%
  filter(SeasonSOGs >= 250)

ggplot(data = GoaliesSummarizedFiltered, aes(x=SavePercent)) + geom_density() +
  labs(title = "Density of Save Percentages of Goalies",
       subtitle = "2024-25 Regular Season\nMinimum 250 Shots on Goal")

StartingGoalies <- goalie_appearances %>%
  filter(Appearances >= 41)

StartingGoaliesIDVector <- StartingGoalies$goalieIdForShot

StartingGoaliesSummarized <- GoalieGames %>%
  filter(goalieIdForShot %in% StartingGoaliesIDVector) %>%
  group_by(game_id, goalieIdForShot, goalieNameForShot) %>%
  summarise(GameFinishShots = last(ShotsInGame),
            GameFinishGoals = last(GoalsInGame)) %>%
  ungroup() %>%
  group_by(goalieIdForShot, goalieNameForShot) %>%
  summarise(SeasonSOGs = sum(GameFinishShots),
            SeasonGoals = sum(GameFinishGoals)) %>%
  ungroup() %>%
  mutate(SavePercent = 1-(SeasonGoals/SeasonSOGs)) %>%
  arrange(desc(SavePercent))

ggplot(data = StartingGoaliesSummarized, aes(x=SavePercent)) + geom_density() +
  labs(title = "Density of Save Percentages of Starting Goalies",
       subtitle = "2024-25 Regular Season")

BackupGoalies <- goalie_appearances %>%
  filter(Appearances <= 41)

BackupGoaliesIDVector <- BackupGoalies$goalieIdForShot

BackupGoaliesSummarized <- GoalieGames %>%
  filter(goalieIdForShot %in% BackupGoaliesIDVector) %>%
  group_by(game_id, goalieIdForShot, goalieNameForShot) %>%
  summarise(GameFinishShots = last(ShotsInGame),
            GameFinishGoals = last(GoalsInGame)) %>%
  ungroup() %>%
  group_by(goalieIdForShot, goalieNameForShot) %>%
  summarise(SeasonSOGs = sum(GameFinishShots),
            SeasonGoals = sum(GameFinishGoals)) %>%
  ungroup() %>%
  mutate(SavePercent = 1-(SeasonGoals/SeasonSOGs)) %>%
  arrange(desc(SavePercent))

ggplot(data = BackupGoaliesSummarized, aes(x=SavePercent)) + geom_density() +
  labs(title = "Density of Save Percentages of Backup Goalies",
       subtitle = "2024-25 Regular Season\nAll Goalies, Inclusive of Non-Qualifiers")

BackupGoaliesSummarizedFiltered <- BackupGoaliesSummarized %>%
  filter(SeasonSOGs >= 250)

ggplot(data = BackupGoaliesSummarizedFiltered, aes(x=SavePercent)) + geom_density() +
  labs(title = "Density of Save Percentages of Backup Goalies",
       subtitle = "2024-25 Regular Season\nMinimum 250 Shots on Goals")



alpha_starter_prior <- 1316 + 1 #average number of saves for starters + 1
beta_starter_prior <- 141 + 1 #average number of goals against for starters + 1

alpha_backup_prior <- 584 + 1 #average number of saves for a backup + 1
beta_backup_prior <- 68 + 1 #average number of goals against for starters + 1

n_simulations <- 100000
switch_threshold <- 0.8

calculate_switch_probability <- function(shots_new, goals_new){
  saves_new <- shots_new - goals_new
  
  alpha_post <- alpha_starter_prior + saves_new
  beta_post <- beta_starter_prior + goals_new
  
  starter_samples <- rbeta(n_simulations, alpha_post, beta_post)
  backup_samples <- rbeta(n_simulations, alpha_backup_prior, beta_backup_prior)
  
  count_worse <- sum(starter_samples < backup_samples)
  
  prob_switch <- count_worse/n_simulations
  return(prob_switch)
}

decision_grid <- expand.grid(Shots = 1:5) %>%
  rowwise() %>%
  mutate(Goals = list(0:Shots)) %>%
  unnest(Goals) %>%
  ungroup()

decision_grid <- decision_grid %>%
  rowwise() %>%
  mutate(Prob_Switch = calculate_switch_probability(Shots, Goals),
         Recommendaiton = ifelse(Prob_Switch >= switch_threshold, "SWITCH", "STAY")) %>%
  ungroup()





alpha_oettinger_prior <- 455 + 1 #average number of saves for starters + 1
beta_oettinger_prior <- 46 + 1 #average number of goals against for starters + 1

alpha_desmith_prior <- 14 + 1 #average number of saves for a backup + 1
beta_desmith_prior <- 1 + 1 #average number of goals against for starters + 1

calculate_switch_probability <- function(shots_new, goals_new){
  saves_new <- shots_new - goals_new
  
  alpha_post <- alpha_oettinger_prior + saves_new
  beta_post <- beta_oettinger_prior + goals_new
  
  starter_samples <- rbeta(n_simulations, alpha_post, beta_post)
  backup_samples <- rbeta(n_simulations, alpha_desmith_prior, beta_desmith_prior)
  
  count_worse <- sum(starter_samples < backup_samples)
  
  prob_switch <- count_worse/n_simulations
  return(prob_switch)
}

oettinger_decision_grid <- expand.grid(Shots = 1:5) %>%
  rowwise() %>%
  mutate(Goals = list(0:Shots)) %>%
  unnest(Goals) %>%
  ungroup()

oettinger_decision_grid <- decision_grid %>%
  rowwise() %>%
  mutate(Prob_Switch = calculate_switch_probability(Shots, Goals),
         Recommendaiton = ifelse(Prob_Switch >= switch_threshold, "SWITCH", "STAY")) %>%
  ungroup()