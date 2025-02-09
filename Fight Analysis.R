### Fight Analysis
library(dplyr)
library(xgboost)
library(caret)
library(tidyr)

#Combine dataframes
fight_data <- HockeyFightLog %>%
  left_join(allPlayersLookup, by = c("Fighter1" = "name")) %>%
  rename(Fighter1Height = height, Fighter1Weight = weight, Fighter1BirthDate = birthDate) %>%
  left_join(allPlayersLookup, by = c("Fighter2" = "name")) %>%
  rename(Fighter2Height = height, Fighter2Weight = weight, Fighter2BirthDate = birthDate)

#Create function to convert text to inches
convert_height_to_inches <- function(height) {
  matches <- regmatches(height, regexec("^(\\d+)'\\s?(\\d{1,2})?\\\"?$", height))
  feet <- as.numeric(matches[[1]][2])
  inches <- as.numeric(matches[[1]][3])
  if (is.na(inches)) inches <- 0
  total_inches <- feet * 12 + inches
  return(total_inches)
}

#Prepare data and add columns for analysis
fight_data$Fighter1HeightInches <- sapply(fight_data$Fighter1Height, convert_height_to_inches)
fight_data$Fighter2HeightInches <- sapply(fight_data$Fighter2Height, convert_height_to_inches)
fight_data$HeightDiff <- fight_data$Fighter1HeightInches - fight_data$Fighter2HeightInches
fight_data$WeightDiff <- fight_data$Fighter1Weight - fight_data$Fighter2Weight
fight_data$Date <- as.Date(fight_data$Date, format = "%m/%d/%Y")

fight_data <- fight_data %>%
  mutate(
    Fighter1Age = as.numeric(difftime(Date, Fighter1BirthDate, units = "weeks")) / 52.25,
    Fighter2Age = as.numeric(difftime(Date, Fighter2BirthDate, units = "weeks")) / 52.25,
    AgeDiff = Fighter1Age - Fighter2Age,
    Win = ifelse(Fighter1WinPct > Fighter2WinPct, 1, 0)
  )

team_mapping <- c("MTL" = 1, "CHI" = 2, "COL" = 3, "EDM" = 4, "VGK" = 5, "PIT" = 6, 
                  "PHI" = 7, "FLA" = 8, "MIN" = 9, "ANA" = 10, "DET" = 11, "TBL" = 12, 
                  "LAK" = 13, "DAL" = 14, "WSH" = 15, "VAN" = 16, "CAR" = 17, "CGY" = 18, 
                  "NYI" = 19, "SJS" = 20, "NYR" = 21, "BOS" = 22, "BUF" = 23, "CBJ" = 24, 
                  "OTT" = 25, "STL" = 26, "WPG" = 27, "ARI" = 28, "NSH" = 29, "NJD" = 30, 
                  "TOR" = 31, "SEA" = 32, "UTA" = 28)

determine_winner1 <- fight_data %>%
  filter(Win == 1)
quantile(determine_winner1$Fighter1WinPct, probs = c(0,0.25,0.5,0.75,1))

determine_winner2 <- fight_data %>%
  filter(Win == 0)
quantile(determine_winner2$Fighter2WinPct, probs = c(0,0.25,0.5,0.75,1))

fight_data_clean <- fight_data %>%
  mutate(
    Fighter1TeamNum = team_mapping[Fighter1Team],  # Map Fighter 1's team to the numeric value
    Fighter2TeamNum = team_mapping[Fighter2Team]   # Map Fighter 2's team to the numeric value
  )

player_lookup <- fight_data %>%
  select(Fighter1, playerId.x) %>%
  rename(Player = Fighter1, PlayerID = playerId.x) %>%
  distinct() %>%
  bind_rows(
    fight_data %>%
      select(Fighter2, playerId.y) %>%
      rename(Player = Fighter2, PlayerID = playerId.y) %>%
      distinct()
  ) %>%
  distinct()

player_lookup <- player_lookup %>%
  mutate(PlayerID = as.character(PlayerID))

player_lookup2 <- fight_data_processed2 %>%
  mutate(
    OriginalFighter1ID = as.character(OriginalFighter1ID),
    OriginalFighter2ID = as.character(OriginalFighter2ID)
  ) %>%
  left_join(player_lookup, by = c("OriginalFighter1ID" = "PlayerID")) %>%
  rename(Fighter1Name = Player) %>%
  left_join(player_lookup, by = c("OriginalFighter2ID" = "PlayerID")) %>%
  rename(Fighter2Name = Player)

player_lookup3 <- player_lookup2 %>%
  select(Fighter1ID, Fighter2ID, OriginalFighter1ID, OriginalFighter2ID, Fighter1Name, Fighter2Name)


fight_data_clean2 <- fight_data_clean %>%
  mutate(
    Fighter1ID = as.factor(playerId.x), 
    Fighter2ID = as.factor(playerId.y),
    Fighter1TeamNum = as.factor(Fighter1TeamNum),
    Fighter2TeamNum = as.factor(Fighter2TeamNum)
  ) %>%
  select(Fighter1TeamNum, Fighter2TeamNum, AgeDiff, HeightDiff, WeightDiff, 
         Fighter1ID, Fighter2ID, Win)

fighter_counts1 <- fight_data_clean2 %>%
  count(Fighter1ID) %>%
  rename(FighterID = Fighter1ID, n1 = n)

fighter_counts2 <- fight_data_clean2 %>%
  count(Fighter2ID) %>%
  rename(FighterID = Fighter2ID, n2 = n)

# Merge both counts
fighter_counts <- full_join(fighter_counts1, fighter_counts2, by = "FighterID") %>%
  replace_na(list(n1 = 0, n2 = 0)) %>%
  mutate(total_fights = n1 + n2)

# Ensure each fighter has at least 2 fights in BOTH roles
eligible_fighters <- fighter_counts %>%
  filter(total_fights >= 3) %>%
  pull(FighterID)

# Apply the filter
fight_data_filtered <- fight_data_clean2 %>%
  filter(Fighter1ID %in% eligible_fighters & Fighter2ID %in% eligible_fighters)

nrow(fight_data_filtered)

fight_data_processed2 <- fight_data_filtered %>%
  mutate(
    OriginalFighter1ID = Fighter1ID,  # Store original Fighter1ID
    OriginalFighter2ID = Fighter2ID,  # Store original Fighter2ID
    Fighter1TeamNum = as.numeric(Fighter1TeamNum),
    Fighter2TeamNum = as.numeric(Fighter2TeamNum),
    Fighter1ID = as.numeric(Fighter1ID),  # Convert Fighter1ID to numeric
    Fighter2ID = as.numeric(Fighter2ID),  # Convert Fighter2ID to numeric
    Win = as.numeric(as.character(Win))  # Convert target variable to binary (0/1)
  )

fight_data_processed <- fight_data_filtered %>%
  mutate(
    Fighter1TeamNum = as.numeric(Fighter1TeamNum),
    Fighter2TeamNum = as.numeric(Fighter2TeamNum),
    Fighter1ID = as.numeric(Fighter1ID),
    Fighter2ID = as.numeric(Fighter2ID),
    Win = as.numeric(as.character(Win))  # Convert target variable to binary (0/1)
  )


#Creat the XGBoost model
set.seed(2025)
X <- fight_data_processed[, -which(names(fight_data_processed) == "Win")]
y <- fight_data_processed$Win

X_train_numeric <- as.data.frame(lapply(X, function(x) as.numeric(as.character(x))))
str(X_train_numeric)
X_train_numeric <- as.matrix(X_train_numeric)
dtrain <- xgb.DMatrix(data = as.matrix(X_train_numeric), label = y)

params <- list(
  objective = "binary:logistic",  
  eval_metric = "logloss",        
  eta = 0.1,                      
  max_depth = 6,                 
  colsample_bytree = 0.8,        
  subsample = 0.8                 
)


cv_results <- xgb.cv(
  params = params,
  data = dtrain,
  nrounds = 100,            
  nfold = 5,               
  showsd = TRUE,           
  stratified = TRUE,       
  print_every_n = 10,      
  early_stopping_rounds = 20, 
  prediction = TRUE        
)


print(cv_results)


best_nrounds <- cv_results$best_iteration


final_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = best_nrounds
)


pred_probs <- predict(final_model, newdata = dtrain)


pred_labels <- ifelse(pred_probs >= 0.5, 1, 0)


accuracy <- sum(pred_labels == y) / length(y)
print(paste("Accuracy of the best model: ", accuracy))


conf_matrix <- confusionMatrix(factor(pred_labels), factor(y))
print(conf_matrix)

importance <- xgb.importance(feature_names = colnames(dtrain), model = final_model)


print(importance)


xgb.plot.importance(importance_matrix = importance)


#Monte Carlo simulation with IDs
monte_carlo_simulation <- function(player_id, num_simulations = 10000) {
  # Get valid fighter IDs
  valid_fighter_ids <- unique(c(fight_data_processed2$Fighter1ID, fight_data_processed2$Fighter2ID))
  
  # Ensure player_id is in valid list
  if (!(player_id %in% valid_fighter_ids)) {
    stop("Invalid player_id. The player must exist in the dataset.")
  }
  
  # Get player team number
  player_team_num <- fight_data_processed2 %>%
    filter(Fighter1ID == player_id | Fighter2ID == player_id) %>%
    pull(Fighter1TeamNum) %>%
    first()
  
  # Generate matchups against real fighters (excluding the player from self-matchups)
  opponent_ids <- valid_fighter_ids[valid_fighter_ids != player_id]
  
  # Simulated fight data
  simulated_fights <- tibble(
    Fighter1ID = rep(player_id, num_simulations),
    Fighter2ID = sample(opponent_ids, num_simulations, replace = TRUE),  # Only real opponents
    AgeDiff = sample(fight_data_processed2$AgeDiff, num_simulations, replace = TRUE),
    HeightDiff = sample(fight_data_processed2$HeightDiff, num_simulations, replace = TRUE),
    WeightDiff = sample(fight_data_processed2$WeightDiff, num_simulations, replace = TRUE),
    Fighter1TeamNum = rep(player_team_num, num_simulations),
    Fighter2TeamNum = sample(fight_data_processed2$Fighter2TeamNum, num_simulations, replace = TRUE)
  )
  
  # Ensure column names match training data
  simulated_matrix <- simulated_fights %>%
    select(all_of(colnames(dtrain))) %>%  # Keep only relevant features
    as.matrix()
  
  # Predict probabilities using trained model
  pred_probs <- predict(final_model, newdata = simulated_matrix)
  
  # Convert probabilities to binary outcomes (win/loss)
  pred_labels <- ifelse(pred_probs >= 0.5, 1, 0)
  
  # Calculate win percentage
  win_rate <- mean(pred_labels)
  
  # Return results
  list(
    player_id = player_id,
    total_simulations = num_simulations,
    estimated_win_rate = win_rate,
    total_wins = sum(pred_labels)
  )
}


simulation_results <- monte_carlo_simulation(player_id = 150, num_simulations = 10000)

# Print results
print(simulation_results)


FightersData2 <- FightersData %>%
  mutate(
    FighterTeamNum = team_mapping[Team],  # Map Fighter 1's team to the numeric value
  )



#Function to predict the winner based on fighter name inputs
predict_fight <- function(fighter1_name, fighter2_name, model, fighter_data) {
  library(dplyr)
  library(xgboost)
  library(gt)
  library(rlang)
  # Retrieve fighter information
  fighter1 <- fighter_data %>% filter(Fighter.Name == fighter1_name)
  fighter2 <- fighter_data %>% filter(Fighter.Name == fighter2_name)
  
  # Check if both fighters exist
  if (nrow(fighter1) == 0 | nrow(fighter2) == 0) {
    stop("One or both fighters not found in dataset.")
  }
  
  # Calculate fighter ages
  Fighter1Age <- as.numeric(difftime(Sys.Date(), as.Date(fighter1$Date.of.Birth), units = "weeks")) / 52.25
  Fighter2Age <- as.numeric(difftime(Sys.Date(), as.Date(fighter2$Date.of.Birth), units = "weeks")) / 52.25
  
  # Create feature matrix
  fight_features <- matrix(c(
    as.numeric(fighter1$Team),
    as.numeric(fighter2$Team),
    as.numeric(Fighter1Age - Fighter2Age),
    as.numeric(fighter1$HeightInches - fighter2$HeightInches),
    as.numeric(fighter1$Weight - fighter2$Weight),
    as.numeric(fighter1$ShortID1),
    as.numeric(fighter2$ShortID2)
  ), nrow = 1, ncol = 7)
  
  # Print the matrix values for debugging
  cat("\nFeature Matrix (Input to Model):\n")
  print(fight_features)
  
  # Convert to xgb.DMatrix to match training format
  fight_matrix <- xgb.DMatrix(data = fight_features)
  
  # Predict probability
  pred_prob <- predict(model, newdata = fight_matrix)
  pred_prob_rounded <- round(pred_prob, 3)
  
  winner <- ifelse(pred_prob_rounded >= 0.5, fighter1_name, fighter2_name)
  loser <- ifelse(pred_prob_rounded < 0.5, fighter1_name, fighter2_name)
  
  # Dynamically name the probability column
  prob_col_name <- paste(fighter1_name, "Win Probability")
  
  result_table <- tibble(
    "Fighter 1" = fighter1_name,
    "Fighter 1 Image" = fighter1$img,
    "Fighter 2" = fighter2_name,
    "Fighter 2 Image" = fighter2$img,
    "Predicted Winner" = winner,
    "Win Probability" = pred_prob_rounded
  ) %>%
    rename(!!prob_col_name := "Win Probability") %>%  # Rename column dynamically
    gt() %>%
    text_transform(
      locations = cells_body(columns = "Fighter 1 Image"),
      fn = function(x) ifelse(!is.na(x) & x != "", web_image(url = x, height = 50), "")
    ) %>%
    text_transform(
      locations = cells_body(columns = "Fighter 2 Image"),
      fn = function(x) ifelse(!is.na(x) & x != "", web_image(url = x, height = 50), "")
    ) %>%
    cols_move_to_start(columns = "Fighter 1 Image") %>%
    cols_move(columns = "Fighter 2 Image", after = "Fighter 1") %>%
    tab_header(
      title = "Fight Prediction",
      subtitle = paste(fighter1_name, "vs", fighter2_name)
    ) %>%
    fmt_percent(columns = !!sym(prob_col_name), decimals = 1) %>%
    cols_label(
      "Fighter 1 Image" = "",
      "Fighter 2 Image" = "",
      !!prob_col_name := paste(fighter1_name, "Win %")
    ) %>%
    cols_align(
      align = "center",
      columns = c("Fighter 1", "Fighter 2", "Predicted Winner", !!sym(prob_col_name))
    )
  
  # Print winner/loser info for debugging
  cat("Winner:", winner, "| Loser:", loser, "\n")
  
  # Return both the table and winner/loser info
  return(list(result_table = result_table, winner = winner, loser = loser))
}

#Checking results
result <- predict_fight("Ryan Reaves", "Arber Xhekaj", final_model, FightersData2)
print(result)

#Creation of the divisions
HeavyweightDivision <- c("Ryan Reaves", "Matt Rempe", "Mathieu Olivier", "Arber Xhekaj",
                         "Tom Wilson", "Nicolas Deslauriers", "Michael McCarron", "Mark Kastelic",
                         "Marcus Foligno", "Liam O'Brien", "Andreas Englund", "Tanner Jeannot",
                         "Ross Johnston", "Jonah Gadjovich", "Patrick Maroon",
                         "Luke Kunin", "Trent Frederic", "Brenden Dillon",
                         "Nikita Zadorov", "Kurtis MacDermid")

HeavyweightDivision2 <- c("Ryan Reaves", "Matt Rempe", "Mathieu Olivier", "Arber Xhekaj",
                         "Nicolas Deslauriers")

fight_matchups <- expand.grid(Fighter1 = HeavyweightDivision, Fighter2 = HeavyweightDivision, 
                              stringsAsFactors = FALSE) %>%  
  filter(Fighter1 != Fighter2)

fight_matchups2 <- expand.grid(Fighter1 = HeavyweightDivision2, Fighter2 = HeavyweightDivision2, 
                              stringsAsFactors = FALSE) %>%  
  filter(Fighter1 != Fighter2)

print(fight_matchups)

#Simulate all macthups
fighters <- unique(c(fight_matchups$Fighter1, fight_matchups$Fighter2))
win_loss_record <- data.frame(Fighter = fighters, Wins = 0, Losses = 0, stringsAsFactors = FALSE)

for (i in seq_len(nrow(fight_matchups))) {
  fighter1_name <- fight_matchups$Fighter1[i]
  fighter2_name <- fight_matchups$Fighter2[i]
  
  result3 <- predict_fight(fighter1_name, fighter2_name, final_model, FightersData2)
  
  print(result3)
  
  winner <- result3$winner  
  loser <- result3$loser   
  
  print(paste("Winner:", winner, "| Loser:", loser))  
  
  win_loss_record$Wins[win_loss_record$Fighter == winner] <- 
    win_loss_record$Wins[win_loss_record$Fighter == winner] + 1
  
  win_loss_record$Losses[win_loss_record$Fighter == loser] <- 
    win_loss_record$Losses[win_loss_record$Fighter == loser] + 1
}

win_loss_record$WinPct <- win_loss_record$Wins/(win_loss_record$Wins+win_loss_record$Losses)
print(win_loss_record)
win_loss_record %>%
  arrange(desc(WinPct))

fighters2 <- unique(c(fight_matchups2$Fighter1, fight_matchups2$Fighter2))
win_loss_record2 <- data.frame(Fighter = fighters2, Wins = 0, Losses = 0, stringsAsFactors = FALSE)

for (i in seq_len(nrow(fight_matchups2))) {
  fighter1_name <- fight_matchups2$Fighter1[i]
  fighter2_name <- fight_matchups2$Fighter2[i]
  
  result3 <- predict_fight(fighter1_name, fighter2_name, final_model, FightersData2)
  
  print(result3)  
  
  winner <- result3$winner  
  loser <- result3$loser   
  
  print(paste("Winner:", winner, "| Loser:", loser))  
  
  # Update win/loss record
  win_loss_record2$Wins[win_loss_record2$Fighter == winner] <- 
    win_loss_record2$Wins[win_loss_record2$Fighter == winner] + 1
  
  win_loss_record2$Losses[win_loss_record2$Fighter == loser] <- 
    win_loss_record2$Losses[win_loss_record2$Fighter == loser] + 1
}

win_loss_record2$WinPct <- win_loss_record2$Wins/(win_loss_record2$Wins+win_loss_record2$Losses)
print(win_loss_record2)
win_loss_record2 %>%
  arrange(desc(WinPct))


#Add photos to the Fighter Data
FightersData2 <- FightersData2 %>%
  mutate(
    img = case_when(
      Fighter.Name == "Arber Xhekaj" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4893969.png&w=350&h=254",
      Fighter.Name == "Vincent Desharnais" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4392260.png&w=350&h=254",
      Fighter.Name == "Keegan Kolesar" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3941546.png&w=350&h=254",
      Fighter.Name == "Marcus Pettersson" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3114995.png&w=350&h=254",
      Fighter.Name == "Nick Seeler" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/2564164.png&w=350&h=254",
      Fighter.Name == "Marcus Foligno" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/5172.png&w=350&h=254",
      Fighter.Name == "Sam Carrick" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/5474.png&w=350&h=254",
      Fighter.Name == "Klim Kostin" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4233738.png&w=350&h=254",
      Fighter.Name == "Brandon Duhaime" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4197009.png&w=350&h=254",
      Fighter.Name == "Austin Watson" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/2525761.png&w=350&h=254",
      Fighter.Name == "Jamie Benn" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3998.png&w=350&h=254",
      Fighter.Name == "Tom Wilson" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/2970615.png&w=350&h=254",
      Fighter.Name == "Ross Johnston" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3067822.png&w=350&h=254",
      Fighter.Name == "Luke Kunin" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4024933.png&w=350&h=254",
      Fighter.Name == "Trent Frederic" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4024997.png&w=350&h=254",
      Fighter.Name == "Alex Tuch" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3114766.png&w=350&h=254",
      Fighter.Name == "Dmitri Voronkov" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4915856.png&w=350&h=254",
      Fighter.Name == "Patrick Maroon" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3853.png&w=350&h=254",
      Fighter.Name == "Mark Kastelic" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4587985.png&w=350&h=254",
      Fighter.Name == "Robert Bortuzzo" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4916.png&w=350&h=254",
      Fighter.Name == "Nicolas Hague" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4233870.png&w=350&h=254",
      Fighter.Name == "Nick Foligno" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3535.png&w=350&h=254",
      Fighter.Name == "Jarred Tinordi" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/5549.png&w=350&h=254",
      Fighter.Name == "Garnet Hathaway" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3149633.png&w=350&h=254",
      Fighter.Name == "Mason Marchment" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4272192.png&w=350&h=254",
      Fighter.Name == "Tanner Jeannot" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4064780.png&w=350&h=254",
      Fighter.Name == "Nicolas Deslauriers" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/5193.png&w=350&h=254",
      Fighter.Name == "Jeremy Lauzon" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3904185.png&w=350&h=254",
      Fighter.Name == "Cole Smith" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4197130.png&w=350&h=254",
      Fighter.Name == "Ian Cole" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4991.png&w=350&h=254",
      Fighter.Name == "Givani Smith" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4063602.png&w=350&h=254",
      Fighter.Name == "Radko Gudas" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/5502.png&w=350&h=254",
      Fighter.Name == "Michael Eyssimont" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4270247.png&w=350&h=254",
      Fighter.Name == "Jake Middleton" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3149839.png&w=350&h=254",
      Fighter.Name == "Liam O'Brien" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3150468.png&w=350&h=254",
      Fighter.Name == "Mathieu Olivier" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4064781.png&w=350&h=254",
      Fighter.Name == "Andreas Englund" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3114997.png&w=350&h=254",
      Fighter.Name == "Jordan Greenway" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3900260.png&w=350&h=254",
      Fighter.Name == "Nikita Zadorov" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3042021.png&w=350&h=254",
      Fighter.Name == "Dennis Gilbert" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3942030.png&w=350&h=254",
      Fighter.Name == "Kyle Burroughs" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3042255.png&w=350&h=254",
      Fighter.Name == "Barclay Goodrow" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3069411.png&w=350&h=254",
      Fighter.Name == "Will Cuylle" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4697468.png&w=350&h=254",
      Fighter.Name == "Sam Bennett" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3114732.png&w=350&h=254",
      Fighter.Name == "Anders Lee" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/5793.png&w=350&h=254",
      Fighter.Name == "John Ludvig" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4846923.png&w=350&h=254",
      Fighter.Name == "Jonah Gadjovich" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4233890.png&w=350&h=254",
      Fighter.Name == "Josh Manson" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/2590829.png&w=350&h=254",
      Fighter.Name == "Connor Clifton" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3042191.png&w=350&h=254",
      Fighter.Name == "Kiefer Sherwood" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4391255.png&w=350&h=254",
      Fighter.Name == "Brayden Schenn" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/5219.png&w=350&h=254",
      Fighter.Name == "Nick Cousins" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/2563027.png&w=350&h=254",
      Fighter.Name == "Mark Scheifele" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/2562632.png&w=350&h=254",
      Fighter.Name == "Brady Tkachuk" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4319858.png&w=350&h=254",
      Fighter.Name == "Jacob Trouba" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/2976839.png&w=350&h=254",
      Fighter.Name == "Tyler Tucker" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4587904.png&w=350&h=254",
      Fighter.Name == "Connor Dewar" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4588004.png&w=350&h=254",
      Fighter.Name == "A.J. Greer" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3648015.png&w=350&h=254",
      Fighter.Name == "Jakub Lauko" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4391789.png&w=350&h=254",
      Fighter.Name == "Erik Johnson" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3649.png&w=350&h=254",
      Fighter.Name == "Simon Benoit" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4392642.png&w=350&h=254",
      Fighter.Name == "Michael McCarron" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3042048.png&w=350&h=254",
      Fighter.Name == "Adam Lowry" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/2563066.png&w=350&h=254",
      Fighter.Name == "Matt Martin" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/5050.png&w=350&h=254",
      Fighter.Name == "Alex Vlasic" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4565266.png&w=350&h=254",
      Fighter.Name == "J.T. Miller" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/2590852.png&w=350&h=254",
      Fighter.Name == "Joshua Brown" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3042228.png&w=350&h=254",
      Fighter.Name == "Michael Pezzetta" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4392215.png&w=350&h=254",
      Fighter.Name == "Brenden Dillon" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/2554903.png&w=350&h=254",
      Fighter.Name == "Josh Anderson" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3069687.png&w=350&h=254",
      Fighter.Name == "Jake McCabe" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3020803.png&w=350&h=254",
      Fighter.Name == "Jack McBain" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4755695.png&w=350&h=254",
      Fighter.Name == "Will Borgen" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3941946.png&w=350&h=254",
      Fighter.Name == "Dakota Joshua" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4588638.png&w=350&h=254",
      Fighter.Name == "Matt Rempe" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4697820.png&w=350&h=254",
      Fighter.Name == "Parker Wotherspoon" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3942638.png&w=350&h=254",
      Fighter.Name == "Michael Kesselring" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4331604.png&w=350&h=254",
      Fighter.Name == "Brayden Pachal" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4392769.png&w=350&h=254",
      Fighter.Name == "Kurtis MacDermid" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/2992930.png&w=350&h=254",
      Fighter.Name == "Ryan Reaves" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3683.png&w=350&h=254",
      Fighter.Name == "Brendan Lemieux" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3114983.png&w=350&h=254",
      Fighter.Name == "Jason Zucker" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/2593315.png&w=350&h=254",
      Fighter.Name == "Corey Perry" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/2273.png&w=350&h=254",
      Fighter.Name == "Darnell Nurse" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3041997.png&w=350&h=254",
      Fighter.Name == "Erik Gudbranson" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/5503.png&w=350&h=254",
      Fighter.Name == "Ty Dellandrea" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4352747.png&w=350&h=254",
      Fighter.Name == "Luke Schenn" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/5092.png&w=350&h=254",
      Fighter.Name == "Kevin Bahl" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4378686.png&w=350&h=254",
      Fighter.Name == "Zack MacEwan" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4063569.png&w=350&h=254",
      Fighter.Name == "Logan Stanley" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4024968.png&w=350&h=254",
      Fighter.Name == "Joel Farabee" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4352750.png&w=350&h=254",
      Fighter.Name == "Zemgus Girgensons" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/2968829.png&w=350&h=254",
      Fighter.Name == "Dylan McIlrath" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/2529256.png&w=350&h=254",
      Fighter.Name == "Nathan Bastian" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4063279.png&w=350&h=254",
      Fighter.Name == "Lawson Crouse" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3899951.png&w=350&h=254",
      Fighter.Name == "Brendan Smith" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4973.png&w=350&h=254",
      Fighter.Name == "Johnathan Kovacevic" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4587933.png&w=350&h=254",
      Fighter.Name == "Martin Pospisil" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4392883.png&w=350&h=254",
      Fighter.Name == "Ryan Lomberg" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3942548.png&w=350&h=254",
      Fighter.Name == "Ryan Strome" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/2562636.png&w=350&h=254",
      Fighter.Name == "Emil Lilleberg" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/5146599.png&w=350&h=254",
      Fighter.Name == "Erik Cernak" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3904178.png&w=350&h=254",
      Fighter.Name == "Zachary L'Heureux" ~
        "https://assets.nhle.com/mugs/nhl/20242025/NSH/8482742.png",
      Fighter.Name == "Dylan Cozens" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4565228.png&w=350&h=254",
      Fighter.Name == "Max Domi" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3042014.png&w=350&h=254",
      Fighter.Name == "Connor Murphy" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/2562618.png&w=350&h=254",
      Fighter.Name == "Brandon Tanev" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3096236.png&w=350&h=254",
      Fighter.Name == "Jack Drury" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4378671.png&w=350&h=254",
      Fighter.Name == "Reese Johnson" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4586824.png&w=350&h=254",
      Fighter.Name == "Mathew Dumba" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/2970689.png&w=350&h=254",
      Fighter.Name == "Christian Fischer" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3904170.png&w=350&h=254",
      Fighter.Name == "MacKenzie Entwistle" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4393072.png&w=350&h=254",
      Fighter.Name == "Peyton Krebs" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4565238.png&w=350&h=254",
      Fighter.Name == "Kaiden Guhle" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/4697399.png&w=350&h=254",
      Fighter.Name == "Nathan Walker" ~
        "https://a.espncdn.com/combiner/i?img=/i/headshots/nhl/players/full/3067860.png&w=350&h=254",
      TRUE ~ "NA"))


