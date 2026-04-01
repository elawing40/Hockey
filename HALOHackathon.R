#HALO Hackathon
library(arrow)
library(gpboost)
library(SHAPforxgboost)
library(tidyverse)
library(parallel)
library(ggplot2)
library(viridis)
library(pROC)
library(gt)




# ahl_teams_2023_24 <- data.frame(
#   abbreviation = c(
#     "ABB", "BAK", "BEL", "BRI", "CAL", "CLT", "CHI", "CLE",
#     "CVF", "COL", "GR", "HFD", "HEN", "HER", "IA", "LAV",
#     "LV", "MB", "MIL", "ONT", "PRO", "RCH", "RFD", "SD",
#     "SJ", "SPR", "SYC", "TEX", "TOR", "TUC", "UTC", "WBS"
#   ),
#   city = c(
#     "Abbotsford", "Bakersfield", "Belleville", "Bridgeport", "Calgary", 
#     "Charlotte", "Chicago", "Cleveland", "Coachella Valley", "Colorado",
#     "Grand Rapids", "Hartford", "Henderson", "Hershey", "Iowa", "Laval",
#     "Lehigh Valley", "Manitoba", "Milwaukee", "Ontario", "Providence", 
#     "Rochester", "Rockford", "San Diego", "San Jose", "Springfield", 
#     "Syracuse", "Texas", "Toronto", "Tucson", "Utica", "Wilkes-Barre/Scranton"
#   ),
#   team_name = c(
#     "Canucks", "Condors", "Senators", "Islanders", "Wranglers",
#     "Checkers", "Wolves", "Monsters", "Firebirds", "Eagles",
#     "Griffins", "Wolf Pack", "Silver Knights", "Bears", "Wild", "Rocket",
#     "Phantoms", "Moose", "Admirals", "Reign", "Bruins",
#     "Americans", "IceHogs", "Gulls", "Barracuda", "Thunderbirds",
#     "Crunch", "Stars", "Marlies", "Roadrunners", "Comets", "Penguins"
#   ),
#   stringsAsFactors = FALSE
# )

# HALOTracking <- HALOTracking %>%
#   left_join(ahl_teams_2023_24, by = c("team_name" = "team_name"))

#want to make a metric to evaluate forecheck pressure

ShowHALOTrackingPlay <- function(gameid, play_id, df = HALOTracking){
  play_to_show <- df %>%
    filter(game_id == gameid & sl_event_id == play_id)
  nhl_rink_plot() + theme_void() +
    geom_point(data = play_to_show, aes(x = tracking_x, y = tracking_y, color = team_name), size = 1)
    
}
library(patchwork)
ShowHALOTrackingPlay("3a80a960-49d1-3367-292b-b24372bc746d", 2803, HALOTracking) / ShowHALOTrackingPlay("3a80a960-49d1-3367-292b-b24372bc746d", 2805, HALOTracking)

ShowHALOTrackingPlay("00b0366a-95c6-5250-2dae-e3dd5c4198bc", 14, HALOTracking)

#Load in all SportLogiq data for HALO Hackathon
HALOEvents <- read_parquet("~/Desktop/HALO Hackathon/events.parquet")
HALOTracking <- read_parquet("~/Desktop/HALO Hackathon/tracking.parquet")
HALOStints <- read_parquet("~/Desktop/HALO Hackathon/stints.parquet")
HALOPlayers <- read_parquet("~/Desktop/HALO Hackathon/players.parquet")
HALOGames <- read_parquet("~/Desktop/HALO Hackathon/games.parquet")

#Filter to loose puck recoveries only
HALOEventsLPRsOnly <- HALOEvents %>%
  filter(event_type == "lpr")

#Filter to only dump-ins
HALOEventsDumpInLPRsOnly <- HALOEventsLPRsOnly %>%
  filter(grepl("DUMP", description))

#Filter to only high pressure dump-ins with tracking data
HALOEventsHiPressDumpInLPRsOnlyWithTracking <- HALOEventsDumpInLPRsOnly %>%
  filter(grepl("hipres", detail) & has_tracking_data == 1)


#Join forechcecking events with the tracking data by game ID and event ID, calculate distance, speed, and speed towards event for each player
#Filter out goalies
#Summarize the dataset to one row per forechecking opportunity with all necessary features to describe the event
HALOTrackingForecheckingEvents <- HALOEventsHiPressDumpInLPRsOnlyWithTracking %>%
  left_join(HALOTracking, by = c("game_id", "sl_event_id")) %>%
  left_join(HALOPlayers %>% select(player_id, primary_position), 
            by = c("player_id.y" = "player_id")) %>%
  mutate(
    DistanceToEvent = sqrt((tracking_x - x)^2 + (tracking_y - y)^2),
    PlayerSpeed = sqrt(tracking_vel_x^2 + tracking_vel_y^2),
    SpeedTowardsEvent = (tracking_vel_x * (x - tracking_x) +
                           tracking_vel_y * (y - tracking_y)) / 
      (DistanceToEvent + 0.00001),
    IsForechecker = team_id.x != team_id.y,
    IsRetriever = team_id.x == team_id.y
  ) %>%
  filter(
    primary_position != "G" | is.na(primary_position),
    !(IsRetriever == TRUE & tracking_x >= -95.2 & tracking_x <= -82 & 
        tracking_y >= -7.5 & tracking_y <= 8.5),
    !(IsRetriever == TRUE & tracking_x >= 82 & tracking_x <= 95.6 & 
        tracking_y >= -7.5 & tracking_y <= 8.5),
    PlayerSpeed < 1000
  ) %>%
  group_by(game_id, sl_event_id) %>%
  summarise(
    event_x = first(x),
    event_y = first(y),
    outcome = first(outcome),
    flags = first(flags),
    f1_distance_to_event = {
      dists <- DistanceToEvent[IsForechecker == TRUE]
      if(length(dists) > 0) min(dists, na.rm = TRUE) else NA_real_
    },
    f1_speed = {
      if(any(IsForechecker == TRUE)) {
        PlayerSpeed[IsForechecker == TRUE][which.min(DistanceToEvent[IsForechecker == TRUE])]
      } else NA_real_
    },
    f1_speed_toward_event = {
      if(any(IsForechecker == TRUE)) {
        SpeedTowardsEvent[IsForechecker == TRUE][which.min(DistanceToEvent[IsForechecker == TRUE])]
      } else NA_real_
    },
    f1_player_id = {
      if(any(IsForechecker == TRUE)) {
        player_id.y[IsForechecker == TRUE][which.min(DistanceToEvent[IsForechecker == TRUE])]
      } else NA_character_
    },
    f2_distance_to_event = {
      dists <- sort(DistanceToEvent[IsForechecker == TRUE])
      if(length(dists) >= 2) dists[2] else NA_real_
    },
    f2_speed = {
      speeds <- PlayerSpeed[IsForechecker == TRUE]
      dists <- DistanceToEvent[IsForechecker == TRUE]
      if(length(dists) >= 2) speeds[order(dists)[2]] else NA_real_
    },
    f2_speed_to_event = {
      speeds <- SpeedTowardsEvent[IsForechecker == TRUE]
      dists <- DistanceToEvent[IsForechecker == TRUE]
      if(length(dists) >= 2) speeds[order(dists)[2]] else NA_real_
    },
    f3_distance_to_event = {
      dists <- sort(DistanceToEvent[IsForechecker == TRUE])
      if(length(dists) >= 3) dists[3] else NA_real_
    },
    f3_speed = {
      speeds <- PlayerSpeed[IsForechecker == TRUE]
      dists <- DistanceToEvent[IsForechecker == TRUE]
      if(length(dists) >= 3) speeds[order(dists)[3]] else NA_real_
    },
    f3_speed_to_event = {
      speeds <- SpeedTowardsEvent[IsForechecker == TRUE]
      dists <- DistanceToEvent[IsForechecker == TRUE]
      if(length(dists) >= 3) speeds[order(dists)[3]] else NA_real_
    },
    forechecker_x_centroid = {
      forechecker_x <- tracking_x[IsForechecker == TRUE]
      if(length(forechecker_x) >= 1) sum(forechecker_x)/length(forechecker_x) else NA_real_
    },
    forechecker_y_centroid = {
      forechecker_y <- tracking_y[IsForechecker == TRUE]
      if(length(forechecker_y) >= 1) sum(forechecker_y)/length(forechecker_y) else NA_real_
    },
    forechecker_spread = {
      forechecker_distances <- DistanceToEvent[IsForechecker == TRUE]
      if(length(forechecker_distances) >= 1) sd(forechecker_distances) else NA_real_
    },
    f1_f2_depth_gap = {
      forechecker_dists <- DistanceToEvent[IsForechecker == TRUE]
      forechecker_xs <- tracking_x[IsForechecker == TRUE]
      if (length(forechecker_dists) >= 2) {
        sorted_indices <- order(forechecker_dists)[1:2]
        abs(forechecker_xs[sorted_indices[1]]-forechecker_xs[sorted_indices[2]])
      } else NA_real_
    },
    f1_f2_distance_gap = {
      forechecker_dists <- DistanceToEvent[IsForechecker == TRUE]
      forechecker_xs <- tracking_x[IsForechecker == TRUE]
      forechecker_ys <- tracking_y[IsForechecker == TRUE]
      if (length(forechecker_dists) >= 2) {
        sorted_indices <- order(forechecker_dists)[1:2]
        sqrt((forechecker_xs[sorted_indices[1]] - forechecker_xs[sorted_indices[2]])^2 + 
               (forechecker_ys[sorted_indices[1]] - forechecker_ys[sorted_indices[2]])^2)
      } else NA_real_
    },
    retriever1_distance_to_event = {
      dists <- DistanceToEvent[IsRetriever == TRUE]
      if(length(dists) > 0) min(dists, na.rm = TRUE) else NA_real_
    },
    retriever1_speed = {
      if(any(IsRetriever == TRUE)) {
        PlayerSpeed[IsRetriever == TRUE][which.min(DistanceToEvent[IsRetriever == TRUE])]
      } else NA_real_
    },
    retriever1_speed_toward_event = {
      if(any(IsRetriever == TRUE)) {
        SpeedTowardsEvent[IsRetriever == TRUE][which.min(DistanceToEvent[IsRetriever == TRUE])]
      } else NA_real_
    },
    retriever2_distance_to_event = {
      dists <- sort(DistanceToEvent[IsRetriever == TRUE])
      if(length(dists) >= 2) dists[2] else NA_real_
    },
    retriever2_speed = {
      speeds <- PlayerSpeed[IsRetriever == TRUE]
      dists <- DistanceToEvent[IsRetriever == TRUE]
      if(length(dists) >= 2) speeds[order(dists)[2]] else NA_real_
    },
    retriever_support_distance = {
      retriever_dists <- DistanceToEvent[IsRetriever == TRUE]
      retriever_xs <- tracking_x[IsRetriever == TRUE]
      retriever_ys <- tracking_y[IsRetriever == TRUE]
      if(length(retriever_dists) >= 2) {
        sorted_indices <- order(retriever_dists)[1:2]
        r1_x <- retriever_xs[sorted_indices[1]]
        r1_y <- retriever_ys[sorted_indices[1]]
        r2_x <- retriever_xs[sorted_indices[2]]
        r2_y <- retriever_ys[sorted_indices[2]]
        sqrt((r1_x - r2_x)^2 + (r1_y - r2_y)^2)
      } else {
        NA_real_
      }
    },
    .groups = "drop"
  ) %>%
  mutate(
    f1_to_r1_distance_ratio = f1_distance_to_event / retriever1_distance_to_event,
    f1_to_r1_speed_ratio = f1_speed / retriever1_speed,
    r1_to_f1_distance = retriever1_distance_to_event-f1_distance_to_event,
    f1_vs_f2_speed = f1_speed-f2_speed
  )

#One hot encode the dataframe
HALOTrackingForecheckingEventsOneHot <- HALOTrackingForecheckingEvents %>%
  mutate(
    outcome_successful_forecheck = ifelse(outcome == "failed", 1, 0),
    flag_alongboards = ifelse(flags == "alongboards", 1, 0)
  ) %>%
  select(-outcome, -flags)

#Determine players that have at least 10 forechecking opportunities to create a better sample
players_to_keep <- HALOTrackingForecheckingEventsOneHot %>%
  group_by(f1_player_id) %>%
  tally() %>%
  filter(n >= 10) %>%
  pull(f1_player_id)

#Filter to only players with 10 forechceking opportunities
HALO_Filtered <- HALOTrackingForecheckingEventsOneHot %>%
  filter(f1_player_id %in% players_to_keep)

#Create a matrix of event locations
forecheck_event_coord_matrix_filt <- as.matrix(HALO_Filtered[,c("event_x","event_y")])
#Create a vector of all F! player IDs
forechecker_ids_vec_filt <- as.character(HALO_Filtered[["f1_player_id"]])

#Get the seed (or set seed)
get_seed() #seed is 10407

#Create the GPModel using bernoulli_logit since we want a binary output
forechecker_gpmodel <- GPModel(
  group_data = forechecker_ids_vec_filt,
  gp_coords = forecheck_event_coord_matrix_filt,
  likelihood = "bernoulli_logit", 
  cov_function = "exponential"
)

#Create the GPBoost dataset and take out all columns that will not be used as features
forechecking_gpboost_data <- gpb.Dataset(
  data = as.matrix(HALO_Filtered %>%
                     select(-game_id, -sl_event_id, -event_x, -event_y, 
                            -outcome_successful_forecheck, -f1_player_id)),
  label = as.numeric(HALO_Filtered[["outcome_successful_forecheck"]]))

#Set parameters for the GPModel
forecheck_gpmodel_params <- list(learning_rate = 0.05, max_depth = 10, num_leaves = 31,
                                 min_data_in_leaf = 20, lambda_l2 = 0)

#Use 3-Fold cross-validation to determine the best iteration
forechecker_gpbcv_model <- gpb.cv(
    params = forecheck_gpmodel_params,
    data = forechecking_gpboost_data,
    gp_model = forechecker_gpmodel,
    nrounds = 50,
    nfold = 3,
    verbose = 1,
    early_stopping_rounds = 10,
    nthread = detectCores() - 1)

#Seave best forechecker iteration
best_forechecker_model <- forechecker_gpbcv_model$best_iter

#Train the final GPModel setting the nrounds to the best iteration
forechecker_final_model <- gpb.train(
  params = forecheck_gpmodel_params,
  data = forechecking_gpboost_data,
  gp_model = forechecker_gpmodel,
  nrounds = best_forechecker_model,
  verbose = 1,
  nthread = detectCores() - 1
)

#Create a matrix for the predictions
forecheck_predict_matrix <- as.matrix(HALO_Filtered %>%
                               select(-game_id, -sl_event_id, -event_x, -event_y, 
                                      -outcome_successful_forecheck, -f1_player_id))

#Make predictions
forecheck_predictions <- predict(forechecker_final_model, data = forecheck_predict_matrix,
                                 gp_coords_pred = forecheck_event_coord_matrix_filt,
                                 group_data_pred = forechecker_ids_vec_filt,
                                 predict_var = TRUE)


#Predicted probability of each forecheck
HALO_Filtered$predicted_prob <- forecheck_predictions$response_mean
#Actual outcome of the forecheck
HALO_Filtered$actual <- HALO_Filtered$outcome_successful_forecheck
#Residuals
HALO_Filtered$residual <- HALO_Filtered$actual - HALO_Filtered$predicted_prob

#Summarize the results for player evaluation
fore_checker_player_rankings <- HALO_Filtered %>%
  group_by(f1_player_id) %>%
  summarize(
    attempts = n(),
    recoveries = sum(actual),
    xRecoveries = sum(predicted_prob),
    RAE = recoveries - xRecoveries,
    RAE_per_attempt = RAE / attempts,
    player_effect = mean(residual),
    success_rate = mean(actual)
  ) %>%
  arrange(desc(RAE)) %>%
  left_join(HALOPlayers, by = c("f1_player_id" = "player_id"))

#Calculate area under curve
forechecker_roc_curve <- roc(HALO_Filtered$actual, HALO_Filtered$predicted_prob)
auc(xReb_roc_curve)

#Calculate the accuracy of the model
forecheck_pred_labels <- ifelse(HALO_Filtered$predicted_prob >= 0.5, 1, 0)
forecheck_accuracy <- sum(forecheck_pred_labels == HALO_Filtered$actual) / length(HALO_Filtered$actual)
print(paste("Accuracy of the best model: ", forecheck_accuracy))

#Create importance plot
forechecker_feature_importances <- gpb.importance(forechecker_final_model, percentage = TRUE)
gpb.plot.importance(forechecker_feature_importances, top_n = 25, measure = "Gain", 
                    main = "Variable importances when including \ncoordinates in the fixed effects")


library(akima)
library(tidyverse)

#Create a heatmap of the forecheck success probability
HALO_Filtered$predicted_prob <- forecheck_predictions$response_mean

nhl_rink_plot() +
  stat_summary_2d(
    data = HALO_Filtered,
    aes(x = event_x, y = event_y, z = predicted_prob),
    fun = mean,
    bins = 30,
    alpha = 0.7
  ) +
  scale_fill_viridis(
    option = "plasma",
    name = "Forecheck\nSuccess\nProbability"
  ) +
  labs(
    title = "Forecheck Success Probability Heatmap",
    x = NULL, 
    y = NULL  
  ) +
  theme_void() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.position = "right"
  )


#Create GT table for the paper
fore_checker_player_rankings %>%
  select(player_name, primary_position, attempts, recoveries, xRecoveries, RAE, RAE_per_attempt, success_rate) %>%
  filter(!is.na(player_name)) %>%
  arrange(desc(RAE)) %>%
  gt() |>
    cols_label(
    player_name = "Player",
    primary_position = "Position",
    attempts = "Attempts",
    recoveries = "Recoveries",
    RAE = "Recoveries Above Expected",
    RAE_per_attempt = "RAE Per Attempt",
    success_rate = "Sucess Rate"
  ) |>
  fmt_number(
    columns = c(xRecoveries, RAE, RAE_per_attempt, success_rate),
    decimals = 3 
  ) |>
  tab_header(
      title = md("**Recoveries Above Expected on Forecheckes Leaderboard**"),
      subtitle = md("2023-24 AHL Season")
  ) |>
  cols_align(
    align = "center",
    columns = everything()
  )
  