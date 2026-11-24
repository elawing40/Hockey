#Creating a traditional RAPM to compare to the THoR Results
library(tidyverse)
library(Matrix)
library(glmnet)

HALOStintsStrengthStatesWide
HALOEventsStrengthStatesStrengthWithGoalDiff

Stint_xG <- HALOEventsStrengthStatesStrengthWithGoalDiff %>%
  filter(!is.na(sl_xg_all_shots), !is.na(game_stint)) %>%
  mutate(side = ifelse(team == home_team, "Home", "Away")) %>%
  group_by(game_id, game_stint, side) %>%
  summarise(xG = sum(sl_xg_all_shots), .groups = "drop") %>%
  pivot_wider(names_from = side, values_from = xG, values_fill = 0, names_glue = "{side}xG")

StintScore <- HALOEventsStrengthStatesStrengthWithGoalDiff %>%
  arrange(game_id, period, period_time, sl_event_id) %>%
  group_by(game_id, game_stint) %>%
  summarise(HomeTeamScoreDiff = first(HomeTeamScoreDiff), .groups = "drop")

HALOStintsForRAPM <- HALOStintsStrengthStatesWide %>%
  ungroup() %>%
  filter(StrengthState != "Uneven Strength State") %>%
  mutate(Duration = period_time_end - period_time_start) %>%
  filter(Duration > 0) %>%
  left_join(Stint_xG, by = c("game_id", "game_stint")) %>%
  left_join(StintScore, by = c("game_id", "game_stint")) %>%
  arrange(game_id, game_stint) %>%
  group_by(game_id) %>%
  fill(HomeTeamScoreDiff, .direction = "downup") %>%
  ungroup() %>%
  mutate(
    HomexG = replace_na(HomexG, 0),
    AwayxG = replace_na(AwayxG, 0),
    HomeTeamScoreDiff = replace_na(HomeTeamScoreDiff, 0L),
    is4on4 = as.integer(StrengthState == "4on4"),
    is3on3 = as.integer(StrengthState == "3on3")
  )


stopifnot(!anyDuplicated(HALOStintsForRAPM[c("game_id", "game_stint")]))

home_cols <- paste0("HomePlayer", 1:5)
away_cols <- paste0("AwayPlayer", 1:5)  

BuildStintRAPMMatrix <- function(stints, player_ids, home_offense = TRUE){
  all_cols <- c(paste0(player_ids, "_O"), paste0(player_ids, "_D"))
  col_index <- setNames(seq_along(all_cols), all_cols)
  
  off_cols <- if (home_offense) home_cols else away_cols
  def_cols <- if (home_offense) away_cols else home_cols
  
  make_triplets <- function(cols, suffix){
    lapply(cols, function(cc){
      nm <- paste0(stints[[cc]], suffix)
      valid <- !is.na(stints[[cc]]) & nm %in% all_cols
      list(rows = which(valid), cols = unname(col_index[nm[valid]]))
    })
  }
  trip <- c(make_triplets(off_cols, "_O"), make_triplets(def_cols, "_D"))
  
  sp <- sparseMatrix(
    i = unlist(lapply(trip, `[[`, "rows")),
    j = unlist(lapply(trip, `[[`, "cols")),
    x = 1, dims = c(nrow(stints), length(all_cols)),
    dimnames = list(NULL, all_cols)
  )
  
  sd <- if (home_offense) stints$HomeTeamScoreDiff else -stints$HomeTeamScoreDiff
  other <- cbind(
    isHome = as.numeric(home_offense),
    is4on4 = stints$is4on4,
    is3on3 = stints$is3on3,
    ScoreDiffUp3Plus = as.numeric(sd >= 3),
    ScoreDiffUp2 = as.numeric(sd == 2),
    ScoreDiffUp1 = as.numeric(sd == 1),
    ScoreDiffDown1 = as.numeric(sd == -1),
    ScoreDiffDown2 = as.numeric(sd == -2),
    ScoreDiffDown3Plus = as.numeric(sd <= -3)
  )
  cbind(sp, other)
}

RAPMMatrixHome <- BuildStintRAPMMatrix(HALOStintsForRAPM, HALOPlayersIDs, home_offense = TRUE)
RAPMMatrixAway <- BuildStintRAPMMatrix(HALOStintsForRAPM, HALOPlayersIDs, home_offense = FALSE)
RAPMMatrixFull <- rbind(RAPMMatrixHome, RAPMMatrixAway)

xGPer60Full <- c(HALOStintsForRAPM$HomexG/HALOStintsForRAPM$Duration * 3600, 
                 HALOStintsForRAPM$AwayxG/HALOStintsForRAPM$Duration * 3600)

WeightsFull <- c(HALOStintsForRAPM$Duration, HALOStintsForRAPM$Duration)

pen <- c(rep(1, 2 * length(HALOPlayersIDs)), rep(0, 9))

TradRAPM_CV <- cv.glmnet(RAPMMatrixFull, xGPer60Full, weights = WeightsFull, alpha = 0, nfolds = 10,
                         standardize = FALSE, penalty.factor = pen, parallel = TRUE)

TradRAPM_GLM <- glmnet(RAPMMatrixFull, xGPer60Full, weights = WeightsFull, alpha = 0,
                       lambda = TradRAPM_CV$lambda.1se, standardize = FALSE, penalty.factor = pen)

HALO_RAPM_Traditional_Coefficients <- data.frame(
  column_name = rownames(TradRAPM_GLM$beta),
  coefficient = as.vector(TradRAPM_GLM$beta)
) %>%
  filter(grepl("_O$|_D$", column_name)) %>%
  mutate(player_id = gsub("_O$|_D$", "", column_name),
         role = ifelse(grepl("_O$", column_name), "Offense", "Defense")) %>%
  select(player_id, role, coefficient) %>%
  pivot_wider(names_from = role, values_from = coefficient, names_glue = "RAPM_{role}") %>%
  left_join(HALOPlayers %>% select(player_id, player_name, position_group), by = "player_id") %>%
  mutate(OffenseRank = rank(desc(RAPM_Offense)),
         DefenseRank = rank(RAPM_Defense)) %>%
  relocate(player_id, player_name, position_group, RAPM_Offense, RAPM_Defense) %>%
  arrange(OffenseRank)
  
  
HALO_RAPM_Constant_THoR <- HALO_RAPM_Traditional_Coefficients %>%
  left_join(HALO_RAPM_Constant_Coefficients, by = "player_id") %>%
  left_join(HALO_RAPM_Exponential_Coefficients_3Players, by = "player_id")

cor.test(HALO_RAPM_Constant_THoR$OffenseRank.x, HALO_RAPM_Constant_THoR$OffenseRank.y, method = "kendall")
cor.test(HALO_RAPM_Constant_THoR$DefenseRank.x, HALO_RAPM_Constant_THoR$DefenseRank.y, method = "kendall")

write.csv(HALO_RAPM_Constant_THoR, "~/Desktop/HALO_RAPM_Constant_THoR.csv", row.names = FALSE)

  