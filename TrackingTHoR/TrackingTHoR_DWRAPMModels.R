#Design Weighted RAPM

#Need to make a new function that uses distances and assigns ranks to players

HALOEventsWithLineupForRAPM #dataset with all tracking IDs and distances
#need to line up the player IDs witht he tracking IDs and distances to the event

ReorderLineupToTracking <- function(df, side = c("Home", "Away")) {
  side <- match.arg(side)
  pc <- paste0(side, "Player", 1:5)
  tc <- paste0(side, "Player", 1:5, "TrackingID")
  dc <- paste0(side, "Player", 1:5, "DistanceToEvent")
  pm <- as.matrix(df[pc]); tm <- as.matrix(df[tc]); dm <- as.matrix(df[dc])
  
  for (i in seq_len(nrow(df))) {
    lineup <- pm[i, ]; lineup <- lineup[!is.na(lineup)]
    out <- rep(NA_character_, 5)
    ok  <- !is.na(tm[i, ]) & tm[i, ] %in% lineup    # tracked AND on the ice
    out[ok] <- tm[i, ok]
    dm[i, !ok] <- NA                                # no valid distance for these slots
    rest <- setdiff(lineup, out[ok])                # on ice but untracked
    free <- which(!ok)
    out[free[seq_along(rest)]] <- rest
    pm[i, ] <- out
  }
  df[pc] <- as.data.frame(pm, stringsAsFactors = FALSE)
  df[dc] <- as.data.frame(dm)
  df
}

HALOEventsWithLineupForRAPM_Ordered <- HALOEventsWithLineupForRAPM %>%
  ReorderLineupToTracking("Home") %>%
  ReorderLineupToTracking("Away")

data.frame(before = na_before, after = na_after, added = na_after - na_before)

set_check <- function(before, after, side) {
  cols <- paste0(side, "Player", 1:5)
  b <- as.matrix(before[cols]); a <- as.matrix(after[cols])
  mean(vapply(seq_len(nrow(b)), function(i) setequal(na.omit(b[i, ]), na.omit(a[i, ])), logical(1)))
}
set_check(HALOEventsWithLineupForRAPM, HALOEventsWithLineupForRAPM_Ordered, "Home")
set_check(HALOEventsWithLineupForRAPM, HALOEventsWithLineupForRAPM_Ordered, "Away")

cols <- c(paste0("HomePlayer",1:5), paste0("AwayPlayer",1:5))
na_before <- sapply(cols, function(c) sum(is.na(HALOEventsWithLineupForRAPM[[c]])))
na_after  <- sapply(cols, function(c) sum(is.na(HALOEventsWithLineupForRAPM_Ordered[[c]])))
data.frame(before = na_before, after = na_after, added = na_after - na_before)

mean(!is.na(as.matrix(HALOEventsWithLineupForRAPM_Ordered[paste0("HomePlayer", 1:5, "DistanceToEvent")])))

HomeMatch <- with(HALOEventsWithLineupForRAPM_Ordered,
                  (HomePlayer1 == HomePlayer1TrackingID | is.na(HomePlayer1) | is.na(HomePlayer1TrackingID)) &
                    (HomePlayer2 == HomePlayer2TrackingID | is.na(HomePlayer2) | is.na(HomePlayer2TrackingID)) &
                    (HomePlayer3 == HomePlayer3TrackingID | is.na(HomePlayer3) | is.na(HomePlayer3TrackingID)) &
                    (HomePlayer4 == HomePlayer4TrackingID | is.na(HomePlayer4) | is.na(HomePlayer4TrackingID)) &
                    (HomePlayer5 == HomePlayer5TrackingID | is.na(HomePlayer5) | is.na(HomePlayer5TrackingID)))
table(HomeMatch)

AwayMatch <- with(HALOEventsWithLineupForRAPM_Ordered,
                  (AwayPlayer1 == AwayPlayer1TrackingID | is.na(AwayPlayer1) | is.na(AwayPlayer1TrackingID)) &
                    (AwayPlayer2 == AwayPlayer2TrackingID | is.na(AwayPlayer2) | is.na(AwayPlayer2TrackingID)) &
                    (AwayPlayer3 == AwayPlayer3TrackingID | is.na(AwayPlayer3) | is.na(AwayPlayer3TrackingID)) &
                    (AwayPlayer4 == AwayPlayer4TrackingID | is.na(AwayPlayer4) | is.na(AwayPlayer4TrackingID)) &
                    (AwayPlayer5 == AwayPlayer5TrackingID | is.na(AwayPlayer5) | is.na(AwayPlayer5TrackingID)))
table(AwayMatch)

na_before <- sapply(c(paste0("HomePlayer",1:5), paste0("AwayPlayer",1:5)),
                    function(c) sum(is.na(HALOEventsWithLineupForRAPM[[c]])))
na_after  <- sapply(c(paste0("HomePlayer",1:5), paste0("AwayPlayer",1:5)),
                    function(c) sum(is.na(HALOEventsWithLineupForRAPM_Ordered[[c]])))
data.frame(before = na_before, after = na_after, added = na_after - na_before)

# Rows where at least one on-ice player was lost
lost_home <- rowSums(is.na(HALOEventsWithLineupForRAPM_Ordered[paste0("HomePlayer",1:5)])) >
  rowSums(is.na(HALOEventsWithLineupForRAPM[paste0("HomePlayer",1:5)]))
lost_away <- rowSums(is.na(HALOEventsWithLineupForRAPM_Ordered[paste0("AwayPlayer",1:5)])) >
  rowSums(is.na(HALOEventsWithLineupForRAPM[paste0("AwayPlayer",1:5)]))
mean(lost_home); mean(lost_away)


check_side <- function(df, side) {
  pl <- as.matrix(df[paste0(side, "Player", 1:5)])
  tr <- as.matrix(df[paste0(side, "Player", 1:5, "TrackingID")])
  tracked   <- !is.na(tr)
  in_lineup <- t(vapply(seq_len(nrow(df)), function(i) tr[i, ] %in% pl[i, ], logical(5)))
  c(tracked_slots = sum(tracked),
    in_lineup     = sum(in_lineup & tracked),
    share         = sum(in_lineup & tracked) / sum(tracked))
}
check_side(HALOEventsWithLineupForRAPM, "Home")
check_side(HALOEventsWithLineupForRAPM, "Away")
#About 98% of rows have valid skaters between tracking and stints dataframes

slot_ok <- function(df, side) {
  ok <- sapply(1:5, function(k) {
    p <- df[[paste0(side, "Player", k)]]
    t <- df[[paste0(side, "Player", k, "TrackingID")]]
    d <- df[[paste0(side, "Player", k, "DistanceToEvent")]]
    is.na(d) | (!is.na(p) & p == t)
  })
  table(rowSums(!ok) == 0)
}
slot_ok(HALOEventsWithLineupForRAPM_Ordered, "Home")
slot_ok(HALOEventsWithLineupForRAPM_Ordered, "Away")

#Did not end up making a stepwise function, felt exponential made more sense
# BuildHomeRAPMMatrixStepWise <- function(events_df, player_ids) {
#   
#   all_cols  <- c(paste0(player_ids, "_O"), paste0(player_ids, "_D"))
#   col_index <- setNames(seq_along(all_cols), all_cols)
#   
#   home_player_cols <- c("HomePlayer1", "HomePlayer2", "HomePlayer3",
#                         "HomePlayer4", "HomePlayer5")
#   away_player_cols <- c("AwayPlayer1", "AwayPlayer2", "AwayPlayer3",
#                         "AwayPlayer4", "AwayPlayer5")
#   home_dist_cols <- paste0(home_player_cols, "DistanceToEvent")
#   away_dist_cols <- paste0(away_player_cols, "DistanceToEvent")
#   
#   slot_weights <- c(1.75, 1.5, 1.25, 1, 0.75)  # rank 1 (closest) -> rank 5
#   
#   home_triplets <- lapply(seq_along(home_player_cols), function(idx) {
#     player_col <- home_player_cols[idx]
#     dist_col   <- home_dist_cols[idx]
#     player_col_names <- paste0(events_df[[player_col]], "_O")
#     valid  <- !is.na(events_df[[player_col]]) & player_col_names %in% all_cols
#     weight <- ifelse(is.na(events_df[[dist_col]]), 0.5, slot_weights[idx])
#     list(
#       rows = which(valid),
#       cols = unname(col_index[player_col_names[valid]]),
#       x    = weight[valid]
#     )
#   })
#   
#   away_triplets <- lapply(seq_along(away_player_cols), function(idx) {
#     player_col <- away_player_cols[idx]
#     dist_col   <- away_dist_cols[idx]
#     player_col_names <- paste0(events_df[[player_col]], "_D")
#     valid  <- !is.na(events_df[[player_col]]) & player_col_names %in% all_cols
#     weight <- ifelse(is.na(events_df[[dist_col]]), 0.5, slot_weights[idx])
#     list(
#       rows = which(valid),
#       cols = unname(col_index[player_col_names[valid]]),
#       x    = weight[valid]
#     )
#   })
#   
#   all_triplets <- c(home_triplets, away_triplets)
#   row_indices  <- unlist(lapply(all_triplets, `[[`, "rows"))
#   col_indices  <- unlist(lapply(all_triplets, `[[`, "cols"))
#   x_values     <- unlist(lapply(all_triplets, `[[`, "x"))
#   
#   sparse_matrix <- sparseMatrix(
#     i = row_indices,
#     j = col_indices,
#     x = x_values,
#     dims = c(nrow(events_df), length(all_cols)),
#     dimnames = list(NULL, all_cols)
#   )
#   
#   OtherRAPMVariablesDF <- events_df %>%
#     mutate(
#       ScoreDiffUp3Plus = ifelse(HomeTeamScoreDiff == 3, 1, 0),
#       ScoreDiffUp2 = ifelse(HomeTeamScoreDiff == 2, 1, 0),
#       ScoreDiffUp1 = ifelse(HomeTeamScoreDiff == 1, 1, 0),
#       ScoreDiffDown1 = ifelse(HomeTeamScoreDiff == -1, 1, 0),
#       ScoreDiffDown2 = ifelse(HomeTeamScoreDiff == -2, 1, 0),
#       ScoreDiffDown3Plus = ifelse(HomeTeamScoreDiff == -3, 1, 0)
#     ) %>%
#     select(isHome, is4on4, is3on3, ScoreDiffUp3Plus, ScoreDiffUp2, ScoreDiffUp1,
#            ScoreDiffDown1, ScoreDiffDown2, ScoreDiffDown3Plus)
#   
#   OtherRAPMVariablesMatrix <- as.matrix(OtherRAPMVariablesDF)
#   FullRAPMMatrix <- cbind(sparse_matrix, OtherRAPMVariablesMatrix)
#   return(FullRAPMMatrix)
# }
# 
# 
# BuildAwayRAPMMatrixStepWise <- function(events_df, player_ids) {
#   
#   all_cols  <- c(paste0(player_ids, "_O"), paste0(player_ids, "_D"))
#   col_index <- setNames(seq_along(all_cols), all_cols)
#   
#   home_player_cols <- c("HomePlayer1", "HomePlayer2", "HomePlayer3",
#                         "HomePlayer4", "HomePlayer5")
#   away_player_cols <- c("AwayPlayer1", "AwayPlayer2", "AwayPlayer3",
#                         "AwayPlayer4", "AwayPlayer5")
#   home_dist_cols <- paste0(home_player_cols, "DistanceToEvent")
#   away_dist_cols <- paste0(away_player_cols, "DistanceToEvent")
#   
#   slot_weights <- c(1.75, 1.5, 1.25, 1, 0.75)  # rank 1 (closest) -> rank 5
#   
#   home_triplets <- lapply(seq_along(home_player_cols), function(idx) {
#     player_col <- home_player_cols[idx]
#     dist_col   <- home_dist_cols[idx]
#     player_col_names <- paste0(events_df[[player_col]], "_D")
#     valid  <- !is.na(events_df[[player_col]]) & player_col_names %in% all_cols
#     weight <- ifelse(is.na(events_df[[dist_col]]), 0.5, slot_weights[idx])
#     list(
#       rows = which(valid),
#       cols = unname(col_index[player_col_names[valid]]),
#       x    = weight[valid]
#     )
#   })
#   
#   away_triplets <- lapply(seq_along(away_player_cols), function(idx) {
#     player_col <- away_player_cols[idx]
#     dist_col   <- away_dist_cols[idx]
#     player_col_names <- paste0(events_df[[player_col]], "_O")
#     valid  <- !is.na(events_df[[player_col]]) & player_col_names %in% all_cols
#     weight <- ifelse(is.na(events_df[[dist_col]]), 0.5, slot_weights[idx])
#     list(
#       rows = which(valid),
#       cols = unname(col_index[player_col_names[valid]]),
#       x    = weight[valid]
#     )
#   })
#   
#   all_triplets <- c(home_triplets, away_triplets)
#   row_indices  <- unlist(lapply(all_triplets, `[[`, "rows"))
#   col_indices  <- unlist(lapply(all_triplets, `[[`, "cols"))
#   x_values     <- unlist(lapply(all_triplets, `[[`, "x"))
#   
#   sparse_matrix <- sparseMatrix(
#     i = row_indices,
#     j = col_indices,
#     x = x_values,
#     dims = c(nrow(events_df), length(all_cols)),
#     dimnames = list(NULL, all_cols)
#   )
#   
#   OtherRAPMVariablesDF <- events_df %>%
#     mutate(
#       ScoreDiffUp3Plus = ifelse(HomeTeamScoreDiff == -3, 1, 0),
#       ScoreDiffUp2 = ifelse(HomeTeamScoreDiff == -2, 1, 0),
#       ScoreDiffUp1 = ifelse(HomeTeamScoreDiff == -1, 1, 0),
#       ScoreDiffDown1 = ifelse(HomeTeamScoreDiff == 1, 1, 0),
#       ScoreDiffDown2 = ifelse(HomeTeamScoreDiff == 2, 1, 0),
#       ScoreDiffDown3Plus = ifelse(HomeTeamScoreDiff == 3, 1, 0)
#     ) %>%
#     select(isHome, is4on4, is3on3, ScoreDiffUp3Plus, ScoreDiffUp2, ScoreDiffUp1, 
#            ScoreDiffDown1, ScoreDiffDown2, ScoreDiffDown3Plus)
#   
#   OtherRAPMVariablesMatrix <- as.matrix(OtherRAPMVariablesDF)
#   FullRAPMMatrix <- cbind(sparse_matrix, OtherRAPMVariablesMatrix)
#   return(FullRAPMMatrix)
# }
# 
# HALOEventsWithLineupForRAPMHomeEvents_3Players <- HALOEventsWithLineupForRAPM_Ordered %>%
#   filter(isHomeEvent == 1)
# HALOEventsWithLineupForRAPMAwayEvents_3Players <- HALOEventsWithLineupForRAPM_Ordered %>%
#   filter(isHomeEvent == 0)
# 
# HALOStepwiseRAPMMatrixHome_3Players <- BuildHomeRAPMMatrixStepWise(HALOEventsWithLineupForRAPMHomeEvents_3Players, HALOPlayersIDs)
# HALOStepwiseRAPMMatrixAway_3Players <- BuildAwayRAPMMatrixStepWise(HALOEventsWithLineupForRAPMAwayEvents_3Players, HALOPlayersIDs)
# 
# P <- HALOStepwiseRAPMMatrixHome_3Players[, 1:(2 * length(HALOPlayersIDs))]
# table(Matrix::rowSums(P != 0))
# summary(Matrix::rowSums(P)) 
# 
# HALOStepwiseRAPMMatrixFull_3Players <- rbind(HALOStepwiseRAPMMatrixHome_3Players, HALOStepwiseRAPMMatrixAway_3Players)
# 
# HALOStepwiseRAPMWeights_3Players <- rep(1, times = nrow(HALOStepwiseRAPMMatrixFull_3Players))
# HALOGoalProbabilites_3Players <- c(HALOEventsWithLineupForRAPMHomeEvents_3Players$goal_prob_raw,HALOEventsWithLineupForRAPMAwayEvents_3Players$goal_prob_raw)
# 
# HALORAPMStepwiseCV_3Players <- cv.glmnet(x = HALOStepwiseRAPMMatrixFull_3Players, y = HALOGoalProbabilites_3Players, weights = HALOStepwiseRAPMWeights_3Players,
#                                          alpha = 0, nfolds = 10, standardize = FALSE, parallel=TRUE )
# 
# HALORAPMStepwiseGLM_3Players <- glmnet(x = HALOStepwiseRAPMMatrixFull_3Players, y = HALOGoalProbabilites_3Players, weights = HALOStepwiseRAPMWeights_3Players,
#                                        alpha = 0, lambda = HALORAPMStepwiseCV_3Players$lambda.1se, standardize = FALSE, parallel=TRUE )
# 
# 
# HALO_RAPM_Stepwise_Coefficients_3Players <- data.frame(
#   column_name = rownames(HALORAPMStepwiseGLM_3Players$beta),
#   coefficient = as.vector(HALORAPMStepwiseGLM_3Players$beta)
# ) %>%
#   filter(grepl("_O$|_D$", column_name)) %>%
#   mutate(
#     player_id = gsub("_O$|_D$", "", column_name),
#     role      = ifelse(grepl("_O$", column_name), "Offense", "Defense")
#   ) %>%
#   select(player_id, role, coefficient) %>%
#   pivot_wider(
#     names_from  = role,
#     values_from = coefficient,
#     names_glue  = "RAPM_{role}"
#   ) %>%
#   left_join(
#     HALOPlayers %>% select(player_id, player_name, position_group),
#     by = "player_id"
#   ) %>%
#   mutate(OffenseRank = rank(desc(RAPM_Offense)),
#          DefenseRank = rank((RAPM_Defense))) %>%
#   relocate(player_id, player_name, position_group, 
#            RAPM_Offense, RAPM_Defense) %>%
#   arrange((OffenseRank))


#Exponential Decay Design Weight

quantile(unlist(HALOEventsWithLineupForRAPM_Ordered[paste0("HomePlayer", 1:5, "DistanceToEvent")]),
         c(.1, .25, .5, .75, .9), na.rm = TRUE)

d_all <- unlist(HALOEventsWithLineupForRAPM_Ordered[c(paste0("HomePlayer", 1:5, "DistanceToEvent"),
                                                      paste0("AwayPlayer", 1:5, "DistanceToEvent"))])
sapply(c(10, 15, 25, 40), function(l) mean(exp(-d_all / l), na.rm = TRUE))

BuildRAPMMatrixExponentialCore <- function(events_df, player_ids,
                                           home_suffix, away_suffix, score_sign,
                                           lambda, untracked_w,
                                           normalize_to = NULL) {
  
  all_cols  <- c(paste0(player_ids, "_O"), paste0(player_ids, "_D"))
  col_index <- setNames(seq_along(all_cols), all_cols)
  
  home_player_cols <- paste0("HomePlayer", 1:5)
  away_player_cols <- paste0("AwayPlayer", 1:5)
  home_dist_cols   <- paste0(home_player_cols, "DistanceToEvent")
  away_dist_cols   <- paste0(away_player_cols, "DistanceToEvent")
  
  make_triplets <- function(player_cols, dist_cols, suffix) {
    lapply(seq_along(player_cols), function(idx) {
      p <- events_df[[player_cols[idx]]]
      d <- events_df[[dist_cols[idx]]]
      col_names <- paste0(p, suffix)
      valid  <- !is.na(p) & col_names %in% all_cols
      weight <- ifelse(is.na(d), untracked_w, exp(-d / lambda))
      list(
        rows = which(valid),
        cols = unname(col_index[col_names[valid]]),
        x    = weight[valid]
      )
    })
  }
  
  all_triplets <- c(make_triplets(home_player_cols, home_dist_cols, home_suffix),
                    make_triplets(away_player_cols, away_dist_cols, away_suffix))
  
  sparse_matrix <- sparseMatrix(
    i = unlist(lapply(all_triplets, `[[`, "rows")),
    j = unlist(lapply(all_triplets, `[[`, "cols")),
    x = unlist(lapply(all_triplets, `[[`, "x")),
    dims = c(nrow(events_df), length(all_cols)),
    dimnames = list(NULL, all_cols)
  )
  

  if (!is.null(normalize_to)) {
    rs <- Matrix::rowSums(sparse_matrix)
    scale_vec <- ifelse(rs > 0, normalize_to / rs, 1)
    sparse_matrix <- Matrix::Diagonal(x = scale_vec) %*% sparse_matrix
    colnames(sparse_matrix) <- all_cols
  }
  
  s <- score_sign * events_df$HomeTeamScoreDiff
  OtherRAPMVariablesMatrix <- cbind(
    isHome             = events_df$isHome,
    is4on4             = events_df$is4on4,
    is3on3             = events_df$is3on3,
    ScoreDiffUp3Plus   = as.numeric(s == 3),
    ScoreDiffUp2       = as.numeric(s == 2),
    ScoreDiffUp1       = as.numeric(s == 1),
    ScoreDiffDown1     = as.numeric(s == -1),
    ScoreDiffDown2     = as.numeric(s == -2),
    ScoreDiffDown3Plus = as.numeric(s == -3)
  )
  
  cbind(sparse_matrix, OtherRAPMVariablesMatrix)
}

# Home events: home skaters are offense (_O), away skaters are defense (_D)
BuildHomeRAPMMatrixExponential <- function(events_df, player_ids, lambda, untracked_w,
                                           normalize_to = NULL) {
  BuildRAPMMatrixExponentialCore(events_df, player_ids,
                                 home_suffix = "_O", away_suffix = "_D", score_sign = 1,
                                 lambda = lambda, untracked_w = untracked_w,
                                 normalize_to = normalize_to)
}

# Away events: home skaters are defense (_D), away skaters are offense (_O),
# and the score diff is flipped to the away team's perspective
BuildAwayRAPMMatrixExponential <- function(events_df, player_ids, lambda, untracked_w,
                                           normalize_to = NULL) {
  BuildRAPMMatrixExponentialCore(events_df, player_ids,
                                 home_suffix = "_D", away_suffix = "_O", score_sign = -1,
                                 lambda = lambda, untracked_w = untracked_w,
                                 normalize_to = normalize_to)
}

HALOExponentialRAPMMatrixHome_3Players <- HALOEventsWithLineupForRAPM_Ordered %>% filter(isHomeEvent == 1)
HALOExponentialRAPMMatrixAway_3Players <- HALOEventsWithLineupForRAPM_Ordered %>% filter(isHomeEvent == 0)

lam <- 15
HALOExponentialRAPMMatrixFull_3Players <- rbind(
  BuildHomeRAPMMatrixExponential(HALOExponentialRAPMMatrixHome_3Players, HALOPlayersIDs, lambda = lam,
                                 untracked_w = untracked_by_lambda[as.character(lam)]),
  BuildAwayRAPMMatrixExponential(HALOExponentialRAPMMatrixAway_3Players, HALOPlayersIDs, lambda = lam,
                                 untracked_w = untracked_by_lambda[as.character(lam)])
)
HALOGoalProbabilites_3Players <- c(HALOExponentialRAPMMatrixHome_3Players$goal_prob_raw, HALOExponentialRAPMMatrixAway_3Players$goal_prob_raw)

# sanity checks
P <- HALOExponentialRAPMMatrixFull_3Players[, 1:(2 * length(HALOPlayersIDs))]
table(Matrix::rowSums(P != 0))      # mostly 10 nonzero entries
summary(Matrix::rowSums(P))         # spread of total weight per row

HALORAPMExponentialCV_3Players <- cv.glmnet(x = HALOExponentialRAPMMatrixFull_3Players, y = HALOGoalProbabilites_3Players, alpha = 0, nfolds = 5, standardize = FALSE)

HALORAPMExponentialGLM_3Players <- glmnet(x = HALOExponentialRAPMMatrixFull_3Players, y = HALOGoalProbabilites_3Players,
                                          alpha = 0, lambda = HALORAPMExponentialCV_3Players$lambda.1se, standardize = FALSE)


HALO_RAPM_Exponential_Coefficients_3Players <- data.frame(
  column_name = rownames(HALORAPMExponentialGLM_3Players$beta),
  coefficient = as.vector(HALORAPMExponentialGLM_3Players$beta)
) %>%
  filter(grepl("_O$|_D$", column_name)) %>%
  mutate(
    player_id = gsub("_O$|_D$", "", column_name),
    role      = ifelse(grepl("_O$", column_name), "Offense", "Defense")
  ) %>%
  select(player_id, role, coefficient) %>%
  pivot_wider(
    names_from  = role,
    values_from = coefficient,
    names_glue  = "THoR_{role}"
  ) %>%
  left_join(
    HALOPlayers %>% select(player_id, player_name, position_group),
    by = "player_id"
  ) %>%
  mutate(OffenseRank = rank(desc(THoR_Offense)),
         DefenseRank = rank((THoR_Defense))) %>%
  relocate(player_id, player_name, position_group, 
           THoR_Offense, THoR_Defense) %>%
  arrange((OffenseRank))

HALO_ExpDWTHoR_and_THoR_Coeffs <- HALO_RAPM_Constant_Coefficients %>%
  left_join(HALO_RAPM_Exponential_Coefficients_3Players %>% select(-player_name, -position_group), by = "player_id") %>%
  rename(
    RAPM_Offense_Constant = RAPM_Offense.x,
    RAPM_Defense_Constant = RAPM_Defense.x,
    OffenseRank_Constant = OffenseRank.x,
    DefenseRank_Constant = DefenseRank.x,
    RAPM_Offense_Exp = RAPM_Offense.y,
    RAPM_Defense_Exp = RAPM_Defense.y,
    OffenseRank_Exp = OffenseRank.y,
    DefenseRank_Exp = DefenseRank.y
  )

cor(HALO_ExpDWTHoR_and_THoR_Coeffs$OffenseRank_Constant, HALO_ExpDWTHoR_and_THoR_Coeffs$OffenseRank_Exp, method = "spearman")
cor(HALO_ExpDWTHoR_and_THoR_Coeffs$DefenseRank_Constant, HALO_ExpDWTHoR_and_THoR_Coeffs$DefenseRank_Exp, method = "spearman")
