#Checking if the weighting actually does anything

# ---------------------------------------------------------------
# 0. Setup: game IDs, response, folds
# ---------------------------------------------------------------
HALOEventsWithLineupForRAPM_Ordered$game_id <- HALOEventsWithLineupWithEventTypeSliced$game_id
stopifnot(identical(HALOEventsWithLineupForRAPM_Ordered$isHomeEvent, HALOEventsWithLineupWithEventTypeSliced$isHomeEvent),
          isTRUE(all.equal(HALOEventsWithLineupForRAPM_Ordered$goal_prob_raw, HALOEventsWithLineupWithEventTypeSliced$goal_prob_raw)))

HALOExponentialRAPMMatrixHome_3Players <- HALOEventsWithLineupForRAPM_Ordered %>% filter(isHomeEvent == 1)
HALOExponentialRAPMMatrixAway_3Players <- HALOEventsWithLineupForRAPM_Ordered %>% filter(isHomeEvent == 0)

HALOGoalProbabilites_3Players <- c(HALOExponentialRAPMMatrixHome_3Players$goal_prob_raw,
                                   HALOExponentialRAPMMatrixAway_3Players$goal_prob_raw)

set.seed(03032002)
HALORAPMGameIDs_3Players <- c(HALOExponentialRAPMMatrixHome_3Players$game_id,
                              HALOExponentialRAPMMatrixAway_3Players$game_id)
HALORAPMUniqueGames_3Players <- unique(HALORAPMGameIDs_3Players)
HALORAPMGameFold_3Players <- setNames(sample(rep(1:5, length.out = length(HALORAPMUniqueGames_3Players))),
                                      HALORAPMUniqueGames_3Players)
HALORAPMFoldID_3Players <- unname(HALORAPMGameFold_3Players[HALORAPMGameIDs_3Players])
table(HALORAPMFoldID_3Players)

n_players_cols <- 2 * length(HALOPlayersIDs)

# decay lambda grid and the matching untracked weight (mean tracked weight at that decay lambda)
lambda_grid <- c(10, 15, 25, 40)
d_all <- unlist(HALOEventsWithLineupForRAPM_Ordered[c(paste0("HomePlayer", 1:5, "DistanceToEvent"),
                                                      paste0("AwayPlayer", 1:5, "DistanceToEvent"))])
untracked_by_lambda <- setNames(sapply(lambda_grid, function(l) mean(exp(-d_all / l), na.rm = TRUE)),
                                as.character(lambda_grid))
untracked_by_lambda
rm(d_all)

lam <- 15

# ---------------------------------------------------------------
# 1. Helper functions
# ---------------------------------------------------------------
HALORAPMSkaterCount_3Players <- function(df) {
  rowSums(!is.na(as.matrix(df[c(paste0("HomePlayer", 1:5), paste0("AwayPlayer", 1:5))])))
}

# normalize = TRUE rescales each row's player weights to sum to that row's skater count
HALORAPMBuildX_3Players <- function(home_df, away_df, lam, untracked_w, normalize = FALSE) {
  n_home <- if (normalize) HALORAPMSkaterCount_3Players(home_df) else NULL
  n_away <- if (normalize) HALORAPMSkaterCount_3Players(away_df) else NULL
  rbind(
    BuildHomeRAPMMatrixExponential(home_df, HALOPlayersIDs, lambda = lam,
                                   untracked_w = untracked_w, normalize_to = n_home),
    BuildAwayRAPMMatrixExponential(away_df, HALOPlayersIDs, lambda = lam,
                                   untracked_w = untracked_w, normalize_to = n_away)
  )
}

# Grouped-by-game CV over an explicit, wide penalty sequence (no early stopping).
# penalty_factor = 0 leaves a column unpenalized.
HALORAPMGroupedCV_3Players <- function(x, y, foldid, lambda_seq = NULL, nlambda = 40,
                                       min_ratio = 1e-8, penalty_factor = NULL) {
  pf <- if (is.null(penalty_factor)) rep(1, ncol(x)) else penalty_factor
  if (is.null(lambda_seq)) {
    full <- glmnet(x, y, alpha = 0, standardize = FALSE, penalty.factor = pf)
    lmax <- max(full$lambda)
    lambda_seq <- exp(seq(log(lmax), log(lmax * min_ratio), length.out = nlambda))
  }
  nf  <- length(unique(foldid))
  mse <- matrix(NA_real_, nf, length(lambda_seq))
  for (k in seq_len(nf)) {
    tr   <- foldid != k
    fit  <- glmnet(x[tr, ], y[tr], alpha = 0, lambda = lambda_seq,
                   standardize = FALSE, penalty.factor = pf)
    pred <- predict(fit, x[!tr, ])
    mse[k, ] <- colMeans((y[!tr] - pred)^2)
  }
  mean_by_lambda <- colMeans(mse)
  se_by_lambda   <- apply(mse, 2, sd) / sqrt(nf)
  best     <- which.min(mean_by_lambda)
  best_1se <- min(which(mean_by_lambda <= mean_by_lambda[best] + se_by_lambda[best]))
  list(lambda = lambda_seq, mse = mse, best = best, best_1se = best_1se,
       best_lambda = lambda_seq[best], lambda_1se = lambda_seq[best_1se],
       mean_mse = mean_by_lambda[best], fold_mse = mse[, best],
       edge = best %in% c(1, length(lambda_seq)))
}

# ---------------------------------------------------------------
# 2. Main comparison: null vs covariates vs constant vs exponential
# ---------------------------------------------------------------
# constant weights = every player weighted 1 (exp(-d/Inf) = 1, untracked = 1)
HALOConstantRAPMMatrixFull_3Players <- HALORAPMBuildX_3Players(
  HALOExponentialRAPMMatrixHome_3Players, HALOExponentialRAPMMatrixAway_3Players,
  lam = Inf, untracked_w = 1)

HALOExponentialRAPMMatrixFull_3Players <- HALORAPMBuildX_3Players(
  HALOExponentialRAPMMatrixHome_3Players, HALOExponentialRAPMMatrixAway_3Players,
  lam = lam, untracked_w = untracked_by_lambda[as.character(lam)])

HALOCovariatesOnlyMatrix_3Players <- HALOExponentialRAPMMatrixFull_3Players[, (n_players_cols + 1):ncol(HALOExponentialRAPMMatrixFull_3Players)]

# sanity checks
P <- HALOExponentialRAPMMatrixFull_3Players[, 1:n_players_cols]
table(Matrix::rowSums(P != 0))
summary(Matrix::rowSums(P))
rm(P)

system.time(
  HALORAPMCVExp_3Players <- HALORAPMGroupedCV_3Players(HALOExponentialRAPMMatrixFull_3Players, HALOGoalProbabilites_3Players, HALORAPMFoldID_3Players)
)
HALORAPMCVConstant_3Players   <- HALORAPMGroupedCV_3Players(HALOConstantRAPMMatrixFull_3Players, HALOGoalProbabilites_3Players, HALORAPMFoldID_3Players)
HALORAPMCVCovariates_3Players <- HALORAPMGroupedCV_3Players(HALOCovariatesOnlyMatrix_3Players, HALOGoalProbabilites_3Players, HALORAPMFoldID_3Players)

# intercept-only baseline per fold
HALORAPMCVNull_3Players <- sapply(sort(unique(HALORAPMFoldID_3Players)), function(k) {
  tr <- HALORAPMFoldID_3Players != k
  mean((HALOGoalProbabilites_3Players[!tr] - mean(HALOGoalProbabilites_3Players[tr]))^2)
})

HALORAPMCVComparison_3Players <- tibble(
  fold        = 1:5,
  null        = HALORAPMCVNull_3Players,
  covariates  = HALORAPMCVCovariates_3Players$fold_mse,
  constant    = HALORAPMCVConstant_3Players$fold_mse,
  exponential = HALORAPMCVExp_3Players$fold_mse
) %>%
  mutate(player_gain_constant = covariates - constant,
         player_gain_exp      = covariates - exponential,
         exp_minus_constant   = constant - exponential)   # positive = exponential predicts better

HALORAPMCVComparison_3Players
c(mean = mean(HALORAPMCVComparison_3Players$exp_minus_constant),
  se   = sd(HALORAPMCVComparison_3Players$exp_minus_constant) / sqrt(5),
  folds_exp_better = sum(HALORAPMCVComparison_3Players$exp_minus_constant > 0))

# edge = TRUE means the best penalty was an end of the grid; lower min_ratio (e.g. 1e-10) and rerun
c(exp_edge = HALORAPMCVExp_3Players$edge, const_edge = HALORAPMCVConstant_3Players$edge,
  cov_edge = HALORAPMCVCovariates_3Players$edge)

# shared penalty sequence for the placebos
HALORAPMPenaltySeq_3Players <- HALORAPMCVExp_3Players$lambda

# ---------------------------------------------------------------
# 3. Decay lambda sweep (dose-response)
# ---------------------------------------------------------------
HALORAPMLambdaSweep_3Players <- bind_rows(lapply(lambda_grid, function(l) {
  Xl <- HALORAPMBuildX_3Players(HALOExponentialRAPMMatrixHome_3Players, HALOExponentialRAPMMatrixAway_3Players,
                                lam = l, untracked_w = untracked_by_lambda[as.character(l)])
  cvl <- HALORAPMGroupedCV_3Players(Xl, HALOGoalProbabilites_3Players, HALORAPMFoldID_3Players)
  rm(Xl); gc()
  tibble(decay_lambda = l, fold = 1:5, mse = cvl$fold_mse, edge = cvl$edge)
})) %>%
  left_join(HALORAPMCVComparison_3Players %>% select(fold, constant), by = "fold") %>%
  mutate(gain_vs_constant = constant - mse)

HALORAPMLambdaSweep_3Players %>%
  group_by(decay_lambda) %>%
  summarise(mean_gain = mean(gain_vs_constant), se = sd(gain_vs_constant) / sqrt(n()),
            folds_better = sum(gain_vs_constant > 0), any_edge = any(edge))

# ---------------------------------------------------------------
# 4. Controls for the row-total proxy
# ---------------------------------------------------------------
# Control 1: row-normalized exponential vs constant
HALOExponentialRAPMMatrixFullNorm_3Players <- HALORAPMBuildX_3Players(
  HALOExponentialRAPMMatrixHome_3Players, HALOExponentialRAPMMatrixAway_3Players,
  lam = lam, untracked_w = untracked_by_lambda[as.character(lam)], normalize = TRUE)

summary(Matrix::rowSums(HALOExponentialRAPMMatrixFullNorm_3Players[, 1:n_players_cols]))  # should be 10 / 8 / 6

HALORAPMCVExpNorm_3Players <- HALORAPMGroupedCV_3Players(
  HALOExponentialRAPMMatrixFullNorm_3Players, HALOGoalProbabilites_3Players, HALORAPMFoldID_3Players)

HALORAPMCVNormComparison_3Players <- tibble(
  fold = 1:5,
  constant = HALORAPMCVConstant_3Players$fold_mse,
  exponential_norm = HALORAPMCVExpNorm_3Players$fold_mse,
  norm_minus_constant = constant - exponential_norm
)
HALORAPMCVNormComparison_3Players
HALORAPMCVExpNorm_3Players$edge

# Control 2: give every model the row total as an unpenalized covariate
HALORAPMRowTotal_3Players <- as.numeric(Matrix::rowSums(HALOExponentialRAPMMatrixFull_3Players[, 1:n_players_cols]))

HALOExponentialRAPMMatrixFullRT_3Players <- cbind(HALOExponentialRAPMMatrixFull_3Players, rowTotal = HALORAPMRowTotal_3Players)
HALOConstantRAPMMatrixFullRT_3Players    <- cbind(HALOConstantRAPMMatrixFull_3Players,    rowTotal = HALORAPMRowTotal_3Players)
HALOCovariatesOnlyMatrixRT_3Players      <- cbind(HALOCovariatesOnlyMatrix_3Players,      rowTotal = HALORAPMRowTotal_3Players)

HALORAPMPenaltyFactorRT_3Players <- c(rep(1, n_players_cols),
                                      rep(0, ncol(HALOExponentialRAPMMatrixFullRT_3Players) - n_players_cols))

HALORAPMCVExpRT_3Players <- HALORAPMGroupedCV_3Players(
  HALOExponentialRAPMMatrixFullRT_3Players, HALOGoalProbabilites_3Players, HALORAPMFoldID_3Players,
  penalty_factor = HALORAPMPenaltyFactorRT_3Players)
HALORAPMCVConstantRT_3Players <- HALORAPMGroupedCV_3Players(
  HALOConstantRAPMMatrixFullRT_3Players, HALOGoalProbabilites_3Players, HALORAPMFoldID_3Players,
  penalty_factor = HALORAPMPenaltyFactorRT_3Players)
HALORAPMCVCovariatesRT_3Players <- local({
  xd <- as.matrix(HALOCovariatesOnlyMatrixRT_3Players)   # only ~10 columns, safe to make dense
  y  <- HALOGoalProbabilites_3Players
  fold_mse <- sapply(sort(unique(HALORAPMFoldID_3Players)), function(k) {
    tr  <- HALORAPMFoldID_3Players != k
    fit <- lm.fit(cbind(1, xd[tr, ]), y[tr])
    b   <- fit$coefficients
    b[is.na(b)] <- 0                                      # guards against any collinear column
    mean((y[!tr] - cbind(1, xd[!tr, ]) %*% b)^2)
  })
  list(fold_mse = fold_mse, mean_mse = mean(fold_mse), edge = FALSE)
})
HALORAPMCVRTComparison_3Players <- tibble(
  fold = 1:5,
  covariates_rt = HALORAPMCVCovariatesRT_3Players$fold_mse,
  constant_rt = HALORAPMCVConstantRT_3Players$fold_mse,
  exponential_rt = HALORAPMCVExpRT_3Players$fold_mse,
  exp_minus_constant_rt = constant_rt - exponential_rt
)
HALORAPMCVRTComparison_3Players
c(exp_rt_edge = HALORAPMCVExpRT_3Players$edge, const_rt_edge = HALORAPMCVConstantRT_3Players$edge)

# free memory before the placebos
rm(HALOExponentialRAPMMatrixFullRT_3Players, HALOConstantRAPMMatrixFullRT_3Players,
   HALOCovariatesOnlyMatrixRT_3Players, HALOExponentialRAPMMatrixFullNorm_3Players); gc()

sort(sapply(ls(), function(x) object.size(get(x))), decreasing = TRUE)[1:15] / 1e9   # GB
# ---------------------------------------------------------------
# 5. Placebos: global shuffle and within-row shuffle
# ---------------------------------------------------------------
# Global: swaps distances between events (same side, same strength state)
HALORAPMShuffleDistances_3Players <- function(df, side) {
  cols <- paste0(side, "Player", 1:5, "DistanceToEvent")
  m <- as.matrix(df[cols])
  grp <- paste(df$is4on4, df$is3on3)
  for (g in unique(grp)) {
    r <- which(grp == g)
    sub <- m[r, , drop = FALSE]
    ok <- which(!is.na(sub))
    sub[ok] <- sub[sample(ok)]
    m[r, ] <- sub
  }
  df[cols] <- as.data.frame(m)
  df
}

# Within-row: permutes tracked distances among the tracked players of the same event.
# Row total, tracked pattern and distance distribution are unchanged; only who gets which distance changes.
HALORAPMShuffleWithinRow_3Players <- function(df, side) {
  cols <- paste0(side, "Player", 1:5, "DistanceToEvent")
  m    <- as.matrix(df[cols])
  ok   <- !is.na(m)
  idx  <- which(ok, arr.ind = TRUE)
  vals <- m[ok]
  pos_order  <- order(idx[, "row"], idx[, "col"])
  shuf_order <- order(idx[, "row"], runif(nrow(idx)))
  new_vals <- numeric(length(vals))
  new_vals[pos_order] <- vals[shuf_order]
  m[ok] <- new_vals
  df[cols] <- as.data.frame(m)
  df
}

HALORAPMRunPlacebo_3Players <- function(shuffle_fn, B, lam, lambda_seq, file) {
  results <- vector("list", B)
  for (b in seq_len(B)) {
    home_b <- HALOExponentialRAPMMatrixHome_3Players %>% shuffle_fn("Home") %>% shuffle_fn("Away")
    away_b <- HALOExponentialRAPMMatrixAway_3Players %>% shuffle_fn("Home") %>% shuffle_fn("Away")
    Xb  <- HALORAPMBuildX_3Players(home_b, away_b, lam = lam, untracked_w = untracked_by_lambda[as.character(lam)])
    cvb <- HALORAPMGroupedCV_3Players(Xb, HALOGoalProbabilites_3Players, HALORAPMFoldID_3Players, lambda_seq = lambda_seq)
    results[[b]] <- tibble(rep = b, fold = 1:5, mse = cvb$fold_mse, edge = cvb$edge)
    rm(Xb, home_b, away_b); gc()
    saveRDS(results, file)   # survives a crash
  }
  bind_rows(results)
}

HALORAPMPlaceboCompare_3Players <- function(placebo_df, real_mse, constant_mse) {
  by_rep <- placebo_df %>% group_by(rep) %>% summarise(mean_mse = mean(mse), .groups = "drop")
  tibble(real = real_mse, constant = constant_mse,
         placebo_mean = mean(by_rep$mean_mse), placebo_sd = sd(by_rep$mean_mse),
         real_gap_in_sd = (mean(by_rep$mean_mse) - real_mse) / sd(by_rep$mean_mse),
         any_edge = any(placebo_df$edge),
         p_value = (1 + sum(by_rep$mean_mse <= real_mse)) / (1 + nrow(by_rep)))
}

B <- 20   # timing test first; raise to 20 once you know the runtime

set.seed(03032002)
system.time(
  HALORAPMPlaceboGlobal_3Players <- HALORAPMRunPlacebo_3Players(
    HALORAPMShuffleDistances_3Players, B = B, lam = lam,
    lambda_seq = HALORAPMPenaltySeq_3Players, file = "placebo_global.rds")
)

set.seed(03032002)
HALORAPMPlaceboWithinRow_3Players <- HALORAPMRunPlacebo_3Players(
  HALORAPMShuffleWithinRow_3Players, B = B, lam = lam,
  lambda_seq = HALORAPMPenaltySeq_3Players, file = "placebo_within_row.rds")

HALORAPMPlaceboCompare_3Players(HALORAPMPlaceboGlobal_3Players,    HALORAPMCVExp_3Players$mean_mse, HALORAPMCVConstant_3Players$mean_mse)
HALORAPMPlaceboCompare_3Players(HALORAPMPlaceboWithinRow_3Players, HALORAPMCVExp_3Players$mean_mse, HALORAPMCVConstant_3Players$mean_mse)
#20 of 20 placebos were worse

HALORAPMPlaceboGainShare_3Players <- function(placebo_df, real_mse, constant_mse) {
  by_rep <- placebo_df %>% group_by(rep) %>% summarise(mean_mse = mean(mse), .groups = "drop")
  tibble(real_gain          = constant_mse - real_mse,
         placebo_gain_mean  = constant_mse - mean(by_rep$mean_mse),
         placebo_gain_max   = constant_mse - min(by_rep$mean_mse),
         share_retained     = (constant_mse - mean(by_rep$mean_mse)) / (constant_mse - real_mse),
         reps_beating_real  = sum(by_rep$mean_mse <= real_mse))
}
options(pillar.sigfig = 5)
HALORAPMPlaceboGainShare_3Players(HALORAPMPlaceboGlobal_3Players,    HALORAPMCVExp_3Players$mean_mse, HALORAPMCVConstant_3Players$mean_mse)
HALORAPMPlaceboGainShare_3Players(HALORAPMPlaceboWithinRow_3Players, HALORAPMCVExp_3Players$mean_mse, HALORAPMCVConstant_3Players$mean_mse)

c(const_best = HALORAPMCVConstant_3Players$best, const_1se = HALORAPMCVConstant_3Players$best_1se,
  exp_best   = HALORAPMCVExp_3Players$best,      exp_1se   = HALORAPMCVExp_3Players$best_1se)

HALORAPMPairedOneSE_3Players <- function(cv) {
  nf        <- nrow(cv$mse)
  diff_mat  <- sweep(cv$mse, 1, cv$mse[, cv$best])      # fold MSE minus fold MSE at the best penalty
  mean_diff <- colMeans(diff_mat)
  se_diff   <- apply(diff_mat, 2, sd) / sqrt(nf)
  idx       <- min(which(mean_diff <= se_diff))
  list(index = idx, lambda = cv$lambda[idx])
}

# Both models use the CV-minimum penalty (the paired 1-SE rule returned the minimum anyway)
HALORAPMPenaltyExp_3Players   <- HALORAPMCVExp_3Players$best_lambda
HALORAPMPenaltyConst_3Players <- HALORAPMCVConstant_3Players$best_lambda
c(exp = HALORAPMPenaltyExp_3Players, const = HALORAPMPenaltyConst_3Players)

# ---------------------------------------------------------------
# 6. Final fits at each model's own CV-minimum penalty
# ---------------------------------------------------------------
HALORAPMCoefTable_3Players <- function(fit) {
  data.frame(column_name = rownames(fit$beta), coefficient = as.vector(fit$beta)) %>%
    filter(grepl("_O$|_D$", column_name)) %>%
    mutate(player_id = gsub("_O$|_D$", "", column_name),
           role      = ifelse(grepl("_O$", column_name), "Offense", "Defense")) %>%
    select(player_id, role, coefficient) %>%
    pivot_wider(names_from = role, values_from = coefficient, names_glue = "RAPM_{role}") %>%
    left_join(HALOPlayers %>% select(player_id, player_name, position_group), by = "player_id") %>%
    mutate(OffenseRank = rank(desc(RAPM_Offense)), DefenseRank = rank(RAPM_Defense)) %>%
    relocate(player_id, player_name, position_group, RAPM_Offense, RAPM_Defense) %>%
    arrange(OffenseRank)
}

HALORAPMFinalExpFit_3Players <- glmnet(HALOExponentialRAPMMatrixFull_3Players, HALOGoalProbabilites_3Players,
                                       alpha = 0, lambda = HALORAPMPenaltyExp_3Players, standardize = FALSE)
HALORAPMFinalConstFit_3Players <- glmnet(HALOConstantRAPMMatrixFull_3Players, HALOGoalProbabilites_3Players,
                                         alpha = 0, lambda = HALORAPMPenaltyConst_3Players, standardize = FALSE)

HALO_RAPM_Exponential_Coefficients_3Players <- HALORAPMCoefTable_3Players(HALORAPMFinalExpFit_3Players)
HALO_RAPM_Constant_Coefficients_3Players    <- HALORAPMCoefTable_3Players(HALORAPMFinalConstFit_3Players)

HALO_ExpDWTHoR_and_THoR_Coeffs <- HALO_RAPM_Constant_Coefficients_3Players %>%
  left_join(HALO_RAPM_Exponential_Coefficients_3Players %>% select(-player_name, -position_group), by = "player_id") %>%
  rename(
    RAPM_Offense_Constant = RAPM_Offense.x, RAPM_Defense_Constant = RAPM_Defense.x,
    OffenseRank_Constant  = OffenseRank.x,  DefenseRank_Constant  = DefenseRank.x,
    RAPM_Offense_Exp = RAPM_Offense.y, RAPM_Defense_Exp = RAPM_Defense.y,
    OffenseRank_Exp  = OffenseRank.y,  DefenseRank_Exp  = DefenseRank.y
  )

cor(HALO_ExpDWTHoR_and_THoR_Coeffs$OffenseRank_Constant, HALO_ExpDWTHoR_and_THoR_Coeffs$OffenseRank_Exp, method = "spearman")
cor(HALO_ExpDWTHoR_and_THoR_Coeffs$DefenseRank_Constant, HALO_ExpDWTHoR_and_THoR_Coeffs$DefenseRank_Exp, method = "spearman")

# ---------------------------------------------------------------
# 7. Split-half stability (each model at its own penalty)
# ---------------------------------------------------------------
HALORAPMSplitHalf_3Players <- function(x, y, game_ids, penalty, reps = 5, min_events = 300) {
  n_p <- n_players_cols / 2
  events_per_col <- as.numeric(Matrix::colSums(x[, 1:n_players_cols] != 0))
  keep_off <- events_per_col[1:n_p] >= min_events
  keep_def <- events_per_col[(n_p + 1):(2 * n_p)] >= min_events
  games <- unique(game_ids)
  out <- vector("list", reps)
  for (r in seq_len(reps)) {
    in_a <- game_ids %in% sample(games, floor(length(games) / 2))
    b_a  <- as.vector(glmnet(x[in_a, ],  y[in_a],  alpha = 0, lambda = penalty, standardize = FALSE)$beta)[1:n_players_cols]
    b_b  <- as.vector(glmnet(x[!in_a, ], y[!in_a], alpha = 0, lambda = penalty, standardize = FALSE)$beta)[1:n_players_cols]
    out[[r]] <- tibble(rep = r,
                       offense = cor(b_a[1:n_p][keep_off], b_b[1:n_p][keep_off], method = "spearman"),
                       defense = cor(b_a[(n_p + 1):(2 * n_p)][keep_def], b_b[(n_p + 1):(2 * n_p)][keep_def], method = "spearman"))
  }
  bind_rows(out)
}

set.seed(03032002)
HALORAPMSplitHalfExp_3Players <- HALORAPMSplitHalf_3Players(HALOExponentialRAPMMatrixFull_3Players, HALOGoalProbabilites_3Players,
                                                            HALORAPMGameIDs_3Players, HALORAPMPenaltyExp_3Players)
set.seed(03032002)
HALORAPMSplitHalfConst_3Players <- HALORAPMSplitHalf_3Players(HALOConstantRAPMMatrixFull_3Players, HALOGoalProbabilites_3Players,
                                                              HALORAPMGameIDs_3Players, HALORAPMPenaltyConst_3Players)
colMeans(HALORAPMSplitHalfExp_3Players[, -1]); colMeans(HALORAPMSplitHalfConst_3Players[, -1])

set.seed(03032002)
HALORAPMHomeShuf_3Players <- HALOExponentialRAPMMatrixHome_3Players %>%
  HALORAPMShuffleWithinRow_3Players("Home") %>% HALORAPMShuffleWithinRow_3Players("Away")
HALORAPMAwayShuf_3Players <- HALOExponentialRAPMMatrixAway_3Players %>%
  HALORAPMShuffleWithinRow_3Players("Home") %>% HALORAPMShuffleWithinRow_3Players("Away")

HALOExponentialRAPMMatrixFullShuf_3Players <- HALORAPMBuildX_3Players(
  HALORAPMHomeShuf_3Players, HALORAPMAwayShuf_3Players,
  lam = lam, untracked_w = untracked_by_lambda[as.character(lam)])

HALORAPMCVShuf_3Players <- HALORAPMGroupedCV_3Players(
  HALOExponentialRAPMMatrixFullShuf_3Players, HALOGoalProbabilites_3Players, HALORAPMFoldID_3Players,
  lambda_seq = HALORAPMPenaltySeq_3Players)
HALORAPMCVShuf_3Players$edge

set.seed(03032002)
HALORAPMSplitHalfShuf_3Players <- HALORAPMSplitHalf_3Players(
  HALOExponentialRAPMMatrixFullShuf_3Players, HALOGoalProbabilites_3Players,
  HALORAPMGameIDs_3Players, HALORAPMCVShuf_3Players$best_lambda)
colMeans(HALORAPMSplitHalfShuf_3Players[, -1])

rm(HALORAPMHomeShuf_3Players, HALORAPMAwayShuf_3Players, HALOExponentialRAPMMatrixFullShuf_3Players); gc()

HALORAPMPlayerEvents_3Players <- bind_rows(
  tibble(player_id = HALOPlayersIDs,
         events_O = as.numeric(Matrix::colSums(HALOExponentialRAPMMatrixFull_3Players[, 1:length(HALOPlayersIDs)] != 0)),
         events_D = as.numeric(Matrix::colSums(HALOExponentialRAPMMatrixFull_3Players[, (length(HALOPlayersIDs) + 1):n_players_cols] != 0)))
)

HALORAPMMovers_3Players <- HALO_ExpDWTHoR_and_THoR_Coeffs %>%
  left_join(HALORAPMPlayerEvents_3Players, by = "player_id") %>%
  mutate(OffenseRankChange = OffenseRank_Constant - OffenseRank_Exp,   # positive = rated higher by exponential
         DefenseRankChange = DefenseRank_Constant - DefenseRank_Exp)

HALORAPMMovers_3Players %>% arrange(desc(abs(OffenseRankChange))) %>%
  select(player_name, position_group, events_O, OffenseRank_Constant, OffenseRank_Exp, OffenseRankChange) %>% head(25)
HALORAPMMovers_3Players %>% arrange(desc(abs(DefenseRankChange))) %>%
  select(player_name, position_group, events_D, DefenseRank_Constant, DefenseRank_Exp, DefenseRankChange) %>% head(25)


mean(HALO_ExpDWTHoR_and_THoR_Coeffs %>%
       left_join(HALORAPMPlayerEvents_3Players, by = "player_id")  %>%
       pull((events_O)))

mean(HALO_ExpDWTHoR_and_THoR_Coeffs %>%
       left_join(HALORAPMPlayerEvents_3Players, by = "player_id")  %>%
       pull((events_D)))

# 1. Is the shift a position-group shift?
HALORAPMMovers_3Players %>%
  group_by(position_group) %>%
  summarise(n = n(),
            mean_off_change = mean(OffenseRankChange), mean_def_change = mean(DefenseRankChange),
            off_cor_within = cor(OffenseRank_Constant, OffenseRank_Exp, method = "spearman"),
            def_cor_within = cor(DefenseRank_Constant, DefenseRank_Exp, method = "spearman"))

# 2. Do defensemen get systematically lower weights?
d_pos <- HALOEventsWithLineupForRAPM_Ordered %>%
  select(starts_with("HomePlayer"), starts_with("AwayPlayer")) %>%
  select(matches("^(Home|Away)Player[1-5]$|DistanceToEvent$"))
# build a long table of (player_id, distance), join position_group, then:
#   mean(exp(-distance/lam)) by position_group, and share of slots tracked by position_group


#Building RAPM on raw 1/0 Team goal within 12 seconds flag
HALOEventsWithLineupForRawOutomeRAPM <- HALOEventsWithLineupWithEventTypeSliced %>%
  select(isHomeEvent, HomePlayer1, HomePlayer2, HomePlayer3, HomePlayer4, HomePlayer5,
         AwayPlayer1, AwayPlayer2, AwayPlayer3, AwayPlayer4, AwayPlayer5, isHomeOffense,
         HomePlayer1TrackingID, HomePlayer2TrackingID, HomePlayer3TrackingID, HomePlayer4TrackingID, HomePlayer5TrackingID,
         HomePlayer1DistanceToEvent, HomePlayer2DistanceToEvent, HomePlayer3DistanceToEvent, HomePlayer4DistanceToEvent, HomePlayer5DistanceToEvent,
         AwayPlayer1TrackingID, AwayPlayer2TrackingID, AwayPlayer3TrackingID, AwayPlayer4TrackingID, AwayPlayer5TrackingID,
         AwayPlayer1DistanceToEvent, AwayPlayer2DistanceToEvent, AwayPlayer3DistanceToEvent, AwayPlayer4DistanceToEvent, AwayPlayer5DistanceToEvent,
         TeamGoalWithinNext12Sec, isHomeEvent, StrengthState, HomeTeamScoreDiff, ZoneByHomePerpsective, ZoneByAwayPerpsective,
         game_id, sl_event_id) %>%
  mutate(
    isHome = ifelse(isHomeEvent==1, 1, 0),
    is4on4 = coalesce(ifelse(StrengthState == "4on4", 1, 0), 0),
    is3on3 = coalesce(ifelse(StrengthState == "3on3", 1, 0), 0),
    TeamGoalWithinNext12Sec = replace_na(TeamGoalWithinNext12Sec, 0)
  )

HALOEventsWithLineupRawOutcomeRAPMHomeEvents <- HALOEventsWithLineupForRawOutomeRAPM %>%
  filter(isHomeEvent == 1)

HALOEventsWithLineupRawOutcomeRAPMAwayEvents <- HALOEventsWithLineupForRawOutomeRAPM %>%
  filter(isHomeEvent == 0)

HALOConstantRawOutcomeRAPMMatrixHome <- BuildHomeRAPMMatrixConstant(HALOEventsWithLineupRawOutcomeRAPMHomeEvents, HALOPlayersIDs)
HALOConstantRawOutcomeRAPMMatrixAway <- BuildAwayRAPMMatrixConstant(HALOEventsWithLineupRawOutcomeRAPMAwayEvents, HALOPlayersIDs)

HALOConstantRawOutcomeRAPMMatrixFull <- rbind(HALOConstantRawOutcomeRAPMMatrixHome, HALOConstantRawOutcomeRAPMMatrixAway)

HALOConstantRAPMWeights <- rep(1, times = nrow(HALOConstantRawOutcomeRAPMMatrixFull))
HALOGoals12Secs <- c(HALOEventsWithLineupRawOutcomeRAPMHomeEvents$TeamGoalWithinNext12Sec,HALOEventsWithLineupRawOutcomeRAPMAwayEvents$TeamGoalWithinNext12Sec)

HALORawOutcomeRAPMConstantCV <- cv.glmnet(x = HALOConstantRawOutcomeRAPMMatrixFull, y = HALOGoals12Secs, weights = HALOConstantRAPMWeights,
                                          alpha = 0, nfolds = 10, standardize = FALSE)

HALORawOutcomeRAPMConstantGLM <- glmnet(x = HALOConstantRawOutcomeRAPMMatrixFull, y = HALOGoals12Secs, weights = HALOConstantRAPMWeights,
                                        alpha = 0, lambda = HALORawOutcomeRAPMConstantCV$lambda.1se, standardize = FALSE)


HALO_Raw_Outcome_RAPM_Constant_Coefficients <- data.frame(
  column_name = rownames(HALORawOutcomeRAPMConstantGLM$beta),
  coefficient = as.vector(HALORawOutcomeRAPMConstantGLM$beta)
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
    names_glue  = "RAPM_{role}"
  ) %>%
  left_join(
    HALOPlayers %>% select(player_id, player_name, position_group),
    by = "player_id"
  ) %>%
  mutate(OffenseRank = rank(desc(RAPM_Offense)),
         DefenseRank = rank((RAPM_Defense))) %>%
  relocate(player_id, player_name, position_group, 
           RAPM_Offense, RAPM_Defense) %>%
  arrange((OffenseRank))



HALOEventsWithLineupForRawOutomeRAPM
# 1. Attach the raw label to the Ordered frame by key (not row position)
HALORawOutcomeLabelLookup_3Players <- HALOEventsWithLineupWithEventTypeSliced %>%
  transmute(game_id, sl_event_id,
            TeamGoalWithinNext12SecRaw = replace_na(TeamGoalWithinNext12Sec, 0))

HALOEventsWithLineupForRawOutcomeRAPM <- HALOEventsWithLineupForRawOutomeRAPM %>%
  left_join(HALORawOutcomeLabelLookup_3Players, by = c("game_id", "sl_event_id"))

# Fails if the join duplicated rows or left NAs
# (game_id and sl_event_id must be in the Ordered frame; if not, carry them through from the Sliced frame)
stopifnot(nrow(HALOEventsWithLineupForRawOutcomeRAPM_Ordered) == 1142418,
          !anyNA(HALOEventsWithLineupForRawOutcomeRAPM_Ordered$TeamGoalWithinNext12SecRaw))

HALORawOutcomeRAPMHomeEvents_3Players <- HALOEventsWithLineupForRawOutcomeRAPM_Ordered %>% filter(isHomeEvent == 1)
HALORawOutcomeRAPMAwayEvents_3Players <- HALOEventsWithLineupForRawOutcomeRAPM_Ordered %>% filter(isHomeEvent == 0)

HALORawOutcomeGoals12Secs_3Players <- c(HALORawOutcomeRAPMHomeEvents_3Players$TeamGoalWithinNext12SecRaw,
                                        HALORawOutcomeRAPMAwayEvents_3Players$TeamGoalWithinNext12SecRaw)
HALORawOutcomeGameIDs_3Players <- c(HALORawOutcomeRAPMHomeEvents_3Players$game_id,
                                    HALORawOutcomeRAPMAwayEvents_3Players$game_id)

# 2. One function builds either model
HALORAPMBuildXRawOutcome_3Players <- function(decay_lambda, untracked_weight) {
  rbind(
    BuildHomeRAPMMatrixExponential(HALORawOutcomeRAPMHomeEvents_3Players, HALOPlayersIDs,
                                   lambda = decay_lambda, untracked_w = untracked_weight),
    BuildAwayRAPMMatrixExponential(HALORawOutcomeRAPMAwayEvents_3Players, HALOPlayersIDs,
                                   lambda = decay_lambda, untracked_w = untracked_weight)
  )
}

HALORawOutcomeRAPMMatrixConstant_3Players    <- HALORAPMBuildXRawOutcome_3Players(Inf, 1)
HALORawOutcomeRAPMMatrixExponential_3Players <- HALORAPMBuildXRawOutcome_3Players(15, untracked_by_lambda[["15"]])

# 3. Shared game-grouped folds and shared penalty grid
set.seed(03032002)
HALORawOutcomeUniqueGames_3Players <- unique(HALORawOutcomeGameIDs_3Players)
HALORawOutcomeFoldByGame_3Players  <- setNames(sample(rep(1:5, length.out = length(HALORawOutcomeUniqueGames_3Players))),
                                               HALORawOutcomeUniqueGames_3Players)
HALORawOutcomeFoldID_3Players      <- unname(HALORawOutcomeFoldByGame_3Players[as.character(HALORawOutcomeGameIDs_3Players)])

# Extend the grid upward (heaviest penalty first, as before)
HALORawOutcomePenaltySequence_3Players <- exp(seq(log(1), log(1e-8), length.out = 60))

# Players penalized, the 9 game-state covariates not
HALORawOutcomePenaltyFactor_3Players <- c(rep(1, 2 * length(HALOPlayersIDs)), rep(0, 9))

HALORAPMRawOutcomeFoldMSE_3Players <- function(x_matrix, y_vector, fold_id, penalty_sequence, penalty_factor = NULL) {
  if (is.null(penalty_factor)) penalty_factor <- rep(1, ncol(x_matrix))
  fold_mse <- matrix(NA_real_, 5, length(penalty_sequence))
  for (k in 1:5) {
    train_rows <- fold_id != k
    fold_fit   <- glmnet(x_matrix[train_rows, ], y_vector[train_rows], alpha = 0,
                         lambda = penalty_sequence, penalty.factor = penalty_factor,
                         standardize = FALSE)
    fold_pred  <- predict(fold_fit, x_matrix[!train_rows, ])
    fold_mse[k, ] <- colMeans((y_vector[!train_rows] - fold_pred)^2)
  }
  fold_mse
}

# Intercept-only floor (training-fold base rate, scored on the held-out fold)
HALORawOutcomeMSEInterceptOnly_3Players <- sapply(1:5, function(k) {
  train_rows <- HALORawOutcomeFoldID_3Players != k
  mean((HALORawOutcomeGoals12Secs_3Players[!train_rows] - mean(HALORawOutcomeGoals12Secs_3Players[train_rows]))^2)
})

HALORawOutcomeMSEConstant_3Players    <- HALORAPMRawOutcomeFoldMSE_3Players(HALORawOutcomeRAPMMatrixConstant_3Players,
                                                                            HALORawOutcomeGoals12Secs_3Players,
                                                                            HALORawOutcomeFoldID_3Players,
                                                                            HALORawOutcomePenaltySequence_3Players)
HALORawOutcomeMSEExponential_3Players <- HALORAPMRawOutcomeFoldMSE_3Players(HALORawOutcomeRAPMMatrixExponential_3Players,
                                                                            HALORawOutcomeGoals12Secs_3Players,
                                                                            HALORawOutcomeFoldID_3Players,
                                                                            HALORawOutcomePenaltySequence_3Players)

# 4. CV-minimum penalty for each model, then paired per-fold comparison
HALORawOutcomeBestIdxConstant_3Players    <- which.min(colMeans(HALORawOutcomeMSEConstant_3Players))
HALORawOutcomeBestIdxExponential_3Players <- which.min(colMeans(HALORawOutcomeMSEExponential_3Players))
c(const_idx = HALORawOutcomeBestIdxConstant_3Players,
  exp_idx   = HALORawOutcomeBestIdxExponential_3Players)   # an index of 1 or 40 means the grid hit its edge

HALORawOutcomeGain_3Players <- HALORawOutcomeMSEConstant_3Players[, HALORawOutcomeBestIdxConstant_3Players] -
  HALORawOutcomeMSEExponential_3Players[, HALORawOutcomeBestIdxExponential_3Players]  # positive = exponential better
HALORawOutcomeGain_3Players
mean(HALORawOutcomeGain_3Players)
mean(HALORawOutcomeGain_3Players) / mean(HALORawOutcomeMSEConstant_3Players[, HALORawOutcomeBestIdxConstant_3Players])

HALORawOutcomeMSEConstantBest_3Players    <- HALORawOutcomeMSEConstant_3Players[, HALORawOutcomeBestIdxConstant_3Players]
HALORawOutcomeMSEExponentialBest_3Players <- HALORawOutcomeMSEExponential_3Players[, HALORawOutcomeBestIdxExponential_3Players]

HALORawOutcomeFloorGainConstant_3Players    <- HALORawOutcomeMSEInterceptOnly_3Players - HALORawOutcomeMSEConstantBest_3Players
HALORawOutcomeFloorGainExponential_3Players <- HALORawOutcomeMSEInterceptOnly_3Players - HALORawOutcomeMSEExponentialBest_3Players

HALORawOutcomeFloorGainConstant_3Players
HALORawOutcomeFloorGainExponential_3Players
mean(HALORawOutcomeGain_3Players) / (sd(HALORawOutcomeGain_3Players) / sqrt(5))   # paired t, descriptive only



# 1. Attach the xG-window response to the Ordered frame by key
HALOEventsWithLineupForxGWindowRAPM_Ordered <- HALOEventsWithLineupForRawOutcomeRAPM_Ordered %>%
  select(-any_of("xGWithinNext12Sec")) %>%
  left_join(HALORawOutcomexGLookup_3Players, by = c("game_id", "sl_event_id"))

stopifnot(nrow(HALOEventsWithLineupForxGWindowRAPM_Ordered) == 1142418,
          !anyNA(HALOEventsWithLineupForxGWindowRAPM_Ordered$xGWithinNext12Sec))

HALOxGWindowRAPMHomeEvents_3Players <- HALOEventsWithLineupForxGWindowRAPM_Ordered %>% filter(isHomeEvent == 1)
HALOxGWindowRAPMAwayEvents_3Players <- HALOEventsWithLineupForxGWindowRAPM_Ordered %>% filter(isHomeEvent == 0)

# Rows must line up with the existing X matrices (same game and event keys, same order)
stopifnot(identical(c(HALOxGWindowRAPMHomeEvents_3Players$sl_event_id, HALOxGWindowRAPMAwayEvents_3Players$sl_event_id),
                    c(HALORawOutcomeRAPMHomeEvents_3Players$sl_event_id, HALORawOutcomeRAPMAwayEvents_3Players$sl_event_id)))

HALOxGWindowResponse_3Players <- c(HALOxGWindowRAPMHomeEvents_3Players$xGWithinNext12Sec,
                                   HALOxGWindowRAPMAwayEvents_3Players$xGWithinNext12Sec)

# 2. Same folds, penalty grid and penalty factor; new floor and fold MSEs
HALOxGWindowMSEInterceptOnly_3Players <- sapply(1:5, function(k) {
  train_rows <- HALORawOutcomeFoldID_3Players != k
  mean((HALOxGWindowResponse_3Players[!train_rows] - mean(HALOxGWindowResponse_3Players[train_rows]))^2)
})

HALOxGWindowMSEConstant_3Players    <- HALORAPMRawOutcomeFoldMSE_3Players(HALORawOutcomeRAPMMatrixConstant_3Players,
                                                                          HALOxGWindowResponse_3Players,
                                                                          HALORawOutcomeFoldID_3Players,
                                                                          HALORawOutcomePenaltySequence_3Players,
                                                                          HALORawOutcomePenaltyFactor_3Players)
HALOxGWindowMSEExponential_3Players <- HALORAPMRawOutcomeFoldMSE_3Players(HALORawOutcomeRAPMMatrixExponential_3Players,
                                                                          HALOxGWindowResponse_3Players,
                                                                          HALORawOutcomeFoldID_3Players,
                                                                          HALORawOutcomePenaltySequence_3Players,
                                                                          HALORawOutcomePenaltyFactor_3Players)

# 3. CV-minimum penalty, floor gaps and paired gain
HALOxGWindowBestIdxConstant_3Players    <- which.min(colMeans(HALOxGWindowMSEConstant_3Players))
HALOxGWindowBestIdxExponential_3Players <- which.min(colMeans(HALOxGWindowMSEExponential_3Players))
c(const_idx = HALOxGWindowBestIdxConstant_3Players, exp_idx = HALOxGWindowBestIdxExponential_3Players)  # 1 or 60 = edge

HALOxGWindowMSEConstantBest_3Players    <- HALOxGWindowMSEConstant_3Players[, HALOxGWindowBestIdxConstant_3Players]
HALOxGWindowMSEExponentialBest_3Players <- HALOxGWindowMSEExponential_3Players[, HALOxGWindowBestIdxExponential_3Players]

HALOxGWindowFloorGainConstant_3Players    <- HALOxGWindowMSEInterceptOnly_3Players - HALOxGWindowMSEConstantBest_3Players
HALOxGWindowFloorGainExponential_3Players <- HALOxGWindowMSEInterceptOnly_3Players - HALOxGWindowMSEExponentialBest_3Players
HALOxGWindowGain_3Players <- HALOxGWindowMSEConstantBest_3Players - HALOxGWindowMSEExponentialBest_3Players  # positive = exponential better

HALOxGWindowFloorGainConstant_3Players
HALOxGWindowFloorGainExponential_3Players
HALOxGWindowGain_3Players
mean(HALOxGWindowGain_3Players) / mean(HALOxGWindowMSEConstantBest_3Players)
mean(HALOxGWindowGain_3Players) / (sd(HALOxGWindowGain_3Players) / sqrt(5))   # descriptive only


# ---------------------------------------------------------------
# Setup: settings, helpers
# ---------------------------------------------------------------
HALOSweepLambdas_3Players   <- c(10, 15, 25, 40)
HALOSweepSeeds_3Players     <- c(03032002, 11, 22)
HALOSweepPositionOptions_3Players <- c(FALSE, TRUE)     # FALSE = no position covariates, TRUE = position-adjusted
HALOSweepResponses_3Players <- list(xGWindow = HALOxGWindowResponse_3Players)
# To also run the goal label as a power-limited control:
# HALOSweepResponses_3Players$GoalLabel <- HALORawOutcomeGoals12Secs_3Players

HALOPlayerPosition_3Players <- HALOPlayers$position_group[match(HALOPlayersIDs, HALOPlayers$player_id)]

# Builds X from any home/away frames (needed for the placebos too)
HALORAPMBuildXFromFrames_3Players <- function(home_df, away_df, decay_lambda, untracked_weight) {
  rbind(
    BuildHomeRAPMMatrixExponential(home_df, HALOPlayersIDs, lambda = decay_lambda, untracked_w = untracked_weight),
    BuildAwayRAPMMatrixExponential(away_df, HALOPlayersIDs, lambda = decay_lambda, untracked_w = untracked_weight)
  )
}

# Four unpenalized columns: summed on-ice weight of D and F, offense side and defense side
HALORAPMAddPositionCovariates_3Players <- function(x_matrix) {
  n_players <- length(HALOPlayersIDs)
  d_idx <- which(HALOPlayerPosition_3Players == "D")
  f_idx <- which(HALOPlayerPosition_3Players == "F")
  position_covariates <- cbind(
    OffenseWeightD = Matrix::rowSums(x_matrix[, d_idx]),
    OffenseWeightF = Matrix::rowSums(x_matrix[, f_idx]),
    DefenseWeightD = Matrix::rowSums(x_matrix[, n_players + d_idx]),
    DefenseWeightF = Matrix::rowSums(x_matrix[, n_players + f_idx])
  )
  cbind(x_matrix, position_covariates)
}

HALORAPMPenaltyFactor_3Players <- function(use_position) {
  c(rep(1, 2 * length(HALOPlayersIDs)), rep(0, 9), rep(0, if (use_position) 4 else 0))
}

HALORAPMFoldIDFromSeed_3Players <- function(seed) {
  set.seed(seed)
  unique_games <- unique(HALORawOutcomeGameIDs_3Players)
  fold_by_game <- setNames(sample(rep(1:5, length.out = length(unique_games))), unique_games)
  unname(fold_by_game[as.character(HALORawOutcomeGameIDs_3Players)])
}

HALORAPMInterceptOnlyMSE_3Players <- function(y_vector, fold_id) {
  sapply(1:5, function(k) mean((y_vector[fold_id == k] - mean(y_vector[fold_id != k]))^2))
}

# Best-penalty MSE per fold for one X / response / seed combination
HALORAPMBestFoldMSE_3Players <- function(x_matrix, y_vector, fold_id, use_position) {
  if (use_position) x_matrix <- HALORAPMAddPositionCovariates_3Players(x_matrix)
  fold_mse <- HALORAPMRawOutcomeFoldMSE_3Players(x_matrix, y_vector, fold_id,
                                                 HALORawOutcomePenaltySequence_3Players,
                                                 HALORAPMPenaltyFactor_3Players(use_position))
  best_idx <- which.min(colMeans(fold_mse))
  list(mse = fold_mse[, best_idx], best_idx = best_idx)
}

HALOSweepFoldIDs_3Players <- setNames(lapply(HALOSweepSeeds_3Players, HALORAPMFoldIDFromSeed_3Players),
                                      as.character(HALOSweepSeeds_3Players))
HALOSweepGridSize_3Players <- length(HALORawOutcomePenaltySequence_3Players)

# ---------------------------------------------------------------
# Step 1: constant model, every response x seed x position option
# ---------------------------------------------------------------
HALOSweepConstantResults_3Players <- list()
X_constant <- HALORAPMBuildXFromFrames_3Players(HALORawOutcomeRAPMHomeEvents_3Players,
                                                HALORawOutcomeRAPMAwayEvents_3Players, Inf, 1)

for (response_name in names(HALOSweepResponses_3Players)) {
  for (seed_name in names(HALOSweepFoldIDs_3Players)) {
    for (use_position in HALOSweepPositionOptions_3Players) {
      key <- paste(response_name, seed_name, use_position, sep = "|")
      HALOSweepConstantResults_3Players[[key]] <- HALORAPMBestFoldMSE_3Players(
        X_constant, HALOSweepResponses_3Players[[response_name]],
        HALOSweepFoldIDs_3Players[[seed_name]], use_position)
    }
  }
}
rm(X_constant); gc()
saveRDS(HALOSweepConstantResults_3Players, "HALOSweepConstantResults_3Players.rds")

# ---------------------------------------------------------------
# Step 2: exponential model, one lambda at a time (X built once per lambda, then dropped)
# ---------------------------------------------------------------
HALOSweepFoldRows_3Players <- list()

for (decay_lambda in HALOSweepLambdas_3Players) {
  X_exponential <- HALORAPMBuildXFromFrames_3Players(HALORawOutcomeRAPMHomeEvents_3Players,
                                                     HALORawOutcomeRAPMAwayEvents_3Players,
                                                     decay_lambda, untracked_by_lambda[[as.character(decay_lambda)]])
  for (response_name in names(HALOSweepResponses_3Players)) {
    y_vector <- HALOSweepResponses_3Players[[response_name]]
    for (seed_name in names(HALOSweepFoldIDs_3Players)) {
      fold_id    <- HALOSweepFoldIDs_3Players[[seed_name]]
      floor_mse  <- HALORAPMInterceptOnlyMSE_3Players(y_vector, fold_id)
      for (use_position in HALOSweepPositionOptions_3Players) {
        key           <- paste(response_name, seed_name, use_position, sep = "|")
        constant_best <- HALOSweepConstantResults_3Players[[key]]
        exponential_best <- HALORAPMBestFoldMSE_3Players(X_exponential, y_vector, fold_id, use_position)
        HALOSweepFoldRows_3Players[[length(HALOSweepFoldRows_3Players) + 1]] <- tibble(
          response       = response_name,
          seed           = seed_name,
          position_adj   = use_position,
          decay_lambda   = decay_lambda,
          fold           = 1:5,
          mse_floor      = floor_mse,
          mse_constant   = constant_best$mse,
          mse_exponential = exponential_best$mse,
          idx_constant   = constant_best$best_idx,
          idx_exponential = exponential_best$best_idx,
          gain           = constant_best$mse - exponential_best$mse,        # positive = exponential better
          floor_gain_constant    = floor_mse - constant_best$mse,
          floor_gain_exponential = floor_mse - exponential_best$mse
        )
      }
    }
  }
  rm(X_exponential); gc()
  saveRDS(bind_rows(HALOSweepFoldRows_3Players), "HALOSweepFoldResults_3Players.rds")
}

HALOSweepFoldResults_3Players <- bind_rows(HALOSweepFoldRows_3Players)

# ---------------------------------------------------------------
# Step 3: summary table
# ---------------------------------------------------------------
HALOSweepSummary_3Players <- HALOSweepFoldResults_3Players %>%
  group_by(response, position_adj, decay_lambda) %>%
  summarise(
    mean_gain          = mean(gain),
    relative_gain      = mean(gain) / mean(mse_constant),
    folds_positive     = sum(gain > 0),
    folds_total        = n(),
    mean_floor_gain_constant    = mean(floor_gain_constant),
    mean_floor_gain_exponential = mean(floor_gain_exponential),
    any_edge = any(idx_constant %in% c(1, HALOSweepGridSize_3Players) |
                     idx_exponential %in% c(1, HALOSweepGridSize_3Players)),
    .groups = "drop"
  )
print(HALOSweepSummary_3Players, n = Inf)

# Seed-level view: is the sign stable across fold assignments?
HALOSweepSeedSummary_3Players <- HALOSweepFoldResults_3Players %>%
  group_by(response, position_adj, decay_lambda, seed) %>%
  summarise(mean_gain = mean(gain), folds_positive = sum(gain > 0), .groups = "drop")
print(HALOSweepSeedSummary_3Players, n = Inf)

# Position covariates computed from the exponential weights at a given lambda
HALORAPMPositionCovariatesFrom_3Players <- function(x_matrix_source) {
  n_players <- length(HALOPlayersIDs)
  d_idx <- which(HALOPlayerPosition_3Players == "D")
  f_idx <- which(HALOPlayerPosition_3Players == "F")
  cbind(OffenseWeightD = Matrix::rowSums(x_matrix_source[, d_idx]),
        OffenseWeightF = Matrix::rowSums(x_matrix_source[, f_idx]),
        DefenseWeightD = Matrix::rowSums(x_matrix_source[, n_players + d_idx]),
        DefenseWeightF = Matrix::rowSums(x_matrix_source[, n_players + f_idx]))
}
# Then: x_const_shared <- cbind(X_constant, covariates_from_X_exponential)
#       x_exp_shared   <- cbind(X_exponential, covariates_from_X_exponential)
# Run both through the same CV (penalty factor 0 on the 13 non-player columns) and compare.

# ---------------------------------------------------------------
# Step 1: untracked weight for any lambda (your table only covers 10, 15, 25, 40)
# ---------------------------------------------------------------
HALORAPMUntrackedWeight_3Players <- function(decay_lambda) {
  dist_cols <- c(paste0("HomePlayer", 1:5, "DistanceToEvent"), paste0("AwayPlayer", 1:5, "DistanceToEvent"))
  all_dist  <- unlist(c(HALORawOutcomeRAPMHomeEvents_3Players[dist_cols],
                        HALORawOutcomeRAPMAwayEvents_3Players[dist_cols]), use.names = FALSE)
  mean(exp(-all_dist / decay_lambda), na.rm = TRUE)
}

# Sanity check: should reproduce your table (about 0.2841 at 15, 0.4220 at 25, 0.5570 at 40)
sapply(c(15, 25, 40), HALORAPMUntrackedWeight_3Players)

# ---------------------------------------------------------------
# Step 2: settings and a CV helper that takes a prebuilt X (13 unpenalized non-player columns)
# ---------------------------------------------------------------
HALOMatchedLambdas_3Players <- c(25, 40, 60, 100)
HALOMatchedSeeds_3Players   <- names(HALOSweepFoldIDs_3Players)
HALOMatchedResponse_3Players <- HALOSweepResponses_3Players$xGWindow

HALORAPMMatchedBestFoldMSE_3Players <- function(x_matrix, y_vector, fold_id) {
  penalty_factor <- c(rep(1, 2 * length(HALOPlayersIDs)), rep(0, ncol(x_matrix) - 2 * length(HALOPlayersIDs)))
  fold_mse <- HALORAPMRawOutcomeFoldMSE_3Players(x_matrix, y_vector, fold_id,
                                                 HALORawOutcomePenaltySequence_3Players, penalty_factor)
  best_idx <- which.min(colMeans(fold_mse))
  list(mse = fold_mse[, best_idx], best_idx = best_idx)
}

# ---------------------------------------------------------------
# Step 3: matched-covariate loop
# ---------------------------------------------------------------
X_constant <- HALORAPMBuildXFromFrames_3Players(HALORawOutcomeRAPMHomeEvents_3Players,
                                                HALORawOutcomeRAPMAwayEvents_3Players, Inf, 1)
HALOMatchedFoldRows_3Players <- list()

for (decay_lambda in HALOMatchedLambdas_3Players) {
  untracked_weight <- HALORAPMUntrackedWeight_3Players(decay_lambda)
  X_exponential <- HALORAPMBuildXFromFrames_3Players(HALORawOutcomeRAPMHomeEvents_3Players,
                                                     HALORawOutcomeRAPMAwayEvents_3Players,
                                                     decay_lambda, untracked_weight)
  shared_covariates <- HALORAPMPositionCovariatesFrom_3Players(X_exponential)
  x_constant_shared    <- cbind(X_constant, shared_covariates)
  x_exponential_shared <- cbind(X_exponential, shared_covariates)
  rm(X_exponential); gc()
  
  for (seed_name in HALOMatchedSeeds_3Players) {
    fold_id   <- HALOSweepFoldIDs_3Players[[seed_name]]
    floor_mse <- HALORAPMInterceptOnlyMSE_3Players(HALOMatchedResponse_3Players, fold_id)
    constant_shared    <- HALORAPMMatchedBestFoldMSE_3Players(x_constant_shared,    HALOMatchedResponse_3Players, fold_id)
    exponential_shared <- HALORAPMMatchedBestFoldMSE_3Players(x_exponential_shared, HALOMatchedResponse_3Players, fold_id)
    
    # Constant model with its own (count-based) position covariates, from your earlier sweep
    constant_own_mse <- if (decay_lambda %in% HALOSweepLambdas_3Players) {
      HALOSweepConstantResults_3Players[[paste("xGWindow", seed_name, TRUE, sep = "|")]]$mse
    } else {
      HALORAPMBestFoldMSE_3Players(X_constant, HALOMatchedResponse_3Players, fold_id, TRUE)$mse
    }
    
    HALOMatchedFoldRows_3Players[[length(HALOMatchedFoldRows_3Players) + 1]] <- tibble(
      seed = seed_name, decay_lambda = decay_lambda, fold = 1:5,
      mse_floor = floor_mse,
      mse_constant_own    = constant_own_mse,
      mse_constant_shared = constant_shared$mse,
      mse_exponential_shared = exponential_shared$mse,
      idx_constant_shared = constant_shared$best_idx,
      idx_exponential_shared = exponential_shared$best_idx,
      gain_matched   = constant_shared$mse - exponential_shared$mse,    # player weights only
      gain_covariate = constant_own_mse - constant_shared$mse,          # covariate definition only
      gain_total     = constant_own_mse - exponential_shared$mse        # both (what your sweep measured)
    )
  }
  rm(x_constant_shared, x_exponential_shared, shared_covariates); gc()
  saveRDS(bind_rows(HALOMatchedFoldRows_3Players), "HALOMatchedFoldResults_3Players.rds")
}
HALOMatchedFoldResults_3Players <- bind_rows(HALOMatchedFoldRows_3Players)

# ---------------------------------------------------------------
# Step 4: summary and seed stability
# ---------------------------------------------------------------
HALOMatchedSummary_3Players <- HALOMatchedFoldResults_3Players %>%
  group_by(decay_lambda) %>%
  summarise(
    mean_gain_matched   = mean(gain_matched),
    relative_gain_matched = mean(gain_matched) / mean(mse_constant_shared),
    folds_positive_matched = sum(gain_matched > 0),
    folds_total = n(),
    mean_gain_covariate = mean(gain_covariate),
    mean_gain_total     = mean(gain_total),
    any_edge = any(idx_constant_shared %in% c(1, 60) | idx_exponential_shared %in% c(1, 60)),
    .groups = "drop"
  )
print(HALOMatchedSummary_3Players, n = Inf)

HALOMatchedSeedSummary_3Players <- HALOMatchedFoldResults_3Players %>%
  group_by(decay_lambda, seed) %>%
  summarise(mean_gain_matched = mean(gain_matched), folds_positive = sum(gain_matched > 0), .groups = "drop")
print(HALOMatchedSeedSummary_3Players, n = Inf)

print(HALOMatchedSummary_3Players %>% select(decay_lambda, mean_gain_covariate, mean_gain_total), n = Inf)
