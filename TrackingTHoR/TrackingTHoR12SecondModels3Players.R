#Creating XGBoost models at the 12 second threshold
#Only using 3 players per side, original model had 5 players per side

library(xgboost)
library(tidymodels)
library(recipes)

set.seed(03032002)

#Not used, switched to sampling by game for training instead of the 80/20 split
# HALOEventsWithTracking_12SecTotalEvents_3Players <- nrow(HALOEventsWithTrackingForXGBoost_3ClosestPerSide12Sec)
# HALOEventsTracking_12SecTrainingSize_3Players <- floor(0.8 * HALOEventsWithTracking_12SecTotalEvents_3Players)
# HALOEventsTracking_12Sec_train_indicies_3Players <- sample(seq_len(HALOEventsWithTracking_12SecTotalEvents_3Players), size = HALOEventsTracking_12SecTrainingSize_3Players)
# HALOEventsTracking_12Sec_train_data_3Players <- HALOEventsWithTrackingForXGBoost_3ClosestPerSide12Sec[HALOEventsTracking_12Sec_train_indicies_3Players, ]
# HALOEventsTracking_12Sec_test_data_3Players  <- HALOEventsWithTrackingForXGBoost_3ClosestPerSide12Sec[-HALOEventsTracking_12Sec_train_indicies_3Players, ]
# 
# HALOEventsTracking_12Sec_X_train_3Players <- HALOEventsTracking_12Sec_train_data_3Players %>% select(-TeamGoalWithinNext12Sec) %>% as.matrix()
# HALOEventsTracking_12Sec_X_test_3Players  <- HALOEventsTracking_12Sec_test_data_3Players  %>% select(-TeamGoalWithinNext12Sec) %>% as.matrix()
# 
# HALOEventsTracking_12Sec_dtrain_3Players <- xgb.DMatrix(HALOEventsTracking_12Sec_X_train_3Players, label = HALOEventsTracking_12Sec_train_data_3Players$TeamGoalWithinNext12Sec)
# HALOEventsTracking_12Sec_dtest_3Players  <- xgb.DMatrix(HALOEventsTracking_12Sec_X_test_3Players,  label = HALOEventsTracking_12Sec_test_data_3Players$TeamGoalWithinNext12Sec)

# classImbalance_12Sec_3Players <- sum(HALOEventsTracking_12Sec_train_data_3Players$TeamGoalWithinNext12Sec == 0) /
#   sum(HALOEventsTracking_12Sec_train_data_3Players$TeamGoalWithinNext12Sec == 1)

# XGBSpec12SecModel_3Players <- boost_tree(
#   trees = tune(),
#   tree_depth = tune(),
#   learn_rate = tune(),
#   loss_reduction = tune(),
#   min_n = tune(),
#   sample_size = tune()
# ) %>%
#   set_engine("xgboost",
#              scale_pos_weight = classImbalance_12Sec_3Players,
#              eval_metric = "aucpr") %>%
#   set_mode("classification")
# 
# HALOEventsTracking_12Sec_train_tidy_3Players <- HALOEventsTracking_12Sec_train_data_3Players %>%
#   mutate(TeamGoalWithinNext12Sec = factor(TeamGoalWithinNext12Sec, levels = c(0, 1)))
# HALOEventsTracking_12Sec_test_tidy_3Players <- HALOEventsTracking_12Sec_test_data_3Players %>%
#   mutate(TeamGoalWithinNext12Sec = factor(TeamGoalWithinNext12Sec, levels = c(0, 1)))
# 
# XGB_12Sec_workflow_3Players <- workflow() %>%
#   add_model(XGBSpec12SecModel_3Players) %>%
#   add_formula(TeamGoalWithinNext12Sec ~ .)
# 
# set.seed(03032002)
# HALOEventsTracking_12Sec_folds_3Players <- vfold_cv(
#   HALOEventsTracking_12Sec_train_tidy_3Players,
#   v = 5,
#   strata = TeamGoalWithinNext12Sec
# )
# 
# XGB_12Sec_grid_3Players <- grid_space_filling(
#   trees(range = c(500, 3000)),
#   tree_depth(range = c(3, 8)),
#   learn_rate(range = c(-3, -1)),
#   loss_reduction(range = c(-5, 0)),
#   min_n(range = c(5, 20)),
#   sample_size = sample_prop(range = c(0.6, 1.0)),
#   size = 30
# )
# 
# 
# XGB_12Sec_metrics_3Players <- metric_set(roc_auc, mn_log_loss, pr_auc)
# 
# XGB_12Sec_tune_results_3Players <- tune_grid(
#   XGB_12Sec_workflow_3Players,
#   resamples = HALOEventsTracking_12Sec_folds_3Players,
#   grid = XGB_12Sec_grid_3Players,
#   metrics = XGB_12Sec_metrics_3Players,
#   control = control_grid(verbose = TRUE, save_pred = TRUE)
# )
# 
# show_best(XGB_12Sec_tune_results_3Players, metric = "mn_log_loss", n = 10)
# show_best(XGB_12Sec_tune_results_3Players, metric = "roc_auc", n = 10)
# show_best(XGB_12Sec_tune_results_3Players, metric = "pr_auc", n = 10)
# 
# XGB_12Sec_best_params_3Players <- select_best(XGB_12Sec_tune_results_3Players, metric = "pr_auc")
# 
# XGB_12Sec_final_workflow_3Players <- finalize_workflow(XGB_12Sec_workflow_3Players, XGB_12Sec_best_params_3Players)
# XGB_12Sec_final_fit_3Players <- fit(XGB_12Sec_final_workflow_3Players, data = HALOEventsTracking_12Sec_train_tidy_3Players)
# 
# XGB_12Sec_test_results_3Players <- augment(XGB_12Sec_final_fit_3Players, new_data = HALOEventsTracking_12Sec_test_tidy_3Players)
# 
# XGB_12Sec_test_results_3Players %>%
#   roc_auc(truth = TeamGoalWithinNext12Sec, .pred_1, event_level = "second")
# 
# XGB_12Sec_test_results_3Players %>%
#   pr_auc(truth = TeamGoalWithinNext12Sec, .pred_1, event_level = "second")
# 
# XGB_12Sec_test_results_3Players %>%
#   mn_log_loss(truth = TeamGoalWithinNext12Sec, .pred_1, event_level = "second")
# 
# XGB_12Sec_test_results_3Players %>%
#   mutate(.pred_class = factor(ifelse(.pred_1 > 0.5, 1, 0), levels = c(0, 1))) %>%
#   mcc(truth = TeamGoalWithinNext12Sec, estimate = .pred_class)
# 
# HALOEventsTracking_12Sec_preds_3Players <- XGB_12Sec_test_results_3Players$.pred_1
# 
# autoplot(XGB_12Sec_tune_results_3Players)
# 
# HALOEventsTracking_12Sec_calibration_3Players <- tibble(raw = HALOEventsTracking_12Sec_preds_3Players, outcome = factor(XGB_12Sec_test_results_3Players$TeamGoalWithinNext12Sec, level = c(0,1)))
# 
# HALOEventsTracking_12Sec_cal_fit_3Players <- glm(as.numeric(as.character(outcome))~raw, data = HALOEventsTracking_12Sec_calibration_3Players, family = binomial)
# 
# HALOEventsTracking_12SecEnvironmentSummarised_3Players <- HALOEventsWithTrackingForXGBoost_3ClosestPerSide12Sec %>%
#   mutate(TeamGoalWithinNext12Sec = factor(TeamGoalWithinNext12Sec, levels = c(0, 1))) %>%
#   bind_cols(
#     predict(XGB_12Sec_final_fit_3Players, new_data = ., type = "prob") %>%
#       rename(goal_prob_raw = .pred_1)
#   ) %>%
#   mutate(goal_prob_calibrated = predict(HALOEventsTracking_12Sec_cal_fit_3Players, newdata = tibble(raw = goal_prob_raw), type = "response"))
# 
# HALOEventsTracking_12Sec_calibration_3Players %>%
#   mutate(
#     calibrated  = predict(HALOEventsTracking_12Sec_cal_fit_3Players, newdata = HALOEventsTracking_12Sec_calibration_3Players, type = "response"),
#     actual = as.numeric(as.character(outcome)),
#     bin = cut(calibrated, breaks = seq(0, 1, 0.1))
#   ) %>%
#   group_by(bin) %>%
#   summarise(
#     mean_pred = mean(calibrated),
#     mean_actual = mean(actual),
#     n = n(),
#     .groups = "drop"
#   ) %>%
#   ggplot(aes(x = mean_pred, y = mean_actual)) +
#   geom_point(aes(size = n)) +
#   geom_abline(linetype = "dashed") +
#   labs(title = "GLM Calibration Plot, 3 Players Per Side",
#        x = "Predicted Probability",
#        y = "Actual Success Rate")
# 
# HALOEventsTracking_12SecEnvironmentSummarised_3Players %>%
#   mutate(.pred_class = factor(ifelse(goal_prob_calibrated > 0.5, 1, 0), levels = c(0, 1))) %>%
#   mcc(truth = TeamGoalWithinNext12Sec, estimate = .pred_class) #MCC is 0.230




#Grouped folds by game
set.seed(03032002)

HALOEvents_xGLookup <- HALOEventsWithTrackingForXGBoost_3ClosestPerSide12Sec %>%
  select(game_id, sl_event_id, sl_xg_all_shots)

HALOEventsWithTrackingForXGBoost_3ClosestPerSide12SecModel <-
  HALOEventsWithTrackingForXGBoost_3ClosestPerSide12Sec %>%
  select(-sl_xg_all_shots, -SuccessfulEventFlag) %>%
  mutate(TeamGoalWithinNext12Sec = factor(TeamGoalWithinNext12Sec, levels = c(0, 1)))

HALOEvents3Closest12SecRec <- recipe(TeamGoalWithinNext12Sec ~ .,
                                     data = HALOEventsWithTrackingForXGBoost_3ClosestPerSide12SecModel) %>%
  update_role(game_id, sl_event_id, new_role = "id")

HALOEvents3Closest12SecFolds <- group_vfold_cv(
  HALOEventsWithTrackingForXGBoost_3ClosestPerSide12SecModel, group = "game_id", v = 3)

map_dbl(HALOEvents3Closest12SecFolds$splits,
        ~ mean(assessment(.x)$TeamGoalWithinNext12Sec == "1"))

# classImbalance_12Sec_3Players <- with(HALOEventsWithTrackingForXGBoost_3ClosestPerSide12SecModel,
#                                       sum(TeamGoalWithinNext12Sec == 0) / sum(TeamGoalWithinNext12Sec == 1))

XGBSpec12SecModel_3Players <- boost_tree(
  trees = tune(), tree_depth = tune(), learn_rate = 0.05,
  loss_reduction = tune(), min_n = tune(), sample_size = tune()
) %>%
  set_engine("xgboost", nthread = 2) %>%
  set_mode("classification")

XGB_12Sec_grid_3Players <- grid_space_filling(
  trees(range = c(100, 600)), tree_depth(range = c(2, 6)), loss_reduction(range = c(-5, 0)),
  min_n(range = c(5, 20)), sample_size = sample_prop(range = c(0.6, 1.0)),
  size = 15
)

HALOEvents3Closest12SecTuneWF <- workflow() %>%
  add_recipe(HALOEvents3Closest12SecRec) %>%
  add_model(XGBSpec12SecModel_3Players)

# pr_auc_goal <- metric_tweak("pr_auc_goal", pr_auc, event_level = "second")
cv_metrics  <- metric_set(roc_auc, mn_log_loss)

saveRDS(HALOEventsWithTrackingForXGBoost_3ClosestPerSide12SecModel, "xgb_model_df.rds")
saveRDS(HALOEvents3Closest12SecFolds, "folds.rds")
HALOEventsWithTrackingForXGBoost_3ClosestPerSide12SecModel <- readRDS("xgb_model_df.rds")
HALOEvents3Closest12SecFolds <- readRDS("folds.rds")

set.seed(03032002)
XGB_12Sec_tune_results_3Players <- tune_grid(
  HALOEvents3Closest12SecTuneWF,
  resamples = HALOEvents3Closest12SecFolds,
  grid = XGB_12Sec_grid_3Players,
  metrics = cv_metrics,
  control = control_grid(verbose = TRUE, save_pred = FALSE)
)

saveRDS(XGB_12Sec_tune_results_3Players, "xgb_model.rds")

HALOEvents3Closest12SecBestParam <- select_best(XGB_12Sec_tune_results_3Players, metric = "mn_log_loss")
HALOEvents3Closest12SecBestSpec  <- finalize_model(XGBSpec12SecModel_3Players,
                                                   HALOEvents3Closest12SecBestParam)

show_best(XGB_12Sec_tune_results_3Players, metric = "mn_log_loss", n = 5)

HALOEvents3Closest12SecWF <- workflow() %>%
  add_recipe(HALOEvents3Closest12SecRec) %>%
  add_model(HALOEvents3Closest12SecBestSpec)

set.seed(03032002)
HALOEvents3Closest12SecOOF_RES <- fit_resamples(
  HALOEvents3Closest12SecWF, resamples = HALOEvents3Closest12SecFolds,
  metrics = cv_metrics,
  control = control_resamples(save_pred = TRUE)
)

collect_metrics(HALOEvents3Closest12SecOOF_RES, summarize = FALSE)
HALOEvents3Closest12SecOOF <- collect_predictions(HALOEvents3Closest12SecOOF_RES) %>% arrange(.row)

# checks before calibration
nrow(HALOEvents3Closest12SecOOF) == nrow(HALOEventsWithTrackingForXGBoost_3ClosestPerSide12SecModel)
anyDuplicated(HALOEvents3Closest12SecOOF$.row)


HALOEvents3Closest12Sec_OOF_Eval <- HALOEventsWithTrackingForXGBoost_3ClosestPerSide12SecModel %>%
  mutate(.row = row_number(),
         is_shot = EventTypeWithZone_shot_OZ == 1 | EventTypeWithZone_shot_NZ == 1 | EventTypeWithZone_shot_DZ == 1) %>%
  select(.row, is_shot) %>%
  left_join(HALOEvents3Closest12SecOOF, by = ".row")

HALOEvents3Closest12Sec_OOF_Eval %>% group_by(is_shot) %>%
  summarise(n = n(), goal_rate = mean(TeamGoalWithinNext12Sec == "1"),
            pr_auc = pr_auc_vec(TeamGoalWithinNext12Sec, .pred_1, event_level = "second"))

#Calibration, did not end up being needed
HALOEvents3Closest12SecCal <- HALOEventsWithTrackingForXGBoost_3ClosestPerSide12SecModel %>%
  select(game_id, sl_event_id, y = TeamGoalWithinNext12Sec) %>%
  mutate(.row = row_number(), y_num = as.numeric(as.character(y))) %>%
  left_join(HALOEvents3Closest12SecOOF %>% select(.row, raw = .pred_1), by = ".row") %>%
  mutate(raw_logit = qlogis(pmin(pmax(raw, 1e-6), 1 - 1e-6)))

HALOEvents3Closest12SecCalFit <- glm(y_num ~ raw_logit,
                                     data = HALOEvents3Closest12SecCal, family = binomial)
HALOEvents3Closest12SecCal$goal_prob_calibrated <- predict(HALOEvents3Closest12SecCalFit, type = "response")

coef(HALOEvents3Closest12SecCalFit)

max(abs(HALOEvents3Closest12SecCal$goal_prob_calibrated - HALOEvents3Closest12SecCal$raw))
cor(HALOEvents3Closest12SecCal$goal_prob_calibrated, HALOEvents3Closest12SecCal$raw)

HALOEvents3Closest12SecCal %>%
  mutate(bin = ntile(raw, 20)) %>%
  group_by(bin) %>%
  summarise(mean_pred = mean(raw), mean_actual = mean(y_num), n = n())

HALOEvents3Closest12SecPreds <- HALOEvents3Closest12SecCal %>%
  select(game_id, sl_event_id, goal_prob_raw = raw)
saveRDS(HALOEvents3Closest12SecPreds, "oof_goal_prob_raw.rds")

HALOESEventSummaryWithPreds <- HALOEventsTrackingDistancesToPuckEventSummaryWithZoneSetOffenseDefense %>%
  left_join(readRDS("oof_goal_prob_raw.rds"), by = c("game_id", "sl_event_id")) %>%
  mutate(goal_prob_calibrated = goal_prob_raw)

sum(is.na(HALOESEventSummaryWithPreds$goal_prob_raw)) 

stopifnot(identical(HALOESEventSummaryWithPreds$game_id, HALOEventsWithTrackingForXGBoost_3ClosestPerSide12Sec$game_id),
          identical(HALOESEventSummaryWithPreds$sl_event_id, HALOEventsWithTrackingForXGBoost_3ClosestPerSide12Sec$sl_event_id),
          isTRUE(all.equal(HALOESEventSummaryWithPreds$goal_prob_raw, HALOEvents3Closest12SecCal$raw)))
