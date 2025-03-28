#xRebounds+ Statistic

library(dplyr)
library(nhlapi)
library(grid)
library(magick)
library(sportyR)
library(viridis)
library(ggplot2)
library(cowplot)
library(tidyverse)
library(caret)
library(xgboost)
library(dplyr)
library(pROC)
library(DiagrammeR)
library(verification)
library(gt)
library(MASS)
library(neuralnet)
library(randomForest)
library(keras)
library(tensorflow)


str(shots_2023)
View(shots_2023)

shots_model_data <- shots_2023 %>%
  select(shotGeneratedRebound, shotAngle, arenaAdjustedShotDistance, shooterLeftRight, 
         shotRush, shotType)

# Convert categorical variables to factors
shots_model_data$shotGeneratedRebound <- as.numeric(shots_model_data$shotGeneratedRebound)  # Convert to 0/1
shots_model_data$shooterLeftRight <- as.factor(shots_model_data$shooterLeftRight)
shots_model_data$shotRush <- as.factor(shots_model_data$shotRush)
shots_model_data$shotType <- as.factor(shots_model_data$shotType)

# Convert categorical variables into dummy variables
shots_model_data <- model.matrix(shotGeneratedRebound ~ ., data = shots_model_data)[, -1]

# Define X (features) and y (target)
y <- shots_2023$shotGeneratedRebound  # Ensure it's numeric (0 or 1)
X <- as.matrix(shots_model_data)

# Convert to DMatrix (XGBoost preferred format)
dtrain <- xgb.DMatrix(data = X, label = y)

# Define model parameters
xReb_params <- list(
  objective = "binary:logistic",  # Logistic regression for classification
  eval_metric = "logloss",        # Logarithmic loss
  eta = 0.1,                      # Learning rate
  max_depth = 6,                  # Tree depth
  colsample_bytree = 0.8,         # Feature selection per tree
  subsample = 0.8                 # Sample fraction per boosting round
)

# Perform cross-validation
set.seed(2025)
xReb_cv_results <- xgb.cv(
  params = xReb_params,
  data = dtrain,
  nrounds = 100,                # Maximum boosting rounds
  nfold = 5,                    # 5-fold cross-validation
  showsd = TRUE,                
  stratified = TRUE,            # Maintain class balance in folds
  print_every_n = 10,           
  early_stopping_rounds = 20,    # Stop early if no improvement
  prediction = TRUE
)

print(xReb_cv_results)

# Get the best number of boosting rounds
xReb_best_nrounds <- xReb_cv_results$best_iteration

# Train the final model
xReb_final_model <- xgb.train(
  params = xReb_params,
  data = dtrain,
  nrounds = xReb_best_nrounds
)

xReb_preds <- predict(xReb_final_model, dtrain)
xReb_roc_curve <- roc(y, xReb_preds)
auc(xReb_roc_curve)

xReb_importance <- xgb.importance(model = xReb_final_model)
xgb.plot.importance(xReb_importance)


xReb_pred_labels <- ifelse(xReb_preds >= 0.5, 1, 0)
xReb_accuracy <- sum(xReb_pred_labels == y) / length(y)
print(paste("Accuracy of the best model: ", xReb_accuracy))
xReb_conf_matrix <- confusionMatrix(factor(xReb_pred_labels), factor(y))
print(xReb_conf_matrix)


#first model overfits so there are no rebounds predicted

sum(y == 0) / sum(y == 1)
#second iteration of model
dtrain <- xgb.DMatrix(data = X, label = y)

xReb_params2 <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  eta = 0.1,                      
  max_depth = 6,                 
  colsample_bytree = 0.8,        
  subsample = 0.8,
  scale_pos_weight = 12.36  # Adjust for class imbalance
)


# Perform cross-validation
xReb_cv_results2 <- xgb.cv(
  params = xReb_params2,
  data = dtrain,
  nrounds = 100,
  nfold = 5,               
  showsd = TRUE,           
  stratified = TRUE,       
  print_every_n = 10,      
  early_stopping_rounds = 20, 
  prediction = TRUE
)


print(xReb_cv_results2)
xReb_best_nrounds2 <- xReb_cv_results2$best_iteration

# Train final model
xReb_final_model2 <- xgb.train(
  params = xReb_params2,
  data = dtrain,
  nrounds = xReb_best_nrounds2
)

# Predictions
xReb_preds2 <- predict(xReb_final_model2, X)

# Evaluate new model
xReb_roc_curve2 <- roc(y, xReb_preds2)
print(auc(xReb_roc_curve2))  # AUC Score

# Feature Importance
xReb_importance2 <- xgb.importance(model = xReb_final_model2)
xgb.plot.importance(xReb_importance2)

# Classification thresholding
xReb_pred_labels2 <- ifelse(xReb_preds2 >= 0.5, 1, 0)

# Accuracy
xReb_accuracy2 <- sum(xReb_pred_labels2 == y) / length(y)
print(paste("Accuracy of the second model: ", xReb_accuracy2))

# Confusion Matrix
xReb_conf_matrix2 <- confusionMatrix(factor(xReb_pred_labels2), factor(y))
print(xReb_conf_matrix2)



#the second model was better, still want to improve

xReb_params3 <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  eta = 0.05,                      # Lower learning rate for better generalization
  max_depth = 7,                    # Increased depth for complexity
  colsample_bytree = 0.7,           # Feature selection tuning
  subsample = 0.75,                 # Slightly more randomization
  min_child_weight = 3,             # Reduce overfitting
  gamma = 0.2,                      # Additional regularization
  scale_pos_weight = 12.36           # Adjust for class imbalance
)

# Perform cross-validation
xReb_cv_results3 <- xgb.cv(
  params = xReb_params3,
  data = dtrain,
  nrounds = 200,                    # Increased rounds for more learning
  nfold = 5,                        
  showsd = TRUE,                    
  stratified = TRUE,                
  print_every_n = 10,               
  early_stopping_rounds = 25,        # Stop earlier to avoid overfitting
  prediction = TRUE
)

print(xReb_cv_results3)
xReb_best_nrounds3 <- xReb_cv_results3$best_iteration

# Train final model with the best number of rounds
xReb_final_model3 <- xgb.train(
  params = xReb_params3,
  data = dtrain,
  nrounds = xReb_best_nrounds3
)

# Predictions
xReb_preds3 <- predict(xReb_final_model3, X)

# Evaluate new model
xReb_roc_curve3 <- roc(y, xReb_preds3)
print(auc(xReb_roc_curve3))  # AUC Score

# Feature Importance
xReb_importance3 <- xgb.importance(model = xReb_final_model3)
xgb.plot.importance(xReb_importance3)

# Classification thresholding
xReb_pred_labels3 <- ifelse(xReb_preds3 >= 0.5, 1, 0)

# Accuracy
xReb_accuracy3 <- sum(xReb_pred_labels3 == y) / length(y)
print(paste("Accuracy of the third model: ", xReb_accuracy3))

# Confusion Matrix
xReb_conf_matrix3 <- confusionMatrix(factor(xReb_pred_labels3), factor(y))
print(xReb_conf_matrix3)


#third model was better, want to give it one more try

xReb_params4 <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  eta = 0.03,                      # Lower learning rate for stability
  max_depth = 8,                   # Increased depth for capturing more patterns
  colsample_bytree = 0.65,         # Slightly reduced to prevent overfitting
  subsample = 0.7,                 # Randomization for generalization
  min_child_weight = 5,            # Higher value to reduce overfitting
  gamma = 0.3,                     # Regularization to penalize complexity
  scale_pos_weight = 12.36           # Adjusted for class imbalance
)

# Perform cross-validation
xReb_cv_results4 <- xgb.cv(
  params = xReb_params4,
  data = dtrain,
  nrounds = 250,                    # More boosting rounds
  nfold = 5,                        
  showsd = TRUE,                    
  stratified = TRUE,                
  print_every_n = 10,               
  early_stopping_rounds = 30,        # Longer patience for improvement
  prediction = TRUE
)

print(xReb_cv_results4)
xReb_best_nrounds4 <- xReb_cv_results4$best_iteration

# Train final model with best rounds
xReb_final_model4 <- xgb.train(
  params = xReb_params4,
  data = dtrain,
  nrounds = xReb_best_nrounds4
)

# Predictions
xReb_preds4 <- predict(xReb_final_model4, X)

# Evaluate new model
xReb_roc_curve4 <- roc(y, xReb_preds4)
print(auc(xReb_roc_curve4))  # AUC Score

# Feature Importance
xReb_importance4 <- xgb.importance(model = xReb_final_model4)
xgb.plot.importance(xReb_importance4)

# Classification thresholding
xReb_pred_labels4 <- ifelse(xReb_preds4 >= 0.5, 1, 0)

# Accuracy
xReb_accuracy4 <- sum(xReb_pred_labels4 == y) / length(y)
print(paste("Accuracy of the fourth model: ", xReb_accuracy4))

# Confusion Matrix
xReb_conf_matrix4 <- confusionMatrix(factor(xReb_pred_labels4), factor(y))
print(xReb_conf_matrix4)


#one more model, for good measure
xReb_params5 <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  eta = 0.085,                     # Even lower learning rate for fine-tuned updates
  max_depth = 9,                   # Deeper trees for capturing more complexity
  colsample_bytree = 0.65,         # Same feature sampling as before
  subsample = 0.7,                 # Slightly reduced to prevent overfitting
  min_child_weight = 5,            # More strict regularization
  gamma = 0.4,                     # Increased regularization
  scale_pos_weight = 10           # Adjusted for class imbalance
)

# Perform cross-validation
xReb_cv_results5 <- xgb.cv(
  params = xReb_params5,
  data = dtrain,
  nrounds = 2100,                   # Increased boosting rounds
  nfold = 7,                        # More folds for better validation
  showsd = TRUE,                    
  stratified = TRUE,                
  print_every_n = 50,               
  early_stopping_rounds = 40,        # More patience for early stopping
  prediction = TRUE
)

print(xReb_cv_results5)
xReb_best_nrounds5 <- xReb_cv_results5$best_iteration

# Train final model with best rounds
xReb_final_model5 <- xgb.train(
  params = xReb_params5,
  data = dtrain,
  nrounds = xReb_best_nrounds5
)

# Predictions
xReb_preds5 <- predict(xReb_final_model5, X)

# Evaluate new model
xReb_roc_curve5 <- roc(y, xReb_preds5)
print(auc(xReb_roc_curve5))  # AUC Score

# Feature Importance
xReb_importance5 <- xgb.importance(model = xReb_final_model5)
xgb.plot.importance(xReb_importance5)

# Classification thresholding
xReb_pred_labels5 <- ifelse(xReb_preds5 >= 0.5, 1, 0)

# Accuracy
xReb_accuracy5 <- sum(xReb_pred_labels5 == y) / length(y)
print(paste("Accuracy of the fifth model: ", xReb_accuracy5))

# Confusion Matrix
xReb_conf_matrix5 <- confusionMatrix(factor(xReb_pred_labels5), factor(y))
print(xReb_conf_matrix5)

calib <- calibration(factor(y) ~ xReb_preds5)
plot(calib, main = "Calibration Curve")


xReboundsData2024_1 <- shots_2024 %>%
  dplyr::select(shotAngle, arenaAdjustedShotDistance, shooterLeftRight, 
                shotRush, shotType, shotID, shooterName)

xReboundsData2024_2 <- xReboundsData2024_1 %>%
  dplyr::select(shotAngle, arenaAdjustedShotDistance, shooterLeftRight, 
                shotRush, shotType)

xReboundsMatrix <- model.matrix(~ . -1, data = xReboundsData2024_2)

head(xReboundsMatrix)
colnames(xReboundsMatrix)
xReboundsRecode <- as.data.frame(xReboundsMatrix)
head(xReboundsRecode)
xReboundsRecode <- xReboundsRecode %>%
  dplyr::select(shotAngle, arenaAdjustedShotDistance, shooterLeftRightL, shooterLeftRightR,
                shotRush, shotTypeBACK, shotTypeDEFL, shotTypeSLAP, shotTypeSNAP, shotTypeTIP, 
         shotTypeWRAP, shotTypeWRIST) %>%
  rename(shotRush1 = shotRush)
xReboundsRecodeMatrix <- as.matrix(xReboundsRecode)
colnames(xReboundsRecode)
colnames(X)
head(X)

xReboundsData2024_1$xReboundsNewModel <- predict(xReb_final_model5, xReboundsRecodeMatrix)
head(xReboundsData2024_1)

xReboundsByPlayer <- xReboundsData2024_1 %>%
  group_by(shooterName) %>%
  summarise(
    total_xRebounds = sum(xReboundsNewModel, na.rm = TRUE),
    total_shots = n() 
  ) %>%
  arrange(desc(total_xRebounds))

xReboundsByPlayer <- xReboundsByPlayer %>%
  mutate(total_xRebounds = format(total_xRebounds, scientific = FALSE))

gt(xReboundsByPlayer)

gt(xReboundsByPlayer) |>
  tab_header(
    title = md("xRebounds 2024-25 Season"))

#Create a model to determine the angle that the rebound comes off at
#bring over the previous shot data to predict the rebound angel
colnames(shots_2023)
ShotsWithPreviousShotData <- shots_2023 %>%
  arrange(game_id, id) %>%  # Ensure data is ordered correctly
  mutate(previousAngle = lag(shotAngle),
         previousDistance = lag(arenaAdjustedShotDistance),
         previousHandedness = lag(shooterLeftRight),
         previousRush = lag(shotRush),
         previousShotType = lag(shotType),
         previousXCoord = lag(arenaAdjustedXCord),
         previousYCoord = lag(arenaAdjustedYCord)) %>%
  filter(shotRebound == 1)
colnames(ShotsWithPreviousShotData)
View(ShotsWithPreviousShotData)

ShotsWithPreviousShotData <- ShotsWithPreviousShotData %>%
  mutate(previousHandedness = factor(previousHandedness),
         shotType = factor(shotType))
ShotsWithPreviousShotDataEncoded <- ShotsWithPreviousShotData %>%
  mutate(previousHandedness = factor(previousHandedness),
         shotType = factor(shotType)) %>%
  dplyr::select(previousHandedness, shotType, previousAngle, previousDistance, previousXCoord, previousYCoord, shotAngle) %>%
  model.matrix(~ previousHandedness + shotType + previousAngle + previousDistance + previousXCoord + previousYCoord + shotAngle - 1, 
               data = .)
index <- sample(1:nrow(ShotsWithPreviousShotDataEncoded), round(0.8 * nrow(ShotsWithPreviousShotDataEncoded)))
train_rebounds <- ShotsWithPreviousShotDataEncoded[index,]
test_rebounds <- ShotsWithPreviousShotDataEncoded[-index,]

train_rebounds_df <- as.data.frame(train_rebounds)
test_rebounds_df <- as.data.frame(test_rebounds)

train_rebounds_scaled <- train_rebounds_df %>%
  mutate(previousAngle = scale(previousAngle),
         previousDistance = scale(previousDistance),
         previousXCoord = scale(previousXCoord),
         previousYCoord = scale(previousYCoord))

test_rebounds_scaled <- test_rebounds_df %>%
  mutate(previousAngle = scale(previousAngle),
         previousDistance = scale(previousDistance),
         previousXCoord = scale(previousXCoord),
         previousYCoord = scale(previousYCoord))
head(train_rebounds_scaled)
head(test_rebounds_scaled)
train_rebounds_scaled <- na.omit(train_rebounds_scaled)
train_rebounds_scaled <- as.data.frame(scale(train_rebounds))
train_rebounds_scaled$shotAngle <- scale(train_rebounds_scaled$shotAngle)
train_rebounds_scaled$shotAngle <- as.numeric(train_rebounds_scaled$shotAngle)

# Ensure the data is numeric
test_rebounds_numeric <- apply(test_rebounds_numeric, 2, as.numeric)

xReboundAngleNN <- neuralnet(shotAngle ~ previousHandedness + previousHandednessL + previousHandednessR + 
                               shotTypeBACK + shotTypeDEFL + shotTypeSLAP + shotTypeSNAP + shotTypeTIP +
                               shotTypeWRAP + shotTypeWRIST + previousAngle + previousDistance + 
                               previousXCoord + previousYCoord, 
                             data = train_rebounds_scaled, hidden = c(7, 4), 
                             linear.output = TRUE, threshold = 0.01, act.fct = "tanh", stepmax = 1e6)
plot(xReboundAngleNN$net.result[[1]], type = "l", main = "Neural Network Training Error")

summary(train_rebounds_scaled)
sum(is.na(train_rebounds_scaled))  # Count NA values
sum(is.infinite(train_rebounds_scaled))

pr.xAngle <- compute(xReboundAngleNN, test_rebounds_scaled)

# Compute mean squared error
pr.xAngle_ <- pr.xAngle$net.result * (max(data$medv) - min(data$medv)) 
+ min(data$medv)
test.r <- (test_$medv) * (max(data$medv) - min(data$medv)) + 
  min(data$medv)
MSE.nn <- sum((test.r - pr.xAngle_)^2) / nrow(test_)

# Plot the neural network
plot(xReboundAngleNN)

plot(test.r, pr.xAngle_, col = "red", 
     main = 'Real vs Predicted')
abline(0, 1, lwd = 2)


#neural network did not converge, switching to xgboost with regression objective

head(ShotsWithPreviousShotDataEncoded)
#have all data needed in the encoded data frame
set.seed(2025)
y_angle <- ShotsWithPreviousShotDataEncoded$shotAngle
X_angle <- ShotsWithPreviousShotDataEncoded %>% select(-shotAngle) %>% as.matrix()

# Split into training and test sets (80% train, 20% test)
train_index <- createDataPartition(y_angle, p = 0.8, list = FALSE)
X_train_angle <- X_angle[train_index, ]
y_train_angle <- y_angle[train_index]
X_test_angle <- X_angle[-train_index, ]
y_test_angle <- y_angle[-train_index]

# Convert to XGBoost DMatrix format
dmatrix_train_angle <- xgb.DMatrix(data = X_train_angle, label = y_train_angle)
dmatrix_test_angle <- xgb.DMatrix(data = X_test_angle, label = y_test_angle)

# Define XGBoost parameters
xgb_params <- list(
  objective = "reg:squarederror",  # Regression for predicting angles
  eval_metric = "rmse",            # Root Mean Squared Error
  max_depth = 6,                   # Tree depth
  eta = 0.05,                      # Learning rate
  subsample = 0.8,                 # Subsample ratio
  colsample_bytree = 0.8           # Feature sampling
)

# Perform k-fold cross-validation
cv_results_angle <- xgb.cv(
  params = xgb_params,
  data = dmatrix_train_angle,
  nrounds = 2000,
  nfold = 5,  
  early_stopping_rounds = 20,
  verbose = 1
)

print(cv_results_angle)

# Train the final model with optimal rounds
best_nrounds <- cv_results_angle$best_iteration

final_model_angle <- xgb.train(
  params = xgb_params,
  data = dmatrix_angle,
  nrounds = best_nrounds,
  verbose = 1
)

# Feature importance plot
importance_matrix <- xgb.importance(feature_names = colnames(X_angle), model = final_model_angle)
xgb.plot.importance(importance_matrix)

# Test set prediction
dtest_angle <- xgb.DMatrix(data = X_test_angle)
predictions <- predict(final_model_angle, dtest_angle)

# Compute RMSE on holdout test set
rmse_test <- sqrt(mean((predictions - y_test_angle)^2))
print(paste("Test RMSE:", rmse_test))

# Create residuals
residuals <- y_test_angle - predictions

# Residuals vs. Fitted Plot
ggplot(data = data.frame(Fitted = predictions, Residuals = residuals), aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Fitted", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

# Actual vs. Predicted Plot
ggplot(data = data.frame(Actual = y_test_angle, Predicted = predictions), aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Actual vs. Predicted", x = "Actual Shot Angle", y = "Predicted Shot Angle") +
  theme_minimal()




#I feel like there are non-linear relationships in this since the goalie could have different reactions
set.seed(2025)
train_indices <- sample(1:nrow(ShotsWithPreviousShotDataEncoded), 
                        size = 0.8 * nrow(ShotsWithPreviousShotDataEncoded))
X_train_angle <- X_angle[train_indices, ]
X_test_angle <- X_angle[-train_indices, ]
y_train_angle <- y_angle[train_indices]
y_test_angle <- y_angle[-train_indices]

# 2. Create polynomial features (X^2 and X^3) for both training and test sets
X_train_angle_poly <- cbind(X_train_angle, X_train_angle^2, X_train_angle^3)
X_test_angle_poly <- cbind(X_test_angle, X_test_angle^2, X_test_angle^3)

# Convert both the training and test sets to XGBoost DMatrix format
dmatrix_train_angle <- xgb.DMatrix(data = as.matrix(X_train_angle_poly), label = y_train_angle)
dmatrix_test_angle <- xgb.DMatrix(data = as.matrix(X_test_angle_poly), label = y_test_angle)

# 3. Define XGBoost parameters for the model
xgb_params <- list(
  objective = "reg:squarederror",  # Regression for predicting angles
  eval_metric = "rmse",            # Root Mean Squared Error
  max_depth = 10,                  # Deeper trees for non-linearity
  eta = 0.01,                      # Learning rate (lower for gradual learning)
  subsample = 0.8,                 # Subsample to reduce overfitting
  colsample_bytree = 0.8,          # Feature sampling
  min_child_weight = 1,            # Allow deeper trees (captures more complexity)
  gamma = 0.1                      # Small gamma for regularization
)

# 4. Train the model with cross-validation (for optimal boosting rounds)
cv_results_angle <- xgb.cv(
  params = xgb_params,
  data = dmatrix_train_angle,
  nrounds = 2000,
  nfold = 5,  # 5-fold cross-validation
  early_stopping_rounds = 20,
  verbose = 1
)

# 5. Final training with the optimal number of rounds from CV
final_model_angle <- xgb.train(
  params = xgb_params,
  data = dmatrix_train_angle,
  nrounds = cv_results_angle$best_iteration,  # Optimal number of rounds from CV
  verbose = 1
)

# 6. Apply the model to the test set
predictions <- predict(final_model_angle, dmatrix_test_angle)

# 7. Evaluate the model using RMSE (Root Mean Squared Error)
rmse_test <- sqrt(mean((predictions - y_test_angle)^2))
print(paste("Test RMSE:", rmse_test))

# 8. Evaluate R-squared (coefficient of determination)
ss_total <- sum((y_test_angle - mean(y_test_angle))^2)
ss_residual <- sum((y_test_angle - predictions)^2)
r_squared <- 1 - (ss_residual / ss_total)
print(paste("Test R-squared:", r_squared))

# 9. Optional: Plot Actual vs. Predicted
plot(y_test_angle, predictions, 
     xlab = "Actual Values", ylab = "Predicted Values", 
     main = "Actual vs Predicted")
abline(0, 1, col = "red")  # Line of perfect prediction

# 10. Optional: Residuals vs. Fitted Plot
residuals <- y_test_angle - predictions
plot(predictions, residuals, 
     xlab = "Fitted Values", ylab = "Residuals", 
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

# 11. Optional: Feature Importance
importance_matrix <- xgb.importance(feature_names = colnames(X_train_angle_poly), model = final_model_angle)
xgb.plot.importance(importance_matrix)


#trying with a log transformation since there was a pattern on the previous two
ShotsWithPreviousShotDataEncoded <- as.data.frame(ShotsWithPreviousShotDataEncoded)

# Define target and predictor variables
y_angle <- ShotsWithPreviousShotDataEncoded$shotAngle
X_angle <- ShotsWithPreviousShotDataEncoded %>% select(-shotAngle) %>% as.matrix()

# 2. Split the data into training and test sets (80-20 split)
set.seed(2025)
train_indices <- sample(1:nrow(ShotsWithPreviousShotDataEncoded), size = 0.8 * nrow(ShotsWithPreviousShotDataEncoded))
X_train_angle <- X_angle[train_indices, ]
X_test_angle <- X_angle[-train_indices, ]
y_train_angle <- y_angle[train_indices]
y_test_angle <- y_angle[-train_indices]

# 3. Shift the target angle variable (ensure all values are positive)
shift_value <- abs(min(y_train_angle)) + 1  # Add the absolute value of the minimum angle + 1
y_train_angle_shifted <- y_train_angle + shift_value
y_test_angle_shifted <- y_test_angle + shift_value

# 4. Apply log transformation to the shifted angles
y_train_angle_log <- log(y_train_angle_shifted)
y_test_angle_log <- log(y_test_angle_shifted)

# 5. Create polynomial features (X^2 and X^3) for both training and test sets
X_train_angle_poly <- cbind(X_train_angle, X_train_angle^2, X_train_angle^3)
X_test_angle_poly <- cbind(X_test_angle, X_test_angle^2, X_test_angle^3)

# Convert both the training and test sets to XGBoost DMatrix format
dmatrix_train_angle <- xgb.DMatrix(data = as.matrix(X_train_angle_poly), label = y_train_angle_log)
dmatrix_test_angle <- xgb.DMatrix(data = as.matrix(X_test_angle_poly), label = y_test_angle_log)

# 6. Define XGBoost parameters with non-linear optimization
xgb_params <- list(
  objective = "reg:squarederror",  # Regression for predicting angles
  eval_metric = "rmse",            # Root Mean Squared Error
  max_depth = 10,                  # Deeper trees for non-linearity
  eta = 0.01,                      # Learning rate (lower for gradual learning)
  subsample = 0.8,                 # Subsample to reduce overfitting
  colsample_bytree = 0.8,          # Feature sampling
  min_child_weight = 1,            # Allow deeper trees (captures more complexity)
  gamma = 0.1                      # Small gamma for regularization
)

# 7. Train the model with cross-validation
cv_results_angle <- xgb.cv(
  params = xgb_params,
  data = dmatrix_train_angle,
  nrounds = 2000,
  nfold = 5,  # 5-fold cross-validation
  early_stopping_rounds = 20,
  verbose = 1
)

# 8. Final training with the optimal number of rounds
final_model_angle <- xgb.train(
  params = xgb_params,
  data = dmatrix_train_angle,
  nrounds = cv_results_angle$best_iteration,  # Optimal number of rounds from CV
  verbose = 1
)

# 9. Make predictions on the test set
predictions_log <- predict(final_model_angle, dmatrix_test_angle)

# 10. Reverse the log transformation (unshift and exponentiate)
predictions_shifted <- exp(predictions_log) - shift_value

# 11. Evaluate the model performance
rmse_test <- sqrt(mean((predictions_shifted - y_test_angle)^2))
print(paste("Test RMSE:", rmse_test))

# 12. Calculate R-squared (coefficient of determination)
ss_total <- sum((y_test_angle - mean(y_test_angle))^2)
ss_residual <- sum((y_test_angle - predictions_shifted)^2)
r_squared <- 1 - (ss_residual / ss_total)
print(paste("Test R-squared:", r_squared))

# 13. Plot Residuals vs Fitted
residuals <- predictions_shifted - y_test_angle
fitted_values <- predictions_shifted

ggplot(data.frame(fitted = fitted_values, residuals = residuals), aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ggtitle("Residuals vs Fitted Values") +
  xlab("Fitted Values") +
  ylab("Residuals")

# 14. Plot Actual vs Predicted
ggplot(data.frame(actual = y_test_angle, predicted = predictions_shifted), aes(x = actual, y = predicted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "blue", linetype = "dashed") +
  ggtitle("Actual vs Predicted Values") +
  xlab("Actual Values") +
  ylab("Predicted Values")

#trying with random forest model instead
train_indices <- sample(1:nrow(ShotsWithPreviousShotDataEncoded), 
                        size = 0.8 * nrow(ShotsWithPreviousShotDataEncoded))
X_train_angle <- X_angle[train_indices, ]
X_test_angle <- X_angle[-train_indices, ]
y_train_angle <- y_angle[train_indices]
y_test_angle <- y_angle[-train_indices]

# Apply the shift transformation if necessary (for angles)
y_train_angle_shifted <- y_train_angle + 180
y_test_angle_shifted <- y_test_angle + 180

# Apply polynomial features (X^2 and X^3)
X_train_angle_poly <- cbind(X_train_angle, X_train_angle^2, X_train_angle^3)
X_test_angle_poly <- cbind(X_test_angle, X_test_angle^2, X_test_angle^3)

rf_model_angle <- randomForest(
  x = X_train_angle_poly,  # Feature matrix
  y = y_train_angle_shifted,  # Target variable (shifted angles)
  ntree = 1000,            # Number of trees (increase for better fit)
  mtry = 5,                # Number of features to try at each split
  importance = TRUE,       # Track feature importance
  maxnodes = 100          # Limit the number of nodes to avoid overfitting
)

# Print model summary
print(rf_model_angle)

predictions_rf <- predict(rf_model_angle, X_test_angle_poly)

# Calculate RMSE (Root Mean Squared Error)
rmse_test_rf <- sqrt(mean((predictions_rf - y_test_angle_shifted)^2))
print(paste("Test RMSE:", rmse_test_rf))

# Calculate R-squared
ss_total <- sum((y_test_angle_shifted - mean(y_test_angle_shifted))^2)
ss_residual <- sum((y_test_angle_shifted - predictions_rf)^2)
r_squared_rf <- 1 - (ss_residual / ss_total)
print(paste("Test R-squared:", r_squared_rf))

residuals_rf <- predictions_rf - y_test_angle_shifted

# Plot Residuals vs Fitted
plot(predictions_rf, residuals_rf,
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted Plot")
abline(h = 0, col = "red")


importance_rf <- importance(rf_model_angle)
varImpPlot(rf_model_angle, main = "Feature Importance for Random Forest Model")


#random forest model no transformation
train_indices <- sample(1:nrow(ShotsWithPreviousShotDataEncoded), 
                        size = 0.8 * nrow(ShotsWithPreviousShotDataEncoded))
X_train_angle <- X_angle[train_indices, ]
X_test_angle <- X_angle[-train_indices, ]
y_train_angle <- y_angle[train_indices]
y_test_angle <- y_angle[-train_indices]

rf_model_angle_raw <- randomForest(
  x = X_train_angle,  # Feature matrix (no polynomial features)
  y = y_train_angle,  # Target variable (no shift applied)
  ntree = 1000,       # Number of trees (increase for better fit)
  mtry = 5,           # Number of features to try at each split
  importance = TRUE,  # Track feature importance
  maxnodes = 100      # Limit the number of nodes to avoid overfitting
)

# Print model summary
print(rf_model_angle_raw)

predictions_rf_raw <- predict(rf_model_angle_raw, X_test_angle)

# Calculate RMSE (Root Mean Squared Error)
rmse_test_rf_raw <- sqrt(mean((predictions_rf_raw - y_test_angle)^2))
print(paste("Test RMSE:", rmse_test_rf_raw))

# Calculate R-squared
ss_total_raw <- sum((y_test_angle - mean(y_test_angle))^2)
ss_residual_raw <- sum((y_test_angle - predictions_rf_raw)^2)
r_squared_rf_raw <- 1 - (ss_residual_raw / ss_total_raw)
print(paste("Test R-squared:", r_squared_rf_raw))

residuals_rf_raw <- predictions_rf_raw - y_test_angle

# Plot Residuals vs Fitted
plot(predictions_rf_raw, residuals_rf_raw,
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted Plot (No Transformations)")
abline(h = 0, col = "red")

importance_rf_raw <- importance(rf_model_angle_raw)
varImpPlot(rf_model_angle_raw, main = "Feature Importance for Random Forest Model (No Transformations)")


colnames(ShotsWithPreviousShotData)
anglePredictionLM <- lm(shotAngle ~ previousAngle + previousDistance + as.factor(previousHandedness) +
                          as.factor(previousRush) + as.factor(previousHandedness) + as.factor(previousShotType), data = ShotsWithPreviousShotData)
summary(anglePredictionLM)
plot(anglePredictionLM$fitted.values, resid(anglePredictionLM),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Plot")
abline(h = 0, col = "red")
qqnorm(resid(anglePredictionLM), main = "QQ Plot of Residuals")
qqline(resid(anglePredictionLM), col = "red")


ShotsWithPreviousShotData$shotAngle_shifted <- ShotsWithPreviousShotData$shotAngle + 180  # Shift angles to positive range
ShotsWithPreviousShotData$shotAngle_log <- log(ShotsWithPreviousShotData$shotAngle_shifted)

# Fit the transformed model
anglePredictionLM_log <- lm(shotAngle_log ~ previousAngle + previousDistance + as.factor(previousHandedness) +
                              as.factor(previousRush) + as.factor(previousShotType) +
                              previousXCoord + previousYCoord, data = ShotsWithPreviousShotData)

# Model summary
summary(anglePredictionLM_log)

anglePredictionLM_poly <- lm(shotAngle ~ poly(previousAngle, 3) + poly(previousDistance, 3) + 
                               as.factor(previousHandedness) + as.factor(previousRush) + 
                               as.factor(previousShotType), 
                             data = ShotsWithPreviousShotData)

summary(anglePredictionLM_poly)
par(mfrow = c(2, 2))  # 4 diagnostic plots in one figure
plot(anglePredictionLM_log)
plot(anglePredictionLM_poly)

library(splines)
anglePredictionLM_spline <- lm(shotAngle ~ ns(previousAngle, df = 3) + 
                                 ns(previousDistance, df = 3), 
                               data = ShotsWithPreviousShotData)
summary(anglePredictionLM_spline)
plot(anglePredictionLM_spline$fitted.values, anglePredictionLM_spline$residuals)

anglePredictionLM_interact <- lm(shotAngle ~ poly(previousAngle, 3) * poly(previousDistance, 3) + 
                                   as.factor(previousShotType), 
                                 data = ShotsWithPreviousShotData)
summary(anglePredictionLM_interact)
plot(anglePredictionLM_interact$fitted.values, anglePredictionLM_interact$residuals)

library(mgcv)
anglePredictionGAM <- gam(shotAngle ~ s(previousAngle, bs = "cs") + 
                            s(previousDistance, bs = "cs") + 
                            as.factor(previousShotType), 
                          data = ShotsWithPreviousShotData)
summary(anglePredictionGAM)
plot(anglePredictionGAM, pages = 1)
plot(anglePredictionGAM$fitted.values, anglePredictionGAM$residuals)

library(brms)
anglePredictionBayes <- brm(shotAngle ~ poly(previousAngle, 3) + poly(previousDistance, 3) + 
                              as.factor(previousShotType), 
                            data = ShotsWithPreviousShotData, family = gaussian())
summary(anglePredictionBayes)
xAngleBayesfitted_values <- fitted(anglePredictionBayes)[, "Estimate"]  # Extract fitted values
xAngleBayesresiduals <- ShotsWithPreviousShotData$shotAngle - fitted_values  # Compute residuals

plot(xAngleBayesfitted_values, xAngleBayesresiduals, main = "Residuals vs. Fitted", 
     xlab = "Fitted Values", ylab = "Residuals", pch = 20)
abline(h = 0, col = "red")


anglePredictionBayes2 <- brm(shotAngle ~ poly(previousAngle, 3) * poly(previousDistance, 3) + 
                              as.factor(previousShotType), 
                            data = ShotsWithPreviousShotData, family = gaussian())
summary(anglePredictionBayes2)
xAngleBayesfitted_values2 <- fitted(anglePredictionBayes2)[, "Estimate"]  # Extract fitted values
xAngleBayesresiduals2 <- ShotsWithPreviousShotData$shotAngle - fitted_values  # Compute residuals

plot(xAngleBayesfitted_values2, xAngleBayesresiduals2, main = "Bayesian Residuals vs. Fitted", 
     xlab = "Fitted Values", ylab = "Residuals", pch = 20)
abline(h = 0, col = "red")

bayes_R2(anglePredictionBayes2)
bayes_R2(anglePredictionBayes)
loo(anglePredictionBayes)
loo(anglePredictionBayes2)
#going to use the the second bayes model to model the xAngle

#creating an xGoals model based off of xRebound, xAngle, fixed distance

#pulling all xRebounds for creating the model
xReboundsData2023_1 <- shots_2023 %>%
  dplyr::select(shotAngle, arenaAdjustedShotDistance, shooterLeftRight, 
                shotRush, shotType, shotID, shooterName)

xReboundsData2023_2 <- xReboundsData2023_1 %>%
  dplyr::select(shotAngle, arenaAdjustedShotDistance, shooterLeftRight, 
                shotRush, shotType)

xReboundsData2023_2$shooterLeftRight <- as.factor(xReboundsData2023_2$shooterLeftRight)
xReboundsData2023_2$shotRush <- as.factor(xReboundsData2023_2$shotRush)

xReboundsMatrix2023 <- model.matrix(~ . -1, data = xReboundsData2023_2)


colnames(xReboundsMatrix2023)
colnames(X)
xReboundsRecode2023 <- as.data.frame(xReboundsMatrix2023)
head(xReboundsRecode2023)
xReboundsRecode2023 <- xReboundsRecode2023 %>%
  dplyr::select(shotAngle, arenaAdjustedShotDistance, shooterLeftRightL, shooterLeftRightR,
                shotRush1, shotTypeBACK, shotTypeDEFL, shotTypeSLAP, shotTypeSNAP, shotTypeTIP, 
                shotTypeWRAP, shotTypeWRIST)
xReboundsRecodeMatrix2023 <- as.matrix(xReboundsRecode2023)
colnames(xReboundsRecode2023)
colnames(X)


shots_2023_1$xReboundsNewModel <- predict(xReb_final_model5, xReboundsRecodeMatrix2023)

#pulling all xAngles for the model
DataForxAngle <- shots_2023 %>%
  dplyr::select(shotAngle, arenaAdjustedShotDistance, shotType) %>%
  rename(
    previousAngle = shotAngle,
    previousDistance = arenaAdjustedShotDistance,
    previousShotType = shotType
  )

# Generate fitted values
fitted_values <- fitted(anglePredictionBayes2, newdata = DataForxAngle, summary = TRUE)

# Add predictions to the original dataframe
shots_2023$xAngleBayes <- fitted_values[, "Estimate"]
summary(shots_2023$xAngleBayes)
summary(shots_2023$shotAngle)

#creating xGoals Neural Network
#average distance of a rebound is 15.72131
ReboundsOnly2023 <- shots_2023 %>%
  filter(shotRebound == 1) %>%
  dplyr::select(arenaAdjustedShotDistance)

mean(ReboundsOnly2023$arenaAdjustedShotDistance)

xGoalData <- shots_2023 %>%
  mutate(goalAfterShot = lead(goal),
         rebound_shot_distance = lead(arenaAdjustedShotDistance)) %>%
  filter(shotGeneratedRebound == 1) %>%
  dplyr::select(goalAfterShot, xReboundsNewModel, xAngleBayes, rebound_shot_distance)

set.seed(2025)


#creating an xgboost with cross validation for the xGoals model
set.seed(2025)
xGoals_X <- as.matrix(xGoalData[, -1])
xGoals_Y <- as.numeric(as.character(xGoalData$goalAfterShot))

#second iteration of model
xGoals_dtrain <- xgb.DMatrix(data = xGoals_X, label = xGoals_Y)

xGoals_params <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  max_depth = 4,  
  eta = 0.07,  
  subsample = 0.8,  
  colsample_bytree = 0.8,  
  scale_pos_weight = sum(xGoals_Y == 0) / sum(xGoals_Y == 1),
  lambda = 5, 
  alpha = 1 
)


# Perform cross-validation
xGoals_cv_results <- xgb.cv(
  params = xGoals_params,
  data = xGoals_dtrain,
  nrounds = 2000,
  nfold = 5,               
  showsd = TRUE,           
  stratified = TRUE,       
  print_every_n = 10,      
  early_stopping_rounds = 10, 
  prediction = TRUE
)


print(xGoals_cv_results)
xGoals_best_nrounds <- xGoals_cv_results$best_iteration

# Train final model
xGoals_final_model2 <- xgb.train(
  params = xGoals_params,
  data = xGoals_dtrain,
  nrounds = xGoals_best_nrounds
)

# Predictions
xGoals_preds <- predict(xGoals_final_model2, xGoals_X)

# Evaluate new model
xGoal_roc_curve <- roc(xGoals_Y, xGoals_preds)
print(auc(xGoal_roc_curve))  # AUC Score


# Classification thresholding
xGoal_pred_labels <- ifelse(xGoals_preds >= 0.5, 1, 0)

# Accuracy
xGoal_accuracy <- sum(xGoal_pred_labels == xGoals_Y) / length(xGoals_Y)
print(paste("Accuracy of the second model: ", xGoal_accuracy))

# Confusion Matrix
xGoal_conf_matrix2 <- confusionMatrix(factor(xGoal_pred_labels), factor(xGoals_Y))
print(xGoal_conf_matrix2)

#Final Dataframe Creation
xReboundsPlus2023 <- shots_2023 %>%
  dplyr::select(xReboundsNewModel, xAngleBayes) %>%
  mutate(rebound_shot_distance = 15.72131)

xReboundsPlus2023Matrix <- as.matrix(xReboundsPlus2023)



shots_2023$xReboundsPlus <- predict(xGoals_final_model2, xReboundsPlus2023Matrix)


xReboundsPlusByPlayer <- shots_2023 %>%
  group_by(shooterName) %>%
  summarise(
    total_xReboundsPlus = sum(xReboundsPlus, na.rm = TRUE),
    total_xRebounds = sum(xReboundsNewModel, na.rm = TRUE),
    total_shots = n() 
  ) %>%
  arrange(desc(total_xReboundsPlus))

xReboundsPlusByPlayer <- xReboundsPlusByPlayer %>%
  mutate(total_xRebounds = format(total_xRebounds, scientific = FALSE),
         total_xReboundsPlus = format(total_xReboundsPlus, scientific = FALSE))

gt(xReboundsPlusByPlayer)

gt(xReboundsPlusByPlayer) |>
  tab_header(
    title = md("xRebounds 2023-24 Season"))


#implementing xRebounds Plus on 2024-25 shots as of the morning of 3/26
shots_2024_25_1 <- shots_2024_25

xReboundsData2024_1 <- shots_2024_25_1 %>%
  dplyr::select(shotAngle, arenaAdjustedShotDistance, shooterLeftRight, 
                shotRush, shotType, shotID, shooterName)

xReboundsData2024_2 <- xReboundsData2024_1 %>%
  dplyr::select(shotAngle, arenaAdjustedShotDistance, shooterLeftRight, 
                shotRush, shotType)

xReboundsData2024_2$shooterLeftRight <- as.factor(xReboundsData2024_2$shooterLeftRight)
xReboundsData2024_2$shotRush <- as.factor(xReboundsData2024_2$shotRush)

xReboundsMatrix2024 <- model.matrix(~ . -1, data = xReboundsData2024_2)


colnames(xReboundsMatrix2024)
colnames(X)
xReboundsRecode2024 <- as.data.frame(xReboundsMatrix2024)
head(xReboundsRecode2024)
xReboundsRecode2024 <- xReboundsRecode2024 %>%
  dplyr::select(shotAngle, arenaAdjustedShotDistance, shooterLeftRightL, shooterLeftRightR,
                shotRush1, shotTypeBACK, shotTypeDEFL, shotTypeSLAP, shotTypeSNAP, shotTypeTIP, 
                shotTypeWRAP, shotTypeWRIST)
xReboundsRecodeMatrix2024 <- as.matrix(xReboundsRecode2024)
colnames(xReboundsRecode2024)
colnames(X)


shots_2024_25_1$xReboundsNewModel <- predict(xReb_final_model5, xReboundsRecodeMatrix2024)

#pulling all xAngles for the model
DataForxAngle2024 <- shots_2024_25_1 %>%
  dplyr::select(shotAngle, arenaAdjustedShotDistance, shotType) %>%
  rename(
    previousAngle = shotAngle,
    previousDistance = arenaAdjustedShotDistance,
    previousShotType = shotType
  )

# Generate fitted values
fitted_values2024 <- fitted(anglePredictionBayes2, newdata = DataForxAngle2024, summary = TRUE)
shots_2024_25_1$xAngleBayes <- fitted_values2024[, "Estimate"]


xReboundsPlus2024 <- shots_2024_25_1 %>%
  dplyr::select(xReboundsNewModel, xAngleBayes) %>%
  mutate(rebound_shot_distance = 15.72131)

xReboundsPlus2024Matrix <- as.matrix(xReboundsPlus2024)

shots_2024_25_1$xReboundsPlus <- predict(xGoals_final_model2, xReboundsPlus2024Matrix)


xReboundsPlusByPlayer2024 <- shots_2024_25_1 %>%
  group_by(shooterName) %>%
  summarise(
    total_xReboundsPlus = sum(xReboundsPlus, na.rm = TRUE),
    total_xRebounds = sum(xReboundsNewModel, na.rm = TRUE),
    total_shots = n(),
    teamCode = last(teamCode)
  ) %>%
  arrange(desc(total_xReboundsPlus))

xReboundsPlusByPlayer2024 <- xReboundsPlusByPlayer2024 %>%
  mutate(total_xRebounds = format(total_xRebounds, scientific = FALSE),
         total_xReboundsPlus = format(total_xReboundsPlus, scientific = FALSE))

xReboundsPlusByPlayer2024WithLogo <- xReboundsPlusByPlayer2024 %>%
  mutate(
    img = case_when(
      teamCode == "DAL" ~
        "https://upload.wikimedia.org/wikipedia/en/c/ce/Dallas_Stars_logo_%282013%29.svg",
      teamCode == "CGY" ~
        "https://upload.wikimedia.org/wikipedia/en/6/61/Calgary_Flames_logo.svg",
      teamCode == "BOS" ~
        "https://upload.wikimedia.org/wikipedia/commons/1/12/Boston_Bruins.svg",
      teamCode == "NYI" ~
        "https://upload.wikimedia.org/wikipedia/en/4/42/Logo_New_York_Islanders.svg",
      teamCode == "OTT" ~
        "https://upload.wikimedia.org/wikipedia/en/b/b2/Ottawa_Senators_2020-2021_logo.svg",
      teamCode == "CBJ" ~
        "https://upload.wikimedia.org/wikipedia/en/5/5d/Columbus_Blue_Jackets_logo.svg",
      teamCode == "SJS" ~
        "https://upload.wikimedia.org/wikipedia/en/3/37/SanJoseSharksLogo.svg",
      teamCode == "VGK" ~
        "https://upload.wikimedia.org/wikipedia/en/a/ac/Vegas_Golden_Knights_logo.svg",
      teamCode == "UTA" ~
        "https://upload.wikimedia.org/wikipedia/commons/9/95/Utah_Hockey_Club_2024-25_Logo.svg",
      teamCode == "SEA" ~
        "https://upload.wikimedia.org/wikipedia/en/4/48/Seattle_Kraken_official_logo.svg",
      teamCode == "ANA" ~
        "https://upload.wikimedia.org/wikipedia/en/9/95/Anaheim_Ducks_logo_2024.svg",
      teamCode == "STL" ~
        "https://upload.wikimedia.org/wikipedia/en/e/ed/St._Louis_Blues_logo.svg",
      teamCode == "TOR" ~
        "https://upload.wikimedia.org/wikipedia/en/b/b6/Toronto_Maple_Leafs_2016_logo.svg",
      teamCode == "BUF" ~
        "https://upload.wikimedia.org/wikipedia/en/9/9e/Buffalo_Sabres_Logo.svg",
      teamCode == "NJD" ~
        "https://upload.wikimedia.org/wikipedia/en/9/9f/New_Jersey_Devils_logo.svg",
      teamCode == "TBL" ~
        "https://upload.wikimedia.org/wikipedia/commons/3/31/Tampa_Bay_Lightning_2011.svg",
      teamCode == "COL" ~
        "https://upload.wikimedia.org/wikipedia/en/4/45/Colorado_Avalanche_logo.svg",
      teamCode == "CHI" ~
        "https://upload.wikimedia.org/wikipedia/en/2/29/Chicago_Blackhawks_logo.svg",
      teamCode == "NYR" ~
        "https://upload.wikimedia.org/wikipedia/commons/a/ae/New_York_Rangers.svg",
      teamCode == "MTL" ~
        "https://upload.wikimedia.org/wikipedia/commons/6/69/Montreal_Canadiens.svg",
      teamCode == "WSH" ~
        "https://upload.wikimedia.org/wikipedia/commons/2/2d/Washington_Capitals.svg",
      teamCode == "PHI" ~
        "https://upload.wikimedia.org/wikipedia/en/d/dc/Philadelphia_Flyers.svg",
      teamCode == "NSH" ~
        "https://upload.wikimedia.org/wikipedia/en/9/9c/Nashville_Predators_Logo_%282011%29.svg",
      teamCode == "VAN" ~
        "https://upload.wikimedia.org/wikipedia/en/3/3a/Vancouver_Canucks_logo.svg",
      teamCode == "CAR" ~
        "https://upload.wikimedia.org/wikipedia/en/3/32/Carolina_Hurricanes.svg",
      teamCode == "EDM" ~
        "https://upload.wikimedia.org/wikipedia/en/4/4d/Logo_Edmonton_Oilers.svg",
      teamCode == "PIT" ~
        "https://upload.wikimedia.org/wikipedia/en/c/c0/Pittsburgh_Penguins_logo_%282016%29.svg",
      teamCode == "MIN" ~
        "https://upload.wikimedia.org/wikipedia/en/1/1b/Minnesota_Wild.svg",
      teamCode == "WPG" ~
        "https://upload.wikimedia.org/wikipedia/en/9/93/Winnipeg_Jets_Logo_2011.svg",
      teamCode == "DET" ~
        "https://upload.wikimedia.org/wikipedia/en/e/e0/Detroit_Red_Wings_logo.svg",
      teamCode == "LAK" ~
        "https://upload.wikimedia.org/wikipedia/en/c/c4/Los_Angeles_Kings_2024_Logo.svg",
      teamCode == "FLA" ~
        "https://upload.wikimedia.org/wikipedia/en/4/43/Florida_Panthers_2016_logo.svg",
      TRUE ~ "NA"))

gt(xReboundsPlusByPlayer2024WithLogo)

xReboundsPlusByPlayer2024WithLogo %>%
  select(img, shooterName, total_xReboundsPlus, total_xRebounds, total_shots) %>% # Reorder columns
  gt() |>
  text_transform(
    locations = cells_body(columns = img),
    fn = function(x) web_image(url = x, height = 30) # Display team logos
  ) |>
  cols_label(
    img = "Team",
    shooterName = "Player",
    total_xReboundsPlus = "xRebounds+",
    total_xRebounds = "xRebounds",
    total_shots = "Shots"
  ) |>
  tab_header(
    title = md("**xRebounds+ 2024-25 Season**")
  ) |>
  cols_align(
    align = "center",
    columns = c(img, total_xReboundsPlus, total_xRebounds, total_shots)
  )



