library(tidyverse)
library(dplyr)
library(caret)
library(estimatr)
library(knitr)
library(ROCR)
library(data.table)

data <- read.csv("imputed_df.csv")
#head(data)
#table(data$diabetes_mellitus)

test_set <- read.csv("unlabeled.csv")

# same thing we've seen before: d1_glucose_max, glucose_apache, etc

# let's focus on numeric values' correlations against our target
correlations <- cor(data %>% 
                      select_if(is.numeric) %>% 
                      # deleting readmission_status as it caused NA values in the matrix
                      # all of its values were 0
                      select(-c(readmission_status)), as.numeric(data$diabetes_mellitus))

# check what correlates with diabetes the most - looking at the top 15 values 
# selected 16 for the positive as diabetes will come first for sure
positive_correlations <- as.data.frame(correlations[order(correlations[,1],decreasing=TRUE),][1:15])
negative_correlations <- as.data.frame(correlations[order(correlations[,1],decreasing=FALSE),][1:15])

# create dfs from the correlation matrices
names(positive_correlations) <- "Coefficient"
positive_correlations <- rownames_to_column(positive_correlations, "Variable")

names(negative_correlations) <- "Coefficient"
negative_correlations <- rownames_to_column(negative_correlations, "Variable")

# bind them together
top_correlations <- rbind(positive_correlations, negative_correlations)

# sort by Coefficient & remove the label
top_correlations <- top_correlations[order(top_correlations[,"Coefficient"], decreasing = TRUE),][-1,]

## Factor variables

factor_variables <- data %>% 
  select_if(is.factor) %>% 
  colnames()

## Split data into training and validation sets
# import library(caret)

training_ratio <- 0.70

set.seed(8879)
train_indices <- createDataPartition(
  y = data[["diabetes_mellitus"]],
  times = 1,
  p = training_ratio,
  list = FALSE
)

data_train <- data[train_indices, ]
data_test <- data[-train_indices, ]

dim(data_train)
dim(data_test)

# set up cross validation
train_control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  savePredictions = TRUE
)

write.csv(data_train, "training_split.csv")
write.csv(data_test, "validation_split.csv")
## Baseline model------------------------------------------------------------------------------------------------------------------------------------------

# I'm going to take the top 4 variables and add gender to them
# this way we can get a baseline model. 
# I'm going to add gender to the model as well. 
# 
# library(estimatr)
# library(knitr)

# remind me what the top correlations were
top_correlations

# this is a pretty shitty model. will need to do better than this. 
# try something else. 

model2 <- glm(diabetes_mellitus ~ d1_glucose_max, family = "binomial", data = data_train)
summary(model2)

## bring in more variables

model3 <- glm(paste0("diabetes_mellitus ~ ", paste0(top_correlations[1:5,1], collapse = " + ")), family = "binomial", data = data_train)
summary(model3)

varImp(model3)

# let's not forget about adding gender back in
model4 <- glm(paste0("diabetes_mellitus ~ ", paste0(factor_variables, collapse = " + "), " + ", paste0(top_correlations[,1], collapse = " + ")), family = "binomial", data = data_train)
summary(model4)
varImp(model4)

### Logit ------------------------------------------------------------------------------------------------------------------------------


# train simple logit model
set.seed(13505)
glm_model <- train(
  formula(paste0("diabetes_mellitus ~", paste0(factor_variables, collapse = " + ")," + ", paste0(top_correlations[1:5,1], collapse = " + "))),
  method = "glm",
  data = data_train,
  family = binomial,
  trControl = train_control
)

# check accuracy of folds
glm_model$resample

# add predictions to test set
validation_prediction_probs <- predict.train(glm_model, 
                                       newdata = data_test, 
                                       type = "prob")

data_test$diabetes_pred <- validation_prediction_probs$Yes

# check performance on validation set
# library(ROCR)

glm_prediction <- prediction(validation_prediction_probs$Yes,
                             data_test[["diabetes_mellitus"]])

(AUC_test <- performance(glm_prediction, "auc")@y.values)
# pretty crappy score.but let's upload to kaggle just as a starter

## predict for actual holdout set

# add predictions to holdout set
test_prediction_probs <- predict.train(glm_model, 
                                       newdata = test_set, 
                                       type = "prob")
test_set$diabetes<- test_prediction_probs$Yes

# put together test file
# library(data.table)

glm_submission <- data.table(
  encounter_id = test_set$encounter_id,
  diabetes_mellitus = test_set$diabetes)

write.csv(glm_submission, "glm_submission.csv", row.names=FALSE)

### LASSO ------------------------------------------------------------------------------------------------------------------

# let's assign different lambdas
lambdas <- 10^seq(-2, -3, by = -0.25)

# tune grid params
lasso_tune_grid <- expand.grid(
  "alpha" = c(0.5,0.75,1),
  "lambda" = c(lambdas, lambdas / 2) 
)

# run model
set.seed(13505)
lasso <- train(
  diabetes_mellitus ~ .,
  data = data_train,
  method = "glmnet",
  preProcess = c("center", "scale"),
  tuneGrid = lasso_tune_grid,
  trControl = train_control
)

# add predictions to test set
validation_prediction_probs <- predict.train(lasso, 
                                             newdata = data_test, 
                                             type = "prob")

data_test$diabetes_pred <- validation_prediction_probs$Yes

# check performance on validation set
lasso_prediction <- prediction(validation_prediction_probs$Yes,
                             data_test[["diabetes_mellitus"]])

(AUC_test <- performance(lasso_prediction, "auc")@y.values)

## predict for actual holdout set

# add predictions to holdout set
test_prediction_probs <- predict.train(lasso, 
                                       newdata = test_set, 
                                       type = "prob")
test_set$diabetes<- test_prediction_probs$Yes

# submission
lasso_submission <- data.table(
  encounter_id = test_set$encounter_id,
  diabetes_mellitus = test_set$diabetes)

write.csv(lasso_submission, "lasso_submission.csv", row.names=FALSE)

## RANDOM FOREST --------------------------------------------------------------

# tune params
tune_grid <- expand.grid(
  .mtry = c(5, 7, 9),
  .splitrule = "gini",
  .min.node.size = c(7, 9, 10)
)

# set seed and run model
set.seed(13505)
rf_model <- train(diabetes_mellitus ~ .,
                  method = "ranger",
                  data = data_train,
                  trControl = train_control,
                  tuneGrid = tune_grid,
                  importance = "impurity",
                  na.action = na.omit
)

# save model to file
saveRDS(rf_model, "rf_model.rds")

# add predictions to test set
validation_prediction_probs <- predict.train(rf_model, 
                                             newdata = data_test, 
                                             type = "prob")

data_test$diabetes_pred <- validation_prediction_probs$Yes

# check performance on validation set
rf_prediction <- prediction(validation_prediction_probs$Yes,
                               data_test[["diabetes_mellitus"]])

(AUC_test <- performance(rf_prediction, "auc")@y.values)

## predict for actual holdout set

# add predictions to holdout set
test_prediction_probs <- predict.train(rf_model, 
                                       newdata = test_set, 
                                       type = "prob")
test_set$diabetes<- test_prediction_probs$Yes

# submission
rf_submission <- data.table(
  encounter_id = test_set$encounter_id,
  diabetes_mellitus = test_set$diabetes)

write.csv(rf_submission, "rf_submission.csv", row.names=FALSE)

## XGBoost 1 ------------------------------------------------------------------------------

### XGBOOST -------------------------------------------------------------------------------------------
xgb_grid <- expand.grid(nrounds = c(500, 1000),
                        max_depth = c(2, 3, 5),
                        eta = c(0.01, 0.05),
                        gamma = 0,
                        colsample_bytree = c(0.5, 0.7),
                        min_child_weight = 1, # similar to n.minobsinnode
                        subsample = c(0.5))

set.seed(13505)
xgboost_model <- train(diabetes_mellitus ~ .,
                       method = "xgbTree",
                       data = data_train,
                       trControl = train_control,
                       tuneGrid = xgb_grid)
xgboost_model

# save model for later
saveRDS(xgboost_model, "xgboost1.rds")
readRDS("xgboost1.rds")
# add predictions to test set
validation_prediction_probs <- predict.train(xgboost_model, 
                                             newdata = data_test, 
                                             type = "prob")

data_test$diabetes_pred <- validation_prediction_probs$Yes

# check performance on validation set
xgboost_prediction <- prediction(validation_prediction_probs$Yes,
                            data_test[["diabetes_mellitus"]])

(AUC_test <- performance(xgboost_prediction, "auc")@y.values)

## predict for actual holdout set

# add predictions to holdout set
test_prediction_probs <- predict.train(xgboost_model, 
                                       newdata = test_set, 
                                       type = "prob")
test_set$diabetes<- test_prediction_probs$Yes

# submission
xgb_submission <- data.table(
  encounter_id = test_set$encounter_id,
  diabetes_mellitus = test_set$diabetes)

write.csv(xgb_submission, "xgb1.csv", row.names=FALSE)

## XGBoost refined

xgb_grid <- expand.grid(nrounds = c(700,800),
                        max_depth = c(9,10),
                        eta = c(0.01,0.009,0.03),
                        gamma = 0,
                        colsample_bytree = c(0.6,0.7),
                        min_child_weight = 1, # similar to n.minobsinnode
                        subsample = c(0.2, 0.5))

set.seed(13505)
xgboost_model <- train(diabetes_mellitus ~ .,
                       method = "xgbTree",
                       data = data_train,
                       trControl = train_control,
                       tuneGrid = xgb_grid)

saveRDS(xgboost_model, "xgboost2.rds")

# add predictions to test set
validation_prediction_probs <- predict.train(xgboost_model, 
                                             newdata = data_test, 
                                             type = "prob")

data_test$diabetes_pred <- validation_prediction_probs$Yes

# check performance on validation set
xgboost_prediction <- prediction(validation_prediction_probs$Yes,
                                 data_test[["diabetes_mellitus"]])

(AUC_test <- performance(xgboost_prediction, "auc")@y.values)

## predict for actual holdout set

# add predictions to holdout set
test_prediction_probs <- predict.train(xgboost_model, 
                                       newdata = test_set, 
                                       type = "prob")
test_set$diabetes<- test_prediction_probs$Yes

# submission
xgb_submission <- data.table(
  encounter_id = test_set$encounter_id,
  diabetes_mellitus = test_set$diabetes)

write.csv(xgb_submission, "xgb2.csv", row.names=FALSE)





