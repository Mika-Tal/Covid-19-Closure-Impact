#Analysis for Random Forest and XGBoost
#install.packages("xgboost")
loadlibs = function(libs) {
  for(lib in libs) {
    class(lib)
    if(!do.call(require,as.list(lib))) {install.packages(lib)}
    do.call(require,as.list(lib))
  }
}
libs = c("tidyverse","data.table","stargazer", "caret", "e1071", "splines",
         "randomForest", "C50", "xgboost", "ggplot2", "cowplot", "forecast")
loadlibs(libs)


# Load Data Set
original_df = read.csv("Data/shiny_merged_dataset_example.csv") %>% 
  mutate_at(c('week_first_date'), ~ as.Date(., "%Y-%m-%d")) %>% 
  mutate(two_week_forecast_date = week_first_date + 14) %>% 
  mutate_if(is.character, as.factor) %>% 
  select(-1)
# Get latest date 
latest_date = max(original_df$week_first_date)
#Two week outcome 
twoWeek_df = original_df %>% 
  select(week_first_date, State, Outcome.Variable) %>% 
  rename(two_week_outcome = Outcome.Variable,
         two_week_forecast_date = week_first_date) 
#Merge with original to create a working df
working_df = original_df %>% 
  left_join(twoWeek_df,
            by = c("State" = "State",
                   "two_week_forecast_date" = "two_week_forecast_date")) %>% 
  mutate(year = as.factor(year(two_week_forecast_date)),
         week = as.factor(week(two_week_forecast_date)))

# Split and prepare train/test datasets
set.seed(1)
#Need if statement - Tobi
#### If use shuffle prediction
#generate random index list for training and test sets
trainIndex = sample(1:nrow(working_df[working_df$two_week_forecast_date <= latest_date , ]),
                    0.8 * nrow(working_df[working_df$two_week_forecast_date <= latest_date , ]))
#create train set 
#train = working_df[trainIndex, ]
#create test set
#test = working_df[-c(trainIndex), ]
#train sequential
#### If use sequential prediciton
train = working_df[trainIndex, ] %>% na.omit()
#test sequential
test = working_df[-trainIndex, ] %>% na.omit()

pred = working_df[working_df$two_week_forecast_date > latest_date, ]

pred_state = working_df[working_df$two_week_forecast_date > latest_date, ] %>% 
  filter(State == 'PA')

#Convert the predictor variable matrices to xgb.DMatrix data types
XGBTrain = xgb.DMatrix(data.matrix(train %>%
                                     select(-c(two_week_forecast_date,
                                               two_week_outcome))))
XGBTest = xgb.DMatrix(data.matrix(test %>%
                                    select(-c(two_week_forecast_date,
                                              two_week_outcome))))
XGBPred = xgb.DMatrix(data.matrix(pred %>%
                                    select(-c(two_week_forecast_date,
                                              two_week_outcome))))

XGBPred_state = xgb.DMatrix(data.matrix(pred_state %>%
                                    select(-c(two_week_forecast_date,
                                              two_week_outcome))))

#Define random grid parameters for tuning the models
XGBtc=trainControl(
  method='cv',
  number=5,
  allowParallel=TRUE,
  verboseIter=FALSE,
  returnData=FALSE
)
# create grid for tuning hyperparameters
XGBtg <- expand.grid(nrounds = c(50, 100, 250),
                     max_depth = c(5, 10, 15),
                     colsample_bytree = seq(0.5, 0.9, length.out = 5),
                     eta = c(0.1, 0.2, 0.4),
                     gamma=c(0, 2, 3),
                     min_child_weight = 1,
                     subsample = 0.75
)


# train model - with dates and state 
XGBModel = train(
  XGBTrain, train$two_week_outcome,  
  trControl = XGBtc,
  tuneGrid = XGBtg,
  importance = TRUE,
  method = "xgbTree")

# get hyperparameters of best model from grid search 
XGBModel$bestTune

#Predict
XGBModel_test=predict(XGBModel, XGBTest)

XGBModel_pred = predict(XGBModel, XGBPred)

XGBModel_state = predict(XGBModel, XGBPred_state)

#Model Performance
#RMSE
XGBRMSE = sqrt(mean ((test$two_week_outcome - XGBModel_test) ^ 2 ) )
#Plot observed vs. predicted
obs_vs_pred = test %>% 
  select(two_week_outcome) %>% 
  rename(Observed = two_week_outcome) %>% 
  bind_cols(tibble(Predicted = XGBModel_test))

obs_vs_pred_plot = ggplot(obs_vs_pred, aes(x = Observed, y = Predicted)) +
  geom_point() +
  geom_abline()


#Plot Predictions
outcome_all = train %>% 
  select(two_week_forecast_date, two_week_outcome) %>%
  rename(Date = two_week_forecast_date,
         Outcome = two_week_outcome) %>% 
  mutate (Pred_vs_Obs = "Observed") %>% 
  rbind(tibble(Date = pred$two_week_forecast_date,
               Outcome = XGBModel_pred,
               Pred_vs_Obs = "Predicted"))

outcome_plot_all = ggplot(outcome_all, aes(x = Date, y = Outcome))+
  geom_point(aes(color = Pred_vs_Obs))

outcome_all_state = train %>% 
  filter(State == 'PA') %>% 
  select(two_week_forecast_date, two_week_outcome) %>%
  rename(Date = two_week_forecast_date,
         Outcome = two_week_outcome) %>% 
  mutate (Pred_vs_Obs = "Observed") %>% 
  rbind(tibble(Date = pred_state$two_week_forecast_date,
               Outcome = XGBModel_state,
               Pred_vs_Obs = "Predicted"))

outcome_plot_state = ggplot(outcome_all_state, aes(x = Date, y = Outcome)) +
  geom_point(aes(color = Pred_vs_Obs)) +
  stat_smooth(method = 'lm', formula = y ~ poly(x,10), se = FALSE, color = "black")


