
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
NSD_original_df = read.csv("Data/shiny_merged_dataset_example.csv") %>% 
  mutate_at(c('week_first_date'), ~ as.Date(., "%Y-%m-%d")) %>% 
  mutate(two_week_forecast_date = week_first_date + 14) %>% 
  mutate_if(is.character, as.factor) %>% 
  select(-1)
# Get latest date 
NSD_latest_date = max(NSD_original_df$week_first_date)
#Two week outcome 
NSD_twoWeek_df = NSD_original_df %>% 
  select(week_first_date, State, Outcome.Variable) %>% 
  rename(two_week_outcome = Outcome.Variable,
         two_week_forecast_date = week_first_date) 
#Merge with original to create a working df
NSD_working_df = NSD_original_df %>% 
  left_join(NSD_twoWeek_df,
            by = c("State" = "State",
                   "two_week_forecast_date" = "two_week_forecast_date")) %>%
  select(-c(6:18)) %>% 
  mutate(year = as.factor(year(two_week_forecast_date)),
         week = as.factor(week(two_week_forecast_date)))

# Split and prepare train/test datasets
set.seed(1)
#Need if statement - Tobi
#### If use shuffle prediction
#generate random index list for training and test sets
NSD_trainIndex = sample(1:nrow(NSD_working_df[NSD_working_df$two_week_forecast_date <= NSD_latest_date , ]),
                    0.8 * nrow(NSD_working_df[NSD_working_df$two_week_forecast_date <= NSD_latest_date , ]))
#create train set 
#train = working_df[trainIndex, ]
#create test set
#test = working_df[-c(trainIndex), ]
#train sequential
#### If use sequential prediciton
NSD_train = NSD_working_df[NSD_trainIndex, ] %>% na.omit()
#test sequential
NSD_test = NSD_working_df[-NSD_trainIndex, ] %>% na.omit()

NSD_pred = NSD_working_df[NSD_working_df$two_week_forecast_date > NSD_latest_date, ]

NSD_pred_state = NSD_working_df[NSD_working_df$two_week_forecast_date > NSD_latest_date, ] %>% 
  filter(State == 'PA')

#Convert the predictor variable matrices to xgb.DMatrix data types
NSD_XGBTrain = xgb.DMatrix(data.matrix(NSD_train %>%
                                     select(-c(two_week_forecast_date,
                                               two_week_outcome))))
NSD_XGBTest = xgb.DMatrix(data.matrix(NSD_test %>%
                                    select(-c(two_week_forecast_date,
                                              two_week_outcome))))
NSD_XGBPred = xgb.DMatrix(data.matrix(NSD_pred %>%
                                    select(-c(two_week_forecast_date,
                                              two_week_outcome))))

NSD_XGBPred_state = xgb.DMatrix(data.matrix(NSD_pred_state %>%
                                          select(-c(two_week_forecast_date,
                                                    two_week_outcome))))

#Define random grid parameters for tuning the models
NSD_XGBtc=trainControl(
  method='cv',
  number=5,
  allowParallel=TRUE,
  verboseIter=FALSE,
  returnData=FALSE
)
# create grid for tuning hyperparameters
NSD_XGBtg <- expand.grid(nrounds = c(50, 100, 250),
                     max_depth = c(5, 10, 15),
                     colsample_bytree = seq(0.5, 0.9, length.out = 5),
                     eta = c(0.1, 0.2, 0.4),
                     gamma=c(0, 2, 3),
                     min_child_weight = 1,
                     subsample = 0.75
)


# train model - with dates and state 
NSD_XGBModel = train(
  NSD_XGBTrain, NSD_train$two_week_outcome,  
  trControl = NSD_XGBtc,
  tuneGrid = NSD_XGBtg,
  importance = TRUE,
  method = "xgbTree")

# get hyperparameters of best model from grid search 
NSD_XGBModel$bestTune

#Predict
NSD_XGBModel_test=predict(NSD_XGBModel, NSD_XGBTest)

NSD_XGBModel_pred = predict(NSD_XGBModel, NSD_XGBPred)

NSD_XGBModel_state = predict(NSD_XGBModel,NSD_XGBPred_state)

#Model Performance
#RMSE
NSD_XGBRMSE = sqrt(mean ((NSD_test$two_week_outcome - NSD_XGBModel_test) ^ 2 ) )
#Plot observed vs. predicted
NSD_obs_vs_pred = NSD_test %>% 
  select(two_week_outcome) %>% 
  rename(Observed = two_week_outcome) %>% 
  bind_cols(tibble(Predicted = NSD_XGBModel_test))

obs_vs_pred_plot = ggplot(NSD_obs_vs_pred, aes(x = Observed, y = Predicted)) +
  geom_point() +
  geom_abline()


#Plot Predictions
NSD_outcome_all = NSD_train %>% 
  select(two_week_forecast_date, two_week_outcome) %>%
  rename(Date = two_week_forecast_date,
         Outcome = two_week_outcome) %>% 
  mutate (Pred_vs_Obs = "Observed") %>% 
  rbind(tibble(Date = NSD_pred$two_week_forecast_date,
               Outcome = NSD_XGBModel_pred,
               Pred_vs_Obs = "Predicted"))

outcome_plot_all = ggplot(NSD_outcome_all, aes(x = Date, y = Outcome))+
  geom_point(aes(color = Pred_vs_Obs))

NSD_outcome_all_state = NSD_train %>% 
  filter(State == 'PA') %>% 
  select(two_week_forecast_date, two_week_outcome) %>%
  rename(Date = two_week_forecast_date,
         Outcome = two_week_outcome) %>% 
  mutate (Pred_vs_Obs = "Observed") %>% 
  rbind(tibble(Date = NSD_pred_state$two_week_forecast_date,
               Outcome = NSD_XGBModel_state,
               Pred_vs_Obs = "Predicted"))

NSD_outcome_plot_state = ggplot(NSD_outcome_all_state, aes(x = Date, y = Outcome)) +
  geom_point(aes(color = Pred_vs_Obs)) +
  stat_smooth(method = 'lm', formula = y ~ poly(x,10), se = TRUE, color = "darkgrey")



