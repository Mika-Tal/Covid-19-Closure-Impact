
#Pass the merged data frame and return the same df with two additional columns:
# 1) two_week_forecast_date : date shifted ahead by 14 days
# 2) two_week_outcome : the covid measure for the two week date if avaialble
addForecastCols = function(df) {
  df = df %>% 
  mutate_at(c('Date'), ~ as.Date(., "%Y-%m-%d")) %>% 
  #Add a forecast date column - shifted ahead by two weeks
  mutate(two_week_forecast_date = Date + 14) %>% 
  mutate_if(is.character, as.factor) %>% 
  #removes the first index column created from the API pull
  select(-1)  
  
#isolate dater and covid measure
twoWeek_df = df %>% 
  select(Date, State, covid_measure) %>% 
  rename(two_week_outcome = covid_measure,
         two_week_forecast_date = Date) 

#merge df and twoWeek_df on two_week_forecast_date to create the new outcome variable:
#two_week_outcome which for every given row is the covid measure two weeks ahead in time
working_df = df %>% 
  left_join(twoWeek_df,
            by = c("State" = "State",
                   "two_week_forecast_date" = "two_week_forecast_date")) 

return(working_df)
}

#Create train set
train_df = function(addForecastCols, df) {
  #Call addForecastCols function to create the outcome variable
  df = addForecastCols(df)
  #create variable for latest date for which two week covid measure is avaiable
  latest_date = max(df$Date)
  #Create a training set - reserving the last two weeks with obvserved 
  #two_week__outcome are reserved as a test set. 
  train = df[df$two_week_forecast_date <= (latest_date - 14), ]
  return(train)
}

#Create test set
test_df = function(addForecastCols, df) {
  #Call addForecastCols function to create the outcome variable
  df = addForecastCols(df)
  #create variable for latest date for which two week covid measure is avaiable
  latest_date = max(df$Date)
  #Create a test set - using the last two weeks for which two_week_outcom is observed. 
  test = df[(df$two_week_forecast_date <= latest_date) &
                      (df$two_week_forecast_date > (latest_date - 14)), ]
  return(test)
}


#Create prediction set - for all states
pred_df = function(addForecastCols, df) {
  #Call addForecastCols function to create the outcome variable
  df = addForecastCols(df)
  #create variable for latest date for which two week covid measure is avaiable
  latest_date = max(df$Date)
  #Create a prediction set - using the  two weeks for which two_week_outcom is not observed. 
  pred = df[df$two_week_forecast_date > latest_date +7, ]
  return(pred)
}

#Create prediction set - for specific state
pred_state_df = function(addForecastCols, df, state) {
  #Call addForecastCols function to create the outcome variable
  df = addForecastCols(df)
  #create variable for latest date for which two week covid measure is avaiable
  latest_date = max(df$Date)
  #Create a prediction set - using the  two weeks for which two_week_outcom is not observed. 
  pred_state = df[df$two_week_forecast_date > latest_date +7, ] %>% filter(State == state)
  return(pred_state)
}


#Get Baseline RMSE off test set average
baselineRMSE = function (test, RMSE){ 
  baseline = test %>%  
    select(two_week_outcome) %>% 
    mutate(average_outcome = mean(two_week_outcome)) 
  
  baselineRMSE = sqrt(mean (
    (baseline$two_week_outcome - baseline$average_outcome) ^ 2 ) )
}

#Convert data frame into dense matrix for XGB model
denseMatrix  = function(FUN, df){
  FUN(df)
  DMatrix = xgb.DMatrix(data.matrix(df %>%
                                       select(-c(two_week_forecast_date,
                                                 two_week_outcome))))
  
  return(DMatrix)
  
}

#XGB Model
XGBModel = function(train){
  #Convert data frame into dense matrix for XGB model
  XGBTrain = xgb.DMatrix(data.matrix(train %>%
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
XGBtg <- expand.grid(nrounds = 20, #temporary value to facilitate faster run time during app construnction c(50, 100, 250),
                     max_depth = 5, #c(5, 10, 15),
                     colsample_bytree = 0.5, #seq(0.5, 0.9, length.out = 5),
                     eta = 0.1, #c(0.1, 0.2, 0.4),
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

return(XGBModel)

}

#Get best model parameters
BestFit = function(XGBModel){
  BestFit = XGBModel$bestTune %>% 
    stargazer(type = 'text', summary = FALSE)
}

#Get predctions based on XGB mmodel
XGBpredictions = function(model, data){
  #Convert data frame into dense matrix for XGB model
  matrixData = xgb.DMatrix(data.matrix(data %>%
                                      select(-c(two_week_forecast_date,
                                                two_week_outcome))))
  #Get predicitons on test set
  XGBpredicitons=predict(model, matrixData)
  return(XGBpredicitons)
}


#XGB RMSE calculation
RMSE = function(test, predictions){
  #Calculate RMSE
  RMSE = sqrt(mean ((test$two_week_outcome - predictions) ^ 2 ) )
  return(RMSE)
}

#Plot observed vs. predicted
obs_vs_pred_plot = function(test, predicted, baselineRMSE){ 
  #Create dataframe
  obs_vs_pred = test %>% 
    select(two_week_outcome) %>% 
    rename(Observed = two_week_outcome) %>% 
    bind_cols(tibble(Predicted = predicted)) %>% 
    mutate(Observed = log(Observed),
           Predicted = log(Predicted))
  
  obs_vs_pred_plot = ggplot(obs_vs_pred, aes(x = Observed, y = Predicted)) +
    geom_point() +
    geom_abline(aes(intercept = 0, slope = 1, color = "blue")) +
    geom_line(aes(y = log(baselineRMSE(test)), color = "red")) +
    xlab("Log(Observed)") +
    ylab("Log(Predicted)") +
    scale_colour_manual(name = "Color", values =c('blue'='blue','red'='red'),
                        labels = c('45-degree','Test Set Avergae'))
}

#Plot predictions - all states
predictions_all_plot = function(train, pred, XGBPredsPred){
  #Plot Predictions
  outcome_all = train %>% 
    select(two_week_forecast_date, two_week_outcome) %>%
    rename(Date = two_week_forecast_date,
           Outcome = two_week_outcome) %>% 
    mutate (Pred_vs_Obs = "Observed") %>% 
    rbind(tibble(Date = pred$two_week_forecast_date,
                 Outcome = XGBPredsPred,
                 Pred_vs_Obs = "Predicted"))
  
  outcome_plot_all = ggplot(outcome_all, aes(x = Date, y = Outcome))+
    geom_point(aes(color = Pred_vs_Obs))
}

#Predictions in tabular form
results_tabular_all = function(XGBPredsPred, test, XGBRMSE, BLRMSE){
  #Summarise predictions by prediction week
  Results = data.frame(Prediction = test$two_week_outcome,
                 Model = rep("Baseline", nrow(test))) %>% 
    bind_rows(data.frame(Prediction  = XGBPredsPred,
                         Model = rep("XGB", length(XGBPredsPred)))) %>% 
    group_by(Model) %>% 
    summarise(Total = sum(Prediction),
              AvgPerState = mean(Prediction)) %>% 
    mutate(RMSE = c(BLRMSE, XGBRMSE)) %>% 
    stargazer(type = 'text', summary = FALSE)
    
}

#Predictions by state - state needs to be a dynamic not static variable
predictions_by_state = function(train, state, pred_state, XGBPredsState){
  outcome_all_state = train %>% 
    filter(State == state) %>% 
    select(two_week_forecast_date, two_week_outcome) %>%
    rename(Date = two_week_forecast_date,
           Outcome = two_week_outcome) %>% 
    mutate (Pred_vs_Obs = "Observed") %>% 
    rbind(tibble(Date = pred_state$two_week_forecast_date,
                 Outcome = XGBPredsState,
                 Pred_vs_Obs = "Predicted"))
  
  outcome_plot_state = ggplot(outcome_all_state, aes(x = Date, y = Outcome)) +
    geom_point(aes(color = Pred_vs_Obs)) 
}

#Predictions in tabular form for selected state
predictions_state_tabular = function(XGBPredsState){
  preds_state = data.frame(XGB = XGBPredsState) %>%
    stargazer(type = 'text', summary = FALSE)
  
}

#Testing function to make sure they work
#Stand in variable for the merged data set
original = read.csv("Data/shiny_merged_dataset_example.csv")
#Create train set and save to variable
train = train_df(addForecastCols, original)
#Create test set and save to variable
test = test_df(addForecastCols, original)
#Create Pred set and save to variable
pred = pred_df(addForecastCols, original)
#Create pred set for specific state (state variable should be dynamic not hard coded)
#Save to variable
pred_state = pred_state_df(addForecastCols, original, 'MA')
#Call model and save to variable
XGBModel = XGBModel(train)
#Get best fit no need to save to variable
BestFit(XGBModel)
#Get test set predictions and save to variable
XGBPredsTest = XGBpredictions(XGBModel,test)
#Calculate XGB RMSE - save to variable 
XGBRMSE = RMSE(test, XGBPredsTest)
#plot observed vs predicted (log scale). Save to variable 
obs_vs_pred_plot(test, XGBPredsTest, baselineRMSE)
#Get pred set predictions - all states. Save to variable
XGBPredsPred = XGBpredictions(XGBModel,pred)
#Plot predictions all states - no need to save to variable
predictions_all_plot(train,pred,XGBPredsPred)
#Get pred set predictions - by state. Save to variable
XGBPredsState = XGBpredictions(XGBModel,pred_state)
#Plot predictions all states - no need to save to variable
predictions_by_state(train, 'MA', pred_state, XGBPredsState)
#Prediction summary table by selected state - no need to save to variable
predictions_state_tabular(XGBPredsState)
#baseline RMSE - based on test average. Save to Variable
BLRMSE = baselineRMSE(test)
#Results table
results_tabular_all(XGBPredsPred, test, XGBRMSE, BLRMSE)
