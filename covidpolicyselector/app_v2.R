#install necessary libraries
#Install Delphi R package from below
#https://cmu-delphi.github.io/covidcast/covidcastR/articles/covidcast.html
# #installs the COVIDCast API information
# devtools::install_github("cmu-delphi/covidcast", ref = "main",
#                          subdir = "R-packages/covidcast")

#Load in necessary libraries

library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(glmnet) # to perform L1 regularization
library(tidymodels)
library(covidcast)
library(lubridate)
library(randomForest)
library(xgboost)
library(caret)
library(fuzzyjoin)
library(maps)
library(stringr) # aids with string handling
# add these libraries 
# libs = c("tidyverse","data.table","stargazer", "caret", "e1071", "splines",
#          "randomForest", "C50", "xgboost", "ggplot2", "cowplot", "forecast")


#read in static datasets -- controls only and the sample user input data
state_controls <- read.csv("~/Documents/GitHub/Covid-19-Closure-Impact/Data/state_controls.csv")


#Dictionary of Datasets Created:
# 
# - merged_dataset()  — before missingness is dealt with or imputed
# - data_after_missing() — merged dataset after missingness has been handled
# - forecast_data() — add the two week forecast columns to the 
# - top_10() - dataset with the top 10 most important features, after feature selection has concluded
# 
# 




#Dashboard Header & Title ---------------------------------------------------------
header <- dashboardHeader(title = "Machine Learning Pipeline Visualization Using COVID-19 Data",
                          titleWidth = 600)


#Dashboard Sidebar --------------------------------------------------

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    
    #Menu Items--------------------------
    menuItem("Overview & Data Ingestion", tabName = "overview") ,
    menuItem("Data Merging", tabName = "merging"),
    menuItem("Data Pre-Processing", tabName = "missing"),
    menuItem("Feature Selection", tabName = "features"),
    menuItem("Model 1: GLM ", tabName = "model_1"),
    menuItem("Model 2: XGBoost", tabName = "model_2"),
    menuItem("Results", tabName = "results")
  )
)



#Dashboard Body ------------------------------------

body <- dashboardBody(
  
  tabItems(
    
    #Overview & Instructions page ----------------------------------------------------------------------------
    tabItem("overview",
            
            
            # welcome message & introduction to shiny app -------
            htmlOutput(outputId = "welcome"),
            
            #changed how the welcome message should display--------
            tags$head(tags$style("#welcome{color: blue;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
            )
            ),
            
            #adds visual space between the page elements
            br(),
            
            br(),
            
            br(),
            
            
            #select the date range to use to query the CMU API
            dateRangeInput(inputId = "dates_of_interest",
                           label = "Choose the Date Range of Interest for COVID-19 Data",
                           start = "2020-03-01",
                           end = "2021-04-09",
                           format = "yyyy-mm-dd",
                           separator = "-"),
            
            #outcome choice (cases or deaths)
            selectInput("outcome_variable", "Choose An Outcome Variable To Forecast:",
                        c("7 Day Average for New Cases per 100,000 People" = "confirmed_7dav_incidence_prop",
                          "7 Day Average for Deaths per 100,000 People" = "deaths_7dav_incidence_prop")),
            
            
            
            #adds visual space between the page elements
            br(),
            
            
            #button to allow user to query API and get the most updated COVID-19 info -----
            actionButton(inputId = "queryAPI",
                         label = "Click to Query CMUDelphi API"),
            
            #adds visual space between the page elements
            br(),
            
            br(),
            
            br(),
            
            #display the queried table to the user
            dataTableOutput(outputId = "api_results"),
            
            
            #adds visual space between the page elements
            br(),
            
            br(),
            
            br(),
            
            
            #shows what control features are available to be used
            radioButtons(inputId = "display_controls",
                         label = "Do you want to see what control features are available for each state?",
                         choices = c("Yes", "No"),
                         selected = c("No")),
            
            #only allows user to see a data table with the control features if checkbox is checked
            conditionalPanel(condition = "input.display_controls == 'Yes'",
                             dataTableOutput(outputId = "controls_only")),
            
            
            
            #displays panel options, if the user wants to add additional features
            radioButtons(inputId = "add_features",
                         label = "Do you want to add additional features to the COVID dataset?",
                         choices = c("Yes", "No"),
                         selected = c("No")),
            
            
            #only allows users to add additional features to the COVID dataset radio option "Yes" is selected
            conditionalPanel(condition = "input.add_features == 'Yes' ",
                             #allows the user to download the template file for adding new features
                             downloadButton(outputId = "template",
                                            label = "Click to Download Data Template"),
                             
                             
                             
                             
                             #adds visual space between the page elements
                             br(),
                             
                             br(),
                             
                             #allows user to upload a file and choose how it is displayed to them
                             fileInput(inputId = "file1",
                                       label = "Upload CSV File with Filled Out Data Template",
                                       multiple = FALSE,
                                       accept = c("text/csv",
                                                  "text/comma-separated-values,text/plain",
                                                  ".csv")
                             ),
                             
                             # Horizontal line ----
                             tags$hr(),
                             
                             # Input: Checkbox if file has header ----
                             checkboxInput("header", "Is there a header in your data?", TRUE),
                             
                             # Input: Select separator ----
                             radioButtons("sep", "How is your data separated?",
                                          choices = c(Comma = ",",
                                                      Semicolon = ";",
                                                      Tab = "\t"),
                                          selected = ","),
                             
                             # Input: Select quotes ----
                             radioButtons("quote", "Quote",
                                          choices = c(None = "",
                                                      "Double Quote" = '"',
                                                      "Single Quote" = "'"),
                                          selected = '"'),
                             
                             # Horizontal line ----
                             tags$hr(),
                             
                             # Input: Select number of rows to display ----
                             radioButtons("disp", "How many rows would you like to display?",
                                          choices = c("First 5 Rows" = "head",
                                                      All = "all"),
                                          selected = "head"),
                             
                             #resulting data from file upload
                             dataTableOutput(outputId = "contents"))
            
            
            
    ),
    
    #Data Merging Page--------------------------------------------------------------------------------
    tabItem("merging",
            
            
            #determines what dataset will be fed into the feature selection & used for modeling
            #asks the user whether to combine their features with our set of control features
            radioButtons(inputId = "merge_decide",
                         label = "Do you want to combine your data with our control features?",
                         choices = c("Yes", "No"),
                         selected = c("No")),
            
            
            #only allows user to see select control features if "merge_decide" = "Yes"
            conditionalPanel(condition = "input.merge_decide == 'Yes'",
                             radioButtons(inputId = "all_controls",
                                          label = "Do you want to use ALL of our control features?",
                                          choices = c("Yes", "No"),
                                          selected = c("Yes")),
                             
                             conditionalPanel(condition = "input.all_controls == 'No'",
                                              selectInput(inputId = "select_controls",
                                                          label = "Select the control features you would like to use:",
                                                          choices = names(state_controls)[-1],
                                                          multiple = TRUE,
                                                          selected = names(state_controls)[2:5])
                             )),
            
            #displays the resulting dataTable from merging
            
            actionButton(inputId = "click_merge",
                         label = "Click to Merge COVID Case Data With Selected  Additional Features"),
            
            #add visual space
            br(),
            br(),
            htmlOutput(outputId = "merged"),
            
            #add visual space
            br(),
            br(),
            dataTableOutput(outputId = "display_merged_data")
            
            
    ),
    
    
    #Missingness Imputation page-----------------------------------------------
    tabItem("missing",
            
            #displays words to go before displaying the dataTable output
            htmlOutput(outputId = "no_miss_mess"),
            
            #adds visual space
            
            br(),
            
            br(),
            
            #displays the % of missingness in the data
            dataTableOutput(outputId = "missing_stats"),
            
            #adds visual space 
            
            br(),
            
            br(),
            
            #allows the user to decide how to deal with missingness
            radioButtons(inputId = "handle_missingness",
                         label = "Do you want handle missing values?",
                         choices = c("Impute missing values", "Drop rows with missing values" , "Delete features with missing values"),
                         selected = c("Drop rows with missing values")),
            
            #adds visual space
            br(),
            
            br(),
            
            #gives the user the option to inspect their dataset after dealing with missingness
            dataTableOutput(outputId = "data_aft_missing")
            
    ),
    
    #Feature Selection page ------------------------------------------------------------------------------
    tabItem("features",
            
          
            #Gives the user a message to describe the data table
            htmlOutput(outputId = "forecast_table"),    
            
            #adds visual space
            
            br(),
            
            br(),
            
            br(),
            
            
            #displays the two week forecast data
            dataTableOutput(outputId = "forecast"),
            
            br(),
            
            br(),
            
            #message to show label what is being displayed
            htmlOutput(outputId = "ft_sel"),
            
            
            br(),
            
            
            #shows the variable importance plot
            plotOutput(outputId = "varImp"),
            
            
    ),
    
    
    
    #GLM Page --------------------------------------------------
    tabItem("model_1",
            
            
            plotOutput(outputId = "outcome_all_glm"),

            br(),


            br(),

            plotOutput(outputId = "preds_act"),

            br(),

            br(),

            plotOutput(outputId = "residuals_glm")


           
            # dataTableOutput(outputId = "testing_glm"),
            # dataTableOutput(outputId = "testing_glm_2")
            # 
    ),
    
    
    #XGBoost Page ------------------------------------------------------------------------------
    tabItem("model_2",
            
            #click to run the XGBoost model
            actionButton(inputId = "runxgb",
                         label = "Click to Run the XGBoost Model"),
            
            
            #creates visual space
            br(),
            
            br(), 
            
            #Gives the user a message while waiting for XGBoost to run
            htmlOutput(outputId = "xgb_pls_wait"),    
            
            
            #creates visual space
            br(),
            
            br(),
            
            htmlOutput(outputId = "display_model"), 
            
            br(),
            
            
            dataTableOutput(outputId = "model_specs"),
            
            
            #creates visual space
            br(),
            
            br(),
            
            plotOutput(outputId = "preds_vs_observed"),
            
            
            #creates visual space
            br(),
            
            br(),
            
          
            plotOutput(outputId = "state_RMSE_XGB"),
          
            br(),
          
            br(),
          
            plotOutput(outputId = "residual_plot_xgb")
            
    ),
    
   
    
    tabItem("results",
            
            
            dataTableOutput(outputId = "result_tabular_all"),
          
            br(), 
            
            #allows a user to select the state that they would like a prediction for
            
            selectInput(inputId = "state",
                        label = "Select a state that you would like predictions for: ",
                        choices = c(state_controls %>% select("State"))),
            
            br(),
            
       
            
            
            plotOutput(outputId = "one_state_preds"),
            
            br(),
            
            
            br(),
            
            
            plotOutput(outputId = "final_preds")
            
            
            
            
            
            
            
        
    )
    
    
  )
)





ui <- dashboardPage(header, sidebar, body)


#Overview & Data Ingestion Output ----------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$welcome <- renderUI({
    
    
    HTML(paste("Hello! Welcome to our Machine Learning Pipeline Visualization App. To get started:", "Step 1: Choose the Date Range For Which You Would Like COVID Data ", "Step 2: Query the CMU Delphi API to Get the Requested COVID Data", "Step 3: Upload The Additional Features You Would Like To Use, Using the Provided Template", sep = "<br/><br/>"))
    
  })
  
  
  
  
  #creates a function for querying the API
  query_API_fun <- function(start_date, end_date, outcome_variable) {
    
    
    #queries the delphi API based on the input from the user
    covid_data <-
      covidcast_signal(data_source = "indicator-combination",
                       signal = outcome_variable,
                       start_day =  start_date, #sets the data queried to user inputs
                       end_day = end_date ,
                       geo_type = "state") %>% 
      
      #conduct necessary data transformations
      select("geo_value", "time_value","value") %>%
      rename( State = geo_value , covid_measure = value) %>%
      
      mutate(
        Year = strftime(time_value , format = "%Y"),
        Week = strftime(time_value , format = "%V"),
        State = toupper(State),
        Day = strftime(time_value , format = "%A"),
        Date = ymd(time_value)
      ) %>%
      subset(Day == "Monday")
    
    #removes some erroneous negative values that get pulled from the Delphi API
    covid_data <- covid_data[covid_data$covid_measure >= 0, ]
    
    covid_data <- subset(covid_data, select=-c(Day,time_value))
    
    return(covid_data)
    
  }
  
  
  #when the action button is pushed, query the CMU Delphi API
  covid_dataset <- eventReactive(input$queryAPI, {
    
    start_date = input$dates_of_interest[1]
    end_date = input$dates_of_interest[2]
    outcome_variable = input$outcome_variable
    
    #call function to query the API
    query_API_fun(start_date, end_date, outcome_variable)
    
  })
  
  
  
  #placeholder for displaying whatever datatable results from querying the API
  output$api_results <- renderDataTable({
    DT::datatable(data = covid_dataset(),
                  rownames = FALSE, options = list(scrollX = T))
  })
  
  
  
  
  
  
  output$controls_only <- renderDataTable({
    
    DT::datatable(data = state_controls,
                  rownames = FALSE,
                  options = list(scrollX = T))
  })
  
  
  #***allows the user to download the data ingestion template
  output$template <- downloadHandler(
    filename = "Data_Ingestion_Template.csv",
    content = function(file) {
      
      write.csv(user_input_example, file, row.names = FALSE) #***change the file that is downloaded
    }
  )
  
  
  #creates reactive variable for the data uploaded by the user to the shiny platform
  userdata <- reactive({
    infile <- input$file1
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    user_df <-  read.csv(infile$datapath,
                         header = input$header,
                         sep = input$sep,
                         quote = input$quote)
    
    user_df <- user_df %>%    mutate(
      Date = as.Date(Date, format = "%m/%d/%Y"),
      Year = strftime(Date , format = "%Y"),
      Week = strftime(Date , format = "%V"),
      State = toupper(State)
    ) %>% 
      
      group_by(State, Year, Week) %>% 
      summarise_each(funs(mean)) %>% subset(select = -c(Date)) %>%  mutate_if(is.numeric, ~round(., 0))
  })
  
  output$contents <- renderDataTable({
    
    if(input$disp == "head") {
      
      #display the datatable
      DT::datatable(data = head(userdata()),
                    rownames = FALSE, options = list(scrollX = T))
      
    }
    else {
      
      #display the data table
      DT::datatable(data = userdata(),
                    rownames = FALSE, options = list(scrollX = T))
      
    }
    
  })
  
  
  # Data Merging Page Output ---------------------------------------------------------
  # covid_dataset()
  # userdata()
  # state_controls 
  
  output$merged <- renderUI({
    
    HTML(paste("Here's Your Merged Dataset:", sep = "<br/><br/>"))
    
  })
  
  merge_dataset_fun <- function(covid_data, user_data = NULL, control_data = NULL){
    
    
    if(is.null(user_data) & !is.null(control_data)) {
      merged <- covid_data %>% 
        left_join(control_data, by = c("State" = "State"))
      
      return(merged)
    }
    
    #If the user does not want to use our control data, then return merged covid and user data only
    else if(!is.null(user_data) & is.null(control_data))  {
      merged <- covid_data %>% 
        left_join(user_data, by = c("State","Year", "Week"), all.x = TRUE)
      
      return(merged)
      
    }
    
    #if the user wants to add = covid data + their own uploaded data + all or a selection of our controls
    else if (!is.null(control_data) & !is.null(user_data)){
      
      #join the covid data to the state controls data by joining on state
      merged <- covid_data %>%
        left_join(control_data, by = c("State" = "State"))
      
      merged <- left_join(merged, user_data,  by=c("State","Year", "Week"), all.x = TRUE)
      
      
      return(merged)
    }
    
    
  }
  
  #creates a reactive dataframe including only the control features that the user selects
  only_sel_ctrls <- reactive({
    state_controls %>% 
      select(State, all_of(input$select_controls))
    
  })
  
  merged_dataset <- eventReactive(input$click_merge, {
    
    
    if(input$merge_decide == "No" & is.null(userdata())) {
      
      dat <- covid_dataset()
    }
    
    else if(input$merge_decide == "No" & !is.null(userdata())) {
      
      merge_dataset_fun(covid_data = covid_dataset(), user_data = userdata())
      
    }
    
    else if(input$merge_decide == "Yes" & input$all_controls == "Yes" & !is.null(userdata())) {
      
      merge_dataset_fun(covid_dataset(), userdata(), state_controls)
      
    }
    
    #if the user didn't upload any data
    else if(input$merge_decide == "Yes" & input$all_controls == "Yes" & is.null(userdata()) ) {
      
      merge_dataset_fun(covid_data = covid_dataset(), control_data = state_controls)
      
    }
    
    #if the user only wants to use a selection of our control features
    else if(input$merge_decide == "Yes" & input$all_controls == "No" & !is.null(userdata())) {
      
      merge_dataset_fun(covid_dataset(), userdata(), only_sel_ctrls())
      
    }
    
    #if the user didn't upload any data
    else if(input$merge_decide == "Yes" & input$all_controls == "No" & is.null(userdata()) ) {
      
      merge_dataset_fun(covid_data = covid_dataset(), control_data = only_sel_ctrls())
      
    }
    
  })
  
  
  output$display_merged_data <- renderDataTable({
    
    DT::datatable(data = merged_dataset(),
                  rownames = FALSE,
                  options = list(scrollX = T))
    
  })
  
  
  #Missingness Imputation Page Output ---------------------------------------------
  
  
  check_missingness <- function(merged_df ){
    missing_percentages_df = colSums(is.na(merged_df)) %>% 
      tibble(name=names(.), percent_missing=. * (1/nrow(merged_df)) * 100) %>% 
      select(name, percent_missing) %>% 
      
      #formats the percent missing nicely
      mutate(percent_missing = round(percent_missing, digits = 2)) %>% 
      arrange(desc(percent_missing)) %>%
      as.data.frame()
    
    
    return(missing_percentages_df)
  }
  
  
  
  #This function takes two arguments, first is the merged dataset (dataframe) and second is the user
  # choice on wether to drop columns with missing data or drop rows or just impute missing values
  handle_missingness <- function(merged_df , choice){
    
    #make necessary data type changes
    miss_df <- merged_df %>%
      ungroup() %>%
      mutate_if(is.character, as.factor) %>% 
      mutate_if(is.Date, as.factor)
    
    
    if (choice == "Impute missing values"){
      imputer_recipe = 
        miss_df %>%
        recipe(formula = "~ . - covid_measure") %>%
        step_knnimpute(all_predictors(),neighbors = 1)
      
      merged_imputed_df = prep(imputer_recipe) %>%
        bake(new_data = miss_df)
    }
    
    if (choice == "Drop rows with missing values"){
      merged_imputed_df <- merged_df[rowSums(is.na(merged_df)) <= 0,]
      
    }
    if (choice == "Delete features with missing values"){
      merged_imputed_df <- merged_df %>%  select_if(~ !any(is.na(.)))
      
    }
    
    return(merged_imputed_df)
    
    
  }
  
  
  output$no_miss_mess <- renderUI({
    
    HTML(paste("Here's The Amount of Missingness in the Data:", sep = "<br/><br/>"))
    
  })
  
  
  #displays the new dataset after addressing potential missingnes in the dataset
  output$missing_stats <- renderDataTable({
    
    merged_df <- merged_dataset()
    miss_stats_df <- check_missingness(merged_df)
    
    DT::datatable(data = miss_stats_df,
                  rownames = FALSE,
                  options = list(scrollX = T))
    
  })
  
  
  #displays the dataset after missingness has been dealt with, according to the "handle_missingness" function
  
  #new dataset after missingness has been handled
  data_after_missing <- reactive({
    handle_missingness(merged_dataset(), input$handle_missingness)
  })
  
  output$data_aft_missing <- renderDataTable({
    
    merged_df <- merged_dataset()
    df_aft_miss <- data_after_missing()
    DT::datatable(data = df_aft_miss,
                  rownames = FALSE,
                  options = list(scrollX = T))
    
    
  })
  
  
  
  
  #CREATING THE DATASETS USED FOR FORECASTING --------------------------------------------
  
  #function creates a new variable to capture the two-week forecast
  addTwoWeekForecast <- function(merged_df) {
    
    
    #create a two week forecast variable
    original_df = merged_df %>% 
      mutate_at(c('Date'), ~ as.Date(., "%Y-%m-%d")) %>% 
      mutate(two_week_forecast_date = Date + 14) %>% 
      mutate_if(is.character, as.factor) 
    
    #map the two week forecast variable to the outcome variable in two weeks
    twoWeek_df = original_df %>%
      select(Date, State, covid_measure) %>%
      rename(two_week_outcome = covid_measure,
             two_week_forecast_date = Date)
    
    
    #combine the original dataframe with the new dataframe with the two week forecast
    working_df = original_df %>%
      left_join(twoWeek_df,
                by = c("State" = "State",
                       "two_week_forecast_date" = "two_week_forecast_date"))
    
    return(working_df)
  }
  
  
  
  
  #make a reactive data frame after the two week forecast has been added
  forecast_data <- reactive({
    
    addTwoWeekForecast(data_after_missing()) 
    
  })
  
  
  
  #Feature Selection Page Output ---------------------------------------------------
  
  output$ft_sel <- renderUI({
    
    HTML(paste("Here's The Results of Feature Selection:", "Here's a Variable Importance Plot:", sep = "<br/><br/>"))
    
  })
  
  
  #RANDOM FOREST FEATURE SELECTION -----------------------------------
  #creates a reactive dataframe for doing random forest
  
  #Creates the forecast data for the generalized linear model
  
  # - merged_dataset()  — before missingness is dealt with or imputed
  # - data_after_missing() — merged dataset after missingness has been handled
  # - forecast_data() — add the two week forecast columns to the 
  # - top_10() - dataset with the top 10 most important features, after feature selection has concluded
  # 
  #merged_dataset()
  
  forecast_data_glm <- reactive({
    
    #store the dataset created after handling missingness in a temporary dataframe
    df <- data_after_missing()
    
    #change the column names in the temporary dataframe to lower case
    names(df) <- tolower(names(df))
    
    
    #changes the name of the outcome column to "y" instead of "covid_measure" for ease of use within the glmnet framework
    
    colnames(df)[which(names(df) == "covid_measure")] <- "y"
    
    
    df <- df %>%
      mutate_at(c('date'), ~ as.Date(., "%Y-%m-%d")) %>%
      mutate(two_week_forecast_date = date + 14)%>%
      mutate_if(is.character, as.factor)
    
    
    
    # Get latest date
    latest_date = max(df$date)
    #Two week outcome
    twoWeek_df = df %>%
      select(date, state, y) %>%
      rename(two_week_outcome = y,
             two_week_forecast_date = date)
    #Merge with original to create a working df
    working_df = df %>%
      left_join(twoWeek_df,
                by = c("state" = "state",
                       "two_week_forecast_date" = "two_week_forecast_date")) %>%
      mutate(year = as.factor(year(two_week_forecast_date)),
             week = as.factor(week(two_week_forecast_date)))
    
    
    working_df[is.na(working_df)] <-0
    
    
    colnames(working_df)[which(names(working_df) == "y")] <- "two_week_backcast"
    colnames(working_df)[which(names(working_df) == "two_week_outcome")] <- "y"
    
    working_df 
  })
  
  
  randfor_data <- reactive({
    
    #drops columns for random forest
    drops <-c("year", "week", "two_week_forecast_date") #droping 
    
    #drops columns that are in the above vector
    df <- forecast_data_glm()[, -which(names(forecast_data_glm()) %in% drops)]
    
    
    ##Moving Ouctome Variable to front of dataset for ease of splitting
    col_idx <- grep("^y$", names(df))
    
    #reorganizes the dataframe so that 
    df <- df[, c(col_idx, (1:ncol(df))[-col_idx])]
    
    
    #replaces NA values in the two_week_outcome variable with NAs
    df <- df %>%  drop_na(y)
    
  }) 
  
  
  bestmtry <- function(randfor_data) ({
    
    #creates the x and y dataframes needed for random forests
    end <- ncol(randfor_data)
    x <- as.data.frame(randfor_data[,2:end])
    y <- as.data.frame(randfor_data[,1])
    
    
    #tune the random forest model
    set.seed(10)
    bestmtry <- as.data.frame(tuneRF(x, y, stepFactor=1.5, improve=1e-5, ntree=500))
    bestmtry_var<- bestmtry[which(bestmtry$OOBError == min(bestmtry$OOBError)),1]
    
    
    return(bestmtry_var)
    
  })
  
  
  
  
  #returns a data frame with the variable importances (according to node purity) after passing in the cleaned data for random forests
  randfor_model <- function(randfor_data, bestmtry) ({
    
 
    
    rf <- randomForest(y ~ ., mtry = bestmtry, data = randfor_data)
    
    #extracts the variable importances
    features <- as.data.frame(rf$importance)
    features <- as.data.frame(setNames(cbind(rownames(features), features, row.names= NULL), c("Feature", "NodPurity")))

    
    
    #extracts the top 10 features with the highest node purity values
     top <- top_n(features, 10, features$NodPurity)


    return(top)
    
  })
  
  #gets the top 10 features from the variable importance plot after random forests has been run
  
  top_10 <- reactive({
    
    mtry <- ncol(randfor_data())/3
    result <- randfor_model(randfor_data(), mtry)
    result
    
  }) 
  
  
  output$varImp <- renderPlot({
    
    var_imp <- top_10()
    
    var_imp <- var_imp %>%
      transform(Feature = reorder(Feature, NodPurity))
    
    var_imp %>%
      ggplot(aes(x = Feature, y = NodPurity)) +
      geom_bar(stat = "identity") +
      labs(title = "Variable Importance Plot", y = "Node Purity") +
      coord_flip() +
      theme(axis.text = element_text(size = 16),
            axis.title = element_text(size = 20, face = "bold"),
            plot.title = element_text(size = 22, face = "bold"))
    
    
  })
  
  
  
  
# --------------------------- Model Building (GLM Model (model 1) and XGBoost (model 2) Pages) ----------------------------
  


  # Create Required Train/Test/Prediction Data Sets ------------------------------------------------
  
 
  createTrainSet <- function(forecast_data) {
    
    latest_date = max(forecast_data$Date)
    
    train = forecast_data[forecast_data$two_week_forecast_date <= (latest_date - 14), ] %>% 
      na.omit()
    
    return(train)
  }
  
  #creates the training dataset for the XGBoost model 
  train_data <- reactive ({
    
    createTrainSet(forecast_data())
    
    
  })
  

  #creates the test set
  createTestSet <- function(forecast_data) {
    
    latest_date = max(forecast_data$Date)
    
    test = forecast_data[(forecast_data$two_week_forecast_date <= latest_date) &
                           (forecast_data$two_week_forecast_date > (latest_date - 14)), ]
    
    return(test)
    
  }
  
  
  #creates a reactive dataframe for the test data
  test_data <- reactive ({
    
    createTestSet(forecast_data())
    
    
  })
  
  
  
  #creates the predictions dataset
  createPredSet <- function(forecast_data) {
    
    latest_date = max(forecast_data$Date) 
    
    
    pred = forecast_data[forecast_data$two_week_forecast_date > latest_date + 7, ]
    
    return(pred)
    
  }
  
  #creates a reactive dataframe for the predictions dataset
  preds_data <- reactive ({
    
    createPredSet(forecast_data())
    
    
  })
  

  
  #--------------------------------------Beginning the GLMNET page (model_1 page)---------------------------------------------
  
  #subsets a given dataframe so that it only includes the 10 most important factors, as determined through random forests
  rf.tops <- function(sample_df) {
    
    top_f <- top_10()$Feature
    
    
    new <- sample_df %>%
      select(y, all_of(top_f))

  
    return(new)
  }
  
  
  train_data_glm <- reactive({
    
    #holds a temporary dataframe that has the forecasting data with a properly formatted date column
    df <- forecast_data_glm() %>%
      rename("Date" = date)
    
     result <- createTrainSet(df)
     
     names(result) <- tolower(names(result))
     

    rf.tops(result)
  })
  
  
  
  test_data_glm <- reactive({
    
    #holds a temporary dataframe that has the forecasting data with a properly formatted date column
        df <- forecast_data_glm() %>%
          rename("Date" = date)

        result <- createTestSet(df)
        
        names(result) <- tolower(names(result))
        

        rf.tops(result)
  })
  
 
  preds_data_glm <- reactive({
    
    #holds a temporary dataframe that has the forecasting data with a properly formatted date column
    df <- forecast_data_glm() %>%
      rename("Date" = date)
    
    result <- createPredSet(df)
    
    names(result) <- tolower(names(result))
    
    
    rf.tops(result)
    
  })
  
  

#------------------ CREATES THE GEOTAGGING REQUIRED FOR GLMNET ------------------------------- #
  
  #Data from: https://public.opendatasoft.com/explore/dataset/us-zip-code-latitude-and-longitude/export/ 
  #Process needed to create 2D for 'state' to be incorporated into GLM with poly option 

  #Geotagging for glmnet
  
  
geo_combine<- function(df){
    geo <- read.csv("~/Documents/GitHub/Covid-19-Closure-Impact/Data/us-zip-code-latitude-and-longitude.csv", header= FALSE, stringsAsFactors = FALSE)
   # geo <- read.csv("https://github.com/Mika-Tal/Covid-19-Closure-Impact/blob/main/Data/us-zip-code-latitude-and-longitude.csv", header = FALSE, stringsAsFactors = FALSE)
    geo<- as.data.frame(geo)
    geo<-geo %>% separate(V1, into=c("Zip", "City", "State", "Latitude","Longitude","Timezone","Daylight_savings", "geopoint"), sep=";" )
    geo<- geo[-1,]
    geo$Latitude<- as.numeric(geo$Latitude)
    geo$Longitude<- as.numeric(geo$Longitude)
    names(geo) <- tolower(names(geo))
    
    state_latLong <- geo%>%
      group_by(state)%>%
      summarise(latitude = mean(latitude, na.rm = TRUE),
                longitude = mean(longitude, na.rm = TRUE))
    
    return(left_join(df, state_latLong, by='state'))
  }
  

  #add state longitude and latitude data to the training, testing, and predictions datasets
  datatrain_lm<- reactive({
    
    geo_combine(train_data_glm())
    
  })
  
  

  datatest_lm<- reactive({
    geo_combine(test_data_glm())
    
  })
  
  datapreds_lm <- reactive({
    
    geo_combine(preds_data_glm())
    
  })
  

  
  cleaning_lm<- function(df){

    df1 <- df %>% select(-c("state"))
    
  
    #dropping date, cannot create a poly w/ cbind() after polys created
    date_hold<- as.data.frame(df1$date)
    colnames(date_hold)<-"date"
    
    drops<-c( "date") #droping 
    df1<-df1[, -which(names(df1) %in% drops)]
    
    feats<- function(xf, column_numbers) {
      xf %>% 
        mutate_at(vars(column_numbers), funs(sqr = (.)^2))%>%
        mutate_at(vars(column_numbers), funs(cube =(.)^3))
    }
    end<-ncol(df1)
    
    #, two_week_forecast_date
    return(cbind(feats(df1, 2:end), date_hold)) #do not include y or "id" in poly calcs
  }
  
  
  

  datatrain_poly<- reactive({
    
    cleaning_lm(datatrain_lm())
    
  })
  
  
  
  datatest_poly <- reactive({
    
    cleaning_lm(datatest_lm())
    
    
  })
  
  datapreds_poly<- reactive({
    
    cleaning_lm(datapreds_lm())
    
  })
  

  
  #Creating the Model Matrices for the GLMnetmodel ---------------------------
  
  createGLM_modelmat <- function(poly_df) {
    
    glmnet_mat <- poly_df %>%
      mutate(across(where(is.factor), ~ fct_lump_lowfreq(.))) %>%
      model.matrix(object= ~ .-1, . , contrasts.arg =
                     lapply(data.frame(.[,sapply(data.frame(.), is.factor)]),
                            contrasts, contrasts = FALSE))
    
    return(glmnet_mat)
    
  }
  
  
  
  #model matrices for each of the different datasets (training, testing, and predictions)
  datatrain_glmnet <- reactive ({
    
    createGLM_modelmat(datatrain_poly())
  })
  
  
  
  datatest_glmnet <- reactive ({
    
    createGLM_modelmat(datatest_poly())
  })
  
  
  datapreds_glmnet <- reactive ({
    
    createGLM_modelmat(datapreds_poly())
  })
  
  
 #-------------------------------Training GLMNnet -------------------
  
  trainGLM_Model <- function(datatrain_glmnet) {
    
    train_glm <- glmnet(
      x = datatrain_glmnet[,-1],
      y = datatrain_glmnet[,1], relax = FALSE, nfolds= 3)
    
    return(train_glm)
    
  }
  
  
  train_glm <- reactive({
    
    trainGLM_Model(datatrain_glmnet())
  })
  
  
#-----------------INSERT COEFFICENTS INFO HERE --------------------#
  
#creates a testing dataframe
  
  # output$testing_glm <- renderDataTable({
  # 
  #   DT::datatable(data = as.matrix(coef(train_glm())),
  #                 rownames = FALSE, options = list(scrollX = T))
  # 
  # 
  # })
  
  
#----------------------------------Predicting with glmnet -------------------
  predictGLM_Model <- function(train_glm, new_data) {
    
    lamMin<- min(train_glm[["lambda"]])
    
    results<-predict(train_glm, 
                     newx= new_data[,-1], 
                     newy= new_data[,1], 
                     s = lamMin , 
                     interval="confidence")
    
    
    return(results)
  }
  
  test_set_predictions <- reactive({
    
    test <- predictGLM_Model(train_glm(), datatest_glmnet())
    as.data.frame(test)
  })
  
  glm_test <- reactive({
    
    as.data.frame(datatest_glmnet())
    
  })
  
  preds_act <- reactive({
    
   preds_act <- cbind(test_set_predictions(), glm_test()$y)
  
   preds_act$Observed<- preds_act[,1]
   preds_act$Predicted<- preds_act[,2]
   
   preds_act <- preds_act %>%
     select(Observed, Predicted)
   
  
  # need to run the createPredSet function again to recover the two_week_forecast_date variable
   df <- forecast_data_glm() %>%
     rename("Date" = date)
   
   result <- createPredSet(df)
  
   names(result) <- tolower(names(result))


   preds_act$two_week_forecast_date <- result$two_week_forecast_date
   
   preds_act
   
    
  })
  
  
  RMSE_glm <- reactive({
    
    sqrt(mean(preds_act()[,2] - preds_act()[,1])^2)
    
    
  })


  preds_set_predictions <- reactive({
    
    preds <-  predictGLM_Model(train_glm(), datapreds_glmnet())
    as.data.frame(preds)
  })
  
final_preds <- reactive({
  
  final_preds <- cbind(preds_set_predictions(), datapreds_lm())
  final_preds$Predicted <- final_preds[,1]
  
  final_preds
  
  
  
})
  
  # output$testing_glm_2 <- renderDataTable({
  # 
  #   DT::datatable(data = final_preds(),
  #                 rownames = FALSE, options = list(scrollX = T))
  # 
  # 
  # })
  # 
  
  
#-------

#datatrain_log <-datatrain_poly
#datatrain_log <- subset(datatrain_log, y>0)
#datatrain_log$y <-log(datatrain_log$y)  

#datatrain_glmnet_log<- datatrain_log %>%
 # mutate(across(where(is.factor), ~ fct_lump_lowfreq(.))) %>%
 # model.matrix(object= ~ .-1, . , contrasts.arg =
              #   lapply(data.frame(.[,sapply(data.frame(.), is.factor)]),
                 #       contrasts, contrasts = FALSE))
#train_glm_log<- glmnet(
 # x = datatrain_glmnet_log[,-1],
 # y = datatrain_glmnet_log[,1], relax = FALSE, nfolds= 3)

#attempt1<-coef.glmnet(datatrain_glmnet, s=lamMin)

#glm_col_names <- names(as.data.frame(datatrain_glmnet_log))
#glm_coef_table<- as.data.frame(matrix(coef.glmnet(train_glm_log, s=lamMin)))

#glm_coef_table$Variables<- glm_col_names
#glm_coef_table<-glm_coef_table%>% rename(Coefficient = 'V1')
#glm_coef_table[1,2]<- "Intercept"
  
#HTML(paste("Coefficients can be interpreted as for each unit of change of the independent (variable) there is that coefficient of percentage change in your COVID-19 measure", sep = "<br/><br/>"))
#------------------------------------------------ Plots --------------------------------
  
  output$final_preds <- renderPlot({


      final_preds = final_preds() %>%
        mutate(state = tolower(setNames(state.name, state.abb)[state]))
     
      
      #create map data
      map = map_data("state") %>%
        full_join(final_preds, by = c("region" = "state"))
      #plot
      map_plot = ggplot(map, aes(long, lat, group = group)) +
        geom_polygon(aes(fill = Predicted), color = "white")+
        scale_fill_viridis_c(option = "B", direction=-1)+
        ggtitle("Predicted COVID Measure (per 100,000) Next Week") +
        theme(axis.text = element_text(size = 16),
              axis.title = element_text(size = 20, face = "bold"),
              plot.title = element_text(size = 22, face = "bold"))
      
      
      
      map_plot 


    })
  
  
  output$preds_act  <- renderPlot({
    
    
    
    obs_vs_pred = preds_act() %>% 
      mutate(Observed = log(Observed),
             Predicted = log(Predicted))
    
    obs_vs_pred_plot = ggplot(obs_vs_pred, aes(x = Observed, y = Predicted),
                              color = 'white') +
      geom_point() +
      geom_abline() +
      #geom_line(aes(y = log(baselineRMSE(test)), color = "#F8766D")) +
      xlab("Log(Observed)") +
      ylab("Log(Predicted)") +
      ggtitle("Log GLM Prediction v. Observed") +
      theme(axis.text = element_text(size = 16),
            axis.title = element_text(size = 20, face = "bold"),
            plot.title = element_text(size = 22, face = "bold"))
    #scale_colour_manual(name = FALSE, values =c('#00BFC4'='#00BFC4','#F8766D'='#F8766D'),
    #labels = c('45-degree','Baseline')) +
    #theme_bw()
    
    obs_vs_pred_plot
    
    
  })
  
  
  output$outcome_all_glm <- renderPlot({
    
    #get the training data
    df <- forecast_data_glm() %>%
      rename("Date" = date)
    
    datatrain <- createTrainSet(df)
    
    names(datatrain) <- tolower(names(datatrain))
    
    
    #Plot Predictions
    outcome_all = datatrain %>% 
      select(two_week_forecast_date, y) %>%
      rename(Date = two_week_forecast_date,
             Outcome = y) %>% 
      mutate (Pred_vs_Obs = "Observed") %>% 
      rbind(tibble(Date = preds_act()$two_week_forecast_date,
                   Outcome = preds_act()$Predicted,
                   Pred_vs_Obs = "Predicted"))
    
    
  
    outcome_plot_all = ggplot(outcome_all, aes(x = Date, y = Outcome))+
      geom_point(aes(color = Pred_vs_Obs)) +
      theme_bw()+
      ggtitle("Glmnet Predictions") +
      
      theme(axis.text = element_text(size = 16),
            axis.title = element_text(size = 20, face = "bold"),
            plot.title = element_text(size = 22, face = "bold"))
    
    outcome_plot_all
    
  })
  
  output$residuals_glm <- renderPlot({
    
    reds<- as.data.frame(preds_act()$Predicted-preds_act()$Observed)
    
    
    GLMResids = preds_act() %>% 
      mutate(Model = "GLM") %>% 
      mutate(Residuals = Predicted - Observed) %>% 
      select(Residuals, Model)
    #df for baseline residuals
    BLResids = tibble(Residuals = mean(datatest_poly()$two_week_backcast) - datatest_poly()$y,
                      Model = "Baseline")
    
    #Combine dfs for the plot
    resids = GLMResids %>% 
      bind_rows(BLResids)
    
    
    #Plot
    resids_plot = ggplot(resids, aes(x = Residuals, fill = Model)) +
      geom_density( color = 'white', alpha = 0.7) +
      theme_bw() +
      theme(axis.text = element_text(size = 16),
            axis.title = element_text(size = 20, face = "bold"),
            plot.title = element_text(size = 22, face = "bold")) +
      
      ggtitle("General Linear Model Residuals")
    
    resids_plot
    
  })
  
  
  
# ------------------------------------XG Boost (Model_2) Page ---------------------------------------------------------------------
  
  output$xgb_pls_wait <- renderUI({
    
  
    
    HTML(paste("Please wait while the XGBoost Model Runs...", sep = "<br/><br/>"))
    
  })
  
  output$display_model <- renderUI({
    
    HTML(paste("Here's the Model Specification for the Best Tuned XGBoost Model:", sep = "<br/><br/>"))
    
  })
  
  
  
  
  
  
  trainXGBoost <- function(train_df) {
    
    #Convert the predictor variable matrices to xgb.DMatrix data types
    XGBTrain = xgb.DMatrix(data.matrix(train_df %>%
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
      XGBTrain, train_df$two_week_outcome,
      trControl = XGBtc,
      tuneGrid = XGBtg,
      method = "xgbTree")
    
    
    
    return(XGBModel)
    
  }
  
  
  
  #displays the title for the forecasting table that is to be displayed
  output$forecast_table <- renderUI({
    
    HTML(paste("Here's The Dataset Including the Two Week Forecasts:", sep = "<br/><br/>"))
    
  })
  
  
  #displays the forecasted data
  output$forecast <- renderDataTable({
    
    DT::datatable(data = forecast_data_glm(),
                  rownames = FALSE,
                  options = list(scrollX = T))
  })
  
  #creates the XGB model to be used by the later functions
  
  
  #captures the results of running the XGB model
  xgb_model_output <- eventReactive(input$runxgb, {
    
    trainXGBoost(train_data())
    
  }
  )
  
  
  #outputs the model specifications for the XGboost model
  output$model_specs <- renderDataTable({
    
    best_model <- xgb_model_output()$bestTune
    
    DT::datatable(data = best_model,
                  rownames = FALSE,
                  options = list(scrollX = T))
    
    
  })
  
  
  # #predictions using the different datasets 
  #  
  #Get predctions based on XGB mmodel
  XGBpredictions <- function(model, data){
    #Convert data frame into dense matrix for XGB model
    matrixData = xgb.DMatrix(data.matrix(data %>%
                                           select(-c(two_week_forecast_date,
                                                     two_week_outcome))))
    #Get predicitons on test set
    XGBpredicitons=predict(model, matrixData)
    return(XGBpredicitons)
  }
  
  
  #creates a dataframe for the results of XGBoost model predictions
  test_data_preds <- reactive({
    XGBpredictions(xgb_model_output(), test_data())
    
  })
  
  
  

  # RMSE = function(test, XGBPredsTest){
  #   #Calculate RMSE
  #   RMSE = sqrt(mean ((test$two_week_outcome - XGBPredsTest) ^ 2 ) )
  #   return(RMSE)
  # }
  
  
  



RMSE_by_state = function (test, XGBPredsTest){
  
  # test_data(), test_data_preds()
  RMSE = sqrt(mean ((test$two_week_outcome - XGBPredsTest) ^ 2 ) )
  
  
  #Create dataframe 
  RMSE_by_state_df = data.frame( State = test$State,
                                 Predictions = XGBPredsTest,
                                 Actual = test$two_week_outcome) %>% 
    group_by(State) %>% 
    mutate(RMSE = sqrt(mean ((Actual - Predictions) ^ 2 ) )) %>% 
    mutate(State = tolower(setNames(state.name, state.abb)[State]))
  
  return(RMSE_by_state_df)
  
}

  
RMSE_by_state_df <- reactive({
  
  RMSE_by_state(test_data(), test_data_preds())
})


output$state_RMSE_XGB <- renderPlot({
  
  #create map data 
  map = map_data("state") %>% 
    left_join(RMSE_by_state_df(), by = c("region" = "State"))
  #plot
  map_plot = ggplot(map, aes(long, lat, group = group)) +
    geom_polygon(aes(fill = -RMSE), color = "white") +
    
    ggtitle("XGB RMSE by State") +
    
    theme(axis.text = element_text(size = 16),
          axis.title = element_text(size = 20, face = "bold"),
          plot.title = element_text(size = 22, face = "bold"))
  
  map_plot
  
})


output$xgb_table <- renderDataTable({
  
  
  DT::datatable(data = RMSE_by_state_df(),
                rownames = FALSE, options = list(scrollX = T))
  
  
  
})
  
#test_data(), test_data_preds()

output$residual_plot_xgb <- renderPlot({
  
  
  #df for XGB Residuals
  XGBResids = test_data() %>% 
    select(covid_measure, two_week_outcome) %>% 
    rename(Observed = two_week_outcome) %>% 
    mutate(Model = "XGB") %>% 
    bind_cols(tibble(Predicted = test_data_preds())) %>% 
    mutate(Residuals = Predicted - Observed) %>% 
    select(Residuals, Model)
  
  
  # df for baseline residuals
  BLResids = tibble(Residuals = mean(test_data()$covid_measure) - test_data()$two_week_outcome,
                    Model = "Baseline")
  
  
  #Combine dfs for the plot
  resids = XGBResids %>%
    bind_rows(BLResids) 
  
  
  #Plot
  resids_plot = ggplot(resids, aes(x = Residuals, fill = Model)) +
    geom_density( color = 'white', alpha = 0.7) +
    theme_bw() + 
    ggtitle("XGB Residual Density") +
    theme(axis.text = element_text(size = 16),
          axis.title = element_text(size = 20, face = "bold"),
          plot.title = element_text(size = 22, face = "bold"))
  
  resids_plot
  
})


  
  
  combinedObvsPredsDf <- function(test, predicted){
    
    #Create dataframe
    obs_vs_pred = test %>%
      select(two_week_outcome) %>%
      rename(Observed = two_week_outcome) %>%
      bind_cols(tibble(Predicted = predicted)) %>%
      mutate(Observed = log(Observed),
             Predicted = log(Predicted))
    
    return(obs_vs_pred)
    
  }
  
  
  output$testing <- renderDataTable({
    
    new_df <- combinedObvsPredsDf(test_data(), test_data_preds())
    
    
    DT::datatable(data = new_df,
                  rownames = FALSE, options = list(scrollX = T))
    
  })
  
  output$preds_vs_observed <- renderPlot({
    
    new_df <- combinedObvsPredsDf(test_data(), test_data_preds())
    
    
    #creates the required dataframe
    new_df %>%
      ggplot(aes(x = Observed, y = Predicted)) +
      geom_point() +
      geom_abline() +
      xlab("Log(Observed)") +
      ylab("Log(Predicted)") +
      theme(axis.text = element_text(size = 16),
            axis.title = element_text(size = 20, face = "bold"),
            plot.title = element_text(size = 22, face = "bold")) +
      ggtitle("XGB Actual vs. Predicted")
    
    
    
    
  })
  
  #creates a new reactive dataframe
  
  outcome_all <- reactive({
    
    XGBPredsPred <- XGBpredictions(xgb_model_output(), preds_data())
    
    train_data() %>% 
      select(two_week_forecast_date, two_week_outcome) %>%
      rename(Date = two_week_forecast_date,
             Outcome = two_week_outcome) %>% 
      mutate (Pred_vs_Obs = "Observed") %>% 
      rbind(tibble(Date = preds_data()$two_week_forecast_date,
                   Outcome = XGBPredsPred,
                   Pred_vs_Obs = "Predicted"))
    
    
  })
  
  
  
  
  
  
  #plots the resulting predictions
  output$all_predictions <- renderPlot({
    
    outcome_all() %>% 
      
      ggplot(aes(x = Date, y = Outcome)) +
      
      geom_point(aes(color = Pred_vs_Obs))  +
      theme(axis.text = element_text(size = 16),
            axis.title = element_text(size = 20, face = "bold"),
            plot.title = element_text(size = 22, face = "bold"))
    
    
    
  })
  
  
  
  createPredState <- function(forecast_data, state) {
    
    latest_date = max(forecast_data$Date)
    pred_state = forecast_data[forecast_data$two_week_forecast_date > latest_date + 7, ] %>%
      filter(State == state)
    
  }
  
  
  #create a new reactive dataframe for the desired predictions for a given state
  pred_state <- reactive({
    
    createPredState(forecast_data(), input$state)
    
  })
  
  
  outcome_state <- reactive({
    
    XGBModel_state <- XGBpredictions(xgb_model_output(), pred_state())
    
    train_data() %>% 
      filter(State == input$state) %>% 
      select(two_week_forecast_date, two_week_outcome) %>%
      rename(Date = two_week_forecast_date,
             Outcome = two_week_outcome) %>% 
      mutate (Pred_vs_Obs = "Observed") %>% 
      rbind(tibble(Date = pred_state()$two_week_forecast_date,
                   Outcome = XGBModel_state,
                   Pred_vs_Obs = "Predicted"))
    
  })
  
  
  output$one_state_preds <- renderPlot({
    
    outcome_state() %>% 
      
      ggplot(aes(x = Date, y = Outcome)) +
      
      geom_point(aes(color = Pred_vs_Obs)) +
      
      theme(axis.text = element_text(size = 16),
            axis.title = element_text(size = 20, face = "bold"),
            plot.title = element_text(size = 22, face = "bold"))
    
    
    
    
    
  })
  
  
  #Results Page -----------------------------------------------------------
  
  
  #prints out the results of running the XGBoost Model in tabular form
  output$predictions_tabular <- renderDataTable({
    
    #Create week labels column
    Week = rep("Week_1", 51) %>% c(rep("Week_2", 51))
    
    XGBPredsPred <- XGBpredictions(xgb_model_output(), preds_data())
    
    
    
    #Summarise predictions by prediction week
    preds_all = data.frame(Prediction  = XGBPredsPred, Week = Week) %>%
      group_by(Week) %>% 
      summarise(Total = sum(Prediction),
                Average = mean(Prediction))
    
    DT::datatable(data = preds_all,
                  rownames = FALSE, options = list(scrollX = T))
    
    
  })
  
  #another reactive object for the second state input option
  pred_state_2 <- reactive({
    
    createPredState(forecast_data(), input$state_2)
    
  })
  
  
  #prints out the predictions by state
  output$predictions_state_tabular <- renderDataTable({
    
    #Create week labels column
    Week = c("Week_1", "Week_2")
    
    #Summarise predictions by prediction week
    XGBPredsState <- XGBpredictions(xgb_model_output(), pred_state_2())
    
    preds_state = data.frame(Week = Week, Prediction  = XGBPredsState) 
    
    DT::datatable(data = preds_state,
                  rownames = FALSE, options = list(scrollX = T))
    
  })
  
  
  output$result_tabular_all <- renderDataTable({
    
    XGBPredsPred <- XGBpredictions(xgb_model_output(), preds_data())
    preds_glm <- final_preds()$Predicted
    test <- test_data()
    
    baseline = test %>%  
      select(covid_measure, two_week_outcome) %>% 
      mutate(average_outcome = mean(covid_measure)) 
    
    XGBRMSE = sqrt(mean ((test$two_week_outcome - XGBPredsPred) ^ 2 ) )
    
    GLMRMSE = sqrt(mean ((test$two_week_outcome - preds_glm) ^ 2 ) )
    
    BLRMSE = sqrt(mean (
      (baseline$two_week_outcome - baseline$average_outcome) ^ 2 ) )
    
    
    results = data.frame(Prediction = test$two_week_outcome,
                         Model = rep("Baseline", nrow(test)),
                         RMSE = BLRMSE) %>% 
      bind_rows(data.frame(Prediction  = XGBPredsPred,
                           Model = rep("XGB", length(XGBPredsPred)),
                           RMSE = XGBRMSE)) %>%
      bind_rows(data.frame(Prediction = as.vector(preds_glm),
                           Model = rep("GLM", length(preds_glm)),
                           RMSE = GLMRMSE)) %>% 
      group_by(Model) %>% 
      summarise(Total = sum(Prediction),
                AvgPerState = mean(Prediction),
                RMSE = mean(RMSE)) 
      
    
       results
    
  })
  

  

}

# Run the application 
shinyApp(ui = ui, server = server)
