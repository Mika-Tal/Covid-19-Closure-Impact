# In order to run our app locally, a user would need to install the following packages:
# 1. Install Delphi R package from below
#https://cmu-delphi.github.io/covidcast/covidcastR/articles/covidcast.html
# #installs the COVIDCast API information


#To test out our App, you can use the sample dataset provided on our Github site, under the "Data" folder: "user_input_policies_full_example.csv"

#Load in necessary libraries

library(covidcast)
library(shiny) #enables the use of shiny functionality
library(shinydashboard) #allows for the creation of a dashboard using shiny
library(tidyverse)
library(DT) #provides easy to use data tables within shiny
library(glmnet) # to perform L1 regularization
library(tidymodels)
library(lubridate)
library(randomForest)
library(xgboost)
library(caret)
library(fuzzyjoin)
library(maps)
library(stringr) # aids with string handling

#read in static datasets -- controls only and the user ingestion template
state_controls <- read.csv("state_controls.csv")
user_input_template <- read.csv("user_input_template.csv")




#Dashboard Header & Title ---------------------------------------------------------
header <- dashboardHeader(title = "Machine Learning Pipeline Visualization Using COVID-19 Data",
                          titleWidth = 600)


#Dashboard Sidebar --------------------------------------------------

sidebar <- dashboardSidebar(width = 250,
                            sidebarMenu(
                              id = "tabs",
                              
                              #Menu Items--------------------------
                              menuItem("Overview & Data Ingestion", tabName = "overview") ,
                              menuItem("Data Merging", tabName = "merging"),
                              menuItem("Data Pre-Processing", tabName = "process"),
                              menuItem("Feature Selection", tabName = "features"),
                              menuItem("Model 1: Generalized Linear Model ", tabName = "model_1"),
                              menuItem("Model 2: eXtreme Gradient Boosting", tabName = "model_2"),
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
            tags$head(tags$style("#welcome{color: black;
                                 font-size: 16px;
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
                           end = "2021-03-01",
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
            
            #adds visual space
            br(),
            br(),
            
            
            #displays the resulting dataTable from merging
            actionButton(inputId = "click_merge",
                         label = "Click to Merge Datasets"),
            
            #adds visual space
            br(),
            br(),
            #adds note about having to click the action button to merge the datsets
            htmlOutput(outputId = "how_to_merge"),
            
            #changes how the 'how_to_merge' message should display--------
            tags$head(tags$style("#how_to_merge{color: black;
                                 font-size: 14px;
                                  }"
            )
            ),
            
            
            
            #add visual space
            br(),
            br(),
            
            #labels the following datatable
            htmlOutput(outputId = "merged"),
            
            #changed how the merged message should display--------
            tags$head(tags$style("#merged{color: black;
                                 font-size: 16px;
                                  }"
            )
            ),
            
            
            #add visual space
            br(),
            br(),
            
            #displays the merged dataset, according to the user's specifications
            dataTableOutput(outputId = "display_merged_data")
            
            
    ),
    
    
    #Data Pre-Processing page-----------------------------------------------
    tabItem("process",
            
            #displays words to go before displaying the dataTable output
            htmlOutput(outputId = "no_miss_mess"),
            
            #changed how the 'no missing' message should display--------
            tags$head(tags$style("#no_miss_mess{color: black;
                                 font-size: 16px;
                                 }"
            )
            ),
            
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
            
            
            #click to run  random forest 
            actionButton(inputId = "run_rand_for",
                         label = "Click to Run Random Forests"),
            
            br(),
            br(),
            
            #once the "Run Random Forest" button is clicked, display the following components
            conditionalPanel(condition = "input.run_rand_for != 0",
                             
                             #message to show label what is being displayed
                             htmlOutput(outputId = "ft_sel"),
                             
                             #changed how the feature selection message should display--------
                             tags$head(tags$style("#ft_sel{color: black;
                                 font-size: 16px;
                                 }"
                             )
                             ),
                             
                             
                             br(),
                             
                             
                             #shows the variable importance plot
                             plotOutput(outputId = "varImp"),
            )
            
    ),
    
    
    #GLM Page ------------------------------------------------------------------------
    tabItem("model_1",
            
            #click to run the GLM model
            actionButton(inputId = "runglm",
                         label = "Click to Run the General Linear Model"),
            
            #adds visual space between the components
            br(),
            
            br(),
            
            #once the "Run GLM" button is clicked & the model is done running, display the following elements
            conditionalPanel(condition = "input.runglm != 0",
                             
                             htmlOutput(outputId = "coefs"),
                             
                             #changed how the coefficients selection message should display--------
                             tags$head(tags$style("#coefs{color: black;
                                 font-size: 16px;
                                 }"
                             )
                             ),
                             
                             br(),
                             br(),
                             
                             #display the coefficients from the general linear model
                             dataTableOutput(outputId = "glm_coefs"),
                             
                             
                             br(),
                             br(),
                             
                             #display the predictions vs the observed plot
                             plotOutput(outputId = "preds_act"),
                             
                             br(),
                             br(),
                             
                             #display a residual density plot
                             plotOutput(outputId = "residuals_glm") 
            )
            
    ),
    
    
    #XGBoost Page ------------------------------------------------------------------------------
    tabItem("model_2",
            
            #click to run the XGBoost model
            actionButton(inputId = "runxgb",
                         label = "Click to Run the XGBoost Model"),
            
            
            #creates visual space
            br(),
            br(), 
            
            
            
            
            #once the "Run XG Boost button has run, display the following information:"
            conditionalPanel(condition = "input.runxgb != 0",
                             
                             
                             #creates a lable message for the model specifications data table
                             htmlOutput(outputId = "display_model"), 
                             
                             #change the formatting of the html text display
                             tags$head(tags$style("#display_model{color: black;
                                 font-size: 16px;
                                  }"
                             )
                             ),
                             
                             br(),
                             br(),
                             
                             #creates a datatable that presents the model specifications for the xg boost model
                             dataTableOutput(outputId = "model_specs"),
                             
                             
                             #creates visual space
                             br(),
                             br(),
                             
                             #creates a plot showing the performance of the XG boost model (predicted vs observed)
                             plotOutput(outputId = "preds_vs_observed"),
                             
                             
                             #creates visual space
                             br(),
                             br(),
                             
                             
                             #plots the RMSE for each state, to show how well the model performed for each state in the dataset
                             plotOutput(outputId = "state_RMSE_XGB"),
                             
                             br(),
                             br(),
                             
                             #creates a plot of the density of the residuals for the XG Boost model
                             plotOutput(outputId = "residual_plot_xgb")
                             
            )
    ),
    
    
    
    tabItem("results",
            
            #displays a comparison between the XG Boost, Baseline, and General Linear Model performance metrics
            dataTableOutput(outputId = "result_tabular_all"),
            
            br(), 
            
            #allows a user to select the state that they would like a prediction for
            selectInput(inputId = "state",
                        label = "Select a state that you would like predictions for: ",
                        choices = c(state_controls %>% select("State"))),
            
            br(),
            
            
            
            #shows a forecast for one state, using the results from the XG Boost model
            plotOutput(outputId = "one_state_preds"),
            
            br(),
            
            
            br(),
            
            #shows a map of the predicted covid cases per 100,000 at a national level
            plotOutput(outputId = "forecast_by_state")
            
    )
    
    
  )
)





ui <- dashboardPage(header, sidebar, body)



# Define server logic required to run the app
server <- function(input, output) {
  
  
  #------------------------------------ OVERVIEW & DATA INGESTION PAGE OUTPUT ---------------------------------------------------------------------------------------
  
  output$welcome <- renderUI({
    
    
    HTML(paste("Hello! Welcome to our COVID-19 Forecasting and Machine Learning Visualization App. To get started:", "Step 1: Choose the Date Range For Which You Would Like COVID Data ", "Step 2: Query the CMU Delphi API to Get the Requested COVID Data", "Step 3: Upload The Additional Features You Would Like To Use, Using the Provided Template", "Step 4: Step Through the Remaining Pages on the Dashboard to walk through the Machine Learning Process",sep = "<br/><br/>"))
    
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
  
  
  #displays the default control variables that come with the app to the user
  output$controls_only <- renderDataTable({
    
    DT::datatable(data = state_controls,
                  rownames = FALSE,
                  options = list(scrollX = T))
  })
  
  
  #allows the user to download the data ingestion template
  output$template <- downloadHandler(
    filename = "Data_Ingestion_Template.csv",
    content = function(file) {
      
      write.csv(user_input_template, file, row.names = FALSE) #***change the file that is downloaded
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
  
  
  #----------------------------------------- DATA MERGING PAGE OUTPUT ------------------------------------------------------------------------------
  
  
  #creates a display message for how/when you need to click the "merge dataset" button
  
  output$how_to_merge <- renderUI({
    
    HTML(paste("Please note: You will need to click on the button above, even if you are not using our set of control features.", "Based on your selections, the above merge button will either return just the COVID-19 data pulled on the previous page or a combination of the COVID-19 data, your inputted data, and which controls of ours you decide to use.", "This button can be clicked several times if your data preferences change.", sep = "<br/><br/>"))
    
    
  })
  
  #creates the display message for the merged dataset
  output$merged <- renderUI({
    
    HTML(paste("Here's Your Merged Dataset:", sep = "<br/><br/>"))
    
  })
  
  
  #creates a function that merges the data according the user's preferences (and various input values available in the UI)
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
  
  
  #creates the merged dataset that the user desires when the user clicks on "merged dataset" button
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
  
  #displays the resutling data set from the options selected by the user
  output$display_merged_data <- renderDataTable({
    
    DT::datatable(data = merged_dataset(),
                  rownames = FALSE,
                  options = list(scrollX = T))
    
  })
  
  
  #------------------------------------- DATA PRE-PROCESSING PAGE OUTPUT ---------------------------------------------------------------
  
  #function checks how much missingness there is in each of the variables and returns a dataframe with the msisingness for each feature in the datset
  check_missingness <- function(merged_df){
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
  
  
  #displays a label for the missingness table
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
  
  #outputs the dataset after missingness has been deal with, to allow the user to inspect the results
  output$data_aft_missing <- renderDataTable({
    
    merged_df <- merged_dataset()
    df_aft_miss <- data_after_missing()
    DT::datatable(data = df_aft_miss,
                  rownames = FALSE,
                  options = list(scrollX = T))
    
    
  })
  
  
  
  
  #------------------------------------------- FEATURE SELECTION (using random forests) PAGE OUTPUT  ---------------------------------------------------
  
  #displays a label for the results of feature selection
  output$ft_sel <- renderUI({
    
    HTML(paste("Here's The Results of Feature Selection:", "Here's the Features with Top 10 Node Purity Values:", sep = "<br/><br/>"))
    
  })
  
  
  #Creates the forecast data for the generalized linear model
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
  
  
  #pre-processes the data before going into random forest
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
  
  #conducts model tuning to find the best mtry value for random forests
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
  top_10 <- eventReactive(input$run_rand_for, {
    
    mtry <- ncol(randfor_data())/3
    result <- randfor_model(randfor_data(), mtry)
    result
    
  }
  )
  
  
  
  #plots the top 10 variables found using random forest 
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
  
  
  
  
  # ------------------------------------ MODEL BUILDING PAGES: (GLM Model (model 1) and XGBoost (model 2) Pages) ----------------------------
  
  
  #------------------------------------ CREATES THE DATASETS & FUNCTIONS USED FOR FORECASTING MODELS--------------------------------------------
  
  #returns a dataframe with function that maps existing data to two weeks in the future (for dates that we have data for both in the present and two weeks from that point)
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
  
  
  #returns the training data set (that is based on a sequential split of the data: all but the latest 2 weeks in the dataset are in the training dataset))
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
  
  
  #returns the test set (that is based on a sequential split of the data: includes the most recent two weeks of data available in the dataset )
  createTestSet <- function(forecast_data) {
    
    latest_date = max(forecast_data$Date)
    
    test = forecast_data[(forecast_data$two_week_forecast_date <= latest_date) &
                           (forecast_data$two_week_forecast_date > (latest_date - 14)), ]
    
    return(test)
    
  }
  
  
  #creates a reactive dataframe for the test data for the XGBoost model
  test_data <- reactive ({
    
    createTestSet(forecast_data())
    
    
  })
  
  
  
  #returns the predictions dataset 
  createPredSet <- function(forecast_data) {
    
    latest_date = max(forecast_data$Date) 
    
    
    pred = forecast_data[forecast_data$two_week_forecast_date > latest_date + 7, ]
    
    return(pred)
    
  }
  
  #creates a reactive dataframe for the predictions dataset
  preds_data <- reactive ({
    
    createPredSet(forecast_data())
    
    
  })
  
  
  
  #-------------------------------------- MODEL_1: GENERAL LINEAR MODEL OUTPUT PAGE ---------------------------------------------
  
  
  #STEP 1: CREATE THE TRAINING, TESTING, AND PREDICTION DATASETS FOR THE GENERAL LINEAR MODEL--------------
  
  #subsets a given dataframe so that it only includes the 10 most important factors, as determined through random forests
  rf.tops <- function(sample_df) {
    
    top_f <- top_10()$Feature
    
    
    new <- sample_df %>%
      select(y, all_of(top_f))
    
    
    return(new)
  }
  
  
  #creates the training dataset for the general linear model
  train_data_glm <- reactive({
    
    #holds a temporary dataframe that has the forecasting data with a properly formatted date column
    df <- forecast_data_glm() %>%
      rename("Date" = date)
    
    result <- createTrainSet(df)
    
    names(result) <- tolower(names(result))
    
    
    rf.tops(result)
  })
  
  
  
  #creates the test dataset for the general linear model
  test_data_glm <- reactive({
    
    #holds a temporary dataframe that has the forecasting data with a properly formatted date column
    df <- forecast_data_glm() %>%
      rename("Date" = date)
    
    result <- createTestSet(df)
    
    names(result) <- tolower(names(result))
    
    
    rf.tops(result)
  })
  
  
  #creates the training dataset for the general linear model
  preds_data_glm <- reactive({
    
    #holds a temporary dataframe that has the forecasting data with a properly formatted date column
    df <- forecast_data_glm() %>%
      rename("Date" = date)
    
    result <- createPredSet(df)
    
    names(result) <- tolower(names(result))
    
    
    rf.tops(result)
    
  })
  
  
  
  #STEP 2: CONDUCT THE GEOTAGGING REQUIRED FOR THE GLM MODEL--------------------------
  
  #Data from: https://public.opendatasoft.com/explore/dataset/us-zip-code-latitude-and-longitude/export/ 
  #Process needed to create 2D for 'state' to be incorporated into GLM with poly option 
  
  #returns a geotagged dataframe 
  geo_combine<- function(df){
    geo <- read.csv("us-zip-code-latitude-and-longitude.csv", header= FALSE, stringsAsFactors = FALSE)
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
  
  
  #STEP 3: ADD STATE LONGITUDE AND LATITUDE DATA TO THE TRAINING, TESTING, AND PREDICTION DATASETS------------
  
  datatrain_lm<- reactive({
    
    geo_combine(train_data_glm())
    
  })
  
  
  
  datatest_lm<- reactive({
    geo_combine(test_data_glm())
    
  })
  
  datapreds_lm <- reactive({
    
    geo_combine(preds_data_glm())
    
  })
  
  
  #STEP 4: ADD POLYNOMIAL TRANSFORMATIONS OF FEATURES TO THE DATASET BEFORE GOING INTO GLM MODEL TRAINING------------
  
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
  
  
  
  
  datatrain_poly <- reactive({
    
    cleaning_lm(datatrain_lm())
    
  })
  
  
  
  datatest_poly <- reactive({
    
    cleaning_lm(datatest_lm())
    
    
  })
  
  datapreds_poly<- reactive({
    
    cleaning_lm(datapreds_lm())
    
  })
  
  
  
  #STEP 5: CREATE THE MODEL MATRICIES BEFORE GOING INTO GLM MODEL TRAINING------------
  
  #returns a model matrix for the training, testing, and prediction datasets
  createGLM_modelmat <- function(poly_df) {
    
    glmnet_mat <- poly_df %>%
      mutate(across(where(is.factor), ~ fct_lump_lowfreq(.))) %>%
      model.matrix(object= ~ .-1, . , contrasts.arg =
                     lapply(data.frame(.[,sapply(data.frame(.), is.factor)]),
                            contrasts, contrasts = FALSE))
    
    return(glmnet_mat)
    
  }
  
  
  #creates model matrix for the training data
  datatrain_glmnet <- reactive ({
    
    createGLM_modelmat(datatrain_poly())
  })
  
  
  
  datatest_glmnet <- reactive ({
    
    createGLM_modelmat(datatest_poly())
  })
  
  
  datapreds_glmnet <- reactive ({
    
    createGLM_modelmat(datapreds_poly())
  })
  
  
  #STEP 6: TRAIN THE GLM MODEL-----------------------------------------------------------
  
  #returns the results of the training the general linear model
  trainGLM_Model <- function(datatrain_glmnet) {
    
    train_glm <- glmnet(
      x = datatrain_glmnet[,-1],
      y = datatrain_glmnet[,1], relax = FALSE, nfolds= 3)
    
    return(train_glm)
    
  }
  
  
  train_glm <- eventReactive(input$runglm, {
    
    trainGLM_Model(datatrain_glmnet())
    
  }
  )
  
  
  #STEP 7: CONDUCT PREDICTION USING THE PREVIOUSLY TRAINED GLM MODEL -----------------------------------------------------------
  
  #returns the results of predicting using the general linear model
  predictGLM_Model <- function(train_glm, new_data) {
    
    lamMin<- min(train_glm[["lambda"]])
    
    results<-predict(train_glm, 
                     newx= new_data[,-1], 
                     newy= new_data[,1], 
                     s = lamMin , 
                     interval="confidence")
    
    
    return(results)
  }
  
  #creates a dataframe of the results of prediction using the test set
  test_set_predictions <- reactive({
    
    test <- predictGLM_Model(train_glm(), datatest_glmnet())
    as.data.frame(test)
  })
  
  #converts a dataframe for the model matrix into a dataframe
  glm_test <- reactive({
    
    as.data.frame(datatest_glmnet())
    
  })
  
  
  #creates a dataframe that combines the test set predictions with the original test set data
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
  
  
  #calculates the Root Mean Squared Error of using the Generalized Linear Model
  RMSE_glm <- reactive({
    
    sqrt(mean(preds_act()[,2] - preds_act()[,1])^2)
    
    
  })
  
  
  #calcualtes the predictions for the prediction dataset
  preds_set_predictions <- reactive({
    
    preds <-  predictGLM_Model(train_glm(), datapreds_glmnet())
    as.data.frame(preds)
  })
  
  #combines the predictions from the model with the original predictions dataset
  final_preds <- reactive({
    
    final_preds <- cbind(preds_set_predictions(), datapreds_lm())
    final_preds$Predicted <- final_preds[,1]
    
    final_preds
    
    
    
  })
  
  
  #STEP 8: CREATE VISUAL REPRESENTATIONS FOR THE OUTPUT FROM THE GLM MODEL -----------------------------------------------------------
  
  
  #------- STEP 8a: CREATE GLM COEFFICIENTS TABLE ---------#
  
  
  #creates a new dataframe that takes the log of the polynomial dataframe
  
  datatrain_log <- reactive({
    
    datatrain_log <- datatrain_poly()
    datatrain_log <- subset(datatrain_log, y>0)
    datatrain_log$y <- log(datatrain_log$y)
    
    datatrain_log
    
    
  })
  
  
  #creates a model matrix for the GLM model using the newly created dataframe with the log of y values
  datatrain_glmnet_log <- reactive({
    
    createGLM_modelmat(datatrain_log())
    
  })
  
  #trains a new glm model using the log of the original dataset
  train_glm_log<- reactive({
    
    trainGLM_Model(datatrain_glmnet_log())
    
  })
  
  
  
  #creates the label for the coefficients table
  output$coefs <- renderUI({
    
    
    HTML(paste("The coefficients below can be interpreted as follows: 'For each unit change in the (variable of interest), there is a (coefficient*100) percentage change in your COVID-19 measure' : ", sep = "<br/><br/>"))
    
  })
  
  
  #creates and formats the datatable for the generalized linear model output
  output$glm_coefs <- renderDataTable({
    
    
    glm_col_names <- names(as.data.frame(datatrain_glmnet_log()))
    
    lamMin<- min(train_glm_log()[["lambda"]])
    
    
    glm_coef_table <- as.data.frame(matrix(coef.glmnet(train_glm_log(), s=lamMin)))
    
    glm_coef_table$Variables<- glm_col_names
    
    glm_coef_table<-glm_coef_table%>% rename(Coefficient = 'V1')
    
    glm_coef_table[1,2]<- "Intercept"
    
    
    DT::datatable(data = glm_coef_table %>% 
                    select(Variables, Coefficient),
                  rownames = FALSE, options = list(scrollX = T))
    
    
  })
  
  
  #------- STEP 8b: CREATE GLM LOG  PREDICTIONS VS OBSERVED PLOT ---------#
  
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
    
    
    obs_vs_pred_plot
    
    
  })
  
  #------- STEP 8c: CREATE GLM RESIDUALS PLOT ---------#
  
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
  
  
  #displays a label before displaying the model specifications
  output$display_model <- renderUI({
    
    HTML(paste("Here's the Model Specification for the Best Tuned XGBoost Model:", sep = "<br/><br/>"))
    
  })
  
  #STEP 1: TRAIN XGBoost Model ------------------------------
  
  #returns the trained XGBoost Model
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
    XGBtg <- expand.grid(nrounds = 20, #temporary value to facilitate faster run time during app construction c(50, 100, 250),
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
  
  
  
  #captures the results of running the XGB model in a reactive dataframe
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
  
  
  #STEP 2: GET PREDICTIONS FOR OTHER DATASETS USING THE TRAINED XGBOOST MODEL---------------------------------
  
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
  
  
  
  #returns the performance (RMSE) of the model by state
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
  
  #captures the resulting dataframe for calculating the RMSE for COVID-19 predictions by state
  RMSE_by_state_df <- reactive({
    
    RMSE_by_state(test_data(), test_data_preds())
  })
  
  
  
  #STEP 3: CREATE VISUAL REPRESENTATIONS FOR THE OUTPUT FROM THE XG BOOST MODEL -----------------------------------------------------------
  
  
  #-------STEP 3a: PLOT OF PREDICTED VERSUS OBSERVED OBSERVATIONS OF THE XGBoost Model (compared to the baseline average) ----------#
  
  #creates a dataframe that compares the predicted versus actual values
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
  
  
  
  #creates the log predicted versus observed observations usingthe XGBoost Model
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
      ggtitle("eXtreme Gradient Boosting Actual vs. Predicted")
    
    
    
    
  })
  
  
  #-------STEP 3b: PLOT OF RMSE BY STATE-----------#
  
  #outputs a map plot of the RMSE by state
  output$state_RMSE_XGB <- renderPlot({
    
    #create map data 
    map = map_data("state") %>% 
      left_join(RMSE_by_state_df(), by = c("region" = "State"))
    #plot
    map_plot = ggplot(map, aes(long, lat, group = group)) +
      geom_polygon(aes(fill = -RMSE), color = "white") +
      
      ggtitle("eXtreme Gradient Boosting RMSE by State") +
      
      theme(axis.text = element_text(size = 16),
            axis.title = element_text(size = 20, face = "bold"),
            plot.title = element_text(size = 22, face = "bold"))
    
    map_plot
    
  })
  
  
  
  #-------STEP 3c: PLOT OF RESIDUALS OF THE XGBoost Model (compared to the baseline average) ----------#
  
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
      ggtitle("eXtreme Gradient Boosting Residual Density") +
      theme(axis.text = element_text(size = 16),
            axis.title = element_text(size = 20, face = "bold"),
            plot.title = element_text(size = 22, face = "bold"))
    
    resids_plot
    
  })
  
  #--------------------------------- RESULTS OUTPUT PAGE ---------------------------------------
  
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
      (test$two_week_outcome - baseline$average_outcome) ^ 2 ) )
    
    
    results = data.frame(Prediction = test$two_week_outcome,
                         Model = rep("Baseline", nrow(test)),
                         RMSE = BLRMSE) %>%
      bind_rows(data.frame(Prediction = as.vector(preds_glm),
                           Model = rep("GLM", length(preds_glm)),
                           RMSE = GLMRMSE)) %>%
      bind_rows(data.frame(Prediction  = XGBPredsPred,
                           Model = rep("XGB", length(XGBPredsPred)),
                           RMSE = XGBRMSE)) %>%
      group_by(Model) %>%
      summarise("Total National Count" = sum(Prediction),
                "Avg Per State" = mean(Prediction),
                RMSE = mean(RMSE))
    
    
    results
    
  })  
  

  
  #creates a new reactive dataframe thart includes all of the predictions across all the states
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
  
  
  #reutrns predictions from the XGBoost model for one state
  createPredState <- function(forecast_data, state) {
    
    latest_date = max(forecast_data$Date)
    pred_state = forecast_data[forecast_data$two_week_forecast_date > latest_date + 7, ] %>%
      filter(State == state)
    
  }
  
  
  #create a new reactive dataframe for the desired predictions for a given state
  pred_state <- reactive({
    
    createPredState(forecast_data(), input$state)
    
  })
  
  #returns the XG Boost Model predictions for the singular tate that the user selects
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
  
  #outputs the results of the predictions for one state as a plot
  output$one_state_preds <- renderPlot({
    
    outcome_state() %>% 
      
      ggplot(aes(x = Date, y = Outcome)) +
      
      geom_point(aes(color = Pred_vs_Obs)) +
      
      theme(axis.text = element_text(size = 16),
            axis.title = element_text(size = 20, face = "bold"),
            plot.title = element_text(size = 22, face = "bold"))
    
  })
  
  
  
  #creates a map plot of the expected forecast in two weeks (nationally)
  output$forecast_by_state <- renderPlot({
    
    #Predict on Pred set using XGB model
    XGBPredsPred = XGBpredictions(xgb_model_output(), preds_data())
    
    #Create state-prediction df
    forecast_by_state = data.frame( State = preds_data()$State,
                                    Predictions = XGBPredsPred) %>%
      mutate(State = tolower(setNames(state.name, state.abb)[State]))
    
    #join to the map data
    map = map_data("state") %>%
      left_join(forecast_by_state, by = c("region" = "State"))
    
    #plot
    map_plot = ggplot(map, aes(long, lat, group = group)) +
      geom_polygon(aes(fill = -Predictions), color = "white") +
      scale_fill_viridis_c(option = "B")+
      ggtitle("Predicted COVID-19 Measure per 100,000") +
      theme_bw() +
      theme(axis.text = element_text(size = 16),
            axis.title = element_text(size = 20, face = "bold"),
            plot.title = element_text(size = 22, face = "bold"))
    
    
    
    
    
    map_plot 
    
    
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
