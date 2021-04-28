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
# library(plotly) #used to create interactive plots
#library(glmnet) # to perform L1 regularization
library(tidymodels)
library(covidcast)
library(lubridate)
library(randomForest)
library(xgboost)
library(caret)
# add these libraries 
libs = c("tidyverse","data.table","stargazer", "caret", "e1071", "splines",
         "randomForest", "C50", "xgboost", "ggplot2", "cowplot", "forecast")


#read in static datasets -- controls only and the sample user input data
state_controls <- read.csv("~/Documents/GitHub/Covid-19-Closure-Impact/Data/state_controls.csv")
user_input_example <- read.csv("~/Documents/GitHub/Covid-19-Closure-Impact/Data/user_input_policies_full_example.csv")

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
    menuItem("Model Building ", tabName = "model")
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
    tabItem("missing"
            
    ),
    
    #Feature Selection page ------------------------------------------------------------------------------
    tabItem("features",
            
            
            
            #allows the user to decide how to create the training subset -----------------------------------
            radioButtons(inputId = "train_decide",
                         label = "How do you want to train your data?",
                         choices = c("Time Ordered", "Random Shuffling"),
                         selected = c("Random Shuffling")),
            
            
            #allows the user to decide how many features to include in their variable importance plot --------------
            sliderInput(inputId = "num_feats",
                        label = "How many features would you like to include?",
                        min = 0,
                        max = 5,
                        value = 4),
            
            
            
            #adds visual space
            
            br(),
            
            br(),
            
            br(),
            
            #message to show label what is being displayed
            htmlOutput(outputId = "ft_sel"),
            
            
            #shows the variable importance plot
            plotOutput(outputId = "varImp"),
            
            
            
            #display the ROC curves for the different methodologies used
            textOutput(outputId = "intro"),
            textOutput(outputId = "selected_vars"), #results from regularization
            
            br(),
            
            br(),
            
            
            plotOutput(outputId = "lasso_results"),
            
            plotOutput(outputId = "policy_over_time"),
            dataTableOutput(outputId = "policy_table")
            
            
            ##Model Building 7 Two-Week Forecast
            
    ),
    
    #Model Building page ------------------------------------------------------------------------------
    tabItem("model",
     
            
            #click to run the XGBoost model
            actionButton(inputId = "runxgb",
                         label = "Click to Run the XGBoost Model"),
            
            br(),
            
            br(), 
            
            #Gives the user a message while waiting for XGBoost to run
            htmlOutput(outputId = "xgb_pls_wait"),    
            
           
            
           
          
            dataTableOutput(outputId = "model_specs"),
            
            plotOutput(outputId = "preds_vs_observed")
            
    
    
    
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
<<<<<<< HEAD
        Day = strftime(time_value , format = "%A"),
        Date = ymd(time_value)
      ) %>%
      subset(Day == "Monday")
    
    covid_data <- subset(covid_data, select=-c(Day,time_value))
=======
        Day = ymd(time_value) #ADDED
      ) %>% 
      group_by(State, Year, Week) %>% 
      
      
      summarise(
         
        week_first_date = min(Day), #ADDED
        `Outcome Variable` = mean(outcome_variable),
       
        
      )
>>>>>>> fb84189e4d3775cb31d65c1144349801f2b8b303
    
      
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
  
<<<<<<< HEAD
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
=======
 #  
 # df<- df[, colSums(is.na(df))==0]
 # df<- select(df, -X)
 # df<-droplevels(df)
 # str(df)
 # 

>>>>>>> fb84189e4d3775cb31d65c1144349801f2b8b303
  
  
  
  #Feature Selection Page Output ---------------------------------------------------
  
 output$ft_sel <- renderUI({
   
   HTML(paste("Here's The Results of Feature Selection:", "Here's a Variable Importance Plot:", sep = "<br/><br/>"))
   
 })
 
#Pre-Processing the Data for Feature Selection ------------------------------------------------
#creates a separate copy of the reactive dataset to deal with feature selection
merged_dataset_feat_sel <- reactive({
      merged_dataset()[, colSums(is.na(merged_dataset()))==0]
  
})


 # output$varImp  <- renderPlot({
 #   
 #  # merged_dataset_feat_sel() <- select(merged_dataset_feat_sel(), -X)
 #   
 #   #drops the unused levels in the feature variables in the dataset
 #   merged_dataset_feat_sel() <- droplevels(merged_dataset_feat_sel())
 #   
 #   # str(df)
 #   # 
 #   # 
 #   
 #   #renaming the outcome variable and the "other" race category for consistency 
 #   names(merged_dataset_feat_sel())[names(merged_dataset_feat_sel()) == "Outcome.Variable"] <- "y" #creating consitency 
 #   names(merged_dataset_feat_sel())[names(merged_dataset_feat_sel()) == "Other"] <- "Other_Race"  #"Other is not meaningful in output
 #   # 
 #   
 #   #reorder the dataframe so that the outcome column is the first column
 #   
 #   col_idx <- grep("y", names(merged_dataset_feat_sel()))
 #   merged_dataset_feat_sel() <- merged_dataset_feat_sel()[, c(col_idx, (1:ncol(merged_dataset_feat_sel()))[-col_idx])]
 #   
 #   #head(df)
 #   # 
 #   # 
 #   end <-ncol(merged_dataset_feat_sel())
 #   x <- merged_dataset_feat_sel()[,2:end]
 #   y <- merged_dataset_feat_sel()[,1]
 #   # 
 #   # 
 #   
 #   set.seed(10)
 #   bestmtry <- tuneRF(x, y, stepFactor=1.5, improve=1e-5, ntree=500)
 #   #print(bestmtry)
 #   bestmtry_var<- bestmtry[1,1]
 #   
 #   
 #   rf <- randomForest(y ~ ., mtry= bestmtry_var, 
 #                      data= merged_dataset_feat_sel)
 #   
 #   
 #   varImpPlot(rf,sort=TRUE, n.var=min(10, nrow(rf$importance)),
 #              type=NULL, class=NULL, scale=TRUE, 
 #              main=deparse(substitute(rf))) 
 #   
 # })
 

 # 
 # 
 # 
 # 
 # 
 # 
  #choices = c("Time Ordered", "Random Shuffling"),
  #for time ordered, would need to use top_n command 
  # 
  # overall_data <- exp_features %>% 
  #   left_join(state_controls, by = c("state")) %>% 
  #   mutate(submission_date = as.Date(submission_date, format = "%m/%d/%Y"))
  # 
  # #finding the last four data entries to filter the data into training and testing
  # most_recent_weeks <- tail(unique(overall_data$submission_date),2)
  # 
  # overall_data_sub <- overall_data %>% 
  #   filter(submission_date == most_recent_weeks[1] | submission_date == most_recent_weeks[2])
  # 
  # if(input$train_decide == "Random Shuffling") {
  #   
  #   
  #   
  #       #Step 1:
  #       set.seed(2022)
  # 
  #       #shuffle the data
  #       overall_data <- overall_data[sample(1:nrow(overall_data)),]
  # 
  #       #split into the training and testing, with 50% of the data in each group
  #       train_index <- sample(1:nrow(covid_info), 0.5 * nrow(covid_info))
  #       train_df <- covid_info[train_index,]
  #   #     test_df <- covid_info[-train_index,]
  # 
  #     #removes submission date, state, total population, and the other outcome variables from the training dataset
  #     train_subset <-
  #       train_df %>%
  #       select(-c(1,2,4,13,15,16))
  # 
  #     #creating the response vector and the covariates matrix
  #     x <- model.matrix(new_cases_per_100k ~ . , train_subset)[,-1]
  #     y <- train_subset$new_cases_per_100k
  # 
  #     #picking the best value for lambda
  #     lasso_results <- cv.glmnet(x, y, alpha = 1)
  # 
  #     #lambda within one standard error of the lowest value of lambda
  #     one_se_lambda <- lasso_results$lambda.1se
  # 
  # 
  #     all_vars <- coef(lasso_results, s = one_se_lambda)
  #     #determines which variables will have a non-zero coefficients
  #     selected_vars <- rownames(all_vars)[all_vars[,1] != 0]
  # 
  
  # }
  # 
  # else {
  #   
  # }
  
  
  
  
  #     policy_subset <- reactive({
  #         covid_info %>% 
  #             rename("Closure of Bars" = bars_closed,
  #                    "Closure of Daycare" = day_care_closed,
  #                    "Closure of Restaurants" = restaurants_closed) %>% 
  #             select(submission_date, input$policySelect)
  #                   
  #     })
  #     
  #     #subset of the dataset based on the users selections
  #     covid_subset <- reactive({
  #       covid_info %>% 
  #         select(submission_date)
  #     })
  #     
  #
  #Step 1:
  #     set.seed(2022)
  # 
  #     #shuffle the data
  #     covid_info <- covid_info[sample(1:nrow(covid_info)),]
  # 
  #     #split into the training and testing, with 50% of the data in each group
  #     train_index <- sample(1:nrow(covid_info), 0.5 * nrow(covid_info))
  # 
  #     train_df <- covid_info[train_index,]
  #     test_df <- covid_info[-train_index,]
  # 
  #     #removes submission date, state, total population, and the other outcome variables from the training dataset
  #     train_subset <-
  #       train_df %>%
  #       select(-c(1,2,4,13,15,16))
  # 
  #     #creating the response vector and the covariates matrix
  #     x <- model.matrix(new_cases_per_100k ~ . , train_subset)[,-1]
  #     y <- train_subset$new_cases_per_100k
  # 
  #     #picking the best value for lambda
  #     lasso_results <- cv.glmnet(x, y, alpha = 1)
  # 
  #     #lambda within one standard error of the lowest value of lambda
  #     one_se_lambda <- lasso_results$lambda.1se
  # 
  # 
  #     all_vars <- coef(lasso_results, s = one_se_lambda)
  #     #determines which variables will have a non-zero coefficients
  #     selected_vars <- rownames(all_vars)[all_vars[,1] != 0]
  # 
  # # # 
  #     
  #     
  #     output$intro <- renderText("The selected variables are:")
  #      output$selected_vars <- renderText({
  #        
  #        
  #     
  #         results <- selected_vars[-1]
  # # 
  # #       results <- paste0("The variables that are the best predictors of ", input$outcome, " are: ", selected_vars)
  # #       results
  # })
  # # 
  # # 
  # # 
  # #     )
  #     
  #     output$lasso_results <- renderPlot({
  #       
  #run model with and without feature selection
  #       
  #       
  #      
  #       
  #       
  #       
  #       #baseline plot
  #      ggplot(covid_info, aes(x = as.Date(submission_date), y = new_deaths_per_100k)) +
  #         geom_line() 
  #       
  #     })
  #     
  #     output$policy_table <- DT::renderDataTable({
  # 
  #         DT::datatable(data = policy_subset(),
  #                       rownames = FALSE)
  #     })
  # 
  #     output$policy_over_time <- renderPlot({
  # 
  #         ggplot(policy_subset(), aes(x = as.Date(submission_date), y = input$policySelect)) +
  #             geom_line(color = "blue")
  # 
  # 
  #     })
  
  
  
<<<<<<< HEAD
#Model Building Page ---------------------------------------------------------------------
 
 output$xgb_pls_wait <- renderUI({
   
   HTML(paste("Please wait while the XGBoost Model Runs...", sep = "<br/><br/>"))
   
 })
 

 
 
#creates a new variable to capture the two-week forecast

 addTwoWeekForecast <- function(merged_df) {
   
   #ungroups the data 
   #merged_df <- merged_df %>% ungroup()
   
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
                      "two_week_forecast_date" = "two_week_forecast_date")) %>%
     mutate(year = as.factor(year(two_week_forecast_date)),
            week = as.factor(week(two_week_forecast_date)))

   return(working_df)
 }
 

 #make a reactive data frame after the two week forecast has been added
 forecast_data <- reactive({
   
   addTwoWeekForecast(data_after_missing()) 
 
 })
 
 



#  createTrainSet <- function(forecast_data) {
# 
#   latest_date = max(forecast_data$week_first_date)
# 
#   train = forecast_data[forecast_data$two_week_forecast_date <= (latest_date - 14), ] %>% na.omit()
# 
#   return(train)
# }
# 
# createTestSet <- function(forecast_data) {
#   
#   latest_date = max(forecast_data$week_first_date)
#   
#   test = forecast_data[(forecast_data$two_week_forecast_date <= latest_date) &
#                          (forecast_data$two_week_forecast_date > (latest_date - 14)), ] %>% na.omit()
# 
#   return(test)
# }
# 
# 
# 
# createPredSet <- function(forecast_data) {
#   
#   latest_date = max(forecast_data$week_first_date)
#   
# 
#   pred = forecast_data[forecast_data$two_week_forecast_date > latest_date, ]
#   
#   return(pred)
#   
# }
#   
# #when called, input$state to as the second argument -- add to the model selection page
# 
# createPredState <- function(forecast_data, state) {
#   
#   latest_date = max(forecast_data$week_first_date)
#   pred_state = forecast_data[forecast_data$two_week_forecast_date > latest_date, ] %>% 
#     filter(State == state)
#   
# }
  



# XGBPred = xgb.DMatrix(data.matrix(pred %>%
#                                     select(-c(two_week_forecast_date,
#                                               two_week_outcome))))
# 
# XGBPred_state = xgb.DMatrix(data.matrix(pred_state %>%
#                                           select(-c(two_week_forecast_date,
#                                                     two_week_outcome))))

 
 #creates the training dataset for the XGBoost model 
 createXGBTrainSet <- function(forecast_data) {
   
   latest_date = max(forecast_data$Date)
   
   train = forecast_data[forecast_data$two_week_forecast_date <= (latest_date - 14), ] %>% na.omit()
   
   return(train)
 }
 
 #creates a reactive dataframe for the training data 
 train_data <- reactive ({
   
   createXGBTrainSet(forecast_data())
   
   
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
XGBtg <- expand.grid(nrounds = 20,
                     max_depth = c(5, 10, 15),
                     colsample_bytree = seq(0.5, 0.9, length.out = 5),
                     eta = c(0.1, 0.2, 0.4),
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


best_model <- XGBModel$bestTune  

return(best_model)

}


#captures the results of running the XGB model
xgb_model_output <- eventReactive(input$runxgb, {
  
  trainXGBoost(train_data())
}
)
  

  
  

 #test dataframe 
 output$model_specs <- renderDataTable({
   
   DT::datatable(data = xgb_model_output(),
                 rownames = FALSE,
                 options = list(scrollX = T))
   
   
 })
 
 # #display the best model specifications -- using renderTable or something..
 #   
 # XGBModel$bestTune  
# 
# createDenseMatrix <- function() {
#   
#   XGBTest = xgb.DMatrix(data.matrix(test %>% #change the name of the dataset
#                                       select(-c(two_week_forecast_date,
#                                                 two_week_outcome))))
# }
#   
# 
# 
# predictXGBoost <- function(XGBModel, predict_df) {
#   
#   xgbmodel_predictions = predict(XGBModel, predict_df)
#   
#   return(xgbmodel_predictions)
#   
# 
# }
# 
# 
# 
# 
# calculateRSME <- function()
#   
#   

# 
#   
#   
# 
# XGBRMSE = sqrt(mean ((test$two_week_outcome - XGBModel_test) ^ 2 ) )
# 
# XGBModel_pred = predict(XGBModel, XGBPred)
# 
# XGBModel_state = predict(XGBModel, XGBPred_state)
# 
# 
# #Plot observed vs. predicted
# obs_vs_pred = test %>% 
#   select(two_week_outcome) %>% 
#   rename(Observed = two_week_outcome) %>% 
#   bind_cols(tibble(Predicted = XGBModel_test)) %>% 
#   mutate(Observed = log(Observed),
#          Predicted = log(Predicted))
# 
# 
# #goodness of fit plot
# obs_vs_pred_plot = ggplot(obs_vs_pred, aes(x = Observed, y = Predicted)) +
#   geom_point() +
#   geom_abline() +
#   xlab("Log(Observed)") +
#   ylab("Log(Predicted)")
# 
# 
# #Plot Predictions -- for all of the states
# 
# output$plot_all_states <- renderPlot{(
#   outcome_all = train %>% 
#     select(two_week_forecast_date, two_week_outcome) %>%
#     rename(Date = two_week_forecast_date,
#            Outcome = two_week_outcome) %>% 
#     mutate (Pred_vs_Obs = "Observed") %>% 
#     rbind(tibble(Date = pred$two_week_forecast_date,
#                  Outcome = XGBModel_pred,
#                  Pred_vs_Obs = "Predicted"))
#   
#   outcome_plot_all = ggplot(outcome_all, aes(x = Date, y = Outcome))+
#     geom_point(aes(color = Pred_vs_Obs))
#   
#   
# )}
# 
# 
# 
# #plot predictions for one state 
# outcome_all_state = train %>% 
#   filter(State == 'PA') %>% 
#   select(two_week_forecast_date, two_week_outcome) %>%
#   rename(Date = two_week_forecast_date,
#          Outcome = two_week_outcome) %>% 
#   mutate (Pred_vs_Obs = "Observed") %>% 
#   rbind(tibble(Date = pred_state$two_week_forecast_date,
#                Outcome = XGBModel_state,
#                Pred_vs_Obs = "Predicted"))
# 
# outcome_plot_state = ggplot(outcome_all_state, aes(x = Date, y = Outcome)) +
#   geom_point(aes(color = Pred_vs_Obs)) +
#   stat_smooth(method = 'lm', formula = y ~ poly(x,10), se = TRUE, color = "darkgrey")
# 
# 
# 
# 
#  
# 
# 





#start troubleshooting -----------------
# 
#  covid_data_ex <- query_API_fun("2020-03-01", "2021-04-23", "confirmed_7dav_incidence_prop")
# 
# 
#  user_input_example <- user_input_example %>%
#    mutate(
#    Date = as.Date(Date, format = "%m/%d/%Y"),
#    Year = strftime(Date , format = "%Y"),
#    Week = strftime(Date , format = "%V"),
#    State = toupper(State)
#  ) %>%
# 
#    group_by(State, Year, Week) %>%
#    summarise_each(funs(mean)) %>% subset(select = -c(Date)) %>%  mutate_if(is.numeric, ~round(., 0))
# 
# 
#  merged_data_ex <- merge_dataset_fun(covid_data_ex,user_input_example, state_controls)

# shiny_ex <- read.csv("~/Documents/GitHub/Covid-19-Closure-Impact/Data/shiny_merged_dataset_example.csv")
#  
# # missing_percentages_df = colSums(is.na(merged_data_ex)) %>% 
# #                          tibble(name=names(.), percent_missing=. * (1/nrow(merged_data_ex)) * 100) %>% 
# #                          select(name, percent_missing) %>% 
# #                          #formats the percent missing nicely
# #                          mutate(percent_missing = round(percent_missing, digits = 2)) %>% 
# #                          arrange(desc(percent_missing)) %>%
# #                          as.data.frame()
#                  
#  #trouble <- merged_data_ex %>% 
#              # filter_all(any_vars(is.na(.)))
#     

#miss_result <- check_missingness(merged_data_ex)




#troubleshooting ends -------------------------------------
 
=======
  
  
  
  
>>>>>>> fb84189e4d3775cb31d65c1144349801f2b8b303
}

# Run the application 
shinyApp(ui = ui, server = server)
