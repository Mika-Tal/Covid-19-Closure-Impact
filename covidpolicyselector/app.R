#Load in necessary libraries

library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
# library(plotly) #used to create interactive plots
library(glmnet) # to perform L1 regularization


# #installs the COVIDCast API information
# devtools::install_github("cmu-delphi/covidcast", ref = "main",
#                          subdir = "R-packages/covidcast")

library(covidcast)

#To do:
# 1. ADD IN OTHER LIBRARIES THAT WE ARE USING FOR THIS APP


covid_info <- read.csv("~/Documents/GitHub/Covid-19-Closure-Impact/Covid_Pred.csv")


#create controls only .csv

controls <- c("paid_sick_leave", "medicade_expansion", "Population_density", 
              "Population_2018", "Number_Homeless_2019", 
              "Percent_living_under_the_federal_poverty_line_2018", 
              "Percent_at_risk_for_serious_illness_due_to_COVID", "X65_and_over", 
              "White", "Black", "American_Indian", "Asian", 
              "Native_Hawaiian_Pacific_Islander", "Other", 
              "Majority_Dem", "Majority_Rep")

controls_only_df <- covid_info %>% 
  select(state, all_of(controls)) %>% 
  group_by(state) %>% 
  slice(1)


#create features of interest .csv
exp_features <- covid_info %>%
  select(-c(all_of(controls))) %>% 
  select(-c(5,9:17)) #removes columns with dates instead of indicator variables

#write a csv of the experimental features for the project
#write.csv(exp_features, "expfeatures.csv")


# Define UI for application that draws a histogram

#Dashboard Header & Title ---------------------------------------------------------
header <- dashboardHeader(title = "Machine Learning Pipeline Visualization Using COVID-19 Data",
                          titleWidth = 600)


#Dashboard Sidebar --------------------------------------------------

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    
    #Menu Items--------------------------
    menuItem("Overview & Data Ingestion", tabName = "overview"),
    menuItem("Data Merging", tabName = "merging"),
    menuItem("Missingness Check", tabName = "missing"),
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
                           end = "2021-03-01",
                           format = "yyyy-mm-dd",
                           separator = "-"),
            
            
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
                             dataTableOutput(outputId = "contents")),
            
            
            #data table ------
            #div(dataTableOutput(outputId = "overall_data")) #style = "font-size: 75%; width: 75%")
    ),
    
    #Data Merging Page--------------------------------------------------------------------------------
    tabItem("merging",
            
            
            #determines what dataset will be fed into the feature selection & used for modeling
            #asks the user whether to combine their features with our set of control features
            radioButtons(inputId = "merge_decide",
                         label = "Do you want to combine your data with our control features?",
                         choices = c("Yes", "No"),
                         selected = c("No")),
            
            # #if merge_decide = No, display the users inputted data with the covid data frame
            # conditionalPanel(condition = "input.merge_decide == 'No'",
            #                  dataTableOutput(outputId = "no_merge")),
            # 
            # 
            #only allows user to see select control features if "merge_decide" = "Yes"
            conditionalPanel(condition = "input.merge_decide == 'Yes'",
                             radioButtons(inputId = "all_controls",
                                          label = "Do you want to use ALL of our control features?",
                                          choices = c("Yes", "No"),
                                          selected = c("Yes")),
                             
                             conditionalPanel(condition = "input.all_controls == 'No'",
                                              selectInput(inputId = "select_controls",
                                                          label = "Select the control features you would like to use:",
                                                          choices = names(controls_only_df)[-1],
                                                          multiple = TRUE,
                                                          selected = names(controls_only_df)[2:5])
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
            dataTableOutput(outputId = "merged_data")
            
            
    ),
    
    
    #Missingness Imputation page-----------------------------------------------
    tabItem("missing"
            
    ),
    
    #Feature Selection page ------------------------------------------------------------------------------
    tabItem("features",
            
            
            #select the input for the outcome variable of interest (everything else will be the features)
            selectInput("outcome",
                        label = "Choose outcome variable of interest:",
                        choices = names(covid_info)),
            
            #allows the user to decide how to create the training subset -----------------------------------
            radioButtons(inputId = "train_decide",
                         label = "How do you want to train your data?",
                         choices = c("Time Ordered", "Random Shuffling"),
                         selected = c("Random Shuffling")),
            
            
            
            
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
    tabItem("model")
    
  )
)





ui <- dashboardPage(header, sidebar, body)


#Overview & Data Ingestion Output ----------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$welcome <- renderUI({
    
    
    HTML(paste("Hello! Welcome to our Machine Learning Pipeline Visualization App. To get started:", "Step 1: Choose the Date Range For Which You Would Like COVID Data ", "Step 2: Query the CMU Delphi API to Get the Requested COVID Data", "Step 3: Upload The Additional Features You Would Like To Use, Using the Provided Template", sep = "<br/><br/>"))
    
  })
  
  
  #when the action button is pushed, query the CMU Delphi API
  observeEvent(input$queryAPI, {
    
    
    #queries the delphi API based on the input from the user
    delphi_results <- suppressMessages(
      covidcast_signal(data_source = "indicator-combination",
                       signal = "confirmed_7dav_incidence_prop",
                       start_day = input$dates_of_interest[1], #sets the data queried to user inputs
                       end_day = input$dates_of_interest[2],
                       geo_type = "state")
    )
    
    #capitalizes the state column of the delphi_results dataFrame for easier merging
    delphi_results$geo_value <- toupper(delphi_results$geo_value)
    
    
    # delphi_results_edit <- reactive({
    
    delphi_results_edit <-
      delphi_results %>%
      select("geo_value", "time_value", "value") %>%
      rename("state" = "geo_value",
             "date" = "time_value",
             "New Cases Per 100k" = "value" )
    
    # #%>%
    #   mutate (Week = strftime(Day , format = "%V"),
    #           State = toupper(State)) %>%
    #   group_by(State, Week) %>%
    #   summarize(Average_Daily_Cases_per_100k = mean(Cases_per_100k))
    # 
    
    
    #placeholder for displaying whatever datatable results from querying the API
    output$api_results <- renderDataTable({
      DT::datatable(data = delphi_results_edit,
                    rownames = FALSE, options = list(scrollX = T))
    })
    
  })
  
  
  
  
  output$controls_only <- renderDataTable({
    
    DT::datatable(data = controls_only_df,
                  rownames = FALSE,
                  options = list(scrollX = T))
  })
  
  
  #***allows the user to download the data ingestion template
  output$template <- downloadHandler(
    filename = "Data_Ingestion_Template.csv",
    content = function(file) {
      
      write.csv(delphi_results, file, row.names = FALSE) #***change the file that is downloaded
    }
  )
  
  output$contents <- renderDataTable({
    
    
    #need this file in order to produce your desired output
    req(input$file1)
    
    tryCatch(
      {
        df_upload <- read.csv(input$file1$datapath,
                              header = input$header,
                              sep = input$sep,
                              quote = input$quote)
        
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      
      #display the datatable
      DT::datatable(data = head(df_upload)[-1],
                    rownames = FALSE, options = list(scrollX = T))
      
    }
    else {
      
      #display the data table
      DT::datatable(data = df_upload[-1],
                    rownames = FALSE, options = list(scrollX = T))
      
      
      
    }
    
  })
  
  
  # Data Merging Page Output ---------------------------------------------------------
  # delphi_results_edit
  # df_upload
  # controls_only_df 
  
  output$merged <- renderUI({
    
    
    HTML(paste("Here's Your Merged Dataset:", sep = "<br/><br/>"))
    
  })
  
  
  
  
  #only merge after clicking on the "merge" button
  observeEvent(input$click_merge,
               
               
               output$merged_data <- renderDataTable({
                 
                 # if(input$merge_decide == "No") {
                 #   
                 #   dat <- 
                 #    delphi_results_edit %>% 
                 #    left_join(df_upload, by = c("state" = "state", "date" = "submission_date"))
                 #     
                 #   DT::datatable(data = dat,
                 #                 rownames = FALSE,
                 #                 options = list(scrollX = T))
                 #   
                 # }
                 
                 if (input$all_controls == "No") {
                   
                   #subsetting the controls to only the controls selected by the user
                   sel_ctrl_df <- 
                     controls_only_df %>% 
                     select(state, input$select_controls)
                   
                   dat <- 
                     delphi_results_edit %>% 
                     left_join(sel_ctrl_df, by = c("state" = "state"))
                   
                   #add merge for the uploaded data from the user
                   
                   DT::datatable(data = dat,
                                 rownames = FALSE,
                                 options = list(scrollX = T))
                   
                 }
                 
                 else {
                   
                   
                   dat <- 
                     delphi_results_edit %>% 
                     left_join(controls_only_df, by = c("state" = "state"))
                   
                   #add merge for the uploaded data from the user
                   
                   DT::datatable(data = dat,
                                 rownames = FALSE,
                                 options = list(scrollX = T))
                   
                   
                 }
                 
                 
                 
                 
               })
               
  )
  
  #   
  #   
  # )
  # 
  # output$overall_data <- renderDataTable({
  #   DT::datatable(data = covid_info,
  #                   rownames = FALSE, options = list(scrollX = T))
  #   
  # })
  #   
  
  
  #Missingness Imputation Page Output ---------------------------------------------
  
  
  
  
  
  
  
  #Feature Selection Page Output ---------------------------------------------------
  
  
  #choices = c("Time Ordered", "Random Shuffling"),
  #for time ordered, would need to use top_n command 
  
  overall_data <- exp_features %>% 
    left_join(controls_only_df, by = c("state")) %>% 
    mutate(submission_date = as.Date(submission_date, format = "%m/%d/%Y"))
  
  #finding the last four data entries to filter the data into training and testing
  most_recent_weeks <- tail(unique(overall_data$submission_date),2)
  
  overall_data_sub <- overall_data %>% 
    filter(submission_date == most_recent_weeks[1] | submission_date == most_recent_weeks[2])
  
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
  
  
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
