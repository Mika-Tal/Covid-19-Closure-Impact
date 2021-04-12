library(shiny)
library(shinydashboard)
# library(httr) #used to query an API
# library(jsonlite) #used to query an API
library(tidyverse)
library(DT)
# library(plotly)
library(glmnet) # to perform L1 regularization



#Loading in the COVID Cases  data using CDC API
# URL Encode the query
# q <- "$select=*"
# url <- paste0("https://data.cdc.gov/resource/9mfq-cb36.json?", q)
# 
# # Run Get Request
# g <- GET(url)
# covid_cases <- fromJSON(content(g, "text"))$result$records

#covid_info <- read.csv("~/Documents/GitHub/Covid-19-Closure-Impact/controls_and_outcomes.csv")[,-1]

covid_info_2 <- read.csv("~/Documents/GitHub/Covid-19-Closure-Impact/Covid_Pred.csv")


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
        menuItem("Feature Selection", tabName = "features"),
        menuItem("Model Building ", tabName = "model")
    )
)



#Dashboard Body ------------------------------------

body <- dashboardBody(
  
  tabItems(
  
  #Overview & Instructions page ---------------------
  tabItem("overview",
          
          
          # welcome message & introduction to shiny app ---------------
          textOutput(outputId = "welcome"),
          
          #adds visual space between the page elements
          br(),
          
          br(),
          
          br(),
          
          #button to allow user to query API and get the most updated COVID-19 info -----
          actionButton(inputId = "queryAPI",
                       label = "Click to Query CMUDelphi API"),
          
          #adds visual space between the page elements
          br(),
          
          br(),
          
          br(),
          
          
          #allows user to upload a file and choose how it is displayed to them
          fileInput(inputId = "file1",
                    label = "Choose CSV file",
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
          dataTableOutput("contents")
          
          
          #data table ------
          #div(dataTableOutput(outputId = "overall_data")) #style = "font-size: 75%; width: 75%")
  ),
  
  #Data Merging Page 
   tabItem("merging",
           
           #shows what control features are available to be used
           checkboxInput(inputId = "display table with control data",
                         label = "Do you want to see what control features are available?"),
           
           
           dataTableOutput(outputId = "controls_only"),
           
           #determines what dataset will be fed into the feature selection & used for modeling
           #asks the user whether to combine their features with our set of control features
           checkboxInput(inputId = "yes_merge",
                         label = "Check to Combine Your Data With Our Control Features")
           
           
          
           
           ),
    
    #Feature Selection page ---------------------
    tabItem("features",
            
            
            # #select input for the non-pharmaceutical interventions
            # selectInput("policySelect",
            #             label = "Select Features:",
            #             choices = names(covid_info),
            #             multiple = TRUE,
            #             selectize = TRUE,
            #             selected = names(covid_info)[6]
                      
            # ),
    
    
            #select the date range of interest (would only)
            
            
            #select the input for the outcome variable of interest (everything else will be the features)
            selectInput("outcome",
                        label = "Choose outcome variable of interest:",
                        choices = names(covid_info)),
            
            
            # #select the input for the control variables only
            # selectInput("control",
            #             label = "Choose control variables of interest:",
            #             choices = names(covid_info),
            #             multiple = TRUE,
            #             selectize = TRUE,
            #             selected = names(covid_info)[3]),
            # 
            
            
            #select the ML methodology that you would like to use
            selectInput("ml_type",
                        label = "Choose the feature selection methodology you want:",
                        choices = c("LASSO regularization", "Best Subset Selection", "Ridge Regularization")),
  
            #checkbox input to select the 
            
            
            
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
    
    #Model Building page ---------------------
    tabItem("model")
    
)
)
    
    
    


ui <- dashboardPage(header, sidebar, body)
    
    
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$welcome <- renderText({
    
    "<Placeholder for welcome message>"
  })
  
  output$contents <- renderDataTable({
    
    
    #need this file in order to produce your desired output
    req(input$file1)
    
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
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
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
    
    
    
  
  #when the action button is pushed, query the CMU Delphi API
  # observeEvent(input$queryAPI, {
  #   
  #   
  #   #placeholder for displaying whatever datatable results from querying the API
  #   output$overall_data <- renderDataTable({
  #                          DT::datatable(data = covid_info,
  #                          rownames = FALSE, options = list(scrollX = T))
  #   })
  #   
      
  #   
  # }
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
#     # if (input$ml_type == "Linear Regression with LASSO regularization") {
#     #   
#     # }
    #Step 1:
    set.seed(2022)

    #shuffle the data
    covid_info <- covid_info[sample(1:nrow(covid_info)),]

    #split into the training and testing, with 50% of the data in each group
    train_index <- sample(1:nrow(covid_info), 0.5 * nrow(covid_info))

    train_df <- covid_info[train_index,]
    test_df <- covid_info[-train_index,]

    #removes submission date, state, total population, and the other outcome variables from the training dataset
    train_subset <-
      train_df %>%
      select(-c(1,2,4,13,15,16))

    #creating the response vector and the covariates matrix
    x <- model.matrix(new_cases_per_100k ~ . , train_subset)[,-1]
    y <- train_subset$new_cases_per_100k

    #picking the best value for lambda
    lasso_results <- cv.glmnet(x, y, alpha = 1)

    #lambda within one standard error of the lowest value of lambda
    one_se_lambda <- lasso_results$lambda.1se


    all_vars <- coef(lasso_results, s = one_se_lambda)
    #determines which variables will have a non-zero coefficients
    selected_vars <- rownames(all_vars)[all_vars[,1] != 0]

# # 
    
    
    output$intro <- renderText("The selected variables are:")
     output$selected_vars <- renderText({
       
       
    
        results <- selected_vars[-1]
# # 
# #       results <- paste0("The variables that are the best predictors of ", input$outcome, " are: ", selected_vars)
# #       results
        })
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
