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

covid_info <- read.csv("~/Documents/GitHub/Covid-19-Closure-Impact/controls_and_outcomes.csv")[,-1]

# Define UI for application that draws a histogram

#Dashboard Header & Title ---------------------------------------------------------
header <- dashboardHeader(title = "Machine Learning Pipeline Visualization Using COVID-19 Data",
                          titleWidth = 600)


#Dashboard Sidebar --------------------------------------------------

sidebar <- dashboardSidebar(
    sidebarMenu(
        id = "tabs",
        
        #Menu Items--------------------------
        menuItem("Overview & Instructions", tabName = "overview"),
        menuItem("Feature Selection", tabName = "features"),
        menuItem("Model Building", tabName = "model")
    )
)



#Dashboard Body ------------------------------------

body <- dashboardBody(
  
  tabItems(
  
  #Overview & Instructions page ---------------------
  tabItem("overview",
          
          #data table ------
          div(dataTableOutput(outputId = "overall_data")) #style = "font-size: 75%; width: 75%")
  ),
    
    #Feature Selection page ---------------------
    tabItem("features",
            
            
            #select input for the non-pharmaceutical interventions
            selectInput("policySelect",
                        label = "Choose Treatment Variable(s):",
                        choices = names(covid_info),
                        multiple = TRUE,
                        selectize = TRUE,
                        selected = names(covid_info)[6]
                      
            ),
    
    
            #select the date range of interest (would only)
            
            
            #select the input for the outcome variable of interest
            selectInput("outcome",
                        label = "Choose outcome variable of interest:",
                        choices = names(covid_info)[13:16]),
            
            
            #select the input for the control variables only
            selectInput("control",
                        label = "Choose control variables of interest:",
                        choices = names(covid_info)[-c(1:2,5:16)],
                        multiple = TRUE,
                        selectize = TRUE,
                        selected = names(covid_info)[3]),
            
            
            
            #select the ML methodology that you would like to use
            selectInput("ml_type",
                        label = "Choose the ML methodology you want to use:",
                        choices = c("Linear Regression with LASSO regularization", "XGBoost")),
            
            
            
            #display the ROC curves for the different methodologies used
        
        textOutput(outputId = "selected_vars"), #results from regularization
        
        br(),
        
        br(),
        
        
        plotOutput(outputId = "lasso_results"),
          
        plotOutput(outputId = "policy_over_time"),
        dataTableOutput(outputId = "policy_table")
        
        
      ##Model Building 7 Two-Week Forecast
         
    ),
    
    #Model Building page ---------------------
    tabItem("model"
    )
    
)
    
    
    
)

ui <- dashboardPage(header, sidebar, body)
    
    
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$overall_data <- renderDataTable({
    DT::datatable(data = covid_info,
                    rownames = FALSE, options = list(scrollX = T))
    
  })
    
  
  
  
  
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
#     #Step 1: 
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
# # 
# #     output$selected_vars <- renderText({
# # 
# #       results <- paste0("The variables that are the best predictors of ", input$outcome, " are: ", selected_vars)
# #       results
# #     }
# # 
# # 
# # 
# #     )
#     
#     output$lasso_results <- renderPlot({
#       
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
