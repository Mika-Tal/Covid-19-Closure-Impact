library(shiny)
library(shinydashboard)
# library(httr) #used to query an API
# library(jsonlite) #used to query an API
library(tidyverse)
library(DT)


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
header <- dashboardHeader(title = "The Effect of Non-Pharmaceutical Interventions on COVID-19 Deaths in the US",
                          titleWidth = 600)


#Dashboard Sidebar --------------------------------------------------

sidebar <- dashboardSidebar(
    sidebarMenu(
        id = "tabs",
        
        #Menu Items--------------------------
        menuItem("Overview", tabName = "overview")
    )
)



#Dashboard Body ------------------------------------

body <- dashboardBody(tabItems(
    
    #Overview page ---------------------
    tabItem("overview",
            
            
            #select input for the non-pharmaceutical interventions
            selectInput("policySelect",
                        label = "Choose a Non-Pharmecutical Intervention:",
                        choices = c("Closure of Bars", "Closure of Daycare", 
                                    "Closure of Restaurants")
            ),
        
          
        plotOutput(outputId = "policy_over_time"),
        dataTableOutput(outputId = "policy_table")
         
    )
)
    
    
    
)

ui <- dashboardPage(header, sidebar, body)
    
    
# Define server logic required to draw a histogram
server <- function(input, output) {
    
    policy_subset <- reactive({
        covid_info %>% 
            rename("Closure of Bars" = bars_closed,
                   "Closure of Daycare" = day_care_closed,
                   "Closure of Restaurants" = restaurants_closed) %>% 
            select(submission_date, input$policySelect)
                  
    })
    
    output$policy_table <- DT::renderDataTable({
        
        DT::datatable(data = policy_subset(),
                      rownames = FALSE)
    })

    output$policy_over_time <- renderPlot({

        ggplot(policy_subset(), aes(x = as.Date(submission_date), y = input$policySelect)) +
            geom_line(color = "blue")


    })


    
            
    
      
    
}

# Run the application 
shinyApp(ui = ui, server = server)
