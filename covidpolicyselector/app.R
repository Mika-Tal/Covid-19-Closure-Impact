#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# #Loading in the COVID Cases  data using CDC API
# # URL Encode the query
# q <- 'SELECT * FROM "395ca98e-75a4-407b-9b76-d2519da28c4a"'
# formatQuery <- URLencode(q, repeated = TRUE)
# # Build URL for GET request
# url <- paste0("https://data.wprdc.org/api/3/action/datastore_search_sql?sql=", formatQuery)
# # Run Get Request
# g <- GET(url)
# stations <- fromJSON(content(g, "text"))$result$records


library(shiny)
library(shinydashboard)
library(httr) #used to query an API
library(jsonlite) #used to query an API


#Loading in the COVID Cases  data using CDC API
# URL Encode the query
# q <- "$select=*"
# url <- paste0("https://data.cdc.gov/resource/9mfq-cb36.json?", q)
# 
# # Run Get Request
# g <- GET(url)
# covid_cases <- fromJSON(content(g, "text"))$result$records



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
                                    "Closure of Non-Essential Businesses")
            )
            
            )
    
)
    
    
    
)

ui <- dashboardPage(header, sidebar, body)
    
    
# Define server logic required to draw a histogram
server <- function(input, output) {

    
}

# Run the application 
shinyApp(ui = ui, server = server)
