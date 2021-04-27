#Install Delphi R package from below
#https://cmu-delphi.github.io/covidcast/covidcastR/articles/covidcast.html
### Load helper packages ###
loadlibs = function(libs) {
  for(lib in libs) {
    class(lib)
    if(!do.call(require,as.list(lib))) {install.packages(lib)}
    do.call(require,as.list(lib))
  }
}
libs = c("tidyverse","data.table", "tidymodels", "lubridate" , "shiny","covidcast")
loadlibs(libs)



#function to create the merged dataset
merge_dataset_fun <- function(start_date , end_date, outcome_variable, user_df ){
  
  controls_df <- read.csv("~/Documents/GitHub/Covid-19-Closure-Impact/Data/state_controls.csv")
  
  covid_df<- covidcast_signal(
    data_source = "indicator-combination",
    signal = outcome_variable,
    start_day = start_date,
    end_day = end_date,
    geo_type = "state")  %>% 
    
    select("geo_value", "time_value","value") %>%
    rename( State = geo_value , Day = time_value , outcome_variable = value) %>%
    
    mutate(
      Year = strftime(Day , format = "%Y"),
      Week = strftime(Day , format = "%V"),
      State = toupper(State)
      ) %>% 
    group_by(State, Year, Week, Day) %>% 
    
    summarize(
      
      `Outcome Variable` = mean(outcome_variable)
    )
  
  merged <- merge(covid_df,controls_df)
  if (!is.null(user_df)){
        merged <- merge (merged , user_df ,  by=c("State","Year", "Week"), all.x = TRUE)}
  write.csv(merged,file = "~/Documents/GitHub/Covid-19-Closure-Impact/Data/shiny_merged_dataset_with_original_dates.csv")
  return(merged)
}



#Rshiny to get user inputs and call the merging function when button pressed
server = function(input, output) {
  
  #This function is repsonsible for loading in the selected file
  userdata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    user_df <- read.csv(infile$datapath)
    
    user_df <- user_df %>%    mutate(
      Date = as.Date(Date, format = "%m/%d/%Y"),
      Year = strftime(Date , format = "%Y"),
      Week = strftime(Date , format = "%V"),
      State = toupper(State)
    ) %>% 
      
      group_by(State, Year, Week) %>% 
      summarise_each(funs(mean)) %>% subset(select = -c(Date)) %>%  mutate_if(is.numeric, ~round(., 0))
  })
  
  create_dataset <- eventReactive(input$button, {
    
    start_day = input$start_date
    end_day = input$end_date
    outcome_variable = input$outcome_variable
    user_data <- userdata()

    merge_dataset_fun(start_day,end_day, outcome_variable, user_data)
    
    
  })
  
  output$my_table <- renderTable({
    create_dataset()
    
  })
  

}
  
  
  
ui =   fluidPage(
  
  #date input
  dateInput("start_date", "Start Date:", value = "2020-04-01"),
  dateInput("end_date", "End Date:",value = "2020-05-01"),
  
  #outcome choice (cases or deaths)
  selectInput("outcome_variable", "To Forecast:",
              c("Cases per 100,000 People" = "confirmed_7dav_incidence_prop",
                "Deaths per 100,000 People" = "deaths_7dav_incidence_prop")),
  
  #file upload
  sidebarPanel(

      fileInput('datafile', 'Choose CSV file',
                accept=c('text/csv', 'text/comma-separated-values,text/plain'))),

  #button to merge datasets
  actionButton("button", "Create Dataset"),
  
    

  tableOutput("my_table")

  )
  
  shinyApp(ui = ui, server = server, options = list(height = 1000))
  