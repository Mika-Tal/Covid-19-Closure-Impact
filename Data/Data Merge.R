
### Load helper packages ###
loadlibs = function(libs) {
  for(lib in libs) {
    class(lib)
    if(!do.call(require,as.list(lib))) {install.packages(lib)}
    do.call(require,as.list(lib))
  }
}
libs = c("tidyverse","data.table","stargazer", "knitr", "shiny",
         "tidymodels", "lubridate", "rpart.plot", "caret", "e1071",
         "readxl", "fuzzyjoin")
loadlibs(libs)
#covid data 
covid_raw = fread("COVID-19.csv") %>% as_tibble()
#Create working dataset
working_df = covid_raw %>% 
  select(submission_date, state, new_case, new_death) %>% 
  filter(!state %in% c('FSM', 'GU', 'PW', 'MP', 'NYC', 'RMI', 'PR', 'AS', 'VI'))
#Check data type of each column
str(working_df)
#Check for missing values 
apply(working_df, 2, function(col)sum(is.na(col))/length(col))
 #Convert date feature from character to string data type
#Aggregate data by week
working_df = working_df %>% 
  mutate(submission_date = as.Date(submission_date, "%m/%d/%Y")) %>% 
  group_by(submission_date = cut(submission_date, "week"), state) %>%
  summarise(new_cases = mean(new_case),
            new_deaths = mean(new_death),
            .groups = "keep") %>% 
  mutate(new_cases = round(new_cases, 2),
         new_deaths = round(new_deaths, 2))

#Pre-covid policies
pre_covid_policies =fread("pre_covid_policies.csv") %>%
  as_tibble() %>% 
  slice_head(n=51) %>% 
  rename(state = 'State Abbreviation',
         paid_sick_leave = 'Paid sick leave',
         medicade_expansion = 'Medicaid Expansion') %>% 
  select(state, paid_sick_leave, medicade_expansion)
#Check structure
str(pre_covid_policies)  
#Check NAs
apply(pre_covid_policies, 2, function(col)sum(is.na(col))/length(col))
#Merge with working_df
working_df = working_df %>% 
  left_join(pre_covid_policies, by = c("state" = "state"))


#Mask Policies - Note NA signifies mandate is still in place
facemask= fread('Facemask.csv') %>% 
  as_tibble() %>% 
  select('State Abbreviation',
         "Mandate face mask use by all individuals in public spaces",
         "Face mask mandate enforced by fines",
         "Face mask mandate enforced by criminal charge/citation",
         "No legal enforcement of face mask mandate",
         "State ended statewide mask use by individuals in public spaces") %>% 
  rename(state = 'State Abbreviation',
         mask_startDate = "Mandate face mask use by all individuals in public spaces",
         mask_enfocedByFines = "Face mask mandate enforced by fines",
         mask_enforcedByCrimCharge = "Face mask mandate enforced by criminal charge/citation",
         mask_noEnfocement = "No legal enforcement of face mask mandate",
         mask_endDate = "State ended statewide mask use by individuals in public spaces") %>% 
  mutate_at(c('mask_startDate', 'mask_endDate'), ~ as.Date(., "%m/%d/%Y"))
#Merge with working_df
working_df = working_df %>% 
  left_join(facemask, by = c("state" = "state"))

#State Characteristics
state_characteristics = fread("state_characteristics.csv") %>% 
  select(State_Abbreviation, Population_density, Population_2018,
         Number_Homeless_2019,
         Percent_living_under_the_federal_poverty_line_2018,
         Percent_at_risk_for_serious_illness_due_to_COVID) %>% 
  slice_head(n=51) %>% 
  rename(state = State_Abbreviation)
#Check structure
str(state_characteristics)  
#Check NAs
apply(state_characteristics, 2, function(col)sum(is.na(col))/length(col))
#Merge with working_df
working_df = working_df %>% 
  left_join(state_characteristics, by = c("state" = "state"))

#Closures 
closures_df = fread("closures.csv") %>% 
  as_tibble() %>% 
  select(-c(1,3,12:31)) %>%
  slice_head(n=51) %>%
  rename(state = "State Abbreviation",
         c_schools = "Closed K-12 public schools",
         c_day_care = "Closed day cares",
         c_nursing_home_visits = "Banned visitors to nursing homes",
         c_non_essential_businesses = "Closed other non-essential businesses",
         c_dine_in_restaurants = "Closed restaurants except take out",
         c_gyms = "Closed gyms",
         c_movie_theaters  = "Closed movie theaters",
         c_bars = "Closed bars") %>% 
  mutate_at(c("c_schools", "c_day_care", "c_nursing_home_visits", "c_non_essential_businesses",
              "c_dine_in_restaurants", "c_gyms", "c_movie_theaters", "c_bars"),
            ~ as.Date(., "%m/%d/%Y")) 
#Merging closure data
working_df = working_df %>% 
  left_join(closures_df, by = c("state" = "state"))
#Add indicator for 14 days from closure start
working_df = working_df %>% 
  mutate(schools_closed_14Days = if_else(submission_date - c_schools  >= 14, 1, 0),
         schools_closed_14Days =  replace_na(schools_closed_14Days, 0),
         dayCare_closed_14Days = if_else(submission_date - c_day_care >= 14, 1, 0),
         dayCare_closed_14Days =  replace_na(dayCare_closed_14Days, 0),
         nursingHomes_closed_14Days = if_else(submission_date -c_nursing_home_visits>= 14, 1, 0),
         nursingHomes_closed_14Days =  replace_na(nursingHomes_closed_14Days, 0),
         essBiz_closed_14Days = if_else(submission_date - c_non_essential_businesses >= 14, 1, 0),
         essBiz_closed_14Days =  replace_na(essBiz_closed_14Days, 0),
         restaurants_closed_14Days = if_else(submission_date - c_dine_in_restaurants >= 14, 1, 0),
         restaurants_closed_14Days =  replace_na(restaurants_closed_14Days, 0),
         gyms_closed_14Days = if_else(submission_date -c_gyms  >= 14, 1, 0),
         gyms_closed_14Days =  replace_na(gyms_closed_14Days, 0),
         movies_closed_14Days = if_else(submission_date -c_movie_theaters >= 14, 1, 0),
         movies_closed_14Days =  replace_na(movies_closed_14Days, 0),
         bars_closed_14Days = if_else(submission_date -c_bars >= 14, 1, 0),
         bars_closed_14Days =  replace_na(bars_closed_14Days, 0))


# Census data and political leanings
census_df = fread("state_census_edited.csv") %>% 
  as_tibble() %>% 
  mutate_at(c("Majority_Dem", "Majority_Rep"), as.factor) %>% 
  rename(state = State)
#Merge census data
working_df = working_df %>% 
  left_join(census_df, by = c("state" = "state"))


write.csv(working_df, "Covid_Pred.csv", row.names = FALSE)


#Vaccine data 
vaccine_df = fread("Vaccine.csv") %>% 
  as_tibble() %>% 
  rename(state = Jurisdiction,
         first_dose = "1st Dose Allocations",
         second_dose = "2nd Dose Allocations",
         allocation_week = "Week of Allocations") %>%
  select(1:5) %>% 
  mutate(allocation_week = as.Date(allocation_week, "%m/%d/%Y"))
#Check for NA values
apply(vaccine_df, 2, function(col)sum(is.na(col))/length(col))
#Merge census data
working_df = working_df %>% 
  left_join(census_df, by = c("state" = "state",
                              "submission_date" ="allocation_week"))

write.csv(vaccine_df, "Vax.csv", row.names = FALSE)
