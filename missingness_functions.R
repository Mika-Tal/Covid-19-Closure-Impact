
#This function should receieve the merged dataset (dataframe) as argument and return a dataframe that
# has percentage missing for each attribute to show the user so they can make a choice 
#on how to deal with them 
check_missingness <- function(merged_df ){
  missing_percentages_df = merged_df %>% transmute_all(is.na) %>% 
    colSums() %>% tibble(name=names(.), percent_missing=. * (1/nrow(merged_df)) * 100) %>% arrange(desc(percent_missing)) %>% as.data.frame()
  return(missing_percentages_df)
}


#This function takes two arguments, first is the merged dataset (dataframe) and second is the user
# choice on wether to drop columns with missing data or drop rows or just impute missing values
handle_missingness <- function(merged_df , choice){
  
  if (choice == "Impute missing values"){
    imputer_recipe = 
    merged_df %>%
    recipe(formula = "~ . - outcome_variable") %>%
    step_knnimpute(all_predictors(),neighbors = 1)
  
    merged_imputed_df = prep(imputer_recipe) %>%
    bake(new_data = merged_df)
  }
  
  if (choice == "Drop rows with missing values"){
    merged_imputed_df <- merged_df[rowSums(is.na(merged_df)) <= 0,]
    
  }
  if (choice == "Delete features with missing values"){
    merged_imputed_df <- merged_df %>%  select_if(~ !any(is.na(.)))
    
  }
  
  return(merged_imputed_df)
  
  
}




#Below is how I am thinking we can call it, I am happy to implement it in shiny myself if you want that
#Can call it from shiny when passing input as below


radioButtons(inputId = "handle_missingness",
             label = "Do you want handle missing values?",
             choices = c("Impute missing values", "Drop rows with missing values" , "Delete features with missing values"),
             selected = c("Impute missing values"))
