# COVID-19 Forecasting and Machine Learning Pipeline Visualization Tool

## Background on our Project
Our team developed a COVID-19 Forecasting and Machine Learning Pipeline Visualization Tool that provides the user with the projected number of COVID-19 two weeks out from the time that they utilize the tool through a Generalized Linear Model and eXtreme Gradient Boosting, gives the user insight into the inputs that get fed into the predictive forecasting models, steps the user through the process of transforming the raw data into insights about predicted COVID-19 cases or death counts, and allows the user to upload state-level custom data to assess the predictive power of particular features that may be of interest to them. 

The primary motivation for providing the user the ability to upload custom data is that the nature of the pandemic and the critical factors that influence its spread currently are likely to change over time as more is learned about the virus and techniques to manage its spread in the future. The user, can therefore, upload temporally relevant data into our forecasting models and get a sense for the real-time predictive power of their inputs. Our tool provides a built in set of state-level features, such as population density, political learning, and racial demographics and the user can built upon these provided features using a template provided in the app.


## How to Access Tool & Notes About Usage of the Tool

Our COVID-19 Forecasting and Machine Learning Pipeline Tool can be accessed at: **https://tobijegede.shinyapps.io/covid19forecast/.**


**Please note:** You need to run the pages on our R Shiny app **in the order in which they appear on the sidebar** (i.e. Overview & Data Ingestion then Data Merging then Data Pre-Processing then Feature Selection, etc.). Additionally, some of the processes running in the background of the app take a minute or so to load. Please allocate additional time for the Feature Selection, Generalized Linear Model, and eXtreme Gradient Boosting pages to run once you've clicked on the action buttons associated with these pages. In particular, the **eXtreme Gradient Boosting page** can take up to 20 minutes to run, due to the complexity of the model.

If you would like to explore the underlying code for our R Shiny app, it can be found in this repository in the **"covid19forecast" folder in the app.R file.** To test out the user input functionality on our app, feel free to upload the **"user_input_policies_full_example.csv"** to our app, which can be found in the "Data" folder on this repository.
