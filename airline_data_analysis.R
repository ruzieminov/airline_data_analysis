library(data.table)
library(inspectdf)
library(dplyr)
library(tidymodels)
library(modeltime)
library(tidyverse)
library(lubridate)
library(timetk)
library(h2o)
library(rsample)
library(forecast)
library(highcharter)

data <- fread('AirPassengers (3).csv')
data %>% View()

data %>% glimpse()

data %>% inspect_na()

names(data)

colnames(data) <- c('Date','Count')

day_col <- '01'
data$Date <- paste0(data$Date, '-',day_col)
data$Date <- data$Date %>% as.Date(.,'%Y-%m-%d')
# library(zoo)
# data$Date <- as.Date(as.yearmon(data$Date)) 


data %>% 
  plot_time_series(
    Date, Count,
    #.color_var = lubridate::year(Date),
    #.color_lab = 'Year',
    .interactive = T,
    .plotly_slider = T)

# --- Seasonality plots ---
data %>% 
  plot_seasonal_diagnostics(
    Date, Count, .interactive = T)

#1. Use arima_boost(), exp_smoothing(), prophet_reg() models;
splits <- initial_time_split(data, prop = 0.8)

# Model: arima_boost
model_fit_arima_boosted <- arima_boost(
  min_n = 2,
  learn_rate = 0.015) %>%
  set_engine(engine = "auto_arima_xgboost") %>%
  fit(Count ~ Date + as.numeric(Date) + factor(lubridate::month(Date, label = TRUE),ordered = F),
      data = training(splits))

# Model: ETS
model_fit_ets <- exp_smoothing() %>% 
  set_engine(engine = 'ets') %>% 
  fit(Count ~ Date, data = training(splits))

# Model: Prophet
model_fit_prophet <- prophet_reg() %>% 
  set_engine(engine = 'prophet') %>% 
  fit(Count ~ Date, data = training(splits))

# Add fitted models to a Model Table.
models_tbl <- modeltime_table(
  model_fit_arima_boosted,
  model_fit_ets,
  model_fit_prophet)

models_tbl

# 2. Compare RMSE scores on test set; 
# Calibrate the model to a testing set.
calibration_tbl <- models_tbl %>% 
  modeltime_calibrate(new_data = testing(splits))

calibration_tbl

# --- Testing Set Forecast & Accuracy Evaluation ---
# - Visualizing the Forecast Test----
calibration_tbl %>% 
  modeltime_forecast(
    new_data = testing(splits),
    actual_data = data) %>% 
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive = T)

# - Accuracy Metrics----
calibration_tbl %>% 
  modeltime_accuracy() %>% 
  table_modeltime_accuracy(.interactive = T)

#3. Make forecast on lowest RMSE score model;
#Prediction for the new data
new_data_n <- seq(as.Date("1961-01-01"), as.Date("1961-12-01"), "month") %>%
  as_tibble() %>% 
  add_column(Count=0) %>% 
  rename(Date=value) %>% 
  tk_augment_timeseries_signature() %>%
  select(-contains("hour"),
         -contains("day"),
         -contains("week"),
         -minute,-second,-am.pm) %>% 
  mutate_if(is.ordered, as.character) %>% 
  mutate_if(is.character,as_factor)

#predictions for the next year
new_predictions <- model_fit_arima_boosted %>%  
  modeltime_calibrate(new_data = new_data_n) %>% 
  modeltime_forecast(new_data_n) %>% 
  as_tibble() %>%
  add_column(Date=new_data_n$Date) %>% 
  select(Date,.value) %>% 
  rename(Count=.value)

#4. Visualizing past data and forecast values on one plot; making separation with two different colors----
# red for past data, green for prediction
data %>% 
  bind_rows(new_predictions) %>% 
  mutate(categories=c(rep('Actual',nrow(data)),rep('Predicted',nrow(new_predictions)))) %>% 
  hchart("line", hcaes(Date, Count, group = categories)) %>% 
  hc_title(text='Forecast the for next year') %>% 
  hc_colors(colors = c('red','green'))
