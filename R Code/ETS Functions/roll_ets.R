roll_ets <- function(ts_data, start_date="2022-01-01", iter=1, timeframe=365,
                     ci.level=0.95, Error="A", Trend="N", Season="A"){
  # Load package if not loaded
  library(dplyr)
  library(fpp3)
  library(feasts)
  library(seasonal)
  
  # Source 'asthma_fit.R' file
  source("R Code/ETS Functions/ets_fit.R")
  
  #####
  
  # Identify latest date
  start_date <- as.Date(start_date)
  
  # Check that the roll_date is later than earliest time point
  if(start_date < min(ts_data$ds)){
    stop("Roll date specified is before the earliest time point in the series.")
  }
  
  # Check that the roll_date is before the latest time point
  if(start_date > max(ts_data$ds)){
    stop("Roll date specified is after the latest time point in the series.")
  }
  
  #####
  
  # Identify row index of starting date
  begin_i <- which(ts_data$ds==start_date)
  
  # Look inside window of time series
  asthma_train <- ts_data %>%
    slice((begin_i-timeframe):(begin_i-1))
  asthma_train
  asthma_train %>%
    tail()
  
  # Set iteration number
  i <- 1
  
  #####
  
  # Repeat the forecasts for 'iter' weeks
  while(i <= iter){
      
    # Get one-step prediction based on specified ARIMA model
    asthma_one <- ets_fit(ts_data = asthma_train, lookahead = 1,
                          ci.level=ci.level, Error=Error, Trend=Trend, Season=Season)
    asthma_one
    
    # Save prediction at each iteration
    if(i==1){
      asthma_iter <- asthma_one
    } else{
      asthma_iter <- bind_rows(asthma_iter, asthma_one)
    }
    
    # Add actual next observation to the train set
    asthma_train <- asthma_train %>%
      add_row(ds=ts_data$ds[begin_i-1+i],
              y=ts_data$y[begin_i-1+i])
    tail(asthma_train)
    
    # Remove oldest (first) row to keep train set within time frame
    asthma_train <- asthma_train %>%
      slice_tail(n = timeframe)
    asthma_train
    tail(asthma_train)
    
    # Update user on progress
    message(paste("Prediction made for", ts_data$ds[begin_i-1+i], sep=" "))
    
    # Go to the next iteration
    i <- i+1
  }
  
  # Select appropriate columns
  asthma_iter <- asthma_iter %>%
    select(ds, Method, Predict, Lower, Upper)
  
  # Return the iterated time series
  return(asthma_iter)
}