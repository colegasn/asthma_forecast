prophet_fit <- function(ts_data, lookahead=1, ci.level=0.95, include_history=FALSE){
  # Load package if not loaded
  library(prophet)
  
  # Fit model - suppress message: "Disabling daily seasonality. Run prophet with daily.seasonality=TRUE to override this."
  prophet_m <-suppressMessages(prophet(ts_data, interval.width = ci.level))
  
  # Test data to predict on
  future_days <- make_future_dataframe(prophet_m, periods = lookahead,
                                       include_history = include_history)
  
  # Make forecasts
  ts_next <- predict(prophet_m, future_days) |>
    as_tibble() |>
    mutate(ds=ymd(ds), Method="Prophet") |>
    rename(Predict=yhat, Lower=yhat_lower, Upper=yhat_upper) |>
    select(ds, Method, Predict, Lower, Upper) 
  
  # Return results
  return(ts_next)
}