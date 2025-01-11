arima_fit <- function(ts_data, lookahead=1, ci.level=0.95,
                       p=0, d=1, q=2, P=0, D=0, Q=0){
  # Load package if not loaded
  library(dplyr)
  library(fpp3)
  library(feasts)
  library(seasonal)
  
  # Fit ARIMA model found by the search algorithm
  asthma_mdl <- ts_data |>
    model(arima=ARIMA(y ~ 1 + pdq(p,d,q) + PDQ(P,D,Q) ))

  # Forecast on future time points
  hilo.name <- as.name(paste(100*ci.level, "%", sep=""))
  ts_next <- forecast(asthma_mdl |> select(arima), h=lookahead) |>
    hilo(level=100*ci.level) |>
    rename(CI=all_of(hilo.name)) |>
    mutate(Lower=CI$lower, Upper=CI$upper, Method="ARIMA") |>
    rename(Dist=y, Predict=.mean) |>
    select(ds, Method, Predict, Lower, Upper)
  ts_next
  
  # Return results
  return(ts_next)
}