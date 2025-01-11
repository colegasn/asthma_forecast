ets_fit <- function(ts_data, lookahead=1, ci.level=0.95,
                    Error="A", Trend="N", Season="A"){
  # Load package if not loaded
  library(dplyr)
  library(fpp3)
  library(feasts)
  library(seasonal)
  
  # Fit ETS model found by the search algorithm
  ets_mdl <- ts_data |>
    mutate(y=replace_na(y, 0)) |>
    model(ets = ETS(y ~ error(Error)+trend(Trend)+season(Season)))

  # Forecast on future time points
  hilo.name <- as.name(paste(100*ci.level, "%", sep=""))
  ts_next <- forecast(ets_mdl, h=lookahead) |>
    hilo(level=100*ci.level) |>
    rename(CI=all_of(hilo.name)) |>
    mutate(Lower=CI$lower, Upper=CI$upper, Method="ETS") |>
    rename(Dist=y, Predict=.mean) |>
    select(ds, Method, Predict, Lower, Upper)
  ts_next
  
  # Return results
  return(ts_next)
}