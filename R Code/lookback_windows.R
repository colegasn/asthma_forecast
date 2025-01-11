##### Analyzing Lookback Windows in Rolling Predictions #####
### Last Update: 1/10/2025

# Load packages
library(readxl)
library(prophet)
library(dplyr)
library(tibble)
library(tsibble)
library(lubridate)
library(fable)
library(feasts)
library(ggplot2)
library(pROC)

# Load in functions to do rolling predictions
source("R Code/ARIMA Functions/roll_arima.R")
source("R Code/ETS Functions/roll_ets.R")
source("R Code/Prophet Functions/roll_prophet.R")


# Load Data ---------------------------------------------------------------

# Read in and format data
asthma <- read_excel("C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/data-raw/asthma.xlsx") |>
  tibble() |>
  mutate(WEEK_NAME=wday(Day, label=TRUE, abbr=TRUE),
         WEEK_NUMBER=epiweek(Day),
         WeekDate=floor_date(Day, unit="week", week_start=7),
         Quarter=quarter(Day, fiscal_start = 7, type="date_first")) |>
  relocate(Quarter, .before=MONTH_NUMBER) |>
  relocate(WEEK_NUMBER, .after=MONTH_NUMBER) |>
  relocate(WEEK_NAME, .after=MONTH_NAME) |>
  relocate(WeekDate, .after=MonthDate)
print(asthma, n=15)

# All asthma admissions
asthma_ts <- asthma |>
  dplyr::select(Day, AllAdmissions) |>
  mutate(Day=ymd(Day)) |>
  rename(ds=Day, y=AllAdmissions) |>
  as_tsibble(index = ds)
asthma_ts

# Split into train and test data
asthma_train <- asthma_ts |>
  filter(ds<"2023-01-01")
asthma_test <- asthma_ts |>
  filter(ds>="2023-01-01")


# 1. Rolling Predictions --------------------------------------------------

# File path where to save predictions
filepath <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Window Predictions/"

# List of years to train (windows)
yrs <- c("2022","2021-2022","2020-2022","2019-2022","2018-2022","2017-2022","2016-2022")
i <- 7
while(i<=length(yrs)){
  # Directory where predictions will be saved
  dir <- paste(filepath, yrs[i], "/", sep="")
  
  # ARIMA
  message(paste("Generating ARIMA predictions trained on data from ", yrs[i], sep=""))
  arima_ts <- suppressMessages(roll_arima(ts_data = asthma_ts, start_date = "2023-01-01",
                           iter = nrow(asthma_test), timeframe = i*365, ci.level = 0.90)) |>
    inner_join(asthma_ts, by="ds") |>
    mutate(Train=yrs[i]) |>
    select(ds, y, Method, Train, Predict, Lower, Upper)
  saveRDS(arima_ts, paste(dir, "arima_", yrs[i], ".rds", sep=""))
  
  # ETS
  message(paste("Generating ETS predictions trained on data from ", yrs[i], sep=""))
  ets_ts <- suppressMessages(roll_ets(ts_data = asthma_ts, start_date = "2023-01-01",
                        iter = nrow(asthma_test), timeframe = i*365, ci.level = 0.90)) |>
    inner_join(asthma_ts, by="ds") |>
    mutate(Train=yrs[i]) |>
    select(ds, y, Method, Train, Predict, Lower, Upper)
  saveRDS(ets_ts, paste(dir, "ets_", yrs[i], ".rds", sep=""))
  
  # Prophet
  message(paste("Generating Prophet predictions trained on data from ", yrs[i], sep=""))
  prophet_ts <- suppressMessages(roll_prophet(ts_data = asthma_ts, start_date = "2023-01-01",
                             iter = nrow(asthma_test), timeframe = i*365, ci.level = 0.90)) |>
    inner_join(asthma_ts, by="ds") |>
    mutate(Train=yrs[i]) |>
    select(ds, y, Method, Train, Predict, Lower, Upper)
  saveRDS(prophet_ts, paste(dir, "prophet_", yrs[i], ".rds", sep=""))
  
  # Next window
  i <- i+1
}


# 2. Compilation ----------------------------------------------------------

# File path to read in predictions
filepath <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Window Predictions/"

# Read in rolling predictions for ARIMA, ETS, and Prophet
yrs <- c("2022","2021-2022","2020-2022","2019-2022","2018-2022","2017-2022","2016-2022")
i <- 1
while(i<=length(yrs)){
  
  # Directory where predictions are saved
  dir <- paste(filepath, yrs[i], "/", sep="")
  
  # Read predictions
  arima_ts <- readRDS(paste(dir, "arima_", yrs[i], ".rds", sep="")) |>
    as_tibble()
  ets_ts <- readRDS(paste(dir, "ets_", yrs[i], ".rds", sep="")) |>
    as_tibble()
  prophet_ts <- readRDS(paste(dir, "prophet_", yrs[i], ".rds", sep="")) |>
    as_tibble()
  
  # Combine in one data frame
  if(i==1){
    arima_predict <- arima_ts
    ets_predict <- ets_ts
    prophet_predict <- prophet_ts
  } else{
    arima_predict <- bind_rows(arima_predict, arima_ts)
    ets_predict <- bind_rows(ets_predict, ets_ts)
    prophet_predict <- bind_rows(prophet_predict, prophet_ts)
  }
  
  # Next window
  i <- i+1
}

# Combine into one data frame
asthma_predict <- bind_rows(arima_predict, ets_predict, prophet_predict)
asthma_predict


# 3. Cross-Validation -----------------------------------------------------

### Absolute Error
# Calculate absolute error
asthma_error <- asthma_predict |>
  mutate(Error=abs(y - Predict))
asthma_error

# 5-number summary and IQR of absolute error
error_summary <- asthma_error |>
  group_by(Method, Train) |>
  summarise(Min=min(Error), Q1=quantile(Error, p=0.25, names=F), Median=median(Error),
         Q3=quantile(Error, p=0.75, names=F), Max=max(Error), IQR=Q3-Q1)
error_summary

# Sort by window
error_summary|>
  arrange(Train, Method) |>
  print(n=21)


### Absolute Percentage Error

# Calculate absolute percentage error
asthma_pcterror <- asthma_predict |>
  mutate(PctError=abs(y - Predict)/Predict)
asthma_pcterror

# 5-number summary and IQR of median absolute percentage error
pcterror_summary <- asthma_pcterror |>
  group_by(Method, Train) |>
  summarise(Min=min(PctError), Q1=quantile(PctError, p=0.25, names=F), Median=median(PctError),
            Q3=quantile(PctError, p=0.75, names=F), Max=max(PctError), IQR=Q3-Q1)
print(pcterror_summary, n=21)

# Sort by window
pcterror_summary|>
  arrange(Train, Method) |>
  print(n=21)


### Mean Squared Error

# Calculate squared error
asthma_sqerror <- asthma_predict |>
  mutate(SqError=(y - Predict)^2)
asthma_sqerror

# 5-number summary and IQR of squared error
sqerror_summary <- asthma_sqerror |>
  group_by(Method, Train) |>
  summarise(Min=min(SqError), Q1=quantile(SqError, p=0.25, names=F), Median=median(SqError),
            Q3=quantile(SqError, p=0.75, names=F), Max=max(SqError), IQR=Q3-Q1)
print(sqerror_summary, n=21)

# Sort by window
sqerror_summary |>
  arrange(Train, Method) |>
  print(n=21)

# Get mean squared error
mse_summary <- asthma_sqerror |>
  group_by(Method, Train) |>
  summarise(MSE=sum(SqError))
print(mse_summary, n=21)

# Sort MSE by window
mse_summary |>
  arrange(Train, Method) |>
  print(n=21)


### Coverage

# Determine if 90% confidence interval captures true observation
asthma_cover <- asthma_predict |>
  mutate(Cover=ifelse(y>Lower & y<=Upper, "COVER",
                      ifelse(y<Lower, "TOO_HIGH",
                             ifelse(y>Upper, "TOO_LOW",NA))))
asthma_cover

# Coverage scorecard
cover_summary <- asthma_cover |>
  group_by(Method,Train) |>
  count(Cover) |>
  mutate(Prop=n/sum(n))
print(cover_summary, n=63)

# ARIMA
cover_summary |>
  filter(Method=="ARIMA") |>
  print(n=21)

# ETS
cover_summary |>
  filter(Method=="ETS") |>
  print(n=21)

# Prophet
cover_summary |>
  filter(Method=="Prophet") |>
  print(n=21)

###

# 2022
cover_summary |>
  filter(Train=="2022") |>
  print(n=9)

# 2021-22
cover_summary |>
  filter(Train=="2021-2022") |>
  print(n=9)

# 2020-22
cover_summary |>
  filter(Train=="2020-2022") |>
  print(n=9)

# 2019-22
cover_summary |>
  filter(Train=="2019-2022") |>
  print(n=9)

# 2018-22
cover_summary |>
  filter(Train=="2018-2022") |>
  print(n=9)

# 2017-22
cover_summary |>
  filter(Train=="2017-2022") |>
  print(n=9)

# 2016-22
cover_summary |>
  filter(Train=="2016-2022") |>
  print(n=9)
