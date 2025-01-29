##### Analyzing Lookback Windows in Rolling Predictions #####
### Last Update: 1/29/2025

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
filepath <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Predictions/Window Predictions/"

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
merge_predict <- bind_rows(arima_predict, ets_predict, prophet_predict)
merge_predict


# 4. Ensemble -------------------------------------------------------------

merge_predict |>
  filter(ds==ymd("2023-01-01") & Train=="2016-2022") |>
  summarise(Predict_mean=mean(Predict),
            Lower_mean=mean(Lower),
            Upper_mean=mean(Upper)) |>
  mutate(Method="Ensemble")

# Ensemble model - average prediction of the three model classes
ensemble_predict <- merge_predict |>
  group_by(ds, Train) |>
  summarise(Predict_mean=mean(Predict),
            Lower_mean=mean(Lower),
            Upper_mean=mean(Upper)) |>
  mutate(Method="Ensemble") |>
  rename(Predict=Predict_mean, Lower=Lower_mean, Upper=Upper_mean) |>
  inner_join(merge_predict |> filter(Method=="ARIMA" & Train=="2022") |> dplyr::select(ds, y), by="ds") |>
  relocate(ds, y, Method, Predict, Lower, Upper)
ensemble_predict

# Merge into one dataframe
asthma_predict <- bind_rows(as_tibble(merge_predict), as_tibble(ensemble_predict)) |>
  arrange(ds, Train)
print(asthma_predict, n=100)

# Save the predictions
dir <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Predictions/Window Predictions/"
# saveRDS(asthma_predict, paste(dir, "window_predict.rds", sep=""))

# Read in the predictions with varying lookback windows
asthma_predict <- readRDS(paste(dir, "window_predict.rds", sep="")) |>
  mutate(Method=factor(Method, levels = c("ARIMA","ETS","Prophet","Ensemble")))
asthma_predict


# 3. Cross-Validation -----------------------------------------------------

# Number of days in training windows
365*7:1

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
  print(n=28)


### Absolute Percentage Error

# Calculate absolute percentage error
asthma_pcterror <- asthma_predict |>
  mutate(PctError=abs(y - Predict)/Predict*100)
asthma_pcterror

# 5-number summary and IQR of median absolute percentage error
pcterror_summary <- asthma_pcterror |>
  group_by(Method, Train) |>
  summarise(Min=min(PctError), Q1=quantile(PctError, p=0.25, names=F), Median=median(PctError),
            Q3=quantile(PctError, p=0.75, names=F), Max=max(PctError), IQR=Q3-Q1, Mean=mean(PctError))
print(pcterror_summary |> select(Train, Mean, Median, IQR), n=28)

# Sort by window
pcterror_summary|>
  arrange(Train, Method) |>
  print(n=28)


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
  print(n=28)

# Get mean squared error
mse_summary <- asthma_sqerror |>
  group_by(Method, Train) |>
  summarise(MSE=sum(SqError))
print(mse_summary, n=28)

# Sort MSE by window
mse_summary |>
  arrange(Train, Method) |>
  print(n=28)


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
print(cover_summary, n=83)

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

# Ensemble
cover_summary |>
  filter(Method=="Ensemble") |>
  print(n=21)

###

# 2022
cover_summary |>
  filter(Train=="2022") |>
  print(n=12)

# 2021-22
cover_summary |>
  filter(Train=="2021-2022") |>
  print(n=12)

# 2020-22
cover_summary |>
  filter(Train=="2020-2022") |>
  print(n=12)

# 2019-22
cover_summary |>
  filter(Train=="2019-2022") |>
  print(n=12)

# 2018-22
cover_summary |>
  filter(Train=="2018-2022") |>
  print(n=12)

# 2017-22
cover_summary |>
  filter(Train=="2017-2022") |>
  print(n=12)

# 2016-22
cover_summary |>
  filter(Train=="2016-2022") |>
  print(n=12)


# 4. Thresholds -----------------------------------------------------------

#####
# Calculate absolute percentage error stats, coverage, and the ROC curve analysis
yrs <- c("2022","2021-2022","2020-2022","2019-2022","2018-2022","2017-2022","2016-2022")
vld <- rep("2023",length(yrs))
mtd <- c("ARIMA","ETS","Prophet","Ensemble")
i <- 1
while(i<=length(yrs)){
  
  # Filter by Train years
  asthma_Train <- asthma_predict |>
    filter(Train==yrs[i])
  
  #####
  
  # Calculate absolute percentage error
  asthma.pcterror <- asthma_Train |>
    mutate(PctError=abs(y - Predict)/Predict*100) |>
    group_by(Method) |>
    summarise(Min=min(PctError), Q1=quantile(PctError, p=0.25, names=F), Median=median(PctError),
              Q3=quantile(PctError, p=0.75, names=F), Max=max(PctError), IQR=Q3-Q1, Mean=mean(PctError)) |>
    mutate(Train=yrs[i], Valid=vld[i]) |>
    select(Method, Train, Valid, Mean, Median, IQR)
  
  # Coverage
  asthma.cover <- asthma_Train |>
    mutate(Cover=ifelse(y>Lower & y<=Upper, "COVER",
                        ifelse(y<Lower, "TOO_HIGH",
                               ifelse(y>Upper, "TOO_LOW",NA)))) |>
    group_by(Method) |>
    count(Cover, .drop = FALSE) |>
    mutate(Train=yrs[i], Valid=vld[i], Prop=n/sum(n)) |>
    select(Method, Train, Valid, Cover, n, Prop)
  
  #####
  
  # Set risk threshold
  p <- 0.05
  
  # Get number of admissions that meet or exceed the risk threshold
  arima.top <- asthma_Train |>
    filter(Method=="ARIMA") |>
    slice_max(order_by=Predict, prop=p)
  ets.top <- asthma_Train |>
    filter(Method=="ETS") |>
    slice_max(order_by=Predict, prop=p)
  prophet.top <- asthma_Train |>
    filter(Method=="Prophet") |>
    slice_max(order_by=Predict, prop=p)
  ensemble.top <- asthma_Train |>
    filter(Method=="Ensemble") |>
    slice_max(order_by=Predict, prop=p)
  actual.top <- asthma_Train |>
    filter(Method=="ARIMA") |>
    slice_max(order_by=y, prop=p)
  
  # Set benchmark to classify HIGH admission days
  arima.thres <- min(arima.top$Predict, na.rm=TRUE)
  ets.thres <- min(ets.top$Predict, na.rm=TRUE)
  prophet.thres <- min(prophet.top$Predict, na.rm=TRUE)
  ensemble.thres <- min(ensemble.top$Predict, na.rm=TRUE)
  actual.thres <- min(actual.top$y, na.rm=TRUE)
  
  # Classify whether the day was HIGH or NORMAL
  asthma_status <- asthma_Train |>
    mutate(Thres=ifelse(Method=="ARIMA", arima.thres,
                        ifelse(Method=="ETS", ets.thres,
                               ifelse(Method=="Prophet", prophet.thres,
                                      ifelse(Method=="Ensemble", ensemble.thres, NA))))) |>
    mutate(model.high = ifelse(Predict >= Thres, 1, 0),
           actual.high = ifelse(y >= actual.thres, 1, 0)) |>
    mutate(Method=factor(Method, levels = c("ARIMA","ETS","Prophet","Ensemble")),
           Train=yrs[i], Valid=vld[i]) |>
    select(ds, y, actual.high, Method, Train, Valid, Predict, Thres, model.high)
  
  # Actual HIGH days
  asthma.act <- asthma_status |>
    filter(Method=="ARIMA") |>
    count(actual.high) |>
    mutate(pct=n/sum(n)) |>
    mutate(Valid=vld[i]) |>
    select(Valid, actual.high, n, pct)
  
  # Classification by model
  asthma.c <- asthma_status |>
    group_by(Method) |>
    count(model.high, actual.high) |>
    mutate(Train=yrs[i], Valid=vld[i]) |>
    relocate(Train, .after=Method) |>
    relocate(Valid, .after=Train) |>
    arrange(Method, Train, desc(actual.high), desc(model.high))
  
  # Calculate misclassification rate
  asthma.miss <- asthma_status |>
    mutate(miss=ifelse(actual.high==model.high, "CORRECT", "INCORRECT")) |>
    group_by(Method) |>
    count(miss) |>
    mutate(miss.rate=n/730) |>
    mutate(Train=yrs[i], Valid=vld[i]) |>
    relocate(Train, .after=Method) |>
    relocate(Valid, .after=Train)
  
  # PPV and NPV
  asthma.ppv_npv <- asthma_status |>
    group_by(Method) |>
    summarise(true.pos=sum(ifelse(model.high==1 & actual.high==1, 1, 0)),
              pos=sum(ifelse(model.high==1, 1, 0)),
              true.neg=sum(ifelse(model.high==0 & actual.high==0, 1, 0)),
              neg=sum(ifelse(model.high==0, 1, 0))) |>
    mutate(ppv=true.pos/pos,
           npv=true.neg/neg) |>
    mutate(Train=yrs[i], Valid=vld[i]) |>
    relocate(Train, .after=Method) |>
    relocate(Valid, .after=Train)
  
  #####
  
  # ROC curve analysis for all models
  j <- 1
  while(j<=length(mtd)){
    # Fit ROC curve
    asthma.roc <- asthma_status |>
      filter(Method==mtd[j]) |>
      roc(response=actual.high, predictor=Predict)
    
    # Pull sensitivity and specificity for different cutoffs
    roc.stat <- data.frame(Sensitivity=asthma.roc$sensitivities,
                                 Specificity=asthma.roc$specificities,
                                 Cutoffs=asthma.roc$thresholds) |>
      mutate(DistSquared=(Sensitivity-1)^2+(Specificity-1)^2) |>
      tibble()
    
    # Which row has the smallest distance?
    asthma.roc_stat <- roc.stat |>
      slice_min(DistSquared)
    
    # Calculate AUC + 95% CI
    asthma.roc_stat$AUC <- as.double(asthma.roc$auc)
    asthma.roc_stat$AUC_Lower <- ci.auc(asthma.roc)[1]
    asthma.roc_stat$AUC_Upper <- ci.auc(asthma.roc)[3]
    
    # Format
    asthma.roc_stat <- asthma.roc_stat |>
      mutate(Method=mtd[j], Train=yrs[i], Valid=vld[i]) |>
      select(Method, Train, Valid, Sensitivity, Specificity, AUC, AUC_Lower, AUC_Upper)
    
    # Combine in one data frame
    if(j==1){
      asthma.ROC <- asthma.roc_stat
    } else{
      asthma.ROC <- bind_rows(asthma.ROC, asthma.roc_stat)
    }
    
    # Go to next model class
    j <- j+1
  }
  
  #####
  
  # Pull results into one data frame
  if(i==1){
    asthma_pcterror <- asthma.pcterror
    asthma_cover <- asthma.cover
    asthma_act <- asthma.act
    asthma_c <- asthma.c
    asthma_miss <- asthma.miss
    asthma_ppv_npv <- asthma.ppv_npv
    asthma_ROC <- asthma.ROC
  } else{
    asthma_pcterror <- bind_rows(asthma_pcterror, asthma.pcterror)
    asthma_cover <- bind_rows(asthma_cover, asthma.cover)
    asthma_act <- bind_rows(asthma_act, asthma.act)
    asthma_c <- bind_rows(asthma_c, asthma.c)
    asthma_miss <- bind_rows(asthma_miss, asthma.miss)
    asthma_ppv_npv <- bind_rows(asthma_ppv_npv, asthma.ppv_npv)
    asthma_ROC <- bind_rows(asthma_ROC, asthma.ROC)
  }
  
  # Clear results from the iterations
  rm(asthma.pcterror, asthma.cover, asthma.c, asthma.miss, asthma.ppv_npv, asthma.ROC)
  
  # Next lookback window
  i <- i+1
}

#####

# Absolute percentage error
print(asthma_pcterror |> arrange(Method, Train), n=28)

# 90% confidence interval coverage 
print(asthma_cover |> arrange(Method, Train), n=83)

# Actual HIGH/NORMAL classifications
asthma.act

# HIGH/NORMAL classification scorecard
print(asthma_c |> arrange(Method, Train), n=112)

# Misclassification rates
print(asthma_miss |> arrange(Method, Train), n=56)

# PPV and NPV
print(asthma_ppv_npv |> arrange(Method, Train), n=28)

# Sensitivity, specificity, AUC + 95% CI from ROC curves
print(asthma_ROC |> arrange(Method, Train), n=28)
