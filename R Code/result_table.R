##### Asthma Forecasting Algorithms - Table Builder #####
### Last Update: 2/6/2025

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
library(tidyr)

# Load in the raw asthma data
asthma <- read_excel("C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/data-raw/asthma.xlsx") |>
  tibble() |>
  mutate(WEEK_NAME=wday(Day, label=TRUE, abbr=TRUE),
         WEEK_NUMBER=epiweek(Day),
         WeekDate=floor_date(Day, unit="week", week_start=7),
         Quarter=quarter(Day, fiscal_start = 7, type="date_first")) |>
  relocate(Quarter, .before=MONTH_NUMBER) |>
  relocate(WEEK_NUMBER, .after=MONTH_NUMBER) |>
  relocate(WEEK_NAME, .after=MONTH_NAME) |>
  relocate(WeekDate, .after=MonthDate) |>
  as_tibble()
print(asthma, n=15)

# Daily predictions - main analysis
dir <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Predictions/Daily Predictions/"
asthma_predict <- readRDS(paste(dir, "daily_predict.rds", sep=""))
asthma_predict

# Daily predictions - precovid analysis (2016-2019)
dir <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Predictions/Precovid Predictions/"
asthma_precovid <- readRDS(paste(dir, "precovid_predict.rds", sep=""))
asthma_precovid

# Daily predictions - postcovid analysis (2020-2023)
dir <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Predictions/Postcovid Predictions/"
asthma_postcovid <- readRDS(paste(dir, "postcovid_predict.rds", sep=""))
asthma_postcovid


# 1. Count Table ----------------------------------------------------------

yrs <- c("2016-2023","2016-2021","2016-2018","2020-2022",
         "2016","2017","2018","2019","2020","2021","2022","2023")
i <- 1
n.max <- 6
while(i<=length(yrs)){
  if(i==1){
    # Filter based on time period
    asthma.yrs <- asthma |> filter(year(Day)>="2016" & year(Day)<="2023")
  } else if(i==2){
    asthma.yrs <- asthma |> filter(year(Day)>="2016" & year(Day)<="2021")
  } else if(i==3){
    asthma.yrs <- asthma |> filter(year(Day)>="2016" & year(Day)<="2018")
  } else if(i==4){
    asthma.yrs <- asthma |> filter(year(Day)>="2020" & year(Day)<="2022")
  } else{
    asthma.yrs <- asthma |> filter(year(Day)==yrs[i])
  }
  
  # Count number of days in each period
  length.days <-  asthma.yrs |>
    count() |>
    pull()
  
  # Count total number of cases in each period
  total.cases <- asthma.yrs |>
    ungroup() |>
    summarise(Total=sum(AllAdmissions)) |>
    pull()
  
  # Count number of days with N cases
  cases <- rep(NA, n.max+2)
  n <- 1
  while(n<=n.max+2){
    if(n<n.max+1){
      
      # Days where exactly N cases were observed
      cases[n] <- asthma.yrs |>
        filter(AllAdmissions==n-1)|>
        count()|>
        pull()
      
    } else{
      
      # Days where at least N or more cases were observed
      cases[n] <- asthma.yrs |>
        filter(AllAdmissions>=n-1)|>
        count()|>
        pull()
      
    }
    
    # Go to next n
    n <- n+1
  }
  
  # Count total number of cases
  
  # Save results to a table
  if(i==1){
    asthma.tbl <- data.frame(Period=yrs[i], Length=length.days, Total=total.cases,
                             Zero=cases[1], One=cases[2], Two=cases[3], Three=cases[4],
                             Four=cases[5], Five=cases[6], Six=cases[7], More=cases[8])
  } else{
    asthma.tbl <- bind_rows(asthma.tbl,
                            data.frame(Period=yrs[i], Length=length.days, Total=total.cases,
                                       Zero=cases[1], One=cases[2], Two=cases[3], Three=cases[4],
                                       Four=cases[5], Five=cases[6], Six=cases[7], More=cases[8]) )
  }
  
  # Go to next period
  i <- i+1
}

# Print table
asthma.tbl


# 2. Table Builder --------------------------------------------------------

# Calculate absolute percentage error stats, coverage, and the ROC curve analysis
yrs <- c("2016-2021","2016-2018","2020-2022")
vld <- c("2022-2023","2019","2023")
mtd <- c("ARIMA","ETS","Prophet","Ensemble")
i <- 1
while(i<=length(yrs)){
  
  # Select appropriate data
  if(i==1){
    asthma_yrs <- asthma_predict |>
      mutate(Method=factor(Method, levels = c("ARIMA","ETS","Prophet","Ensemble")))
  } else if(i==2){
    asthma_yrs <- asthma_precovid |>
      mutate(Method=factor(Method, levels = c("ARIMA","ETS","Prophet","Ensemble")))
  } else {
    asthma_yrs <- asthma_postcovid |>
      mutate(Method=factor(Method, levels = c("ARIMA","ETS","Prophet","Ensemble")))
  }
  
  #####
  
  # Calculate absolute percentage error
  asthma.pcterror <- asthma_yrs |>
    mutate(PctError=abs(y - Predict)/Predict*100) |>
    group_by(Method) |>
    summarise(Min=min(PctError), Q1=quantile(PctError, p=0.25, names=F), Median=median(PctError),
              Q3=quantile(PctError, p=0.75, names=F), Max=max(PctError), IQR=Q3-Q1, Mean=mean(PctError)) |>
    mutate(Train=yrs[i], Valid=vld[i]) |>
    select(Method, Train, Valid, Mean, Median, IQR)
  
  # Coverage
  asthma.cover <- asthma_yrs |>
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
  arima.top <- asthma_yrs |>
    filter(Method=="ARIMA") |>
    slice_max(order_by=Predict, prop=p)
  ets.top <- asthma_yrs |>
    filter(Method=="ETS") |>
    slice_max(order_by=Predict, prop=p)
  prophet.top <- asthma_yrs |>
    filter(Method=="Prophet") |>
    slice_max(order_by=Predict, prop=p)
  ensemble.top <- asthma_yrs |>
    filter(Method=="Ensemble") |>
    slice_max(order_by=Predict, prop=p)
  actual.top <- asthma_yrs |>
    filter(Method=="ARIMA") |>
    slice_max(order_by=y, prop=p)
  
  # Set benchmark to classify HIGH admission days
  arima.thres <- min(arima.top$Predict, na.rm=TRUE)
  ets.thres <- min(ets.top$Predict, na.rm=TRUE)
  prophet.thres <- min(prophet.top$Predict, na.rm=TRUE)
  ensemble.thres <- min(ensemble.top$Predict, na.rm=TRUE)
  actual.thres <- min(actual.top$y, na.rm=TRUE)
  
  # Classify whether the day was HIGH or NORMAL
  asthma_status <- asthma_yrs |>
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

# 3. Table Results --------------------------------------------------------

# Absolute percentage error
print(asthma_pcterror |> arrange(Method), n=12)

# 90% confidence interval coverage 
print(asthma_cover |> arrange(Method), n=36)

# Actual HIGH/NORMAL classifications
asthma_act

# HIGH/NORMAL classification scorecard
print(asthma_c |> arrange(Method), n=48)

# Misclassification rates
print(asthma_miss |> arrange(Method), n=24)
print(asthma_miss |> filter(miss=="INCORRECT") |> arrange(Method))

# PPV and NPV
print(asthma_ppv_npv |> arrange(Method), n=12)

# Sensitivity, specificity, AUC + 95% CI from ROC curves
asthma_ROC |>
  mutate(Method=factor(Method, levels=c("ARIMA","ETS","Prophet","Ensemble"))) |>
  arrange(Method)

#####

# Save results
dir <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Predictions/"
saveRDS(asthma_ROC, file = paste(dir, "ROC_table.RDS", sep=""))

# 4. Thresholds -----------------------------------------------------------

# Calculate thresholds
yrs <- c("2016-2021","2016-2018","2020-2022")
vld <- c("2022-2023","2019","2023")
mtd <- c("ARIMA","ETS","Prophet","Ensemble","Actual")
thres <- c(0.01, 0.05, 0.10, 0.20, 0.25, 0.50)
i <- 1
while(i<=length(yrs)){
  
  # Select appropriate data
  if(i==1){
    asthma_yrs <- asthma_predict |>
      mutate(Method=factor(Method, levels = c("ARIMA","ETS","Prophet","Ensemble")))
  } else if(i==2){
    asthma_yrs <- asthma_precovid |>
      mutate(Method=factor(Method, levels = c("ARIMA","ETS","Prophet","Ensemble")))
  } else {
    asthma_yrs <- asthma_postcovid |>
      mutate(Method=factor(Method, levels = c("ARIMA","ETS","Prophet","Ensemble")))
  }
  
  # Obtain the threshold
  p <- 1
  while(p<=length(thres)){
    j <- 1
    while(j<=length(mtd)){
      # Get number of admissions that meet or exceed the risk threshold
      if(mtd[j]=="Actual"){
        
        # Actual threshold
        asthma.top <- asthma_yrs |>
          filter(Method=="ARIMA") |>
          slice_max(order_by=y, prop=thres[p])
        asthma.thres <- min(asthma.top$y, na.rm=TRUE)
      } else{
        
        # Threshold from model
        asthma.top <- asthma_yrs |>
          filter(Method==mtd[j]) |>
          slice_max(order_by=Predict, prop=thres[p])
        asthma.thres <- min(asthma.top$Predict, na.rm=TRUE)
      }
      
      # Build table
      if(i==1 & p==1 & j==1){
        thresholds <- data.frame(Method=mtd[j], Train=yrs[i], Valid=vld[i],
                                 p=thres[p], Threshold=asthma.thres)
      } else{
        thresholds <- bind_rows(thresholds,
                                data.frame(Method=mtd[j], Train=yrs[i], Valid=vld[i],
                                           p=thres[p], Threshold=asthma.thres))
      }
      
      # Go to next model class
      j <- j+1
    }
    
    # Go to next threshold
    p <- p+1
    
  }
  
  # Go to next subset
  i <- i+1

}

# Print threshold results
thresholds |>
  mutate(Method=factor(Method, levels=c("ARIMA","ETS","Prophet","Ensemble","Actual"))) |>
  arrange(Method) |>
  tibble() |>
  print(n=90)

# Convert to wide for table
thresholds |>
  pivot_wider(names_from = p, values_from = Threshold) |>
  mutate(Method=factor(Method, levels=c("ARIMA","ETS","Prophet","Ensemble","Actual"))) |>
  arrange(Method) |>
  tibble() |>
  print(n=15)
