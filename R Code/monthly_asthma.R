##### Asthma Forecasting Algorithms - Monthly Admissions #####
### Last Update: 1/21/2025

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

# Load Data ---------------------------------------------------------------

# Read in and format data
asthma <- read_excel("C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/data-raw/asthma.xlsx") |>
  tibble() |>
  mutate(WEEK_NAME=factor(wday(Day, label=TRUE, abbr=TRUE), ordered = FALSE),
         WEEK_NUMBER=epiweek(Day),
         WeekDate=floor_date(Day, unit="week", week_start=1),
         Quarter=quarter(Day, fiscal_start = 7, type="date_first")) |>
  relocate(Quarter, .before=MONTH_NUMBER) |>
  relocate(WEEK_NUMBER, .after=MONTH_NUMBER) |>
  relocate(WEEK_NAME, .after=MONTH_NAME) |>
  relocate(WeekDate, .after=MonthDate)
print(asthma, n=15)

# All monthly asthma admissions
asthma_ts <- asthma |>
  group_by(MonthDate) |>
  summarise(y=sum(AllAdmissions)) |>
  mutate(ds=ymd(MonthDate)) |>
  dplyr::select(ds, y) |>
  as_tsibble(index = ds)
print(asthma_ts, n=15)

#####

# Distribution of months with admissions: 1/1/2016 to 12/31/2023 (n=96 weeks)
asthma_ts |>
  as_tibble() |>
  tidyr::drop_na(y) |>
  summarise(Min=min(y), Q1=quantile(y, probs=0.25), Med=median(y),
            Q3=quantile(y, probs=0.75), max=max(y), IQR=Q3-Q1)

# Plot the time series for monthly admissions
asthma_ts |>
  autoplot(y, color="#00b5d1", lwd=0.8)+
  theme_bw()+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  theme(axis.title.x=element_text(color="black", size=14, margin=margin(t=10)),
        axis.text.x=element_text(color="black", size=12),
        axis.title.y=element_text(color="black", size=14, margin=margin(r=10), angle=90),
        axis.text.y=element_text(color="black", size=12),
        plot.title=element_text(color="black",size=18),
        plot.subtitle=element_text(color="black",size=12),
        panel.grid.minor = element_blank())+
  xlab("Time")+
  ylab("Count")+
  ggtitle("Monthly Hospitalizations of Patients Admitted for Asthma",
          subtitle="Jan 1, 2016 - December 31, 2023")

# Distribution of the number of admissions by week
asthma_n_mth <- asthma |>
  group_by(MONTH_NAME) |>
  summarise(Sum=sum(AllAdmissions))
asthma_n_mth

# Barplot of number of admissions per day of the week
ggplot(asthma_n_mth |>
        mutate(Sum.adj=Sum-200,
               MONTH_NAME=factor(MONTH_NAME,
                                 levels = c("January","February","March","April",
                                 "May","June","July","August","September",
                                 "October","November","December"))),
       aes(x=MONTH_NAME, y=Sum.adj))+
  geom_bar(stat="identity", width = 0.7, color="black", fill=rep("grey85",12))+
  geom_text(aes(label=Sum), vjust=-0.4, size=6)+
  theme_bw()+
  scale_x_discrete(name="Hospitalizations By Month")+
  scale_y_continuous(limits = c(0,700))+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size=16),
        axis.text.x = element_text(size=16),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=14))+
  ggtitle("Monthly Asthma Hospitalizations",
          subtitle = "January 1, 2016 - December 31, 2023")


# 1. ARIMA ----------------------------------------------------------------

# Format date column to 'yearmonth' 
asthma_ts <- asthma_ts |>
  mutate(ds=yearmonth(ds)) |>
  as_tsibble(index=ds)
asthma_ts

# Split into train and test data
asthma_train <- asthma_ts |>
  filter(ds<yearmonth("2022 Jan"))
asthma_test <- asthma_ts |>
  filter(ds>=yearmonth("2022 Jan"))


# 1.1 ARIMA Model Formulation ---------------------------------------------

# Examine time plot, ACF and PACF of original series
asthma_train |>
  gg_tsdisplay(y, plot_type = 'partial')

# Test if seasonal differencing is needed
asthma_train |>
  features(y, unitroot_nsdiffs) # no seasonal difference needed

# Test if differencing is needed
asthma_train |>
  features(y, unitroot_ndiffs) # differencing required

# Fit ARIMA models
asthma_mdl <- asthma_train |>
  model(arima001201=ARIMA(y ~ 1+pdq(0,0,1)+PDQ(2,0,1)),
        arima001101=ARIMA(y ~ 1+pdq(0,0,1)+PDQ(1,0,1)),
        arima100=ARIMA(y ~ 1+pdq(1,0,0)),
        arima001=ARIMA(y ~ 1+pdq(2,0,0)),
        search=ARIMA(y, stepwise = FALSE))
asthma_mdl
glance(asthma_mdl)

# Examine residuals
asthma_mdl |>
  select(arima001201) |>
  gg_tsresiduals()

# Test if residuals follows a white noise process
augment(asthma_mdl) |>
  filter(.model == "arima001201") |>
  features(.innov, ljung_box, lag = 36, dof = 4)

# Report the values of the best ARIMA model
report(asthma_mdl |> select(arima001201))


# 1.2 ARIMA Prediction ----------------------------------------------------

# Load in functions to do rolling predictions
source("R Code/ARIMA Functions/arima_fit.R")
source("R Code/ARIMA Functions/roll_arima.R")

# Test the prediction function
test <- arima_fit(ts_data = asthma_train, lookahead = 24, p=0, d=0, q=1, P=2, D=0, Q=1)
print(test, n=24)

# Forecast admissions for 2022 and 2023 through rolling prediction
future_ts <- roll_arima(ts_data = asthma_ts, start_date = yearmonth("2022 Jan"), iter = 24,
                        timeframe = 12*6, p=0, d=0, q=1, P=2, D=0, Q=1, ci.level = 0.90)
print(future_ts, n=24)

# Merge with observed admissions
arima_predict <- inner_join(asthma_ts, future_ts, by="ds") |>
  select(ds, y, Method, Predict, Lower, Upper)
arima_predict

# Save the predictions
dir <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Monthly Predictions/"
saveRDS(arima_predict, paste(dir, "monthly_arima_predict.rds", sep=""))

# Read back in rolling predictions
arima_predict <- readRDS(paste(dir, "monthly_arima_predict.rds", sep="")) |>
  tsibble(index=ds)


# 2. ETS ------------------------------------------------------------------

# Split into train and test data
asthma_train <- asthma_ts |>
  filter(ds<yearmonth("2022 Jan"))
asthma_test <- asthma_ts |>
  filter(ds>=yearmonth("2022 Jan"))


# 2.1 ETS Model Formulation -----------------------------------------------

# Fit ETS models
asthma_mdl <- asthma_train |>
  mutate(y=replace_na(y, 0)) |>
  model(SES = ETS(y ~ error("A")+trend("N")+season("N")),
        Holt = ETS(y ~ error("A")+trend("A")+season("N")),
        Damped = ETS(y ~ error("A")+trend("Ad")+season("N")),
        search = ETS(y))
asthma_mdl
glance(asthma_mdl)

# Test accuracy of the ETS models
asthma_mdl |>
  forecast(h = nrow(asthma_test)) |>
  accuracy(asthma_test)

# Estimated parameters of MNM model
mnm_mdl <- asthma_train |>
  mutate(y=replace_na(y, 0)) |>
  model(ana = ETS(y ~ error("M")+trend("N")+season("M")))
report(mnm_mdl)


# 2.2 ETS Prediction ------------------------------------------------------

# Load in functions to do rolling predictions
source("R Code/ETS Functions/ets_fit.R")
source("R Code/ETS Functions/roll_ets.R")

# Test the prediction function
test <- ets_fit(ts_data = asthma_train, lookahead = 24, Error = "M", Trend = "N", Season = "M")
print(test, n=24)

# Forecast admissions for 2022 and 2023 through rolling prediction
future_ts <- roll_ets(ts_data = asthma_ts, start_date = yearmonth("2022 Jan"), iter = 24, 
                      timeframe = 12*6, Error = "M", Trend = "N", Season = "M", ci.level = 0.90)
print(future_ts, n=24)

# Merge with observed admissions
ets_predict <- inner_join(asthma_ts, future_ts, by="ds") |>
  select(ds, y, Method, Predict, Lower, Upper)
ets_predict

# Save the predictions
dir <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Monthly Predictions/"
saveRDS(ets_predict, paste(dir, "monthly_ets_predict.rds", sep=""))

# Read back in rolling predictions
ets_predict <- readRDS(paste(dir, "monthly_ets_predict.rds", sep="")) |>
  tsibble(index=ds)


# 3. Prophet --------------------------------------------------------------

# Format 'ds' column to 'ymd' format
asthma_ts <- asthma |>
  group_by(MonthDate) |>
  summarise(y=sum(AllAdmissions)) |>
  mutate(ds=ymd(MonthDate)) |>
  dplyr::select(ds, y) |>
  as_tsibble(index = ds)
print(asthma_ts, n=15)

# Split into train and test data
asthma_train <- asthma_ts |>
  filter(ds<ymd("2022-01-01"))
asthma_test <- asthma_ts |>
  filter(ds>=ymd("2022-01-01"))

# Load in functions to do rolling predictions
source("R Code/Prophet Functions/prophet_fit.R")
source("R Code/Prophet Functions/roll_prophet.R")

# Test the prediction function
test <- prophet_fit(ts_data = asthma_train, lookahead = 24)
print(test, n=7)

# Forecast admissions for 2022 and 2023 through rolling prediction
future_ts <- roll_prophet(ts_data = asthma_ts, start_date = "2022-01-01", iter = nrow(asthma_test),
                          timeframe = 12*6, ci.level = 0.90) |>
  mutate(ds=yearmonth(ds)+1)
print(future_ts, n=24)

# Merge with observed admissions
prophet_predict <- inner_join(asthma_ts, future_ts, by="ds") |>
  mutate(ds=yearmonth(ds)) |>
  select(ds, y, Method, Predict, Lower, Upper)
prophet_predict

# Save the predictions
dir <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Monthly Predictions/"
# saveRDS(prophet_predict, paste(dir, "monthly_prophet_predict.rds", sep=""))

# Read back in rolling predictions
prophet_predict <- readRDS(paste(dir, "monthly_prophet_predict.rds", sep="")) |>
  tsibble(index=ds)


# 4. Compilation ----------------------------------------------------------

# Directory where predictions are saved
dir <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Monthly Predictions/"

# Predictions from ARIMA model
arima_predict <- readRDS(paste(dir, "monthly_arima_predict.rds", sep="")) |>
  tsibble(index=ds) |>
  fill_gaps()

# Predictions from ETS model
ets_predict <- readRDS(paste(dir, "monthly_ets_predict.rds", sep="")) |>
  tsibble(index=ds) |>
  fill_gaps()

# Predictions from Prophet model
prophet_predict <- readRDS(paste(dir, "monthly_prophet_predict.rds", sep="")) |>
  tsibble(index=ds) |>
  fill_gaps()

# Merge into one dataframe
asthma_predict <- bind_rows(as_tibble(arima_predict), as_tibble(ets_predict), as_tibble(prophet_predict))
asthma_predict


# 5. Analysis -------------------------------------------------------------

# Plot the time series predictions with actual observations
plot_predict <- asthma_predict |>
  mutate(ds=as_date(ds))|>
  ggplot(aes(x=ds))+
  facet_grid(Method~.)+
  geom_ribbon(aes(ymin=Lower, ymax=Upper, group=Method, fill=Method), alpha=0.3)+
  geom_line(aes(y=y), lwd=0.5, lty="solid", color="grey10", alpha=0.6)+
  geom_line(aes(y=Predict, color=Method), lwd=0.6, lty="solid")+
  scale_color_manual(values=c("red3","gold3","blue3"))+
  scale_fill_manual(values=c("red","gold","blue"))+
  scale_x_date(name="Date", date_breaks = "1 months", date_labels = "%b %Y",
               limits = c(as.Date("2022-01-01"), as.Date("2023-12-31")),
               expand = c(0,0))+
  scale_y_continuous(name="Hospitalizations", breaks = seq(-20,300,by=20))+
  theme_bw()+
  theme(panel.grid.minor.y = element_blank(),
        panel.border = element_rect(fill = "transparent", color="black", linewidth = 0.7),
        panel.background = element_rect(fill = "white"),
        panel.spacing.x = unit(0, 'points'),
        strip.background.x = element_rect(fill="grey30", color="black", linewidth=1),
        strip.background.y = element_rect(fill="black", color="black", linewidth=1),
        strip.text.x = element_text(size=11, color = "white", face="bold"),
        strip.text.y = element_text(size=12, color= "white", face="bold"),
        axis.text.x = element_text(size=10, angle=45, colour="black",
                                   vjust=1, hjust=1),
        plot.title = element_text(size=16, color="black"),
        legend.position = "none")+
  ggtitle("Monthly Asthma Hospitalization Forecast",
          subtitle = "Observed with Prediction + 90% Confidence Interval")
plot_predict


### Absolute Error

# Calculate absolute error
asthma_error <- asthma_predict |>
  mutate(Error=abs(y - Predict))
asthma_error

# 5-number summary and IQR of absolute error
error_summary <- asthma_error |>
  group_by(Method) |>
  summarise(Min=min(Error), Q1=quantile(Error, p=0.25, names=F), Median=median(Error),
            Q3=quantile(Error, p=0.75, names=F), Max=max(Error), IQR=Q3-Q1)
error_summary

### Median Absolute Percentage Error

# Calculate absolute percentage error
asthma_pcterror <- asthma_predict |>
  mutate(PctError=abs(y - Predict)/Predict)
asthma_pcterror

# 5-number summary and IQR of median absolute percentage error
pcterror_summary <- asthma_pcterror |>
  group_by(Method) |>
  summarise(Min=min(PctError), Q1=quantile(PctError, p=0.25, names=F), Median=median(PctError),
            Q3=quantile(PctError, p=0.75, names=F), Max=max(PctError), IQR=Q3-Q1)
pcterror_summary

### Mean Squared Error

# Calculate squared error
asthma_sqerror <- asthma_predict |>
  mutate(SqError=(y - Predict)^2)
asthma_sqerror

# 5-number summary and IQR of squared error
sqerror_summary <- asthma_sqerror |>
  group_by(Method) |>
  summarise(Min=min(SqError), Q1=quantile(SqError, p=0.25, names=F), Median=median(SqError),
            Q3=quantile(SqError, p=0.75, names=F), Max=max(SqError), IQR=Q3-Q1)
sqerror_summary

# Get mean squared error
mse_summary <- asthma_sqerror |>
  group_by(Method) |>
  summarise(MSE=sum(SqError))
mse_summary

### Coverage

# Determine if 90% confidence interval captures true observation
asthma_cover <- asthma_predict |>
  mutate(Cover=ifelse(y>Lower & y<=Upper, "COVER",
                      ifelse(y<Lower, "TOO_HIGH",
                             ifelse(y>Upper, "TOO_LOW",NA))))
asthma_cover

# Coverage scorecard
cover_summary <- asthma_cover |>
  group_by(Method) |>
  mutate(Cover=factor(Cover)) |>
  count(Cover, .drop = FALSE) |>
  mutate(Prop=n/sum(n))
print(cover_summary, n=63)


# 6. Thresholds -----------------------------------------------------------

# Set risk threshold
p <- 0.05

# Get number of admissions that meet or exceed the risk threshold
arima.top <- asthma_predict |>
  filter(Method=="ARIMA") |>
  slice_max(order_by=Predict, prop=p)
ets.top <- asthma_predict |>
  filter(Method=="ETS") |>
  slice_max(order_by=Predict, prop=p)
prophet.top <- asthma_predict |>
  filter(Method=="Prophet") |>
  slice_max(order_by=Predict, prop=p)
actual.top <- asthma_predict |>
  filter(Method=="ARIMA") |>
  slice_max(order_by=y, prop=p)

# Set benchmark to classify HIGH admission days
arima.thres <- min(arima.top$Predict, na.rm=TRUE)
ets.thres <- min(ets.top$Predict, na.rm=TRUE)
prophet.thres <- min(prophet.top$Predict, na.rm=TRUE)
actual.thres <- min(actual.top$y, na.rm=TRUE)

arima.thres
ets.thres
prophet.thres
actual.thres

# Classify whether the day was HIGH or NORMAL
asthma_status <- asthma_predict |>
  mutate(arima.high = ifelse(Method=="ARIMA" & Predict > arima.thres, 1, 0),
         ets.high = ifelse(Method=="ETS" & Predict > ets.thres, 1, 0),
         prophet.high = ifelse(Method=="Prophet" & Predict > prophet.thres, 1, 0),
         actual.high = ifelse(y > actual.thres, 1, 0)) |>
  select(ds, y, actual.high, Method, Predict, arima.high, ets.high, prophet.high)
asthma_status

### Proportion of weeks where actual hospitalizations were HIGH
# Actual
asthma_status |>
  filter(Method=="ARIMA") |>
  count(actual.high) |>
  mutate(pct=n/sum(n))

# ARIMA
asthma_status |>
  count(arima.high, actual.high) |>
  arrange(actual.high)

# ETS
asthma_status |>
  count(ets.high, actual.high) |>
  arrange(actual.high)

# Prophet
asthma_status |>
  count(prophet.high, actual.high) |>
  arrange(actual.high)

# Compare outcomes
asthma_status |>
  count(arima.high, ets.high, prophet.high, actual.high) |>
  mutate(pct=n/sum(n)) |>
  relocate(actual.high) |>
  arrange(desc(n))

# Calculate misclassifications
asthma.c <- asthma_status %>%
  mutate(arima.c=ifelse(actual.high==arima.high, "CORRECT", "INCORRECT"),
         ets.c=ifelse(actual.high==ets.high, "CORRECT", "INCORRECT"),
         prophet.c=ifelse(actual.high==prophet.high, "CORRECT", "INCORRECT"))
asthma.c %>%
  count(arima.c, ets.c, prophet.c)

#### Misclassification rates
# ARIMA
asthma.c %>%
  count(arima.c) %>%
  mutate(pct=n/sum(n))

# ETS
asthma.c %>%
  count(ets.c) %>%
  mutate(pct=n/sum(n))

# Prophet
asthma.c %>%
  count(prophet.c) %>%
  mutate(pct=n/sum(n))

### PPV
# ARIMA
true.pos <- sum(asthma_status$arima.high==1 & asthma_status$actual.high==1)
pos <- sum(asthma_status$arima.high==1)
arima.ppv <- true.pos/pos

# ETS
true.pos <- sum(asthma_status$ets.high==1 & asthma_status$actual.high==1)
pos <- sum(asthma_status$ets.high==1)
ets.ppv <- true.pos/pos

# Prophet
true.pos <- sum(asthma_status$prophet.high==1 & asthma_status$actual.high==1)
pos <- sum(asthma_status$prophet.high==1)
prophet.ppv <- true.pos/pos

c(ARIMA=arima.ppv, ETS=ets.ppv, Prophet=prophet.ppv)

### NPV
# ARIMA
true.neg <- sum(asthma_status$arima.high==0 & asthma_status$actual.high==0)
neg <- sum(asthma_status$arima.high==0)
arima.npv <- true.neg/neg

# ETS
true.neg <- sum(asthma_status$ets.high==0 & asthma_status$actual.high==0)
neg <- sum(asthma_status$ets.high==0)
ets.npv <- true.neg/neg

# Prophet
true.neg <- sum(asthma_status$prophet.high==0 & asthma_status$actual.high==0)
neg <- sum(asthma_status$prophet.high==0)
prophet.npv <- true.neg/neg

c(ARIMA=arima.npv, ETS=ets.npv, Prophet=prophet.npv)


# 7. ROC Curve Analysis ---------------------------------------------------

### ARIMA

# Fit ROC curve
arima.roc <- asthma_status |>
  filter(Method=="ARIMA") |>
  roc(response=actual.high, predictor=Predict)

# Pull sensitivity and specificity for different cutoffs
arima_roc.stat <- data.frame(Sensitivity=arima.roc$sensitivities,
                             Specificity=arima.roc$specificities,
                             Cutoffs=arima.roc$thresholds) %>%
  mutate(DistSquared=(Sensitivity-1)^2+(Specificity-1)^2) %>%
  tibble()

# Which row has the smallest distance?
arima_roc.stat %>%
  slice_min(DistSquared)

# Plot the ROC curve for the 3-months prediction
plot.roc(arima.roc, col="red", lwd=2.5, print.thres=TRUE,
         identity=TRUE, identity.lwd=1.5,
         identity.lty="dashed", identity.col="black",
         print.auc=TRUE, auc.polygon = TRUE, 
         main="ROC from ARIMA(0,1,2)(1,0,2)[7] (HIGH=5)")

### ETS

# Fit ROC curve
ets.roc <- asthma_status |>
  filter(Method=="ETS") |>
  roc(response=actual.high, predictor=Predict)

# Pull sensitivity and specificity for different cutoffs
ets_roc.stat <- data.frame(Sensitivity=ets.roc$sensitivities,
                           Specificity=ets.roc$specificities,
                           Cutoffs=ets.roc$thresholds) %>%
  mutate(DistSquared=(Sensitivity-1)^2+(Specificity-1)^2) %>%
  tibble()

# Which row has the smallest distance?
ets_roc.stat %>%
  slice_min(DistSquared)

# Plot the ROC curve for the 3-months prediction
plot.roc(ets.roc, col="red", lwd=2.5, print.thres=TRUE,
         identity=TRUE, identity.lwd=1.5,
         identity.lty="dashed", identity.col="black",
         print.auc=TRUE, auc.polygon = TRUE, 
         main="ROC from ETS(A,N,A) (HIGH=5)")

### Prophet
# Fit ROC curve
prophet.roc <- asthma_status |>
  filter(Method=="Prophet") |>
  roc(response=actual.high, predictor=Predict)

# Pull sensitivity and specificity for different cutoffs
prophet_roc.stat <- data.frame(Sensitivity=prophet.roc$sensitivities,
                             Specificity=prophet.roc$specificities,
                             Cutoffs=prophet.roc$thresholds) %>%
  mutate(DistSquared=(Sensitivity-1)^2+(Specificity-1)^2) %>%
  tibble()

# Which row has the smallest distance?
prophet_roc.stat %>%
  slice_min(DistSquared)

# Plot the ROC curve for the 3-months prediction
plot.roc(prophet.roc, col="red", lwd=2.5, print.thres=TRUE,
         identity=TRUE, identity.lwd=1.5,
         identity.lty="dashed", identity.col="black",
         print.auc=TRUE, auc.polygon = TRUE, 
         main="ROC from Prophet (HIGH=5)")


### Print AUC and 95% CI and SE
# ARIMA
arima.roc$auc
ci.auc(arima.roc)

# ETS
ets.roc$auc
ci.auc(ets.roc)

# Prophet
prophet.roc$auc
ci.auc(prophet.roc)
