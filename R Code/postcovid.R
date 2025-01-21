##### Asthma Forecasting Algorithms - Post-Covid Analysis #####
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


# 1. ARIMA ----------------------------------------------------------------

# Split into train and test data
asthma_train <- asthma_ts |>
  filter(ds>="2020-01-01" & ds<="2022-12-31")
asthma_test <- asthma_ts |>
  filter(ds>="2023-01-01")


# 1.1 ARIMA Model Formulation ---------------------------------------------

# Examine time plot, ACF and PACF of original series
asthma_train |>
  gg_tsdisplay(y, plot_type = 'partial')

# Test if seasonal differencing is needed
asthma_train |>
  features(y, unitroot_nsdiffs) # no seasonal difference needed

# Test if differencing is needed
asthma_train |>
  features(y, unitroot_ndiffs) # differencing needed

# Fit ARIMA models
asthma_mdl <- asthma_train |>
  model(arima012002=ARIMA(y ~ 1+pdq(0,1,2)+PDQ(0,0,2)),
        arima013002=ARIMA(y ~ 1+pdq(0,1,3)+PDQ(0,0,2)),
        arima012001=ARIMA(y ~ 1+pdq(0,1,2)+PDQ(0,0,1)),
        arima301001=ARIMA(y ~ 1+pdq(3,0,1)+PDQ(0,0,1)),
        arima112001=ARIMA(y ~ 1+pdq(1,1,2)+PDQ(0,0,1)),
        arima012=ARIMA(y ~ 1+pdq(0,1,2)),
        search=ARIMA(y, stepwise = FALSE))
asthma_mdl
glance(asthma_mdl)

# Examine residuals
asthma_mdl |>
  select(arima012001) |>
  gg_tsresiduals()

# Test if residuals follows a white noise process
augment(asthma_mdl) |>
  filter(.model == "arima012001") |>
  features(.innov, ljung_box, lag = 36, dof = 3)

# Report the values of the best ARIMA model
report(asthma_mdl |> select(arima012001))


# 1.2 ARIMA Prediction ----------------------------------------------------

# Load in functions to do rolling predictions
source("R Code/ARIMA Functions/arima_fit.R")
source("R Code/ARIMA Functions/roll_arima.R")

# Test the prediction function
test <- arima_fit(ts_data = asthma_train, lookahead = 7, p=0, d=1, q=2, P=0, D=0, Q=1)
print(test, n=7)

# Test the rolling prediction function
test2 <- roll_arima(ts_data = asthma_ts, start_date = "2023-01-01", iter = 7,
                    p=0, d=1, q=2, P=0, D=0, Q=1, ci.level = 0.90)
print(test2, n=7)

#####

# Forecast admissions for 2023 through rolling prediction
future_ts <- roll_arima(ts_data = asthma_ts, start_date = "2023-01-01", iter = nrow(asthma_test),
                        p=0, d=1, q=2, P=0, D=0, Q=1, ci.level = 0.90)

# Merge with observed admissions
arima_predict <- inner_join(asthma_ts, future_ts, by="ds") |>
  select(ds, y, Method, Predict, Lower, Upper)
arima_predict

# Save the predictions
dir <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Postcovid Predictions/"
saveRDS(arima_predict, paste(dir, "postcovid_arima_predict.rds", sep=""))

# Read back in rolling predictions
arima_predict <- readRDS(paste(dir, "postcovid_arima_predict.rds", sep="")) |>
  tsibble(index=ds)


# 2. ETS ------------------------------------------------------------------

# Split into train and test data
asthma_train <- asthma_ts |>
  filter(ds>="2020-01-01" & ds<="2022-12-31")
asthma_test <- asthma_ts |>
  filter(ds>="2023-01-01")


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

# Estimated parameters of ANA model
ana_mdl <- asthma_train |>
  mutate(y=replace_na(y, 0)) |>
  model(ana = ETS(y ~ error("A")+trend("N")+season("A")))
report(ana_mdl)


# 2.2 ETS Prediction ------------------------------------------------------

# Load in functions to do rolling predictions
source("R Code/ETS Functions/ets_fit.R")
source("R Code/ETS Functions/roll_ets.R")

# Test the prediction function
test <- ets_fit(ts_data = asthma_train, lookahead = 7, Error = "A", Trend = "N", Season = "A")
print(test, n=7)

# Test the rolling prediction function 
test2 <- roll_ets(ts_data = asthma_ts, start_date = "2023-01-01", iter = 7, ci.level = 0.90)
print(test, n=7)

#####

# Forecast admissions for 2019 through rolling prediction
future_ts <- roll_ets(ts_data = asthma_ts, start_date = "2023-01-01", iter = nrow(asthma_test), ci.level = 0.90)

# Merge with observed admissions
ets_predict <- inner_join(asthma_ts, future_ts, by="ds") |>
  select(ds, y, Method, Predict, Lower, Upper)
ets_predict

# Save the predictions
dir <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Postcovid Predictions/"
# saveRDS(ets_predict, paste(dir, "postcovid_ets_predict.rds", sep=""))

# Read back in rolling predictions
ets_predict <- readRDS(paste(dir, "postcovid_ets_predict.rds", sep="")) |>
  tsibble(index=ds)


# 3. Prophet --------------------------------------------------------------

# Split into train and test data
asthma_train <- asthma_ts |>
  filter(ds>="2020-01-01" & ds<="2022-12-31")
asthma_test <- asthma_ts |>
  filter(ds>="2023-01-01")

# Load in functions to do rolling predictions
source("R Code/Prophet Functions/prophet_fit.R")
source("R Code/Prophet Functions/roll_prophet.R")

# Test the prediction function
test <- prophet_fit(ts_data = asthma_train, lookahead = 7)
print(test, n=7)

# Test the rolling prediction function
test2 <- roll_prophet(ts_data = asthma_ts, start_date = "2023-01-01", iter = 7, ci.level = 0.90)
print(test2, n=7)

#####

# Forecast admissions for 2019 through rolling prediction
future_ts <- roll_prophet(ts_data = asthma_ts, start_date = "2023-01-01", iter = nrow(asthma_test), ci.level = 0.90)

# Merge with observed admissions
prophet_predict <- inner_join(asthma_ts, future_ts, by="ds") |>
  select(ds, y, Method, Predict, Lower, Upper)
prophet_predict

# Save the predictions
dir <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Postcovid Predictions/"
# saveRDS(prophet_predict, paste(dir, "postcovid_prophet_predict.rds", sep=""))

# Read back in rolling predictions
prophet_predict <- readRDS(paste(dir, "postcovid_prophet_predict.rds", sep="")) |>
  tsibble(index=ds)



# 4. Compilation ----------------------------------------------------------

# Directory where predictions are saved
dir <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Postcovid Predictions/"

# Predictions from ARIMA model
arima_predict <- readRDS(paste(dir, "postcovid_arima_predict.rds", sep="")) |>
  tsibble(index=ds) |>
  fill_gaps()

# Predictions from ETS model
ets_predict <- readRDS(paste(dir, "postcovid_ets_predict.rds", sep="")) |>
  tsibble(index=ds) |>
  fill_gaps()

# Predictions from Prophet model
prophet_predict <- readRDS(paste(dir, "postcovid_prophet_predict.rds", sep="")) |>
  tsibble(index=ds) |>
  fill_gaps()

# Merge into one dataframe
asthma_postcovid <- bind_rows(as_tibble(arima_predict), as_tibble(ets_predict), as_tibble(prophet_predict))
asthma_postcovid


# 5. Analysis -------------------------------------------------------------

# Plot the time series predictions with actual observations
plot_predict <- asthma_postcovid |>
  ggplot(aes(x=ds))+
  facet_grid(Method~.)+
  geom_ribbon(aes(ymin=Lower, ymax=Upper, group=Method, fill=Method), alpha=0.3)+
  geom_line(aes(y=y), lwd=0.5, lty="solid", color="grey10", alpha=0.6)+
  geom_line(aes(y=Predict, color=Method), lwd=0.6, lty="solid")+
  scale_color_manual(values=c("red3","gold3","blue3"))+
  scale_fill_manual(values=c("red","gold","blue"))+
  scale_x_date(name="Date", date_breaks = "1 months", date_labels = "%b %Y",
               limits = c(as.Date("2023-01-01"), as.Date("2023-12-31")),
               expand = c(0,0))+
  scale_y_continuous(name="Hospitalizations", breaks = 0:14)+
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
  ggtitle("Daily Asthma Hospitalization Forecast",
          subtitle = "Observed with Prediction + 90% Confidence Interval")
plot_predict


### Absolute Error

# Calculate absolute error
asthma_error <- asthma_postcovid |>
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
asthma_pcterror <- asthma_postcovid |>
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
asthma_sqerror <- asthma_postcovid |>
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
asthma_cover <- asthma_postcovid |>
  mutate(Cover=ifelse(y>Lower & y<=Upper, "COVER",
                      ifelse(y<Lower, "TOO_HIGH",
                             ifelse(y>Upper, "TOO_LOW",NA))))
asthma_cover

# Coverage scorecard
cover_summary <- asthma_cover |>
  group_by(Method) |>
  count(Cover) |>
  mutate(Prop=n/sum(n))
print(cover_summary, n=63)


# 6. Thresholds -----------------------------------------------------------

# Set risk threshold
p <- 0.05

# Get number of admissions that meet or exceed the risk threshold
arima.top <- asthma_postcovid |>
  arrange(desc(arima.yhat_upper)) |>
  top_frac(n=p) |>
  select(ds, y, arima.yhat, arima.yhat_upper)
ets.top <- asthma_postcovid |>
  arrange(desc(ets.yhat_upper)) |>
  top_frac(n=p) |>
  select(ds, y, ets.yhat, ets.yhat_upper)
prophet.top <- asthma_postcovid |>
  arrange(desc(prophet.yhat_upper)) |>
  top_frac(n=p) |>
  select(ds, y, prophet.yhat, prophet.yhat_upper)

# Set benchmark to classify HIGH admission days
arima.thres <- min(arima.top$arima.yhat_upper, na.rm=TRUE)
ets.thres <- min(ets.top$ets.yhat_upper, na.rm=TRUE)
prophet.thres <- min(prophet.top$prophet.yhat_upper, na.rm=TRUE)
actual.thres <- 5

arima.thres
ets.thres
prophet.thres

# Classify whether the day was HIGH or NORMAL
asthma_status <- asthma_postcovid |>
  mutate(arima.status = ifelse(arima.yhat_upper > arima.thres, "HIGH", "NORMAL"),
         ets.status = ifelse(ets.yhat_upper > ets.thres, "HIGH", "NORMAL"),
         prophet.status = ifelse(prophet.yhat_upper > prophet.thres, "HIGH", "NORMAL"),
         actual.status = ifelse(y > actual.thres, "HIGH", "NORMAL")) |>
  select(ds, y, arima.yhat_upper, ets.yhat_upper, prophet.yhat_upper, arima.status, ets.status, prophet.status, actual.status)
asthma_status

### Proportion of days where actual hospitalizations were HIGH
# Actual
asthma_status |>
  count(actual.status) |>
  mutate(pct=n/sum(n))

# ARIMA
asthma_status |>
  count(arima.status, actual.status) |>
  arrange(actual.status)

# ETS
asthma_status |>
  count(ets.status, actual.status) |>
  arrange(actual.status)

# Prophet
asthma_status |>
  count(prophet.status, actual.status) |>
  arrange(actual.status)

# Compare outcomes
asthma_status |>
  count(arima.status, ets.status, prophet.status, actual.status) |>
  mutate(pct=n/sum(n)) |>
  relocate(actual.status) |>
  arrange(desc(n))

# Calculate misclassifications
asthma.c <- asthma_status %>%
  mutate(arima.c=ifelse(actual.status==arima.status, "CORRECT", "INCORRECT"),
         ets.c=ifelse(actual.status==ets.status, "CORRECT", "INCORRECT"),
         prophet.c=ifelse(actual.status==prophet.status, "CORRECT", "INCORRECT"))
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
true.pos <- sum(asthma_status$arima.status=="HIGH" & asthma_status$actual.status=="HIGH")
pos <- sum(asthma_status$arima.status=="HIGH")
arima.ppv <- true.pos/pos

# ETS
true.pos <- sum(asthma_status$ets.status=="HIGH" & asthma_status$actual.status=="HIGH")
pos <- sum(asthma_status$ets.status=="HIGH")
ets.ppv <- true.pos/pos

# Prophet
true.pos <- sum(asthma_status$prophet.status=="HIGH" & asthma_status$actual.status=="HIGH")
pos <- sum(asthma_status$prophet.status=="HIGH")
prophet.ppv <- true.pos/pos

c(ARIMA=arima.ppv, ETS=ets.ppv, Prophet=prophet.ppv)

### NPV
# ARIMA
true.neg <- sum(asthma_status$arima.status=="NORMAL" & asthma_status$actual.status=="NORMAL")
neg <- sum(asthma_status$arima.status=="NORMAL")
arima.npv <- true.neg/neg

# ETS
true.neg <- sum(asthma_status$ets.status=="NORMAL" & asthma_status$actual.status=="NORMAL")
neg <- sum(asthma_status$ets.status=="NORMAL")
ets.npv <- true.neg/neg

# Prophet
true.neg <- sum(asthma_status$prophet.status=="NORMAL" & asthma_status$actual.status=="NORMAL")
neg <- sum(asthma_status$prophet.status=="NORMAL")
prophet.npv <- true.neg/neg

c(ARIMA=arima.npv, ETS=ets.npv, Prophet=prophet.npv)


# 7. ROC Curve Analysis ---------------------------------------------------

### ARIMA

# Fit ROC curve
arima.roc <- roc(response=asthma_status$actual.status,
                 predictor=asthma_status$arima.yhat_upper)
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
ets.roc <- roc(response=asthma_status$actual.status,
               predictor=asthma_status$ets.yhat_upper)
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
prophet.roc <- roc(response=asthma_status$actual.status,
                   predictor=asthma_status$prophet.yhat_upper)
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
