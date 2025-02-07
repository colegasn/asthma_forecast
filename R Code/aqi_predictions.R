##### AQI ANALYSIS ON ASTHMA FORECASTING MODELS #####
### Last Update: 2/6/2025

# Load packages
library(readxl)
library(prophet)
library(dplyr)
library(tibble)
library(tsibble)
library(lubridate)
library(fable)
library(ggplot2)
library(ggpubr)
library(feasts)
library(tidyr)


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

# Read in AQI and categorize their values
aqi <- readRDS("C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/data-raw/daily_aqi.RDS") |>
  rename(Day=date, AQI=aqi) |>
  mutate(Health=ifelse(AQI<=50, "Good",
                       ifelse(AQI>50 & AQI<=100, "Moderate",
                              ifelse(AQI>100 & AQI<=150, "Sensitive",
                                     ifelse(AQI>150 & AQI<=200, "Unhealthy",
                                            ifelse(AQI>200 & AQI<=300, "Very Unhealthy",
                                                   ifelse(AQI>300, "Hazardous", NA))))))
  ) |>
  mutate(Health=factor(Health, levels = c("Good", "Moderate", "Sensitive", "Unhealthy",
                                          "Very Unhealthy", "Hazardous")))

# Join AQI to asthma admissions
asthma <- inner_join(asthma, aqi, by="Day")

# Convert asthma admissions to time series
asthma_ts <- asthma |>
  dplyr::select(Day, AllAdmissions, AQI, Health) |>
  mutate(Day=ymd(Day)) |>
  rename(ds=Day, y=AQI) |>
  as_tsibble(index=ds)
asthma_ts


# 1. AQI Summary ----------------------------------------------------------

# Convert to tsibble object
aqi <- as_tsibble(aqi, index=Day)

# 5-number summary
summary(aqi$AQI)

# Plot AQI
aqi_plot <- ggplot(aqi, aes(x=Day, y=AQI))+
  geom_line(color="purple", lwd=0.4)+
  scale_x_date(name="Date", date_breaks = "6 months", date_labels = "%b %Y",
               limits = c(as.Date("2016-01-01"), as.Date("2023-12-31")),
               expand = c(0,0))+
  scale_y_continuous(name="AQI", breaks = seq(0,200,by=20))+
  theme_bw()+
  theme(axis.text.x = element_text(size=10, angle=45, colour="black",
                                   vjust=1, hjust=1),
        axis.title = element_text(size=14),
        plot.title = element_text(size=16),
        plot.subtitle = element_text(size=12))+
  ggtitle("Daily Asthma Hospitalizations",
          subtitle = "January 1, 2016 - December 31, 2023")
aqi_plot

# AQI by category
aqi_n <- aqi |>
  count(Health, name = "Count", .drop=FALSE) |>
  mutate(Prop=Count/sum(Count))
aqi_n

# Category by year
aqi_yr <- aqi |>
  mutate(Year=factor(year(Day))) |>
  group_by(Year) |>
  count(Health, name = "Count", .drop=FALSE) |>
  mutate(Prop=Count/sum(Count))
print(aqi_yr, n=54)

# Barplot of AQI
ggplot(aqi_n, aes(x=Health, y=Count))+
  geom_bar(stat="identity", width = 0.7, color="black", fill="purple")+
  geom_text(aes(label=Count), vjust=-0.4, size=6)+
  theme_bw()+
  scale_x_discrete(name="Air Quality Index (AQI)")+
  scale_y_continuous(limits = c(0,2500))+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size=16),
        axis.text.x = element_text(size=16),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=14))+
  ggtitle("Daily AQI for Hamilton County, Ohio",
          subtitle = "January 1, 2016 - December 31, 2023")

# Calendar heat map of AQI
cal <- asthma |>
  group_by(MonthDate) |>
  mutate(WEEK_MONTH=(5+day(Day) + wday(floor_date(Day, 'month'))) %/% 7,
         WEEK_NAME=factor(WEEK_NAME, labels=c("Su","M","Tu","W","Th","F","Sa")),
         MONTH_NAME=factor(MONTH_NAME,
                           levels=c("January","February","March",
                                    "April","May","June","July",
                                    "August","September","October",
                                    "November","December"))) |>
  ggplot(aes(x=WEEK_NAME, y=WEEK_MONTH, fill=Health))+
  geom_tile(color="black")+
  facet_grid(year(Day) ~ MONTH_NAME)+
  scale_y_reverse()+
  scale_fill_manual(values=c("grey95","grey70","grey50","grey25","grey10"),
                    labels=c("Good","Moderate","Sensitive","Unhealthy"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = "transparent", color="black", linewidth = 0.7),
        panel.background = element_rect(fill = "white"),
        panel.spacing.x = unit(0, 'points'),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background.x = element_rect(fill="grey30", color="black", linewidth=1),
        strip.background.y = element_rect(fill="black", color="black", linewidth=1),
        strip.text.x = element_text(size=11, color = "white", face="bold"),
        strip.text.y = element_text(size=12, color= "white", face="bold"),
        plot.title = element_text(size=16, color="black"),
        legend.position = "bottom")+
  xlab("")+ylab("")+
  labs(title="Daily AQI",
       subtitle="January 1, 2016 - December 31, 2023",
       fill="AQI")
cal


# 2. ARIMA ----------------------------------------------------------------

# Split into train and test data
asthma_train <- asthma_ts |>
  filter(ds<"2022-01-01") |>
  select(ds, y)
asthma_test <- asthma_ts |>
  filter(ds>="2022-01-01") |>
  select(ds, y)

# 2.1 ARIMA Model Formulation ---------------------------------------------

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
  model(arima103001=ARIMA(y ~ 1+pdq(1,0,3)+PDQ(0,0,1)),
        arima102001=ARIMA(y ~ 1+pdq(1,0,2)+PDQ(0,0,1)),
        arima101001=ARIMA(y ~ 1+pdq(1,0,1)+PDQ(0,0,1)),
        arima200001=ARIMA(y ~ 1+pdq(2,0,0)+PDQ(0,0,1)),
        search=ARIMA(y, stepwise = FALSE))
asthma_mdl
glance(asthma_mdl)

# Examine residuals
asthma_mdl |>
  select(arima103001) |>
  gg_tsresiduals()

# Test if residuals follows a white noise process
augment(asthma_mdl) |>
  filter(.model == "arima103001") |>
  features(.innov, ljung_box, lag = 36, dof = 5)

# Report the values of the best ARIMA model
report(asthma_mdl |> select(arima103001))


# 2.2 ARIMA Prediction ----------------------------------------------------

# Load in functions to do rolling predictions
source("R Code/ARIMA Functions/arima_fit.R")
source("R Code/ARIMA Functions/roll_arima.R")

# Test the prediction function
test <- arima_fit(ts_data = asthma_train, lookahead = 7, p=1, d=0, q=3, P=0, D=0, Q=1)
print(test, n=7)

# Test the rolling prediction function
test2 <- roll_arima(ts_data = asthma_ts, start_date = "2022-01-01", iter = 7,
                    p=1, d=0, q=3, P=0, D=0, Q=1, ci.level = 0.90)
print(test2, n=7)

#####

# Forecast AQI for 2022-2023 through rolling prediction
future_ts <- roll_arima(ts_data = asthma_ts, start_date = "2022-01-01", iter = nrow(asthma_test),
                        p=1, d=0, q=3, P=0, D=0, Q=1, ci.level = 0.90)

# Merge with observed admissions
arima_aqi <- inner_join(asthma_ts, future_ts, by="ds") |>
  rename(AQI=y) |>
  select(ds, AQI, Method, Predict, Lower, Upper)
arima_aqi

# Save the predictions
dir <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Predictions/AQI Predictions/"
# saveRDS(arima_aqi, paste(dir, "arima_aqi.rds", sep=""))

# Read back in rolling predictions
arima_aqi <- readRDS(paste(dir, "arima_aqi.rds", sep="")) |>
  tsibble(index=ds)


# 3. ETS ------------------------------------------------------------------
# Split into train and test data
asthma_train <- asthma_ts |>
  filter(ds<"2022-01-01") |>
  select(ds, y)
asthma_test <- asthma_ts |>
  filter(ds>="2022-01-01") |>
  select(ds, y)


# 3.1 ETS Model Formulation -----------------------------------------------

# Fit ETS models
asthma_mdl <- asthma_train |>
  mutate(y=replace_na(y, 0)) |>
  model(SES = ETS(y ~ error("A")+trend("N")+season("N")),
        Holt = ETS(y ~ error("A")+trend("A")+season("N")),
        Damped = ETS(y ~ error("A")+trend("Ad")+season("N")),
        ANA = ETS(y ~ error("A")+trend("N")+season("A")),
        search = ETS(y))
asthma_mdl
glance(asthma_mdl)

# Test accuracy of the ETS models
asthma_mdl |>
  forecast(h = nrow(asthma_test)) |>
  accuracy(asthma_test)

# Estimated parameters of MAN model
man_mdl <- asthma_train |>
  mutate(y=replace_na(y, 0)) |>
  model(ana = ETS(y ~ error("M")+trend("A")+season("N")))
report(man_mdl)


# 3.2 ETS Prediction ------------------------------------------------------

# Load in functions to do rolling predictions
source("R Code/ETS Functions/ets_fit.R")
source("R Code/ETS Functions/roll_ets.R")

# Test the prediction function
test <- ets_fit(ts_data = asthma_train, lookahead = 7, Error = "M", Trend = "A", Season = "N")
print(test, n=7)

# Test the rolling prediction function 
test2 <- roll_ets(ts_data = asthma_ts, start_date = "2022-01-01", iter = 7, ci.level = 0.90)
print(test, n=7)

#####

# Forecast admissions for 2022 and 2023 through rolling prediction
future_ts <- roll_ets(ts_data = asthma_ts, start_date = "2022-01-01", iter = nrow(asthma_test), ci.level = 0.90)

# Merge with observed admissions
ets_aqi <- inner_join(asthma_ts, future_ts, by="ds") |>
  rename(AQI=y) |>
  select(ds, AQI, Method, Predict, Lower, Upper)
ets_aqi

# Save the predictions
dir <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Predictions/AQI Predictions/"
# saveRDS(ets_aqi, paste(dir, "ets_aqi.rds", sep=""))

# Read back in rolling predictions
ets_aqi <- readRDS(paste(dir, "ets_aqi.rds", sep="")) |>
  tsibble(index=ds)


# 4. Prophet --------------------------------------------------------------
# Split into train and test data
asthma_train <- asthma_ts |>
  filter(ds<"2022-01-01") |>
  select(ds, y)
asthma_test <- asthma_ts |>
  filter(ds>="2022-01-01") |>
  select(ds, y)

# Load in functions to do rolling predictions
source("R Code/Prophet Functions/prophet_fit.R")
source("R Code/Prophet Functions/roll_prophet.R")

# Test the prediction function
test <- prophet_fit(ts_data = asthma_train, lookahead = 7)
print(test, n=7)

# Test the rolling prediction function
test2 <- roll_prophet(ts_data = asthma_ts, start_date = "2022-01-01", iter = 7, ci.level = 0.90)
print(test2, n=7)

#####

# Forecast admissions for 2022 and 2023 through rolling prediction
future_ts <- roll_prophet(ts_data = asthma_ts, start_date = "2022-01-01", iter = nrow(asthma_test), ci.level = 0.90)

# Merge with observed admissions
prophet_aqi <- inner_join(asthma_ts, future_ts, by="ds") |>
  rename(AQI=y) |>
  select(ds, AQI, Method, Predict, Lower, Upper)
prophet_aqi

# Save the predictions
dir <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Predictions/AQI Predictions/"
# saveRDS(prophet_aqi, paste(dir, "prophet_aqi.rds", sep=""))

# Read back in rolling predictions
prophet_aqi <- readRDS(paste(dir, "prophet_aqi.rds", sep="")) |>
  tsibble(index=ds)


# 5. Ensemble -------------------------------------------------------------

# Directory where predictions are saved
dir <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Predictions/AQI Predictions/"

# Predictions from ARIMA model
arima_aqi <- readRDS(paste(dir, "arima_aqi.rds", sep="")) |>
  tsibble(index=ds) |>
  fill_gaps()

# Predictions from ETS model
ets_aqi <- readRDS(paste(dir, "ets_aqi.rds", sep="")) |>
  tsibble(index=ds) |>
  fill_gaps()

# Predictions from Prophet model
prophet_aqi <- readRDS(paste(dir, "prophet_aqi.rds", sep="")) |>
  tsibble(index=ds) |>
  fill_gaps()

# Merge into one dataframe
merge_aqi <- bind_rows(as_tibble(arima_aqi), as_tibble(ets_aqi), as_tibble(prophet_aqi))
merge_aqi

# Ensemble model - average prediction of the three model classes
ensemble_aqi <- merge_aqi |>
  group_by(ds) |>
  summarise(Predict_mean=mean(Predict),
            Lower_mean=mean(Lower),
            Upper_mean=mean(Upper)) |>
  mutate(Method="Ensemble") |>
  rename(Predict=Predict_mean, Lower=Lower_mean, Upper=Upper_mean) |>
  inner_join(merge_aqi |> filter(Method=="ARIMA") |> dplyr::select(ds, AQI), by="ds") |>
  relocate(ds, AQI, Method, Predict, Lower, Upper)
ensemble_aqi

# Merge into one dataframe
aqi_predict <- bind_rows(as_tibble(merge_aqi), as_tibble(ensemble_aqi))
aqi_predict

# Save the predictions
dir <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Predictions/AQI Predictions/"
# saveRDS(aqi_predict, paste(dir, "aqi_predict.rds", sep=""))


# 6. Compilation ----------------------------------------------------------
# AQI Predictions
# Directory where predictions are saved
dir <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Predictions/AQI Predictions/"

# Read in the daily rolling predictions
aqi_predict <- readRDS(paste(dir, "aqi_predict.rds", sep=""))
aqi_predict

#####

# Asthma Predictions
# Directory where predictions are saved
dir <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Predictions/Daily Predictions/"

# Read in the daily rolling predictions
asthma_predict <- readRDS(paste(dir, "daily_predict.rds", sep="")) |>
  mutate(Residual=y-Predict)
asthma_predict


# 7. Residuals of Asthma Admissions ---------------------------------------

# Plot the time series predictions with actual observations
plot_predict <- aqi_predict |>
  mutate(Method=factor(Method, levels = c("ARIMA","ETS","Prophet","Ensemble"))) |>
  ggplot(aes(x=ds))+
  facet_grid(Method~.)+
  geom_ribbon(aes(ymin=Lower, ymax=Upper, group=Method, fill=Method), alpha=0.3)+
  geom_line(aes(y=AQI), lwd=0.5, lty="solid", color="grey10", alpha=0.6)+
  geom_line(aes(y=Predict, color=Method), lwd=0.6, lty="solid")+
  scale_color_manual(values=c("red3","green3","blue3","gold3"))+
  scale_fill_manual(values=c("red","green","blue","gold"))+
  scale_x_date(name="Date", date_breaks = "1 months", date_labels = "%b %Y",
               limits = c(as.Date("2022-01-01"), as.Date("2023-12-31")),
               expand = c(0,0))+
  scale_y_continuous(name="AQI", breaks = seq(0,200,by=40))+
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
  ggtitle("Daily AQI Forecast",
          subtitle = "Observed with Prediction + 90% Confidence Interval")
plot_predict

# Save plot
savepath <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Plots/"
ggsave(plot_predict, filename=paste(savepath, "aqi_time_series.pdf", sep=""), device=cairo_pdf,
       width=14, height=8, units="in")

#####

# Residual plot of future asthma admissions
residual.plot <- asthma_predict |>
  mutate(Method=factor(Method, levels = c("ARIMA","ETS","Prophet","Ensemble"))) |>
  ggplot(aes(x=ds, y=Residual))+
  facet_grid(~Method)+
  geom_line(color="black", lwd=0.4)+
  geom_hline(yintercept=0, linetype="solid", color="red", linewidth=1)+
  scale_x_date(name="Date", date_breaks = "6 months", date_labels = "%b %Y",
               limits = c(as.Date("2022-01-01"), as.Date("2023-12-31")),
               expand = c(0,0))+
  scale_y_continuous(name="Residual", breaks = seq(-10,10,by=2))+
  theme_bw()+
  theme(axis.text.x = element_text(size=10, angle=45, colour="black",
                                   vjust=1, hjust=1),
        axis.title = element_text(size=14),
        plot.title = element_text(size=16),
        strip.text = element_text(color="white", size=14, face="bold"),
        strip.background = element_rect(fill="black"),
        plot.subtitle = element_text(size=12))+
  ggtitle("Asthma Prediction Residuals",
          subtitle = "January 1, 2022 - December 31, 2023")
residual.plot

# Save plot
savepath <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Plots/"
ggsave(residual.plot, filename=paste(savepath, "residual_plot.pdf", sep=""), device=cairo_pdf,
       width=14, height=8, units="in")


# 7. Residuals of Admissions and AQI Predictions --------------------------

# Join asthma admission predictions with AQI observations
asthma_aqi <- inner_join(asthma_predict |>
                           rename(Predict_y=Predict, Lower_y=Lower, Upper_y=Upper), 
                         aqi_predict |>
                           rename(Predict_AQI=Predict, Lower_AQI=Lower, Upper_AQI=Upper),
                         by=join_by(ds,Method)) |>
  relocate(ds, Method)
asthma_aqi

# Plot residuals of asthma admissions against AQI
residual_predict.plot <- asthma_aqi |>
  mutate(Method=factor(Method, levels = c("ARIMA","ETS","Prophet","Ensemble"))) |>
  ggplot(aes(x=AQI, y=Residual))+
  facet_grid(~Method)+
  geom_point(color="black", size=1.1)+
  geom_hline(yintercept=0, linetype="solid", color="purple", linewidth=1)+
  scale_x_continuous(name="AQI", breaks = seq(0,200,by=20))+
  scale_y_continuous(name="Residual", breaks = seq(-10,10,by=2))+
  theme_bw()+
  theme(axis.text.x = element_text(size=10, angle=45, colour="black",
                                   vjust=1, hjust=1),
        axis.title = element_text(size=14),
        plot.title = element_text(size=16),
        strip.text = element_text(color="white", size=14, face="bold"),
        strip.background = element_rect(fill="black"),
        plot.subtitle = element_text(size=12))+
  ggtitle("Residuals versus AQI",
          subtitle = "January 1, 2022 - December 31, 2023")
residual_predict.plot

# Save plot
savepath <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Plots/"
ggsave(residual.plot, filename=paste(savepath, "residual_AQI.pdf", sep=""), device=cairo_pdf,
       width=14, height=8, units="in")

#####

# Correlation test between predicted AQI and residuals of asthma admissions
mtd <- c("ARIMA","ETS","Prophet","Ensemble")
i <- 1
while(i<=length(mtd)){
  # Filter predictions by model class
  asthma_aqi.mtd <- asthma_aqi |>
    filter(Method==mtd[i])
  
  # Perform correlation test
  cor_test <- cor.test(asthma_aqi.mtd$AQI, asthma_aqi.mtd$Residual, method = "kendall")
  
  # Save results from the correlation test
  if(i==1){
    cor_result <- tibble(Method=mtd[i], tau=cor_test$estimate,
                             Z=cor_test$statistic, p_value=cor_test$p.value)
  } else{
    cor_result <- bind_rows(cor_result,
                            tibble(Method=mtd[i], tau=cor_test$estimate,
                                       Z=cor_test$statistic, p_value=cor_test$p.value))
  }
  
  # Go to next model class
  i <- i+1
}

# Print results from correlation tests
cor_result


# 8. Lagged AQI Exposure --------------------------------------------------

# Create Lag 0, Lag 1, Lag 2, Lag 3, Lag 4, and Lag 5 exposures
lag.aqi <- aqi_predict |>
  group_by(Method) |>
  mutate(lag0_AQI=lag(AQI, n=0),
         lag1_AQI=lag(AQI, n=1),
         lag2_AQI=lag(AQI, n=2),
         lag3_AQI=lag(AQI, n=3),
         lag4_AQI=lag(AQI, n=4),
         lag5_AQI=lag(AQI, n=5)) |>
  select(-AQI, -Predict, -Lower, -Upper) |>
  relocate(ds, Method)
lag.aqi

# Pivot wide to long, adding a Lag column
lag.aqi2 <- lag.aqi |>
  pivot_longer(cols = c(lag0_AQI, lag1_AQI, lag2_AQI, lag3_AQI, lag4_AQI, lag5_AQI),
               names_to = "Lag", values_to = "AQI") |>
  mutate(Lag=recode(Lag, lag0_AQI="0", lag1_AQI="1", lag2_AQI="2", lag3_AQI="3", lag4_AQI="4", lag5_AQI="5"))

# Join asthma admission predictions with lagged AQI observations
asthma_lag.aqi <- right_join(asthma_predict, lag.aqi2, by=join_by(ds, Method))
asthma_lag.aqi

# Plot residuals of asthma admissions against actual AQI observations
residual_predict.plot <- asthma_lag.aqi |>
  mutate(Method=factor(Method, levels = c("ARIMA","ETS","Prophet","Ensemble"))) |>
  mutate(Lag=factor(Lag, labels = c("Lag 0 AQI","Lag 1 AQI","Lag 2 AQI","Lag 3 AQI","Lag 4 AQI","Lag 5 AQI"))) |>
  ggplot(aes(x=AQI, y=Residual))+
  facet_grid(Method~Lag)+
  geom_point(color="black", size=1.1)+
  geom_hline(yintercept=0, linetype="solid", color="purple", linewidth=1)+
  scale_x_continuous(name="AQI", breaks = seq(0,200,by=20))+
  scale_y_continuous(name="Residual", breaks = seq(-10,12,by=2))+
  theme_bw()+
  theme(axis.text.x = element_text(size=10, colour="black"),
        axis.title = element_text(size=14),
        plot.title = element_text(size=16),
        strip.text = element_text(color="white", size=14, face="bold"),
        strip.text.y = element_text(angle = -90),
        strip.background = element_rect(fill="black"),
        plot.subtitle = element_text(size=12))+
  ggtitle("Residuals versus AQI",
          subtitle = "January 1, 2022 - December 31, 2023")
residual_predict.plot

# Save plot
savepath <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Plots/"
ggsave(residual.predict.plot, filename=paste(savepath, "residual_lagAQI.pdf", sep=""), device=cairo_pdf,
       width=14, height=8, units="in")

#####

# Correlation tests on lag AQI and asthma predictions
mtd <- c("ARIMA","ETS","Prophet","Ensemble")
i <- 1
while(i<=length(mtd)){
  l <- 0
  while(l<=5){
    # Filter by model class for a particular lag AQI
    mtd_lag <- asthma_lag.aqi |>
      filter(Method==mtd[i] & Lag==l)
    
    # Calculate correlation test
    cor_test <- cor.test(mtd_lag$AQI, mtd_lag$Residual, method = "kendall")
    
    # Save results from the correlation test
    if(i==1 & l==0){
      lag_cor_result <- tibble(Method=mtd[i], Lag=l, tau=cor_test$estimate,
                               Z=cor_test$statistic, p_value=cor_test$p.value)
    } else{
      lag_cor_result <- bind_rows(lag_cor_result,
                                  tibble(Method=mtd[i], Lag=l, tau=cor_test$estimate,
                                  Z=cor_test$statistic, p_value=cor_test$p.value))
    }
    
    # Go to the next AQI lag
    l <- l+1
  }
  
  # Go to the next model class
  i <- i+1
}

# Print the results
lag_cor_result |>
  arrange(Lag) |>
  print(n=24)

# P-value of correlation results
pvalue_plot <- lag_cor_result |>
  mutate(Method=factor(Method, levels = c("Ensemble","Prophet","ETS","ARIMA"))) |>
  mutate(Sig=factor(ifelse(p_value>0.05, 1, 
                           ifelse(p_value<=0.05 & p_value>0.01, 2,
                                  ifelse(p_value<=0.01 & p_value>0.001, 3,
                                         ifelse(p_value<=0.001 & p_value>0.0001, 4, 5)))))) |>
  ggplot(aes(x=Lag, y=Method, fill=Sig))+
  geom_tile()+
  geom_text(aes(label=sprintf("%.4f", p_value)), color = "white", size=4, fontface="bold")+
  scale_x_continuous(breaks = 0:5)+
  scale_y_discrete(name="Model Class")+
  scale_fill_manual(values=c("grey90","grey50","grey30","grey15","black"))+
  theme_bw()+
  theme(axis.title = element_text(size=12),
        axis.text = element_text(size=12),
        plot.title = element_text(size=16),
        plot.subtitle = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")+
  ggtitle("P-Values of Correlations",
          subtitle = "Lag AQI and Asthma Predictions by Model Class")
pvalue_plot

# Save plot
savepath <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Plots/"
ggsave(pvalue_plot, filename=paste(savepath, "pvalue_plot.pdf", sep=""), device=cairo_pdf,
       width=14, height=8, units="in")

# Heat map of correlations
cor_plot <- lag_cor_result |>
  mutate(Method=factor(Method, levels = c("Ensemble","Prophet","ETS","ARIMA"))) |>
  mutate(Sig=factor(ifelse(p_value>0.05, 1, 
                           ifelse(p_value<=0.05 & p_value>0.01, 2, 3)))) |>
  ggplot(aes(x=Lag, y=Method, fill=Sig))+
  geom_tile()+
  geom_text(aes(label=sprintf("%.4f", tau)), color = "white", size=4, fontface="bold")+
  scale_x_continuous(name="AQI Lag", breaks = 0:5)+
  scale_y_discrete(name="Model Class")+
  scale_fill_manual(name="P-values", values=c("grey70","grey40","black"),
                    labels=c("p > 0.05","p <= 0.05","p <= 0.01"))+
  theme_bw()+
  theme(axis.title = element_text(size=12),
        axis.text = element_text(size=12),
        axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5),
        plot.title = element_text(size=16),
        plot.subtitle = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")+
  ggtitle("Correlation Test of AQI and Prediction Residual",
          subtitle = "By Model Class")
cor_plot

# Save plot
savepath <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Plots/"
ggsave(cor_plot, filename=paste(savepath, "cor_test.pdf", sep=""), device=cairo_pdf,
       width=14, height=8, units="in")
