### PLOTS FOR THE ASTHMA FORECASTING PAPER #####
# Last Updated: 2/6/2025

# Load packages
library(ggplot2)
library(tidyverse)
library(feasts)

# Daily rolling predictions
dir <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Predictions/Daily Predictions/"
asthma_predict <- readRDS(paste(dir, "daily_predict.rds", sep=""))
asthma_predict

# Weekly rolling predictions
dir <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Predictions/Weekly Predictions/"
asthma_week <- readRDS(paste(dir, "weekly_predict.rds", sep=""))
asthma_week

# Absolute percent error table
dir <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Predictions/"
asthma_pcterror <- readRDS(file = paste(dir, "pcterror_table.RDS", sep=""))
asthma_pcterror

# ROC table
dir <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Predictions/"
asthma_ROC <- readRDS(file = paste(dir, "ROC_table.RDS", sep=""))
asthma_ROC

# AQI predictions - see 'aqi_predictions.R' for Lag AQI plot and correlation plot
dir <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Predictions/AQI Predictions/"
aqi_predict <- readRDS(paste(dir, "aqi_predict.rds", sep=""))
aqi_predict


# 1. Weekly Time Series Plot ----------------------------------------------

# Plot the time series predictions with actual observations
plot_predict <- asthma_week |>
  mutate(Method=factor(Method, levels = c("ARIMA","ETS","Prophet","Ensemble"))) |>
  ggplot(aes(x=ds))+
  facet_grid(Method~.)+
  geom_ribbon(aes(ymin=Lower, ymax=Upper, group=Method, fill=Method), alpha=0.3)+
  geom_line(aes(y=y), lwd=0.5, lty="solid", color="grey10", alpha=0.6)+
  geom_line(aes(y=Predict, color=Method), lwd=0.6, lty="solid")+
  scale_color_manual(values=c("red3","green3","blue3","gold3"))+
  scale_fill_manual(values=c("red","green","blue", "gold"))+
  scale_x_date(name="Date", date_breaks = "1 months", date_labels = "%b %Y",
               limits = c(as.Date("2022-01-01"), as.Date("2023-12-31")),
               expand = c(0,0))+
  scale_y_continuous(name="Hospitalizations", breaks = seq(-10, 60, by=10))+
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
  ggtitle("Weekly Asthma Hospitalization Forecast",
          subtitle = "Observed and with Prediction + 90% Confidence Interval")
plot_predict

# Save plot
savepath <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Plots/"
ggsave(plot_predict, filename=paste(savepath, "weekly_time_series.pdf", sep=""), device=cairo_pdf,
       width=14, height=8, units="in")


# 2. MAPE Plot ------------------------------------------------------------

mape_plot <- asthma_pcterror |>
  mutate(Median.adj=Median-40) |>
  ggplot(aes(x=Train, y=Median.adj, fill=Method))+
  geom_bar(position = "dodge", stat = "identity")+
  scale_x_discrete(name = "Train History", labels=c("2016-22","2017-22","2018-22","2019-22","2020-22","2021-22","2022"))+
  scale_y_continuous(name = "Median Absolute Percent Error (%)", breaks = seq(0,15,by=5),
                     labels = seq(40,55,by=5), limits = c(0,15))+
  scale_fill_manual(breaks = c("ARIMA","ETS","Prophet","Ensemble"),
                    values=c("grey70","grey80","grey90","black"))+
  theme_bw()+
  theme(axis.text.x = element_text(size=12, angle=0, colour="black",
                                   vjust=0, hjust=0.5),
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=16),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=14),
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  ggtitle("Median Absolute Percent Error Comparison",
          subtitle = "Validated on 2023 Data")
mape_plot

# Save the plot
savepath <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Plots/"
ggsave(mape_plot, filename=paste(savepath, "mape_plot.pdf", sep=""), device=cairo_pdf,
       width=14, height=8, units="in")



# 3. AUC Plot -------------------------------------------------------------

# AUC Plot
AUC_plot <- asthma_ROC |>
  mutate(Method=factor(Method, levels=c("ARIMA","ETS","Prophet","Ensemble")),
         Train=factor(Train, levels=c("2016-2021","2016-2018","2020-2022"),
                      labels = c("Train: 2016-2021 \n Valid: 2022-2023",
                                 "Train: 2016-2018 \n Valid: 2019",
                                 "Train: 2020-2022 \n Valid: 2023")))|>
  ggplot()+ 
  facet_grid(.~Train)+
  geom_pointrange(mapping=aes(x=Method, color=Method, y=100*AUC,
                              ymin=100*AUC_Lower, ymax=100*AUC_Upper),
                  lwd=1.1, size=0.8)+
  scale_color_manual(name="Model Class", values=c("grey40","grey60","grey80","black"))+
  scale_y_continuous(name="AUC (%)", breaks=seq(30,90,by=10), limits = c(30,90))+
  theme_bw()+
  theme(strip.text.x=element_text(face="bold", size=12, color="white"),
        strip.background.x=element_rect(color="black", fill="black"),
        axis.title.x=element_text(color="black", size=16, margin=margin(t=10)),
        axis.text.x=element_text(color="black", size=12),
        axis.title.y=element_text(color="black", size=16),
        axis.text.y=element_text(color="black", size=14),
        plot.title=element_text(color="black",size=18),
        plot.subtitle=element_text(color="black", size=14),
        plot.caption=element_text(color="black", size=12),
        panel.grid.major.x=element_blank(),
        legend.direction = "horizontal", legend.position="bottom",
        legend.text=element_text(color="black", size=12),
        legend.title=element_text(color="black", size=14))+
  guides(fill = guide_legend(nrow = 1))+
  xlab("Train History")+ylab("AUC (%)")+
  labs(title="Area Under the ROC Curve")
AUC_plot

# Save the plot
savepath <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Plots/"
ggsave(AUC_plot, filename=paste(savepath, "auc_plot.pdf", sep=""), device=cairo_pdf,
       width=14, height=8, units="in")


# 4. Sensitivity and Specificity ------------------------------------------

sens_plot <- asthma_ROC |>
  select(Method, Train, Valid, Sensitivity, Specificity) |>
  pivot_longer(cols = c(Sensitivity, Specificity), names_to = "SS") |>
  mutate(Method=factor(Method, levels=c("ARIMA","ETS","Prophet","Ensemble")),
         Train=factor(Train, levels=c("2016-2021","2016-2018","2020-2022"),
                      labels = c("Train: 2016-2021 \n Valid: 2022-2023",
                                 "Train: 2016-2018 \n Valid: 2019",
                                 "Train: 2020-2022 \n Valid: 2023")))|>
  mutate(value.adj=value-0.4)|>
  ggplot(aes(x=Train, y=value.adj, fill=Method))+ 
  facet_grid(.~SS) +
  geom_bar(position = "dodge", stat = "identity")+
  scale_x_discrete(name = "")+
  scale_y_continuous(name = "", breaks = seq(0.00, 0.45, by=0.05),
                     labels = sprintf("%.2f", seq(0.40, 0.85, by=0.05)))+
  scale_fill_manual(breaks = c("ARIMA","ETS","Prophet","Ensemble"),
                    values=c("grey70","grey80","grey90","black"))+
  theme_bw()+
  theme(strip.text.x=element_text(face="bold", size=12, color="white"),
        strip.background.x=element_rect(color="black", fill="black"),
        axis.text.x = element_text(size=8, angle=0, colour="black",
                                   vjust=0, hjust=0.5),
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=16),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=14),
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  ggtitle("Sensitivity and Specificity by Model Class")
sens_plot

# Save the plot
savepath <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Plots/"
ggsave(sens_plot, filename=paste(savepath, "ss_plot.pdf", sep=""), device=cairo_pdf,
       width=14, height=8, units="in")



# 5. Calendar Plot --------------------------------------------------------

# Set actual threshold and model thresholds
act.thres <- 6
mtd.thres <- c(4.18,4.17,4.10,4.13)

# Add indicator whether model missed/hit actual HIGH days
asthma_plot <- asthma_predict |>
  mutate(Thres=ifelse(Method=="ARIMA", mtd.thres[1],
               ifelse(Method=="ETS", mtd.thres[2],
               ifelse(Method=="Prophet", mtd.thres[3],
               ifelse(Method=="Ensemble", mtd.thres[4], NA)))),
    HIGH.mtd=ifelse(Predict>=Thres, 1, 0),
    HIGH.act=ifelse(y>=act.thres, 1, 0),
    HIGH.plot=factor(ifelse(HIGH.mtd==1 & HIGH.act==1, "Correct High",
                     ifelse(HIGH.mtd==0 & HIGH.act==0, "Correct Normal",
                     ifelse(HIGH.mtd==1 & HIGH.act==0, "Missed Prediction",
                     ifelse(HIGH.mtd==0 & HIGH.act==1, "Missed High", NA)))))) |>
  mutate(WEEK_MONTH=(5+day(ds) + wday(floor_date(ds, 'month'))) %/% 7,
         WEEK_NAME=factor(weekdays(ds),
                          levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"),
                          labels=c("Su","M","Tu","W","Th","F","Sa")),
         MONTH_NAME=factor(month(ds, label=TRUE, abbr=FALSE),
                           levels=c("January","February","March",
                                    "April","May","June","July",
                                    "August","September","October",
                                    "November","December")),
         MonthDate=floor_date(ds, unit="month")) |>
  group_by(MonthDate)

# ARIMA
cal_arima <- asthma_plot |>
  filter(Method=="ARIMA") |>
  ggplot(aes(x=WEEK_NAME, y=WEEK_MONTH, fill=HIGH.plot))+
  geom_tile(color="black")+
  facet_grid(year(ds) ~ MONTH_NAME)+
  scale_y_reverse()+
  scale_fill_manual(values = c("green","grey","red","blue"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = "transparent", color="black", linewidth = 0.7),
        panel.background = element_rect(fill = "grey85"),
        panel.spacing.x = unit(0, 'points'),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background.x = element_rect(fill="grey30", color="black", linewidth=1),
        strip.background.y = element_rect(fill="black", color="black", linewidth=1),
        strip.text.x = element_text(size=11, color = "white", face="bold"),
        strip.text.y = element_text(size=12, color= "white", face="bold"),
        plot.title = element_text(size=16, color="black"),
        legend.position = "bottom")+
  guides(fill = guide_legend(nrow = 2))+
  xlab("")+ylab("")+
  labs(fill="Evaluation",
       title = "ARIMA Model Accuracy",
       subtitle = "Asthma Hospitalization Admissions",
       caption = paste("High Actual Threshold:", act.thres, "\n",
                       "High Predicted Threshold:", mtd.thres[1], sep=" "))
cal_arima

# ETS
cal_ets <- asthma_plot |>
  filter(Method=="ETS") |>
  ggplot(aes(x=WEEK_NAME, y=WEEK_MONTH, fill=HIGH.plot))+
  geom_tile(color="black")+
  facet_grid(year(ds) ~ MONTH_NAME)+
  scale_y_reverse()+
  scale_fill_manual(values = c("green","grey","red","blue"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = "transparent", color="black", linewidth = 0.7),
        panel.background = element_rect(fill = "grey85"),
        panel.spacing.x = unit(0, 'points'),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background.x = element_rect(fill="grey30", color="black", linewidth=1),
        strip.background.y = element_rect(fill="black", color="black", linewidth=1),
        strip.text.x = element_text(size=11, color = "white", face="bold"),
        strip.text.y = element_text(size=12, color= "white", face="bold"),
        plot.title = element_text(size=16, color="black"),
        legend.position = "bottom")+
  guides(fill = guide_legend(nrow = 2))+
  xlab("")+ylab("")+
  labs(fill="Evaluation",
       title = "ETS Model Accuracy",
       subtitle = "Asthma Hospitalization Admissions",
       caption = paste("High Actual Threshold:", act.thres, "\n",
                       "High Predicted Threshold:", mtd.thres[2], sep=" "))
cal_ets

# Prophet
cal_prophet <- asthma_plot |>
  filter(Method=="Prophet") |>
  ggplot(aes(x=WEEK_NAME, y=WEEK_MONTH, fill=HIGH.plot))+
  geom_tile(color="black")+
  facet_grid(year(ds) ~ MONTH_NAME)+
  scale_y_reverse()+
  scale_fill_manual(values = c("green","grey","red","blue"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = "transparent", color="black", linewidth = 0.7),
        panel.background = element_rect(fill = "grey85"),
        panel.spacing.x = unit(0, 'points'),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background.x = element_rect(fill="grey30", color="black", linewidth=1),
        strip.background.y = element_rect(fill="black", color="black", linewidth=1),
        strip.text.x = element_text(size=11, color = "white", face="bold"),
        strip.text.y = element_text(size=12, color= "white", face="bold"),
        plot.title = element_text(size=16, color="black"),
        legend.position = "bottom")+
  guides(fill = guide_legend(nrow = 2))+
  xlab("")+ylab("")+
  labs(fill="Evaluation",
       title = "Prophet Model Accuracy",
       subtitle = "Asthma Hospitalization Admissions",
       caption = paste("High Actual Threshold:", act.thres, "\n",
                       "High Predicted Threshold:", mtd.thres[3], sep=" "))
cal_prophet

# Ensemble
cal_ensemble <- asthma_plot |>
  filter(Method=="Ensemble") |>
  ggplot(aes(x=WEEK_NAME, y=WEEK_MONTH, fill=HIGH.plot))+
  geom_tile(color="black")+
  facet_grid(year(ds) ~ MONTH_NAME)+
  scale_y_reverse()+
  scale_fill_manual(values = c("green","grey","red","blue"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = "transparent", color="black", linewidth = 0.7),
        panel.background = element_rect(fill = "grey85"),
        panel.spacing.x = unit(0, 'points'),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background.x = element_rect(fill="grey30", color="black", linewidth=1),
        strip.background.y = element_rect(fill="black", color="black", linewidth=1),
        strip.text.x = element_text(size=11, color = "white", face="bold"),
        strip.text.y = element_text(size=12, color= "white", face="bold"),
        plot.title = element_text(size=16, color="black"),
        legend.position = "bottom")+
  guides(fill = guide_legend(nrow = 2))+
  xlab("")+ylab("")+
  labs(fill="Evaluation",
       title = "Ensemble Model Accuracy",
       subtitle = "Asthma Hospitalization Admissions",
       caption = paste("High Actual Threshold:", act.thres, "\n",
                       "High Predicted Threshold:", mtd.thres[4], sep=" "))
cal_ensemble

# Save the plots
savepath <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Plots/"
ggsave(cal_arima, filename=paste(savepath, "calendar_arima.pdf", sep=""), device=cairo_pdf,
       width=14, height=8, units="in")
ggsave(cal_ets, filename=paste(savepath, "calendar_ets.pdf", sep=""), device=cairo_pdf,
       width=14, height=8, units="in")
ggsave(cal_prophet, filename=paste(savepath, "calendar_prophet.pdf", sep=""), device=cairo_pdf,
       width=14, height=8, units="in")
ggsave(cal_ensemble, filename=paste(savepath, "calendar_ensemble.pdf", sep=""), device=cairo_pdf,
       width=14, height=8, units="in")

# Comparison plot
library(cowplot)
cal_predict <- plot_grid(cal_arima+theme(legend.position = "none",
                                         plot.subtitle = element_blank(),
                                         axis.text.x = element_text(size=5.5),
                                         strip.text.x = element_text(size=7)),
                         cal_ets+theme(legend.position = "none",
                                       plot.subtitle = element_blank(),
                                       axis.text.x = element_text(size=5.5),
                                       strip.text.x = element_text(size=7)),
                         cal_prophet+theme(legend.position = "none",
                                           plot.subtitle = element_blank(),
                                           axis.text.x = element_text(size=5.5),
                                           strip.text.x = element_text(size=7)),
                         cal_ensemble+theme(legend.position = "none",
                                            plot.subtitle = element_blank(),
                                            axis.text.x = element_text(size=5.5),
                                            strip.text.x = element_text(size=7)),
          nrow=2, ncol=2)
cal_predict

# Save plot
savepath <- "C:/Users/wiinu/OneDrive - cchmc/Documents/AHLS/Plots/"
ggsave(cal_predict, filename=paste(savepath, "calendar_predict.pdf", sep=""), device=cairo_pdf,
       width=14, height=8, units="in")
