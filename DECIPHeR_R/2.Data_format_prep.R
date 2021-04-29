#---DECIPHeR model scenario output preparation script---#
#Author: Nicola Ellis
#Created: January 2021
#Last modified: February 2021
#Separating different gauge simulations from original data and model runs.

setwd('D:/DECIPHeR/Outputs/Scenarios/Simulations_30')
library(tidyverse)

#OPTION 1 (raw data script):
#Crop model output to same as simulation
#---CROWFORD---#
date.time <- read.delim('D:/DECIPHeR/Outputs/date.time_ceh.txt', header = TRUE)
precip <- read.delim('D:/DECIPHeR/Outputs/Cf_precip_clip.txt')
cf_qsim <- function(.x, .n) {
  qsim <- read.delim(.x, sep = '', header = TRUE)
  qsim <- qsim %>% select(3)
  CF <- cbind(date.time, qsim)
  CF <- CF[-c(1:4346),]
  CF <- cbind(CF, precip)
  colnames(CF) <- c('datetime', 'q', 'rainfall')
  CF$rainfall <- CF$rainfall * 1000 #convert from m to mm
  CF$q <- (CF$q * 75500000)/3600 #convert from m/hr to m/sec and normalise for catchment size
  write.csv(CF, row.names = FALSE, file = .n)
}

#Qsim
cf_qsim(.x = ('tamar_sim2.flow'), .n = 'CF_qsim2_30.csv')
cf_qsim(.x = ('tamar_sim66.flow'), .n = 'CF_qsim66_30.csv')
cf_qsim(.x = ('tamar_sim258.flow'), .n = 'CF_qsim258_30.csv')
cf_qsim(.x = ('tamar_sim320.flow'), .n = 'CF_qsim320_30.csv')
cf_qsim(.x = ('tamar_sim380.flow'), .n = 'CF_qsim380_30.csv')
cf_qsim(.x = ('tamar_sim415.flow'), .n = 'CF_qsim415_30.csv')
cf_qsim(.x = ('tamar_sim729.flow'), .n = 'CF_qsim729_30.csv')
cf_qsim(.x = ('tamar_sim806.flow'), .n = 'CF_qsim806_30.csv')
cf_qsim(.x = ('tamar_sim898.flow'), .n = 'CF_qsim898_30.csv')
cf_qsim(.x = ('tamar_sim971.flow'), .n = 'CF_qsim971_30.csv')


#---TAMARSTONE---#
date.time <- read.delim('D:/DECIPHeR/Outputs/date.time_ceh.txt', header = TRUE)
precip <- read.delim('D:/DECIPHeR/Outputs/Cf_precip_clip.txt')
ts_qsim <- function(.x, .n) {
  qsim <- read.delim(.x, sep = '', header = TRUE)
  qsim <- qsim %>% select(1)
  CF <- cbind(date.time, qsim)
  CF <- CF[-c(1:4346),]
  CF <- cbind(CF, precip)
  colnames(CF) <- c('datetime', 'q', 'rainfall')
  CF$rainfall <- CF$rainfall * 1000 #convert from m to mm
  CF$q <- (CF$q * 48100000)/3600 #convert from m/hr to m/sec and normalise for catchment size
  write.csv(CF, row.names = FALSE, file = .n)
}

#Qsim
ts_qsim(.x = ('tamar_sim2.flow'), .n = 'TS_qsim2_30.csv')
ts_qsim(.x = ('tamar_sim66.flow'), .n = 'TS_qsim66_30.csv')
ts_qsim(.x = ('tamar_sim258.flow'), .n = 'TS_qsim258_30.csv')
ts_qsim(.x = ('tamar_sim320.flow'), .n = 'TS_qsim320_30.csv')
ts_qsim(.x = ('tamar_sim380.flow'), .n = 'TS_qsim380_30.csv')
ts_qsim(.x = ('tamar_sim415.flow'), .n = 'TS_qsim415_30.csv')
ts_qsim(.x = ('tamar_sim729.flow'), .n = 'TS_qsim729_30.csv')
ts_qsim(.x = ('tamar_sim806.flow'), .n = 'TS_qsim806_30.csv')
ts_qsim(.x = ('tamar_sim898.flow'), .n = 'TS_qsim898_30.csv')
ts_qsim(.x = ('tamar_sim971.flow'), .n = 'TS_qsim971_30.csv')

#OPTION 2: (Post metrics processing, has been run through DECIPHeR_model_runs.R first)
#Tamarstone
df_TS <- read.delim('final_simulations_Tamarstone.txt')
precip_TS <- read.delim('D:/DECIPHeR/Outputs/Cf_precip_clip.txt')
df_TS <- cbind(df_TS, precip_TS$rainfall)
colnames(df_TS)[13] <- c('rainfall')

ts_qsim <- function(.x, .n) {
  ts <- df_TS
  ts <- ts %>% select(1, .x, 13)
  colnames(ts) <- c('datetime', 'q', 'rainfall')
  ts$rainfall <- ts$rainfall * 1000 #convert from m to mm
  ts$q <- (ts$q * 48100000)/3600 #convert from m/hr to m/sec and normalise for catchment size
  write.csv(ts, row.names = FALSE, file = .n)
}

ts_qsim(.x = 3, .n = 'TS_sim258.csv')
ts_qsim(.x = 4, .n = 'TS_sim971.csv')
ts_qsim(.x = 5, .n = 'TS_sim729.csv')
ts_qsim(.x = 6, .n = 'TS_sim898.csv')
ts_qsim(.x = 7, .n = 'TS_sim320.csv')
ts_qsim(.x = 8, .n = 'TS_sim380.csv')
ts_qsim(.x = 9, .n = 'TS_sim415.csv')
ts_qsim(.x = 10, .n = 'TS_sim806.csv')
ts_qsim(.x = 11, .n = 'TS_sim66.csv')
ts_qsim(.x = 12, .n = 'TS_sim2.csv')

#Crowford
df_CF <- read.delim('final_simulations_Crowford.txt')
precip_CF <- read.delim('D:/DECIPHeR/Outputs/Cf_precip_clip.txt')
df_CF <- cbind(df_CF, precip_CF$rainfall)
colnames(df_CF)[13] <- c('rainfall')

cf_qsim <- function(.x, .n) {
  cf <- df_CF
  cf <- cf %>% select(1, .x, 13)
  colnames(cf) <- c('datetime', 'q', 'rainfall')
  cf$rainfall <- cf$rainfall * 1000 #convert from m to mm
  cf$q <- (cf$q * 75500000)/3600 #convert from m/hr to mm/sec
  write.csv(cf, row.names = FALSE, file = .n)
}

cf_qsim(.x = 3, .n = 'CF_sim258.csv')
cf_qsim(.x = 4, .n = 'CF_sim971.csv')
cf_qsim(.x = 5, .n = 'CF_sim729.csv')
cf_qsim(.x = 6, .n = 'CF_sim898.csv')
cf_qsim(.x = 7, .n = 'CF_sim320.csv')
cf_qsim(.x = 8, .n = 'CF_sim380.csv')
cf_qsim(.x = 9, .n = 'CF_sim415.csv')
cf_qsim(.x = 10, .n = 'CF_sim806.csv')
cf_qsim(.x = 11, .n = 'CF_sim66.csv')
cf_qsim(.x = 12, .n = 'CF_sim2.csv')

#Extension: Combining the same simulations
setwd('D:/DECIPHeR/Outputs/Scenarios/-35')

org <- read.csv('D:/DECIPHeR/Outputs/2021_03_17/CF_sim2.csv')
x10 <- read.csv('D:/DECIPHeR/Outputs/Scenarios/+35/Sim_10/CF_qsim2_10.csv')
x20 <- read.csv('D:/DECIPHeR/Outputs/Scenarios/+35/Sim_20/CF_qsim2_20.csv')
x30 <- read.csv('D:/DECIPHeR/Outputs/Scenarios/+35/Sim_30/CF_qsim2_30.csv')

all <- cbind(org, x10$q, x20$q, x30$q)
colnames(all) <- c('datetime', 'Original_Q', 'rainfall', '10_Q', '20_Q', '30_Q')
write.table(all, sep = '\t', file = 'all_sim2_Q_CF.txt')

