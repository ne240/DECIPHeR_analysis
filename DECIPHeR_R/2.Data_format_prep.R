#---DECIPHeR model scenario output preparation script---#
#Author: Nicola Ellis
#Created: January 2021
#Last modified: February 2021
#Separating different gauge simulations from original data and model runs.

setwd('D:/DECIPHeR/Outputs/Scenarios/-35/Sim_30')
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
  CF$q <- CF$q * 277777.77878788 #convert from m/hr to mm/sec
  write.csv(CF, row.names = FALSE, file = .n)
}

#Qsim
cf_qsim(.x = ('tamar_sim59.flow'), .n = 'CF_qsim59_30.csv')
cf_qsim(.x = ('tamar_sim99.flow'), .n = 'CF_qsim99_30.csv')
cf_qsim(.x = ('tamar_sim154.flow'), .n = 'CF_qsim154_30.csv')
cf_qsim(.x = ('tamar_sim155.flow'), .n = 'CF_qsim155_30.csv')
cf_qsim(.x = ('tamar_sim170.flow'), .n = 'CF_qsim170_30.csv')
cf_qsim(.x = ('tamar_sim328.flow'), .n = 'CF_qsim328_30.csv')
cf_qsim(.x = ('tamar_sim535.flow'), .n = 'CF_qsim535_30.csv')
cf_qsim(.x = ('tamar_sim577.flow'), .n = 'CF_qsim577_30.csv')
cf_qsim(.x = ('tamar_sim664.flow'), .n = 'CF_qsim664_30.csv')
cf_qsim(.x = ('tamar_sim687.flow'), .n = 'CF_qsim687_30.csv')


#---TAMARSTONE---#
precipts <- read.delim('D:/DECIPHeR/Outputs/Tamarstone_precip.txt')
date.time_ts <- read.delim('D:/DECIPHeR/Outputs/date.timeNSE_ts_ceh.txt')
ts_qsim <- function(.x, .n) {
  qsim <- read.delim(.x, sep = '', header = TRUE)
  qsim <- qsim %>% select(1)
  TS <- qsim[c(59952:94104),]
  TS <- cbind(date.time_ts, TS)
  TS <- cbind(TS, precipts)
  colnames(TS) <- c('datetime', 'q', 'rainfall')
  TS$rainfall <- TS$rainfall * 1000 #convert from m to mm
  TS$q <- TS$q * 277777.77878788 #convert from m/hr to mm/sec
  write.csv(TS, row.names = FALSE, file = .n)
}

#Qsim
ts_qsim(.x = ('tamar_sim59.flow'), .n = 'TS_qsim59_30.csv')
ts_qsim(.x = ('tamar_sim99.flow'), .n = 'TS_qsim99_30.csv')
ts_qsim(.x = ('tamar_sim154.flow'), .n = 'TS_qsim154_30.csv')
ts_qsim(.x = ('tamar_sim155.flow'), .n = 'TS_qsim155_30.csv')
ts_qsim(.x = ('tamar_sim170.flow'), .n = 'TS_qsim170_30.csv')
ts_qsim(.x = ('tamar_sim328.flow'), .n = 'TS_qsim328_30.csv')
ts_qsim(.x = ('tamar_sim535.flow'), .n = 'TS_qsim535_30.csv')
ts_qsim(.x = ('tamar_sim577.flow'), .n = 'TS_qsim577_30.csv')
ts_qsim(.x = ('tamar_sim664.flow'), .n = 'TS_qsim664_30.csv')
ts_qsim(.x = ('tamar_sim687.flow'), .n = 'TS_qsim687_30.csv')

#OPTION 2: (Post metrics processing, has been run through DECIPHeR_output.R first)
#Tamarstone
df_TS <- read.delim('final_simulations_Tamarstone.txt')
precip_TS <- read.delim('D:/DECIPHeR/Outputs/Tamarstone_precip.txt')
df_TS <- cbind(df_TS, precip_TS$precip)
colnames(df_TS)[13] <- c('rainfall')

ts_qsim <- function(.x, .n) {
  ts <- df_TS
  ts <- ts %>% select(1, .x, 13)
  colnames(ts) <- c('datetime', 'q', 'rainfall')
  ts$rainfall <- ts$rainfall * 1000 #convert from m to mm
  ts$q <- ts$q * 277777.77878788 #convert from m/hr to mm/sec
  write.csv(ts, row.names = FALSE, file = .n)
}

ts_qsim(.x = 3, .n = 'TF_sim59.csv')
ts_qsim(.x = 4, .n = 'TF_sim99.csv')
ts_qsim(.x = 5, .n = 'TF_sim154.csv')
ts_qsim(.x = 6, .n = 'TF_sim155.csv')
ts_qsim(.x = 7, .n = 'TF_sim170.csv')
ts_qsim(.x = 8, .n = 'TF_sim328.csv')
ts_qsim(.x = 9, .n = 'TF_sim535.csv')
ts_qsim(.x = 10, .n = 'TF_sim577.csv')
ts_qsim(.x = 11, .n = 'TF_sim664.csv')
ts_qsim(.x = 12, .n = 'TF_sim687.csv')

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
  cf$q <- cf$q * 277777.77878788 #convert from m/hr to mm/sec
  write.csv(cf, row.names = FALSE, file = .n)
}

cf_qsim(.x = 3, .n = 'CF_sim154.csv')
cf_qsim(.x = 4, .n = 'CF_sim155.csv')
cf_qsim(.x = 5, .n = 'CF_sim687.csv')
cf_qsim(.x = 6, .n = 'CF_sim170.csv')
cf_qsim(.x = 7, .n = 'CF_sim328.csv')
cf_qsim(.x = 8, .n = 'CF_sim59.csv')
cf_qsim(.x = 9, .n = 'CF_sim535.csv')
cf_qsim(.x = 10, .n = 'CF_sim99.csv')
cf_qsim(.x = 11, .n = 'CF_sim577.csv')
cf_qsim(.x = 12, .n = 'CF_sim664.csv')

#Extension: Combining the same simulations
setwd('D:/DECIPHeR/Outputs/Scenarios/-35')

org <- read.csv('D:/DECIPHeR/Outputs/2021_03_04/CF_sim577.csv')
x10 <- read.csv('D:/DECIPHeR/Outputs/Scenarios/-35/Sim_10/CF_qsim577_10.csv')
x20 <- read.csv('D:/DECIPHeR/Outputs/Scenarios/-35/Sim_20/CF_qsim577_20.csv')
x30 <- read.csv('D:/DECIPHeR/Outputs/Scenarios/-35/Sim_30/CF_qsim577_30.csv')

all <- cbind(org, x10$q, x20$q, x30$q)
colnames(all) <- c('datetime', 'Original_Q', 'rainfall', '10_Q', '20_Q', '30_Q')
write.table(all, sep = '\t', file = 'all_sim577_Q_CF.txt')

