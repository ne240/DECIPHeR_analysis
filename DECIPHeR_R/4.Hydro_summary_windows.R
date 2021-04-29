#----Event windows following event extraction----#
#Author: Nicola Ellis, Hugh Graham
#Created March 2021
#Last modified March 2021
#After running the original data through event extraction, this script can be used to extract the exact same
#windows from different data.
library(tidyverse)
library(purrr)
library(here)
library(dplyr)
library(Hmisc)
library(ggplot2)

# --- Data folder -----

data_folder <- file.path('D:', 'DECIPHeR', 'Outputs', 'R_code', 'DECIPHeR_R', 'data')

#1. Read in event extraction summary statistics
#1.1 Original data

sum_stat_df <- read_csv(file.path(data_folder, 'eventEx_EVENTS_metrics_2.csv')) %>%
  select(-X1)  %>% # This just removes that annoying 1st column with no colname (not essential - I'm just a pedant)
  mutate(event_start = lubridate::ymd_hms(event.start.ts),
         event_end = lubridate::ymd_hms(event.end.ts))

#1.2 Simulation data
df_0 <- read_csv(file.path(data_folder, 'CF_qsim2_0.csv')) %>%
  mutate(datetime = lubridate::dmy_hm(datetime))
df_10 <- read_csv(file.path(data_folder, 'CF_qsim2_10.csv')) %>%
  mutate(datetime = lubridate::dmy_hm(datetime))
df_20 <- read_csv(file.path(data_folder, 'CF_qsim2_20.csv')) %>%
  mutate(datetime = lubridate::dmy_hm(datetime))
df_30 <- read_csv(file.path(data_folder, 'CF_qsim2_30.csv')) %>%
  mutate(datetime = lubridate::dmy_hm(datetime))

# load function which extracts the event window from the event time series and appends an event ID number
extract_windows <- function(sum_df, timeseries_df){
  ev_ID <- sum_df$eventID[1]
  # print(ev_ID)
  ev_start <- sum_df$event_start[1]
  ev_end <- sum_df$event_end[1]
  timeseries_df %>%
    filter(datetime >= ev_start & datetime <=ev_end) %>%
    mutate(eventID = ev_ID)
}

# map function to the time series and summarise to retrieve stats summary.
stats <- function(time_ser_df){
sum_df <- sum_stat_df %>%
    group_by(eventID) %>% # groups dataframe by event id
    group_split() %>% # splits dataframe up into list of single row dataframes
    purrr::map(., ~extract_windows(sum_df=., timeseries_df = time_ser_df)) %>% # maps t
    bind_rows() %>% #join together the results
    group_by(eventID) %>%  # regroup the df by event ID
    summarise(peak_q = max(q), tot_q = sum(q), mean_q = mean(q), # apply all the stats by group that you want...
              max_rain = max(rainfall), tot_rain = sum(rainfall), mean_rain = mean(rainfall),
              start_to_peak = lubridate::as.period(datetime[q == peak_q][1] - min(datetime)),
              peak_to_peak = lubridate::as.period(datetime[q == peak_q][1] - datetime[rainfall == max_rain][1]),
              tot_time = lubridate::as.period(max(datetime) - min(datetime)))
}


#2. Simulation statistics
#2.1 Outputs
df0 <- stats(time_ser_df = df_0)
df0 <- df0[!duplicated(df0$peak_q),]

df1 <- stats(time_ser_df = df_10)
df1 <- df1[!duplicated(df1$peak_q),] #removes pesky duplicated events on the same day

df2 <- stats(time_ser_df = df_20)
df2 <- df2[!duplicated(df2$peak_q),]

df3 <- stats(time_ser_df = df_30)
df3 <- df3[!duplicated(df3$peak_q),]

#Merge by event so all are the same events
events_keep <- df1$eventID
df0 <- subset(df0, eventID %in% events_keep)
df1 <- subset(df1, eventID %in% events_keep)
df2 <- subset(df2, eventID %in% events_keep)
df3 <- subset(df3, eventID %in% events_keep)
event <- subset(sum_stat_df, eventID %in% events_keep)

#Mutate data values to match (if using hourly data)
event <- event %>% mutate(rain.tot.mm = rain.tot.mm*4)
event <- event %>% mutate(Q.response.tot.m3 = Q.response.tot.m3*4)
df0 <- df0 %>% mutate(tot_q = tot_q*3600)
df1 <- df1 %>% mutate(tot_q = tot_q*3600)
df2 <- df2 %>% mutate(tot_q = tot_q*3600)
df3 <- df3 %>% mutate(tot_q = tot_q*3600)

write.table(event, file = 'Sim2_org.txt', sep = '\t')
write.table(df0, file = 'Sim2_0.txt', sep = '\t')
write.table(df1, file = 'Sim2_10.txt', sep = '\t')
write.table(df2, file = 'Sim2_20.txt', sep = '\t')
write.table(df3, file = 'Sim2_30.txt', sep = '\t')


#2.2 Plot data to check
orig_data <- event %>%
  select(eventID, rain.tot.mm, Q.peak.m3.s) %>%
  rename(tot_rain=rain.tot.mm, peak_q = Q.peak.m3.s)%>%
  mutate(name = 'Original')

sym_data0 <- df0%>%
  select(eventID, tot_rain, peak_q) %>%
  mutate(name = 'sim0')

sym_data1 <- df1%>%
  select(eventID, tot_rain, peak_q) %>%
  mutate(name = 'sim10')

sym_data2 <- df2%>%
  select(eventID, tot_rain, peak_q) %>%
  mutate(name = 'sim20')

sym_data3 <- df3%>%
  select(eventID, tot_rain, peak_q) %>%
  mutate(name = 'sim30')

comb_dat <- bind_rows(orig_data, sym_data0, sym_data1, sym_data2, sym_data3)

ggplot(comb_dat, aes(x=tot_rain, y= peak_q, colour=name, fill=name)) +
  geom_point() +
  geom_smooth(method='gam', se=F) +
  ylab(('Peak flow m³ sec¯¹')) +
  xlab('Total rain (mm)') +
  theme_minimal() +
  scale_colour_manual(values = c('tomato3', 'orange1', 'green3', 'mediumpurple3', 'royalblue3')) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))

#3. Basic statistics
#Percentage change function
percentage <- function(.qobs, .qsim) {
  n <- .qsim - .qobs
  (n/.qobs)*100
}

#3.1 Peak_Q
peak_0 <- (event$Q.peak.m3.s - df0$peak_q)
peak_0p <- percentage(.qobs = event$Q.peak.m3.s, .qsim = df0$peak_q)
print(mean(peak_00))
print(mean(peak_0p))

peak_10 <- (event$Q.peak.m3.s - df1$peak_q)
peak_10p <- percentage(.qobs = event$Q.peak.m3.s, .qsim = df1$peak_q)
print(mean(peak_10))
print(mean(peak_10p))

peak_20 <- (event$Q.peak.m3.s - df2$peak_q)
peak_20p <- percentage(.qobs = event$Q.peak.m3.s, .qsim = df2$peak_q)
print(mean(peak_20))
print(mean(peak_20p))

peak_30 <- (event$Q.peak.m3.s - df3$peak_q)
peak_30p <- percentage(.qobs = event$Q.peak.m3.s, .qsim = df3$peak_q)
print(mean(peak_30))
print(mean(peak_30p))

#3.2 Total Q
totq_0 <- (event$Q.response.tot.m3 - df0$tot_q)
totq_0p <- percentage(.qobs = event$Q.response.tot.m3, .qsim = df0$tot_q)
print(mean(totq_0))
print(mean(totq_0p))

totq_10 <- (event$Q.response.tot.m3 - df1$tot_q)
totq_10p <- percentage(.qobs = event$Q.response.tot.m3, .qsim = df1$tot_q)
print(mean(totq_10))
print(mean(totq_10p))

totq_20 <- (event$Q.response.tot.m3 - df2$tot_q)
totq_20p <- percentage(.qobs = event$Q.response.tot.m3, .qsim = df2$tot_q)
print(mean(totq_20))
print(mean(totq_20p))

totq_30 <- (event$Q.response.tot.m3 - df3$tot_q)
totq_30p <- percentage(.qobs = event$Q.response.tot.m3, .qsim = df3$tot_q)
print(mean(totq_30))
print(mean(totq_30p))

all <- cbind(peak_0, peak_10, peak_20, peak_30, peak_0p, peak_10p, peak_20p, peak_30p,
             totq_0, totq_10, totq_20, totq_30, totq_0p, totq_10p, totq_20p, totq_30p)

write.csv(all, file = 'all_metrics_sim2.csv', row.names = TRUE)






