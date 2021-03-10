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

# --- Data folder -----

data_folder <- file.path('D:', 'DECIPHeR', 'Outputs', 'R_code', 'Nic_WindowFilter', 'data')

#1. Read in event extraction summary statistics
#1.1 Original data

sum_stat_df <- read.csv(file.path(data_folder, 'eventEx_EVENTS_metrics_org.csv')) %>%
  select(-X)  %>% # Removes first column
  mutate(event_start = lubridate::ymd_hms(event.start.ts),
         event_end = lubridate::ymd_hms(event.end.ts)) %>%
  relocate(event_start, .before = event.start.ts) %>%
  relocate(event_end, .after = event_start)

#1.2 Simulation data
df_10 <- read.csv(file.path(data_folder, 'CF_qsim2_10.csv')) %>%
                    mutate(datetime = lubridate::dmy_hm(datetime)) #previously time_ser_df if broken
df_20 <- read.csv(file.path(data_folder, 'CF_qsim2_20.csv')) %>%
                    mutate(datetime = lubridate::dmy_hm(datetime))
df_30 <- read.csv(file.path(data_folder, 'CF_qsim2_30.csv')) %>%
                    mutate(datetime = lubridate::dmy_hm(datetime))

# function which extracts the event window from the event time series and appends an event ID number
extract_windows <- function(sum_df, timeseries_df){
  ev_ID <- sum_df$eventID[1]
  print(ev_ID)
  ev_start <- lubridate::ymd_hms(sum_df$event_start[1])
  ev_end <- lubridate::ymd_hms(sum_df$event_end[1])
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
            lag_time = lubridate::as.period(datetime[q == peak_q][1] - min(datetime)),
            tot_time = lubridate::as.period(max(datetime) - min(datetime)))
}

#Process 10, 20 and 30% scenarios
df1 <- stats(time_ser_df = df_10)
write.table(df1, file = 'Sim2_10.txt', sep= '\t', col.names = TRUE)

df2 <- stats(time_ser_df = df_20)

df3 <- stats(time_ser_df = df_30)

#Analyzing outputs script, compare original, 10, 20 and 30. May require some matching for lost events
