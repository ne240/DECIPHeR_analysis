#----Rainfall peak and peak Q----#
#Author: Nicola Ellis
#Created April 2021
#Last modified April 2021

library(dplyr)
library(ggplot2)

data_folder <- file.path('D:', 'DECIPHeR', 'Outputs', 'R_code', 'DECIPHeR_R', 'Output')
#1. All results summary
#Read in results
org1 <- read.delim(file.path(data_folder, 'Sim59_org.txt'))
org2 <- read.delim(file.path(data_folder, 'Sim99_org.txt'))
org3 <- read.delim(file.path(data_folder, 'Sim154_org.txt'))
org4 <- read.delim(file.path(data_folder, 'Sim155_org.txt'))
org5 <- read.delim(file.path(data_folder, 'Sim170_org.txt'))
org6 <- read.delim(file.path(data_folder, 'Sim328_org.txt'))
org7 <- read.delim(file.path(data_folder, 'Sim535_org.txt'))
org8 <- read.delim(file.path(data_folder, 'Sim577_org.txt'))
org9 <- read.delim(file.path(data_folder, 'Sim664_org.txt'))
org10 <- read.delim(file.path(data_folder, 'Sim687_org.txt'))

sim10_1 <- read.delim(file.path(data_folder, 'Sim59_10.txt'))
sim10_2 <- read.delim(file.path(data_folder, 'Sim99_10.txt'))
sim10_3 <- read.delim(file.path(data_folder, 'Sim154_10.txt'))
sim10_4 <- read.delim(file.path(data_folder, 'Sim155_10.txt'))
sim10_5 <- read.delim(file.path(data_folder, 'Sim170_10.txt'))
sim10_6 <- read.delim(file.path(data_folder, 'Sim328_10.txt'))
sim10_7 <- read.delim(file.path(data_folder, 'Sim535_10.txt'))
sim10_8 <- read.delim(file.path(data_folder, 'Sim577_10.txt'))
sim10_9 <- read.delim(file.path(data_folder, 'Sim664_10.txt'))
sim10_10 <- read.delim(file.path(data_folder, 'Sim687_10.txt'))

sim20_1 <- read.delim(file.path(data_folder, 'Sim59_20.txt'))
sim20_2 <- read.delim(file.path(data_folder, 'Sim99_20.txt'))
sim20_3 <- read.delim(file.path(data_folder, 'Sim154_20.txt'))
sim20_4 <- read.delim(file.path(data_folder, 'Sim155_20.txt'))
sim20_5 <- read.delim(file.path(data_folder, 'Sim170_20.txt'))
sim20_6 <- read.delim(file.path(data_folder, 'Sim328_20.txt'))
sim20_7 <- read.delim(file.path(data_folder, 'Sim535_20.txt'))
sim20_8 <- read.delim(file.path(data_folder, 'Sim577_20.txt'))
sim20_9 <- read.delim(file.path(data_folder, 'Sim664_20.txt'))
sim20_10 <- read.delim(file.path(data_folder, 'Sim687_20.txt'))

sim30_1 <- read.delim(file.path(data_folder, 'Sim59_30.txt'))
sim30_2 <- read.delim(file.path(data_folder, 'Sim99_30.txt'))
sim30_3 <- read.delim(file.path(data_folder, 'Sim154_30.txt'))
sim30_4 <- read.delim(file.path(data_folder, 'Sim155_30.txt'))
sim30_5 <- read.delim(file.path(data_folder, 'Sim170_30.txt'))
sim30_6 <- read.delim(file.path(data_folder, 'Sim328_30.txt'))
sim30_7 <- read.delim(file.path(data_folder, 'Sim535_30.txt'))
sim30_8 <- read.delim(file.path(data_folder, 'Sim577_30.txt'))
sim30_9 <- read.delim(file.path(data_folder, 'Sim664_30.txt'))
sim30_10 <- read.delim(file.path(data_folder, 'Sim687_30.txt'))

#Functions
Original <- function(.df){
  orig_data <- .df %>%
  select(eventID, rain.tot.mm, Q.peak.m3.s) %>%
  rename(tot_rain=rain.tot.mm, peak_q = Q.peak.m3.s)%>%
  mutate(name = 'Present')
}

Sim1 <- function(.df){
  sym_data1 <- .df %>%
    select(eventID, tot_rain, peak_q) %>%
    mutate(name = 'sim10')
}

Sim2 <- function(.df){
  sym_data1 <- .df %>%
    select(eventID, tot_rain, peak_q) %>%
    mutate(name = 'sim20')
}

Sim3 <- function(.df){
  sym_data1 <- .df %>%
    select(eventID, tot_rain, peak_q) %>%
    mutate(name = 'sim30')
}

#Run functions
org1 <- Original(.df = org1)
org2 <- Original(.df = org2)
org3 <- Original(.df = org3)
org4 <- Original(.df = org4)
org5 <- Original(.df = org5)
org6 <- Original(.df = org6)
org7 <- Original(.df = org7)
org8 <- Original(.df = org8)
org9 <- Original(.df = org9)
org10 <- Original(.df = org10)

sim10_1 <- Sim1(.df = sim10_1)
sim10_2 <- Sim1(.df = sim10_2)
sim10_3 <- Sim1(.df = sim10_3)
sim10_4 <- Sim1(.df = sim10_4)
sim10_5 <- Sim1(.df = sim10_5)
sim10_6 <- Sim1(.df = sim10_6)
sim10_7 <- Sim1(.df = sim10_7)
sim10_8 <- Sim1(.df = sim10_8)
sim10_9 <- Sim1(.df = sim10_9)
sim10_10 <- Sim1(.df = sim10_10)

sim20_1 <- Sim2(.df = sim20_1)
sim20_2 <- Sim2(.df = sim20_2)
sim20_3 <- Sim2(.df = sim20_3)
sim20_4 <- Sim2(.df = sim20_4)
sim20_5 <- Sim2(.df = sim20_5)
sim20_6 <- Sim2(.df = sim20_6)
sim20_7 <- Sim2(.df = sim20_7)
sim20_8 <- Sim2(.df = sim20_8)
sim20_9 <- Sim2(.df = sim20_9)
sim20_10 <- Sim2(.df = sim20_10)

sim30_1 <- Sim3(.df = sim30_1)
sim30_2 <- Sim3(.df = sim30_2)
sim30_3 <- Sim3(.df = sim30_3)
sim30_4 <- Sim3(.df = sim30_4)
sim30_5 <- Sim3(.df = sim30_5)
sim30_6 <- Sim3(.df = sim30_6)
sim30_7 <- Sim3(.df = sim30_7)
sim30_8 <- Sim3(.df = sim30_8)
sim30_9 <- Sim3(.df = sim30_9)
sim30_10 <- Sim3(.df = sim30_10)


comb_dat <- bind_rows(org2, org3, org4, org5, org6, org7, org8, org9, org10,
                      sim10_1, sim10_2, sim10_3, sim10_4, sim10_5, sim10_6, sim10_7, sim10_8, sim10_9, sim10_10,
                      sim20_1, sim20_2, sim20_3, sim20_4, sim20_5, sim20_6, sim20_7, sim20_8, sim20_9, sim20_10,
                      sim30_1, sim30_2, sim30_3, sim30_4, sim30_5, sim30_6, sim30_7, sim30_8, sim30_9, sim30_10)

ggplot(comb_dat, aes(x=tot_rain, y= peak_q, colour=name, fill=name)) +
  geom_point() +
  geom_smooth(method = "glm") +
  ylab(('Peak flow m³ sec¯¹')) +
  xlab('Total rain (mm)') +
  theme_minimal() +
  scale_colour_manual(values = c('tomato3', 'seagreen', 'royalblue3', 'mediumpurple3')) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))



