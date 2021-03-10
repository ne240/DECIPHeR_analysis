#------DECIPHeR Output Setup------#
#Author: Nicola Ellis
#Created April 2020
#Last modified February 2021
#Script used for subsetting DECIPHeR raw outputs into different gauges and calculating goodness of fit metrics
setwd('D:/DECIPHeR/Outputs/2021_03_04') #set model run directory
library(reshape2)
library(dplyr)
library(lubridate)
library(tidyverse)
library(hydroGOF)
library(readr)

#------------STAGE 1: Data prep------------#
#1.1 Parameter file
read_res <- function(.path){
  res.number <- parse_number(.path)
  read_delim(.path, skip = 8, delim=" ", n_max = 6, trim_ws = TRUE) %>%
    select(-c(X9)) %>%
    mutate(res.num = res.number)
}

file_list <- list.files(getwd(),pattern= '\\.res') #list all .res files
out_df <- lapply(file_list, read_res) %>% #runs through each .res file and extracts parameters
  bind_rows() #binds all results into one value

write.table(out_df, sep = '\t', row.names = FALSE, file = 'parameters.txt')
rm(list=ls()) #clear R environment

#1.2 Merge into one file
#!!!! Make sure date.time file is not in the folder or previous files created in this script!!!!#
file_list <- list.files(path="D:/DECIPHeR/Outputs/2021_03_04", pattern = ('\\.flow')) #lists the simulations, change to appropriate file
f <- lapply(file_list, read.delim, header = TRUE, sep = "", row.names=NULL)
f <- do.call(cbind, f)



#-----------STAGE 2: Separate the gauges------------#
#Crowford (gauge 3)
CF <- f[seq(3, ncol(f), 3) ]  #creates index from column 3 every 3 columns
date.time <- read.delim("D:/DECIPHeR/Outputs/date.time_ceh.txt")
qobs <- read.delim("D:/DECIPHeR/OUTPUTS/crowford_qobs_ceh.txt")
CF <- cbind(date.time, qobs, CF)
subsetcf <- CF[-c(1:4346),] #removes first 6 months
colnames(subsetcf) <- c("date.time", "Qobs", 1:1000) #names date.time, observed discharge and simulations as headings
write.table(subsetcf, sep = "\t", file = "Crowford_all.txt", row.names = FALSE)

#Tamarstone (gauge 1)
TS <- f[seq(1, ncol(f), 3) ] #creates index from row 1 every 3 rows
date.time <- read.delim("D:/DECIPHeR/Outputs/date.time_ceh.txt")
qobsTS <- read.delim("D:/DECIPHeR/OUTPUTS/tamarstone_qobs_ceh.txt")
TS <- cbind(date.time, qobsTS, TS)
subsetTS <- TS[c(59952:94104),] #subsets years of TS gauge
colnames(subsetTS) <- c("date.time", "Qobs", 1:1000) #change to number of simulations
write.table(subsetTS, sep = "\t", file = "Tamarstone_all.txt")




#------------STAGE 3: NSE, Qmax and Pbias of simulations and extractions------------#
#3.1 Calculate NSE
#Tamarstone
df_ts <- subsetTS
Qsims <- colnames(df_ts[3:1002])
obs <-  select(df_ts, Qobs)
NSE_calcs <- list() #empty list

for (i in 1:length(Qsims)){
  sim <- select(df_ts, (Qsims[i]))
  NSE_val <- hydroGOF::NSE(sim, obs) # calculate NSE
  NSE_val$qsimID <- Qsims[i]      # add qsim ID to output
  NSE_calcs[[i]] <- NSE_val    #populate list

}
all_NSE_calcs <- bind_rows(NSE_calcs) #all NSE values table
mean(all_NSE_calcs$Qobs) #NSE mean

#Extract NSE values above a threshold
ok_NSE <- all_NSE_calcs %>%
  filter(Qobs > 0.85 ) %>%   # NSE criteria
  pull(qsimID)  # gives you a list of cols that match your NSE criteria

sub_df <- df_ts %>%
  select(ok_NSE)

NSE_date.time <- read.delim("D:/DECIPHeR/Outputs/date.timeNSE_ts_ceh.txt") #change to correct date.time file
NSE_qsim <- cbind(NSE_date.time, sub_df) #combine date and time (- 6 months) with Qsims and Qobs
write.table(NSE_qsim, sep = "\t", file = "TS_NSE_Qsim.txt", row.names = FALSE) #write file for simulations above NSE threshold
write.table(all_NSE_calcs, sep = "\t", file = "TS_NSE_scores.txt", row.names = FALSE) #write file of all the NSE scores

#Crowford
df_cf <- subsetcf
Qsims <- colnames(df_cf[3:1002])
obs <-  select(df_cf, Qobs)
NSE_calcs <- list() #empty list

for (i in 1:length(Qsims)){
  sim <- select(df_cf, (Qsims[i]))
  NSE_val <- hydroGOF::NSE(sim, obs) # calculate NSE
  NSE_val$qsimID <- Qsims[i]      # add qsim ID to output
  NSE_calcs[[i]] <- NSE_val    #populate list

}
all_NSE_calcs <- bind_rows(NSE_calcs) #all NSE values table
mean(all_NSE_calcs$Qobs) #NSE mean

#Extract NSE values above a threshold
ok_NSE <- all_NSE_calcs %>%
  filter(Qobs > 0.85 ) %>%   # NSE criteria
  pull(qsimID)  # gives you a list of cols that match your NSE criteria

sub_df <- df_cf %>%
  select(ok_NSE)

NSE_date.time <- read.delim("D:/DECIPHeR/Outputs/date.timeNSE_cf_ceh.txt") #change to correct date.time file
NSE_qsim <- cbind(NSE_date.time, sub_df) #combine date and time (- 6 months) with Qsims and Qobs
write.table(NSE_qsim, sep = "\t", file = "CF_NSE_Qsim.txt", row.names = FALSE) #write file for simulations above NSE threshold
write.table(all_NSE_calcs, sep = "\t", file = "CF_NSE_scores.txt", row.names = FALSE)

#3.2 Qmax
#Tamarstone
all_qmaxts <- read.delim("Tamarstone_all.txt", header = TRUE) %>%
  select(!date.time) %>%
  summarise_all( max) %>%
  pivot_longer(!Qobs)%>%
  mutate(qmax_val = Qobs-value) %>%
  rename(max_value=value, sim=name) %>%
  select(!Qobs)

write.table(all_qmaxts, sep = '\t', file = 'TS_qmax.txt', row.names = FALSE)

#Crowford
all_qmaxcf <- read.delim("Crowford_all.txt", header = TRUE) %>%
  select(!date.time) %>%
  summarise_all( max) %>%
  pivot_longer(!Qobs)%>%
  mutate(qmax_val = Qobs-value) %>%
  rename(max_value=value, sim=name) %>%
  select(!Qobs)

write.table(all_qmaxcf, sep = '\t', file = 'CF_qmax.txt', row.names = FALSE)

#3.3 Percentage bias
#Tamarstone
#Error 'pbias.matrix' means need to rerun obs (line 47)
obs <-  select(df_ts, Qobs)
PB_list <- list()
for (i in 1:length(Qsims)) {
  sim <- select(df_ts, (Qsims[i]))
  PB_val <- data.frame(PB = hydroGOF::pbias(sim, obs))
  PB_val$qsimID <- Qsims[i]
  PB_list[[i]] <- PB_val
}
all_pb <- bind_rows(PB_list)

write.table(all_pb, sep = '\t', file = 'TS_pbias.txt', row.names = FALSE)

#Crowford
obs <- select(df_cf, Qobs)
PB_list <- list()
for (i in 1:length(Qsims)) {
  sim <- select(df_cf, (Qsims[i]))
  PB_val <- data.frame(PB = hydroGOF::pbias(sim, obs))
  PB_val$qsimID <- Qsims[i]
  PB_list[[i]] <- PB_val
}
all_pb <- bind_rows(PB_list)

write.table(all_pb, sep = '\t', file = 'CF_pbias.txt', row.names = FALSE)




#---------------STAGE 4: Rank each metric-----------------#
rm(list=ls()) #tidy R environment
#--4.1 Tamarstone--#
NSE_TS <- read.delim('TS_NSE_scores.txt')
Qmax_TS <- read.delim('TS_qmax.txt')
Pbias_TS <- read.delim('TS_pbias.txt')

#4.1.1 Order data
NSE_TS <- NSE_TS[order(-NSE_TS$Qobs),]
Pbias_TS <- Pbias_TS[order(Pbias_TS$PB),]
#Remove negatives from Qmax_TS
Q <- abs(Qmax_TS$qmax_val)
Q <- data.frame(Q)
Qmax_TS <- cbind(Qmax_TS$sim, Q)
Qmax_TS <- Qmax_TS[order(Qmax_TS$Q),]

#4.1.2 Rank data
NSE_TS$rank <- NA #create rank column
NSE_TS$rank <- rank(-NSE_TS$Qobs) #rank highest to lowest
Pbias_TS$rank <- NA
Pbias_TS$rank <- rank(Pbias_TS$PB)
Qmax_TS$rank <- NA
Qmax_TS$rank <- rank(Qmax_TS$Q)

#4.1.3 Restore to original order
NSE_TS <- NSE_TS[order(NSE_TS$qsimID),]
Qmax_TS <- Qmax_TS[order(Qmax_TS$`Qmax_TS$sim`),]
Pbias_TS <- Pbias_TS[order(Pbias_TS$qsimID),]
#sum ranks
df <- cbind(NSE_TS$qsimID, NSE_TS$rank, Qmax_TS$rank, Pbias_TS$rank)
df <- data.frame(df)
colnames(df) <- c('sim', 'NSE', 'Qmax', 'Pbias')
df$NSE <- as.numeric(as.character(df$NSE)) #covert to numeric
df$Qmax <- as.numeric(as.character(df$Qmax))
df$Pbias <- as.numeric(as.character(df$Pbias))
df$sum <- NA
df$sum <- df$NSE + df$Qmax + df$Pbias #sum of ranks
#Final list, ascending order
df <- df[order(df$sum),]
write.table(df, sep = '\t', file = 'final_score_Tamarstone.txt', row.names = FALSE)

#4.1.4 Final simulations
sim <- read.delim('Tamarstone_all.txt')
##insert columns of selected simulations from df
final <- cbind(sim$date.time, sim$Qobs, sim$X602, sim$X461, sim$X711, sim$X31, sim$X52, sim$X733, sim$X294, sim$X127, sim$X308, sim$X379)
colnames(final) <- c('date.time', 'Qobs', 602, 461, 711, 31, 52, 733, 294, 127, 308, 379)
write.table(final, sep = '\t', file = 'final_simulations_Tamarstone.txt', row.names = FALSE)



#--4.2 Crowford--#
NSE_CF <- read.delim('CF_NSE_scores.txt')
Qmax_CF <- read.delim('CF_qmax.txt')
Pbias_CF <- read.delim('CF_pbias.txt')

#4.2.1 Order data
NSE_CF <- NSE_CF[order(-NSE_CF$Qobs),]
Pbias_CF <- Pbias_CF[order(Pbias_CF$PB),]
#Remove negatives from Qmax_TS
Q <- abs(Qmax_CF$qmax_val)
Q <- data.frame(Q)
Qmax_CF <- cbind(Qmax_CF$sim, Q)
Qmax_CF <- Qmax_CF[order(Qmax_CF$Q),]

#4.2.2 Rank data
NSE_CF$rank <- NA #create rank column
NSE_CF$rank <- rank(-NSE_CF$Qobs) #rank highest to lowest
Pbias_CF$rank <- NA
Pbias_CF$rank <- rank(Pbias_CF$PB)
Qmax_CF$rank <- NA
Qmax_CF$rank <- rank(Qmax_CF$Q)

#4.2.3 Restore to original order
NSE_CF <- NSE_CF[order(NSE_CF$qsimID),]
Qmax_CF <- Qmax_CF[order(Qmax_CF$`Qmax_CF$sim`),]
Pbias_CF <- Pbias_CF[order(Pbias_CF$qsimID),]
#sum ranks
df <- cbind(NSE_CF$qsimID, NSE_CF$rank, Qmax_CF$rank, Pbias_CF$rank)
df <- data.frame(df)
colnames(df) <- c('sim', 'NSE', 'Qmax', 'Pbias')
df$NSE <- as.numeric(as.character(df$NSE)) #covert to numeric
df$Qmax <- as.numeric(as.character(df$Qmax))
df$Pbias <- as.numeric(as.character(df$Pbias))
df$sum <- NA
df$sum <- df$NSE + df$Qmax + df$Pbias #sum of ranks
#Final list, ascending order
df <- df[order(df$sum),]
write.table(df, sep = '\t', file = 'final_score_Crowford.txt')

#4.2.4 Final simulations
sim <- read.delim('Crowford_all.txt')
##insert columns of selected simulations from df
final <- cbind(sim$date.time, sim$Qobs, sim$X154, sim$X155, sim$X687, sim$X107, sim$X328, sim$X59, sim$X535, sim$X99, sim$X577, sim$X644)
colnames(final) <- c('date.time', 'Qobs', 154, 155, 687, 170, 328, 59, 535, 99, 577, 664)
write.table(final, sep = '\t', file = 'final_simulations_Crowford.txt', row.names = FALSE)


