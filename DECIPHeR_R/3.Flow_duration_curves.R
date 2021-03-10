#Flow duration curve from DECIPHeR output
#Author: Nicola Ellis
#Created: February 2021
#Last modified: February 2021
#Calculate FDC and analysis

setwd('D:/DECIPHeR/Outputs/FDC')
library(tidyverse)
library(reshape2)
library(dplyr)
library(ggplot2)

#1. Data pre
#1.1 Read in original flow, 10%, 20% and 30% restoration
qobs <- read.csv('D:/DECIPHeR/Outputs/2021_03_04/TS_sim687.csv')
qsim_10 <- read.csv('D:/DECIPHeR/Outputs/Scenarios/-35/Sim_10/TS_qsim687_10.csv')
qsim_20 <- read.csv('D:/DECIPHeR/Outputs/Scenarios/-35/Sim_20/TS_qsim687_20.csv')
qsim_30 <- read.csv('D:/DECIPHeR/Outputs/Scenarios/-35/Sim_30/TS_qsim687_30.csv')

#1.2 Function to create FDC
fdc <-function(.df, .name){
  dat <- sort(.df$q, decreasing = T)
  dat <- data.frame(x = 100/length(dat) * 1:length(dat), y = dat)
  colnames(dat) <- c('Prob', .name)
  dat <- data.frame(dat)
}
#1.3 Create FDC
fdcorg <- fdc(.df = qobs, .name = 'Org_Q')
fdc_10 <- fdc(.df = qsim_10, .name = 'Q_10')
fdc_20 <- fdc(.df = qsim_20, .name = 'Q_20')
fdc_30 <- fdc(.df = qsim_30, .name = 'Q_30')

#1. 4convert to long format
fdc_graph <- cbind(fdcorg, fdc_10, fdc_20, fdc_30)
data_long <- melt(fdc_graph, id = 'Prob', variable.name = 'run')

#2. Graph
g1 <- ggplot(data_long, aes(x = Prob, y = value)) +
  geom_line(aes(colour = run, group = run)) +
  scale_y_continuous(trans = 'log10') +
  xlab('% Flow equalled or exceeded') +
  ylab((Flow~(m^{3}~s^{-1}))) +
  theme_minimal() +  theme(legend.title=element_blank(),
                           legend.position = "bottom",
                           axis.line.x = element_line(color="black"),
                           axis.line.y = element_line(color="black"),
                           axis.text.x = element_text(size = 12),
                           axis.text.y = element_text(size = 12),
                           legend.text = element_text(size = 12),
                           axis.title.x = element_text(size = 12),
                           axis.title.y = element_text(size = 12)) +
  scale_colour_manual(labels = c('Original', '10%', '20%', '30%'),
                      values = c('black', 'blue', 'green', 'red'))

g1

#3. FDC statistics
#Mean
meanorg <- mean(fdcorg$Org_Q)
mean10 <- mean(fdc_10$Q_10)
mean20 <- mean(fdc_20$Q_20)
mean30 <- mean(fdc_30$Q_30)
#Median
medianorg <- median(fdcorg$Org_Q)
median10 <- median(fdc_10$Q_10)
median20 <- median(fdc_20$Q_20)
median30 <- median(fdc_30$Q_30)
#Q5
Q5_org <- as.numeric(quantile(fdcorg$Org_Q, 0.5))
Q5_10 <- as.numeric(quantile(fdc_10$Q_10, 0.5))
Q5_20 <- as.numeric(quantile(fdc_20$Q_20, 0.5))
Q5_30 <- as.numeric(quantile(fdc_30$Q_30, 0.5))
#Q95
Q95_org <- as.numeric(quantile(fdcorg$Org_Q, 0.95))
Q95_10 <- as.numeric(quantile(fdc_10$Q_10, 0.95))
Q95_20 <- as.numeric(quantile(fdc_20$Q_20, 0.95))
Q95_30 <- as.numeric(quantile(fdc_30$Q_30, 0.95))
#Q5:Q95
Q_org <- as.numeric(quantile(fdcorg$Org_Q, 0.95, na.rm = TRUE))/
  as.numeric(quantile(fdcorg$Org_Q, 0.5, na.rm = TRUE))
Q_10 <- as.numeric(quantile(fdc_10$Q_10, 0.95, na.rm = TRUE))/
  as.numeric(quantile(fdc_10$Q_10, 0.5, na.rm = TRUE))
Q_20 <- as.numeric(quantile(fdc_20$Q_20, 0.95, na.rm = TRUE))/
  as.numeric(quantile(fdc_20$Q_20, 0.5, na.rm = TRUE))
Q_30 <- as.numeric(quantile(fdc_30$Q_30, 0.95, na.rm = TRUE))/
  as.numeric(quantile(fdc_30$Q_30, 0.5, na.rm = TRUE))

file <- rbind(meanorg, mean10, mean20, mean30, medianorg, median10, median20, median30,
              Q5_org, Q5_10, Q5_20, Q5_30, Q95_org, Q95_10, Q95_20, Q95_30, Q_org, Q_10,
              Q_20, Q_30)
write.table(file, sep = '\t', file = 'FDC_metrics_TSsim687.txt')

#4. Summary stats
CF_all <- read.delim('CF_all_fdc.txt')


