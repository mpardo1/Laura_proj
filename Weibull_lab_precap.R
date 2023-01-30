library(survival)
library(survminer)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(readxl)
library(lubridate)
library(fitdistrplus)
library(viridis)
library(ggfortify)

#probatinas
lab <- read_excel("/home/marta/Laura_proj/lab.xlsx", 
                  col_types = c("numeric", "date", "date", 
                                "numeric", "date", "numeric", "numeric", 
                                "numeric", "numeric", 
                                "numeric", "numeric"))

lab <- filter(lab, location == 3)
lab <- filter (lab, censored ==1)
lab$precapture_lived <- interval(lab$date_emergence,  lab$start_date) %/% days(1)
lab$postcap_lived <- interval(lab$start_date, lab$end_date) %/% days(1)
lab <- lab %>%
  add_column(month = NA)
lab$month <- lubridate::month(ymd(lab$start_date))
lab <- lab %>%
  add_column(month_emerg = NA)
lab$month_emerg <- lubridate::month(ymd(lab$date_emergence))
lab$trial <- "2021-09-30"
lab_september_s <- filter(lab, date_emergence <= "2021-09-30" & end_date >= "2021-09-30")
range(lab_september_s$date_emergence)
range(lab_september_s$end_date)
lab_september_s <- lab_september_s[order(as.Date(lab_september_s$date_emergence, format="%m/%d/%Y")),]
lab_september_s$id_mosquito<- as.factor(lab_september_s$id_mosquito)
lab_september_s$triallived <- interval(lab_september_s$date_emergence, lab_september_s$trial) %/% days(1)

labsep2 <- lab_september_s %>%
  group_by(triallived) 
Plot_sep2 <- labsep2 %>%
  ggplot(aes(x=triallived)) +
  xlim(0,60)+
  geom_density(aes(y=..count..)) +
  xlab("Mosquito Age") + ylab("Number of mosquitoes") +
  ggtitle("Plot September") +
  theme_bw()

lab$trial <- "2021-08-30"
lab_august_s <- filter(lab, date_emergence <= "2021-08-30" & end_date >= "2021-08-30")
range(lab_august_s$date_emergence)
range(lab_august_s$end_date)
lab_august_s <- lab_august_s[order(as.Date(lab_august_s$date_emergence, format="%m/%d/%Y")),]
lab_august_s$id_mosquito<- as.factor(lab_august_s$id_mosquito)
lab_august_s$triallived <- interval(lab_august_s$date_emergence, lab_august_s$trial) %/% days(1)

labaug2 <- lab_august_s %>%
  group_by(triallived) 
Plot_aug2 <- labaug2 %>%
  ggplot(aes(x=triallived)) +
  geom_density(aes(y=..count..)) +
  xlab("Mosquito Age") + ylab("Number of mosquitoes") +
  xlim(0,60)+
  ggtitle("Plot August") +
  theme_bw()

 
lab$trial <- "2021-07-30"
  
lab_july_s <- filter(lab, date_emergence <= "2021-07-30" & end_date >= "2021-07-30")
range(lab_july_s$date_emergence)
range(lab_july_s$end_date)
lab_july_s <- lab_july_s[order(as.Date(lab_july_s$date_emergence, format="%m/%d/%Y")),]
lab_july_s$id_mosquito<- as.factor(lab_july_s$id_mosquito)
lab_july_s$triallived <- interval(lab_july_s$date_emergence, lab_july_s$trial) %/% days(1)
lab_july_s$date_emergence <- as.Date(lab_july_s$date_emergence)
str(lab_july_s)
labjuly <- lab_july_s %>%
  group_by(total_lived)

labjuly2 <- lab_july_s %>%
  group_by(triallived) 
Plot_july2 <- labjuly2 %>%
  ggplot(aes(x=triallived)) +
  geom_density(aes(y=..count..)) +
  xlab("Mosquito Age") + ylab("Number of mosquitoes") +
  xlim(0,60)+
  ggtitle("Plot July") +
  theme_bw()

lab$trial <- "2021-06-30"
lab_june_s <- filter(lab, date_emergence <= "2021-06-30" & end_date >= "2021-06-30")
range(lab_june_s$date_emergence)
range(lab_june_s$end_date)
lab_june_s <- lab_june_s[order(as.Date(lab_june_s$date_emergence, format="%m/%d/%Y")),]
lab_june_s$id_mosquito<- as.factor(lab_june_s$id_mosquito)
lab_june_s$triallived <- interval(lab_june_s$date_emergence, lab_june_s$trial) %/% days(1)

labjune2 <- lab_june_s %>%
  group_by(triallived) 
Plot_june2 <- labjune2 %>%
  ggplot(aes(x=triallived)) +
  geom_density(aes(y=..count..)) +
  xlab("Mosquito Age") + ylab("Number of mosquitoes") +
  xlim(0,60) +
  ggtitle("Plot June") +
  theme_bw()

library(patchwork)
library(ggpubr)
plot<- ggarrange(Plot_june2,Plot_july2,Plot_aug2,Plot_sep2, nrow = 2, ncol = 2)
plot

Plot_lab <- lab %>%
  ggplot(aes(x=total_lived)) +
  geom_density(aes(y=..count..)) +
  xlab("Mosquito Age") + ylab("Number of mosquitoes") +
  xlim(0,60)+
  ggtitle("Plot all Lab") +
  theme_bw()
