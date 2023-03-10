rm(list = ls())
start_time <- Sys.time()
library("deSolve")
library("readxl")
library("tidyverse")
library("ggplot2")

# Functions:
aging_mod <- function(time, y, parameters, signal){
  ydot <- c()
  cc <- signal(time)
  print(paste0("cc:",cc))
  with(as.list(c(y, parameters)), {
    ydot[1] = cc - (1 + gam)*y[1]
    for (i in c(2:dim)) {
      ydot[i] <- y[i-1] - (1 + gam)*y[i]
    }
    return(list(ydot, c = cc))
  })
}

# Read input data:
data_lau <- as.data.frame(read_excel("~/Laura_proj/lab.xlsx"))
data_lau$date_emergence <- as.Date(data_lau$date_emergence)
data_comp <- data_lau[which(data_lau$date_emergence >= as.Date("2021-07-30") & data_lau$date_emergence <= as.Date("2021-08-02")),]
# Check number of censored:
data_cen <- data_lau[,c(2,7,11)]  %>% filter(censored == 0)
data_cen_group <- data_cen  %>%  group_by(colony) %>% 
  summarise(sum_cen = sum(censored), n = n(), date_em = min(date_emergence))
data_cen_group$month <- lubridate::month(data_cen_group$date_em)
ggplot(data_cen_group) + 
  geom_point(aes(month,n))

# Create forcing:
data_forc <- data_lau[,c(1,2,8)] %>% filter(censored == 1)
data_forc <- data_forc  %>%  group_by(date_emergence) %>% 
  summarise(n = n())
data_forc$time <- 1
# Column with times:
for(i in c(2:length(data_forc$n))){
  data_forc$time[i] <- as.numeric(data_forc$date_emergence[i] -
                                    data_forc$date_emergence[i-1]) +
    data_forc$time[i-1]
}

signal <- function(x){
  if(x %in% data_forc$time){
    y = data_forc$n[which(data_forc$time == x)]
  }else{
    y = 0
  }
  return(y)
}

time <- seq(0, max(data_forc$time), by=1)
data_forc_p <- data.frame(time =time, n = sapply(time, signal))
# Create the forcing function with the data:
signal <- approxfun(x = data_forc_p$time,
                    y = data_forc_p$n,
                    method = "linear", rule = 2)

# Parameters: 
time <- seq(0, max(data_forc_p$time), by=1)
dim <- 60
parameters <- c(dim=dim, gam=(1/32))
state <- rep(0,dim)


# Run the model:
out <- as.data.frame(ode(y = state, times = time, func = aging_mod, 
           parms = parameters, signal = signal))
# 
# plot(out)
# end_time <- Sys.time()
# print(paste("time:", end_time-start_time))

out$date <- seq(as.Date(min(data_forc$date_emergence)-1),as.Date("2021-10-28"),"days")

# Plot de output:
plot_dist <- function(date_ref, name_plot){
  June_out <- as.data.frame(t(out[which(out$date == as.Date(date_ref)),]))
  colnames(June_out) <- c("Numb")
  June_out$Numb <- as.numeric(June_out$Numb)
  l <- length(June_out$Numb)
  June_out <- as.numeric(June_out[-c(1, l,l-1),])
  June_df <- data.frame(age = seq(1,length(June_out),1), numb = June_out )
  plot <- ggplot(June_df) + 
    geom_line(aes(age,numb)) +
    theme_bw() +
    xlab("Mosquito Age") + 
    ylab("Number Mosquitoes") + 
    ggtitle(name_plot)
  return(plot)
}


date <- "2021-06-30"
Jun_plot <- plot_dist(date, "June")
date <- "2021-07-30"
Jul_plot <- plot_dist(date, "July")
date <- "2021-08-30"
Aug_plot <- plot_dist(date, "August")
date <- "2021-09-30"
Sept_plot <- plot_dist(date, "September")

library("cowplot")
plot_grid(Jun_plot, Jul_plot, Aug_plot, Sept_plot)

### Laura Code:
# Function to plot both results:
# Plot de output:
plot_dist_both <- function(date_ref, name_plot, data2){
  June_out <- as.data.frame(t(out[which(out$date == as.Date(date_ref)),]))
  colnames(June_out) <- c("Numb")
  June_out$Numb <- as.numeric(June_out$Numb)
  l <- length(June_out$Numb)
  June_out <- as.numeric(June_out[-c(1, l,l-1),])
  June_df <- data.frame(age = seq(1,length(June_out),1), numb = June_out )
  plot <- ggplot(June_df) + 
    geom_line(aes(age,numb)) +
    geom_density(data = data2, aes(x=triallived, y=..count..), 
                 linetype = "dashed", color = "red") +
    theme_bw() +
    xlab("Mosquito Age") + 
    ylab("Number Mosquitoes") + 
    ggtitle(name_plot)
  
  return(plot)
}


lab <- data_lau

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
date =  "2021-09-30"
labsep2 <- lab_september_s %>%
  group_by(triallived) 
Plot_sep2 <- plot_dist_both(date, "September",lab_september_s)
  
  
lab$trial <- "2021-08-30"
lab_august_s <- filter(lab, date_emergence <= "2021-08-30" & end_date >= "2021-08-30")
range(lab_august_s$date_emergence)
range(lab_august_s$end_date)
lab_august_s <- lab_august_s[order(as.Date(lab_august_s$date_emergence, format="%m/%d/%Y")),]
lab_august_s$id_mosquito<- as.factor(lab_august_s$id_mosquito)
lab_august_s$triallived <- interval(lab_august_s$date_emergence, lab_august_s$trial) %/% days(1)

labaug2 <- lab_august_s %>%
  group_by(triallived) 
date = "2021-08-30"
Plot_aug2 <- plot_dist_both(date, "August",labaug2)


lab$trial <- "2021-07-30"

lab_july_s <- filter(lab, date_emergence <= "2021-07-30" & end_date >= "2021-07-30")
range(lab_july_s$date_emergence)
range(lab_july_s$end_date)
lab_july_s <- lab_july_s[order(as.Date(lab_july_s$date_emergence, format="%m/%d/%Y")),]
lab_july_s$id_mosquito<- as.factor(lab_july_s$id_mosquito)
lab_july_s$triallived <- interval(lab_july_s$date_emergence, lab_july_s$trial) %/% days(1)
lab_july_s$date_emergence <- as.Date(lab_july_s$date_emergence)
str(lab_july_s)
# labjuly <- lab_july_s %>%
#   group_by(total_lived)
labjuly2 <- lab_july_s %>%
  group_by(triallived) 

date = "2021-07-30"
Plot_july2 <- plot_dist_both(date, "July",labjuly2)


lab$trial <- "2021-06-30"
lab_june_s <- filter(lab, date_emergence <= "2021-06-30" & end_date >= "2021-06-30")
range(lab_june_s$date_emergence)
range(lab_june_s$end_date)
lab_june_s <- lab_june_s[order(as.Date(lab_june_s$date_emergence, format="%m/%d/%Y")),]
lab_june_s$id_mosquito<- as.factor(lab_june_s$id_mosquito)
lab_june_s$triallived <- interval(lab_june_s$date_emergence, lab_june_s$trial) %/% days(1)

labjune2 <- lab_june_s %>%
  group_by(triallived) 

date = "2021-06-30"
Plot_june2 <- plot_dist_both(date, "June",labjune2)


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

