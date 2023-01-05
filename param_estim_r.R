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
data <- as.data.frame(read_excel("~/Laura_proj/lab.xlsx"))
# Check number of censored:
data_cen <- data[,c(2,6,11)]  %>% filter(censored == 0)
data_cen_group <- data_cen  %>%  group_by(colony) %>% 
  summarise(sum_cen = sum(censored), n = n(), date_em = min(date_emergence))
data_cen_group$month <- lubridate::month(data_cen_group$date_em)
ggplot(data_cen_group) + 
  geom_point(aes(month,n))
# Create forcing:
data_forc <- data[,c(1,2,6)] %>% filter(censored == 1)
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
time <- seq(0, max(data_forc$time), by=1)
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

