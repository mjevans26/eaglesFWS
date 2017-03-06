library(dplyr)
library(plotly)
library(shiny)
library(shinydashboard)

#Bay16 <- read.csv("C:/Users/mevans/repos/eaglesFWS/BayData.csv", header = TRUE)
#save(Bay16, file = "C:/Users/mevans/repos/eaglesFWS/inst/eagles-fws/data/app_data.RData")
#Bay16$OBS_MIN <- as.numeric(Bay16$OBS_MIN)
#Bay16$FLIGHT_MIN <- as.numeric(Bay16$FLIGHT_MIN)
#Bay16$EFFORT <- (Bay16$HECTARES*0.01)*(Bay16$OBS_MIN/60)
load("data/app_data.RData")

prior <- rgamma(10000,shape = mean(Bay16$FLIGHT_MIN),  rate = mean(Bay16$EFFORT))%>%
  density()

obs <- rgamma(10000,shape = mean(Bay16$FLIGHT_MIN),  rate = mean(Bay16$EFFORT))%>%
  density()

collision <- rbeta(10000, shape1 = 9.38, shape2 = 3224.51)%>%
  density

#gamma_plot <- function(obs){
#  curve(dgamma(x, shape = mean(Bay16$flight_time),
#               rate = mean(Bay16$effort)),0,1)
#  curve(dgamma(x, shape = mean(Bay16$flight_time)+Bay16$flight_time[obs],
#               rate = mean(Bay16$effort)+Bay16$effort[obs]),0,1, add = TRUE, lty = 2)
#  abline( v = Bay16$flight_time[obs]/Bay16$effort[obs])
#}

prediction <- function(iters, alpha, beta){
out <- data.frame(collision = rep(NA,iters), expose = rep(NA, iters), fatality = rep(NA, iters))
for(n in 1:iters){
  c <- rbeta(1, shape1 = 9.38, shape2 = 3224.51)
  e <- rgamma(1, shape = alpha, rate = beta)
  f <- c*e
  out[n,] <- c(c,e,f)
}
return(out)
}
