library(dplyr)
library(plotly)
library(shiny)
library(shinydashboard)

#plot(Bay16$RISK_HA, Bay16$COLLISIONS/(Bay16$FLIGHT_MIN/Bay16$EFFORT))
#test <- lm(Bay16$COLLISIONS/(Bay16$FLIGHT_MIN/Bay16$EFFORT)~Bay16$RISK_HA)
#Bay16 <- read.csv("C:/Users/mevans/repos/eaglesFWS/BayData.csv", header = TRUE)

#Bay16$OBS_MIN <- as.character(Bay16$OBS_MIN)%>%
#  gsub(",","",.)%>%
#  as.numeric()
#Bay16$FLIGHT_MIN <- as.character(Bay16$FLIGHT_MIN)%>%
#  gsub(",","",.)%>%
#  as.numeric()

#Bay16$EFFORT <- (Bay16$HECTARES*0.01)*(Bay16$OBS_MIN/60)
#Bay16$SCALE <- -362.57580 + (33.38994*Bay16$RISK_HA) + (-0.03774*(Bay16$RISK_HA^2))

load("data/app_data.RData")
Bay16$EFFORT <- (Bay16$HECTARES*0.01*0.2)*(Bay16$OBS_MIN/60)
prior <- rgamma(10000,shape = mean(Bay16$FLIGHT_MIN),  rate = mean(Bay16$EFFORT))%>%
  density()

obs <- rgamma(10000,shape = mean(Bay16$FLIGHT_MIN),  rate = mean(Bay16$EFFORT))%>%
  density()

collision <- rbeta(10000, shape1 = 9.38, shape2 = 3224.51)%>%
  density

act <- mean(Bay16$FLIGHT_MIN)/mean(Bay16$EFFORT)
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
