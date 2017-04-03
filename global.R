library(dplyr)
library(plotly)
library(shiny)
library(shinydashboard)

plot(Bay16$RISK_HA, (Bay16$COLLISIONS/(Bay16$FLIGHT_MIN/Bay16$EFFORT))/mean(rbeta(1000, 9.38, 3224.51)))
test <- glm(data = Bay16, (COLLISIONS/(FLIGHT_MIN/EFFORT))/0.002895415~RISK_HA + r2)
#Bay16 <- read.csv("C:/Users/mevans/repos/eaglesFWS/BayData.csv", header = TRUE)

#Bay16$OBS_MIN <- as.character(Bay16$OBS_MIN)%>%
#  gsub(",","",.)%>%
#  as.numeric()
#Bay16$FLIGHT_MIN <- as.character(Bay16$FLIGHT_MIN)%>%
#  gsub(",","",.)%>%
#  as.numeric()

#Bay16$EFFORT <- (Bay16$HECTARES*0.01)*(Bay16$OBS_MIN/60)
Bay16$SCALE <- -22560 + (2306*Bay16$RISK_HA) + (-2.607*(Bay16$RISK_HA^2))

load("app_data.RData")
Bay16$EFFORT <- (Bay16$HECTARES*0.01*0.2)*(Bay16$OBS_MIN/60)

prior <- curve(dgamma(x, shape = mean(Bay16$FLIGHT_MIN),  rate = mean(Bay16$EFFORT)),
               from = 0.5,
               to = 2)


#obs <- rgamma(10000,shape = mean(Bay16$FLIGHT_MIN),  rate = mean(Bay16$EFFORT))%>%
#  density()

#collision <- data.frame(y = dbeta(seq(0,0.01,0.0001), shape1 = 9.38, shape2 = 3224.51),
#                        x = seq(0, 0.01, 0.0001))

collision <- curve(dbeta(x, shape1 = 9.38, shape2 = 3224.51), from = 0, to = 0.01)

act <- mean(Bay16$FLIGHT_MIN)/mean(Bay16$EFFORT)


#prediction <- function(iters, alpha, beta){
#out <- data.frame(collision = rep(NA,iters), expose = rep(NA, iters), fatality = rep(NA, iters))
#for(n in 1:iters){
#  c <- rbeta(1, shape1 = 9.38, shape2 = 3224.51)
#  e <- rgamma(1, shape = alpha, rate = beta)
#  f <- c*e
#  out[n,] <- c(c,e,f)
#}
#return(out)
#}
