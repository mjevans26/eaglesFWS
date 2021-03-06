library(dplyr)
library(plotly)
library(shiny)
library(shinydashboard)
plot(Bay16$RISK_HA, (fatality/(Bay16$FLIGHT_MIN/Bay16$EFFORT))/mean(rbeta(1000, 9.38, 3224.51)))

test <- glm(data = Bay16, (COLLISIONS/(FLIGHT_MIN/EFFORT))/0.002895415~RISK_HA + r2)
#Bay16 <- read.csv("C:/Users/mevans/repos/eaglesFWS/BayData.csv", header = TRUE)

#Bay16$OBS_MIN <- as.character(Bay16$OBS_MIN)%>%
#  gsub(",","",.)%>%
#  as.numeric()
#Bay16$FLIGHT_MIN <- as.character(Bay16$FLIGHT_MIN)%>%
#  gsub(",","",.)%>%
#  as.numeric()

#Bay16$EFFORT <- (Bay16$HECTARES*0.01)*(Bay16$OBS_MIN/60)
Bay16$SCALE <- (0.02112*Bay16$RISK_HA*Bay16$DIAMETER*Bay16$PERIOD + 0.3503)

load("app_data.RData")
Bay16$EFFORT <- (Bay16$HECTARES*0.01*0.2)*(Bay16$OBS_MIN/60)

prior <- curve(dgamma(x, shape = sum(Bay16$FLIGHT_MIN),  rate = sum(Bay16$EFFORT)),
               from = 0.5,
               to = 2)


#obs <- rgamma(10000,shape = mean(Bay16$FLIGHT_MIN),  rate = mean(Bay16$EFFORT))%>%
#  density()

#collision <- data.frame(y = dbeta(seq(0,0.01,0.0001), shape1 = 9.38, shape2 = 3224.51),
#                        x = seq(0, 0.01, 0.0001))

collision <- curve(dbeta(x, shape1 = 9.38, shape2 = 3224.51), from = 0, to = 0.01)

act <- sum(Bay16$FLIGHT_MIN)/sum(Bay16$EFFORT)
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

parallel_predict <- function(iters, alpha, beta){
  out <- data.frame(collision = rep(NA, iters), expose = rep(NA, iters), fatality = rep(NA, iters))
  foreach(n = 1:iters, .combine = m)%dopar%{
    c <- rbeat(1, shape1 = 9.38, shape2 = 3224.51)
    e <- rgamma(1, shape = alpha, rate = beta)
    f <- c*e
    return()
  }

  }

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

