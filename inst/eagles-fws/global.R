library(dplyr)
library(plotly)
library(shiny)
library(shinydashboard)

load("data/app_data.RData")

prior <- rgamma(1000,shape = mean(Bay16$flight_time),  rate = mean(Bay16$effort))%>%
  density()

obs <- rgamma(1000,shape = mean(Bay16$flight_time),  rate = mean(Bay16$effort))%>%
  density()

gamma_plot <- function(obs){
  curve(dgamma(x, shape = mean(Bay16$flight_time),
               rate = mean(Bay16$effort)),0,1)
  curve(dgamma(x, shape = mean(Bay16$flight_time)+Bay16$flight_time[obs],
               rate = mean(Bay16$effort)+Bay16$effort[obs]),0,1, add = TRUE, lty = 2)
  abline( v = Bay16$flight_time[obs]/Bay16$effort[obs])

}
