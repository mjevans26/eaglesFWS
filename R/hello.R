# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}


gamma_plot <- function(obs){
  curve(dgamma(x, shape = mean(Bay16$flight_time),
               rate = mean(Bay16$effort)),0,1)
  curve(dgamma(x, shape = mean(Bay16$flight_time)+Bay16$flight_time[obs],
               rate = mean(Bay16$effort)+Bay16$effort[obs]),0,1, add = TRUE, lty = 2)
  abline( v = Bay16$flight_time[obs]/Bay16$effort[obs])

}

library(plotly)
