bayesian <- function(input, output, session) {

observeEvent(input$time,{
  obs <- isolate({
  rgamma(1000,shape = mean(Bay16$flight_time) + input$time,  rate = mean(Bay16$effort) + input$effort)%>%
    density()
})
})


observeEvent(input$time,{
line <- isolate({
  input$time/input$effort
})
})

output$distributions <- renderPlotly({
plot_ly()%>%
  add_trace(x = ~prior$x, y = ~ prior$y, type = "scatter", mode = "lines", fill = "tozeroy", name = "FWS Prior")%>%
  add_trace(x = ~obs$x, y = ~obs$y, type = "scatter", mode = "lines", fill = "tozeroy", name = "Posterior")%>%
  add_trace(x = c(line, line), y = c(0, 10), name = "Observed Exposure", type = "scatter", mode = "lines")%>%
  layout(title = "Bayesian")

})
}
