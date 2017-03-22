site_preds <- vapply(1:nrow(Bay16), function(x) {

 a <- mean(Bay16$FLIGHT_MIN) + Bay16$FLIGHT_MIN[x]
 b <- mean(Bay16$EFFORT) + Bay16$EFFORT[x]
 out <- prediction(10000, a, b)
 fatality <- mean(out$fatality*Bay16$SCALE[x])
 q80 <- quantile(out$fatality*Bay16$SCALE[x], c(0.1, 0.9))

 out2 <- prediction(10000, a-mean(Bay16$FLIGHT_MIN), b-mean(Bay16$EFFORT))
 fatality2 <- mean(out2$fatality*Bay16$SCALE[x])
 q82 <- quantile(out2$fatality*Bay16$SCALE[x], c(0.1, 0.9))
 return (c(fatality, q80[1], q80[2], fatality2, q82[1], q82[2]))

}, USE.NAMES = FALSE, FUN.VALUE = c(0,0,0,0,0,0))

Bay16$MN_F <- site_preds[1,]
Bay16$LQ_F <- site_preds[2,]
Bay16$UQ_F <- site_preds[3,]
Bay16$MN <- site_preds[4,]
Bay16$LQ <- site_preds[5,]
Bay16$UQ <- site_preds[6,]

output$states <- renderPlotly({
  plot_ly(Bay16)%>%
    add_trace(x = ~MN_F, y = ~MN, type = "scatter", mode = "markers", size = ~(UQ_F-LQ_F)/MN_F,
              marker = list(line = list(color = "black"),
                            sizemode = "diameter",
                            colorbar = list(y = 0.75, x = 0.85,
                                            title = "Survey Effort")
                            ),
              sizes = c(5,15), color = ~ EFFORT, name = "Estimated Eagle Fatalities",
              text = ~paste(round(EFFORT,3)),
              hoverinfo = "text")%>%
    add_trace(x = ~c(0, max(MN_F)), y = ~c(0, max(MN_F)), type = "scatter", mode = "lines", name = "1:1 Line")%>%
    layout(hovermode = "closest", font = list(color = "black"),
           xaxis = list(title = "Estimates using Priors"),
           yaxis = list(title = "Estimates without Priors"),
           legend = list(x = 0.10, y = 0.95, bordercolor = "black", borderwidth = 1))
})

plot_ly(Bay16)%>%
  add_trace(x = ~EFFORT, y = ~abs(MN_F-MN)/MN, type = "scatter", mode = "markers",
            size = ~ (UQ_F-LQ_F)/MN_F)

plot_ly(Bay16)%>%
  add_trace(x = ~EFFORT, y = ~ (UQ_F-LQ_F)/MN_F, type = "scatter", mode = "markers")
