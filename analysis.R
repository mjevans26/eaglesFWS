source("C:/Users/mevans/repos/eaglesFWS/inst/eagles-fws/helper_fxns.R")
site_preds <- vapply(1:nrow(Bay16), function(x) {

 a <- mean(Bay16$FLIGHT_MIN) + Bay16$FLIGHT_MIN[x]
 b <- mean(Bay16$EFFORT) + Bay16$EFFORT[x]
 out <- prediction(10000, a, b)
 fatality <- mean(out$fatality)
 q80 <- quantile(out$fatality, c(0.1, 0.8, 0.9))

 out2 <- prediction(10000, a-mean(Bay16$FLIGHT_MIN), b-mean(Bay16$EFFORT))
 fatality2 <- mean(out2$fatality)
 q82 <- quantile(out2$fatality, c(0.1, 0.8, 0.9))
 return (c(fatality, q80[1], q80[3], fatality2, q82[1], q82[3], q80[2], q82[2]))

}, USE.NAMES = FALSE, FUN.VALUE = c(0,0,0,0,0,0,0,0))

Bay16$MN_F <- site_preds[1,]
Bay16$LQ_F <- site_preds[2,]
Bay16$UQ_F <- site_preds[3,]
Bay16$MN <- site_preds[4,]
Bay16$LQ <- site_preds[5,]
Bay16$UQ <- site_preds[6,]
Bay16$U_F <- site_preds[7,]
Bay16$U <- site_preds[8,]

#function to return mean and 80% CI estimates from a predicted fatality distribution
#hardwired to use Bay16 data as exposure priors
estimates <- function(niters, a, b){
  out <- prediction(niters, a+mean(Bay16$FLIGHT_MIN), b+mean(Bay16$EFFORT))
  #fatality <- mean(out$fatality)
  q80 <- quantile(out$fatality, 0.8)
  out2 <- prediction(10000, a, b)
  #fatality2 <- mean(out2$fatality)
  q82 <- quantile(out2$fatality, 0.8)
  return (c("U_F" = q80, "U" = q82))
}

#cretae simulated eagle flight observation and survey data
#values based off of minimum survey requirements:
#1hr per plot per month for two years
#survey plot of 0.8km radius * 0.2km height

flight <- c(0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.10,0.15,0.20,0.25,0.50,0.75,1.00,1.50,2.00,2.50,3)
time <- seq(1, 10, 1)*12*2
area <- seq(0.402, 2.01, 0.402)
df <- expand.grid(TIME = time, AREA = area, eagle_rate = flight)
df$b <- df$TIME*df$AREA
df$a <- df$eagle_rate*df$b

uppers <- vapply(1:nrow(df), function(x) {
  a <- df$a[x]
  b <- df$b[x]
  out <- prediction(10000, a+mean(Bay16$FLIGHT_MIN), b+mean(Bay16$EFFORT))
  q80 <- quantile(out$fatality, 0.8)
  out2 <- prediction(10000, a, b)
  q82 <- quantile(out2$fatality, 0.8)
  return (c("U_F" = q80, "U" = q82))
}, USE.NAMES = FALSE, FUN.VALUE = c(0,0))

#create dataset of estimated fatality distributions based on simulated
#survey and flight observation data
sim <- plyr::mdply(df[, c(5, 4)], estimates, niters = 10000)
colnames(sim)[c(4,5,7,8)] <- c("LQ_F", "UQ_F", "LQ", "UQ")
sim$eagle_rate <- flight

  plot_ly(Bay16)%>%
    add_trace(x = ~MN_F, y = ~MN, type = "scatter", mode = "markers", size = ~(UQ_F-LQ_F),
              marker = list(line = list(color = "black"),
                            sizemode = "diameter",
                            colorbar = list(y = 0.75, x = 0.85,
                                            title = "Survey Effort")
                            ),
              color = ~ log(EFFORT), name = "Annual Eagle Fatalities",
              text = ~paste("Effort =", round(EFFORT,2), "hr*km<sup>3</sup>",
                            "<br>80% CI =", round(LQ_F,2),"-", round(UQ_F,2)),
              hoverinfo = "text")%>%
    add_trace(x = ~c(0, max(MN_F)), y = ~c(0, max(MN_F)), type = "scatter", mode = "lines", name = "1:1 Line")%>%
    layout(hovermode = "closest", font = list(color = "black"),
           xaxis = list(title = "Estimates using Priors"),
           yaxis = list(title = "Estimates without Priors"),
           legend = list(x = 0.10, y = 0.95, bordercolor = "black", borderwidth = 1))

  plot_ly(Bay16)%>%
    add_trace(x = ~c(0, max(MN_F)), y = ~c(0, max(MN_F)), type = "scatter", mode = "lines", name = "1:1 Line")%>%
    add_trace(x = ~MN_F, y = ~MN, type = "scatter", mode = "markers", size = ~OBS_MIN,
              marker = list(line = list(color = "black"),
                            colorbar = list(y = 0.75, x = 0.85,
                                            title = "Survey Effort")
              ),
              color = ~ EFFORT, name = "Annual Eagle Fatalities",
              error_y = list(type = 'array',
                             array = ~(UQ - MN),
                             arrayminus = ~(MN - LQ),
                             color = ~ "black"),
              error_x = list(type = 'array',
                             array = ~(UQ_F - MN_F),
                             arrayminus = ~(MN_F - LQ_F),
                             color = ~ "black"),
              text = ~paste("Effort =", round(EFFORT,2), "hr*km<sup>3</sup>",
                            "<br>80% CI =", round(LQ_F,2),"-", round(UQ_F,2)),
              hoverinfo = "text")%>%
        layout(hovermode = "closest", font = list(color = "black"),
           xaxis = list(title = "Estimates using Priors"),
           yaxis = list(title = "Estimates without Priors"),
           legend = list(x = 0.10, y = 0.95, bordercolor = "black", borderwidth = 1))

hist(((Bay16$MN_F/Bay16$SCALE) - (Bay16$MN/Bay16$SCALE))/(Bay16$MN/Bay16$SCALE))
summary(glm((MN_F - MN)/MN~a*b, data = sim))


plot_ly(Bay16)%>%
  add_trace(x = ~EFFORT, y = ~(MN_F/SCALE - MN/SCALE)/(MN/SCALE), type = "scatter", mode = "markers",
            size = ~ (UQ_F-LQ_F)/MN_F, color = ~FLIGHT_MIN,
            text = ~paste("Effort =", round(EFFORT, 3)), "hr*km<sup>3</sup>",
            hoverinfo = 'text')%>%
  layout(hovermode = 'closest',
         font = list(color = 'black'),
         xaxis = list(title = "Survey Effort (hr*km<sup>3</sup>)"
                      ),
         yaxis = list(title = "Standardized Deviance")
  )

plot_ly(Bay16)%>%
  add_trace(x = ~EFFORT, y = ~ (UQ_F-LQ_F)/MN_F, type = "scatter", mode = "markers")

plot_ly(sim)%>%
  add_trace(x = ~MN_F, y = ~MN, type = "scatter", mode = "markers", size = ~b,
            marker = list(line = list(color = "black"),
                          sizemode = "diameter",
                          colorbar = list(y = 0.75, x = 0.85,
                                          title = "Eagle Activity<br>Rate")
            ),
            sizes = c(5,15), color = ~ eagle_rate, name = "Estimated Eagle Fatalities",
            text = ~paste("Effort =", round(b, 3), "hr*km<sup>3</sup>", "<br>Eagles =", eagle_rate),
            hoverinfo = "text")%>%
  add_trace(x = ~c(0, max(MN_F)), y = ~c(0, max(MN_F)), type = "scatter", mode = "lines", name = "1:1 Line")%>%
  add_trace(x = ~c(0.002, 0.006), y = ~c(0.004, 0.004), type = "scatter", mode = "lines", name = "Mean")%>%
  layout(hovermode = "closest", font = list(color = "black"),
         xaxis = list(title = "Estimates using Priors"),
         yaxis = list(title = "Estimates without Priors"),
         legend = list(x = 0.10, y = 0.95, bordercolor = "black", borderwidth = 1))

plot_ly(sim)%>%
  add_trace(x = ~b, y = ~(MN_F-MN), type = "scatter", mode = "markers",
            color = ~ eagle_rate,
            marker = list(colorbar = list(title = "Eagle Obs<br>(min)")
            ),
            text = ~paste("Effort =", round(b, 3), "hr*km<sup>3</sup>", "<br>Eagles =", eagle_rate),
            hoverinfo = 'text')%>%
  layout(hovermode = 'closest',
         font = list(color = 'black'),
         xaxis = list(title = "Survey Effort (hr*km<sup>3</sup>)"
         ),
         yaxis = list(title = "Standardized Deviance")
  )

plot_ly(sim)%>%
  add_trace(x = ~b, y = ~UQ_F-LQ_F, type = "scatter", mode = "markers",
            color = ~ eagle_rate,
            marker = list(colorbar = list(title = "Eagle Obs<br>(min)")
            ),
            text = ~paste("Effort =", round(b, 3), "<br>Eagles =", a), "hr*km<sup>3</sup>",
            hoverinfo = 'text')%>%
  layout(hovermode = 'closest',
         font = list(color = 'black'),
         xaxis = list(title = "Survey Effort (hr*km<sup>3</sup>)"
         ),
         yaxis = list(title = "Size of 80% CI")
  )

plot_ly(sim)%>%
  add_trace(x = ~eagle_rate, y = ~abs(MN_F-MN)/MN, type = "scatter", mode = "markers",
            color = ~ b,
            marker = list(colorbar = list(title = "Survey Effort<br>(hr*km<sup>3</sup>)")
            ),
            text = ~paste("Effort =", round(b, 3), "<br>Eagles =", a), "hr*km<sup>3</sup>",
            hoverinfo = 'text')%>%
  layout(hovermode = 'closest',
         font = list(color = 'black'),
         xaxis = list(title = "Eagle Obs (min)"
         ),
         yaxis = list(title = "Standardized Deviance")
  )

plot_ly(sim)%>%
  add_trace(x = ~a, y = ~(UQ_F-LQ_F)/MN_F, type = "scatter", mode = "markers",
            color = ~ b,
            marker = list(colorbar = list(title = "Survey Effort<br>(hr*km<sup>3</sup>)")
            ),
            text = ~paste("Effort =", round(b, 3), "<br>Eagles =", a), "hr*km<sup>3</sup>",
            hoverinfo = 'text')%>%
  layout(hovermode = 'closest',
         font = list(color = 'black'),
         xaxis = list(title = "Eagle Obs (min)"
         ),
         yaxis = list(title = "Size of 80% CI")
  )


