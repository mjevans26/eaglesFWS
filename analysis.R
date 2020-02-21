library(Deriv)
library(plyr)
library(dplyr)
library(plotly)
library(reshape2)
library(stringr)
library(tidyr)
library(rv)
library(viridis)

source("C:/Users/mevans/repos/eaglesFWS/inst/eagles-fws/helper_fxns.R")

site_preds18 <- vapply(1:nrow(Bay16), function(x) {
 #a <- sum(Bay16$FLIGHT_MIN) + Bay16$FLIGHT_MIN[x]
 #b <- sum(Bay16$EFFORT) + Bay16$EFFORT[x]
  out <- simFatal(BMin = Bay16$FLIGHT_MIN[x],
                  Fatal = -1,
                  SmpHrKm = Bay16$EFFORT[x],
                  ExpFac = Bay16$SCALE[x],
                  aPriExp = 11.81641,
                  bPriExp = 9.7656250,
                  aPriCPr = 1.638029,
                  bPriCPr = 290.0193,
                  iters = 1000)
  #out <- prediction(10000, a, b)
  #fatality <- mean(out$fatality)
  fatality <- mean(out$fatality)
  q80 <- quantile(out$fatality, c(0.8))
  out2 <- simFatal(BMin = Bay16$FLIGHT_MIN[x],
                   Fatal = -1,
                   SmpHrKm = Bay16$EFFORT[x],
                   ExpFac = Bay16$SCALE[x],
                   aPriExp = 0,
                   bPriExp = 0,
                   aPriCPr = 1.638029,
                   bPriCPr = 290.0193,
                   iters = 1000)
  fatality2 <- mean(out2$fatality)
  q82 <- quantile(out2$fatality, c(0.8))
  return (c(fatality, q80, fatality2, q82))
}, USE.NAMES = FALSE, FUN.VALUE = c(0,0,0,0))

Bay16$MN_F <- site_preds18[1,]
#Bay16$LQ_F <- site_preds[2,]
Bay16$UQ_F <- site_preds18[2,]
Bay16$MN <- site_preds18[3,]
#Bay16$LQ <- site_preds[5,]
Bay16$UQ <- site_preds18[4,]
#Bay16$U_F <- site_preds[7,]
#Bay16$U <- site_preds[8,]

#function to return mean and 80% CI estimates from a predicted fatality distribution
#hardwired to use Bay16 data as exposure priors
estimates <- function(iters, a, b){
  out <- simFatal(BMin = a,
                  Fatal = -1,
                  SmpHrKm = b,
                  ExpFac = mean(Bay16$SCALE),
                  aPriExp = 11.81641,
                  bPriExp = 9.7656250,
                  aPriCPr = 1.638029,
                  bPriCPr = 290.0193,
                  iters = iters)
  fatality <- mean(out$fatality)
  q80 <- quantile(out$fatality, c(0.8))
  out2 <- simFatal(BMin = a,
                  Fatal = -1,
                  SmpHrKm = b,
                  ExpFac = mean(Bay16$SCALE),
                  aPriExp = 0,
                  bPriExp = 0,
                  aPriCPr = 1.638029,
                  bPriCPr = 290.0193,
                  iters = iters)
  fatality2 <- mean(out2$fatality)
  q82 <- quantile(out2$fatality, 0.8)
  return (c("MN_F" = fatality, "UQ_F" = q80, "MN" = fatality2, "UQ" = q82))
}

#cretae simulated eagle flight observation and survey data
#values based off of minimum survey requirements:
#1hr per plot per month for two years
#survey plot of 0.8km radius * 0.2km height

flight <- c(0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.10,0.15,0.20,0.25,0.50,0.75,1.00,1.50,2.00,2.50,3)
#Survey time in hours
time <- seq(1, 10, 1)*12*2
#Survey area in km3
area <- seq(0.402, 2.01, 0.402)
df <- expand.grid(TIME = time, AREA = area, eagle_rate = flight)
#Effort is time(hrs)*area(km3)
df$b <- df$TIME*df$AREA
#Eagle activity is min/hr*km3
df$a <- df$eagle_rate*df$b

uppers <- vapply(1:nrow(df), function(x) {
  a <- df$a
  b <- df$b
  out <- prediction(10000, a+sum(Bay16$FLIGHT_MIN), b+sum(Bay16$EFFORT))
  q80 <- quantile(out$fatality, 0.8)
  out2 <- prediction(10000, a, b)
  q82 <- quantile(out2$fatality, 0.8)
  return (c("U_F" = q80, "U" = q82))
}, USE.NAMES = FALSE, FUN.VALUE = c(0,0))

#create dataset of estimated fatality distributions based on simulated
#survey and flight observation data

sim2 <- plyr::mdply(df[, c(5, 4)], estimates, niters = 10000)
sim <- plyr::mdply(df[, c(5, 4)], estimates, niters = 10000)
#sim 3 run with new priors
sim3 <- plyr::mdply(df[,c(5, 4)], estimates, iters = 10000)
sim3$eagle_rate <- df$eagle_rate
colnames(sim3) <- c("a", "b", "MN_F", "UQ_F", 'MN', "UQ", 'eagle_rate')
colnames(sim2)[c(4,5,7,8)] <- c("LQ_F", "UQ_F", "LQ", "UQ")

#Lets look at the case where there are zero eagles observed
#We want to plot, for a variety of project sizes, predicted take/effort
zeroest <- function(iters, Effort, ExpFac){
  out <- simFatal(BMin = 0,
                  Fatal = -1,
                  SmpHrKm = Effort,
                  ExpFac = ExpFac,
                  aPriExp = 11.81641,
                  bPriExp = 9.7656250,
                  aPriCPr = 1.638029,
                  bPriCPr = 290.0193,
                  iters = iters)
  fatality <- mean(out$fatality)
  q80 <- quantile(out$fatality, c(0.8))
  return (c("MN_F" = fatality, "UQ_F" = q80))
}

zerodf <- expand.grid(Time = time, Area = area, Hectares = seq(50, 400, 50))%>%
  mutate(Effort = Time * Area, ExpFac = 0.005648*(-22558 + 2306*Hectares - 2.61*I(Hectares^2)))

zerosim <- plyr::mdply(zerodf[, 4:5], zeroest, iters = 10000)

zerosim$Hectares <- zerodf$Hectares

plot_ly(Bay16)%>%
    add_trace(x = ~MN_F, y = ~MN, type = "scatter", mode = "markers", size = ~(((UQ_F-MN_F)/(MN_F))/1000),
              marker = list(line = list(color = "black"),
                            sizemode = "diameter",
                            colorbar = list(y = 0.55, x = 0.1,
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
           legend = list(x = 0.30, y = 0.95, bordercolor = "black", borderwidth = 1))

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

#Scatter plot estimates with priors vs. without priors for simulated data. Color by eagle rate, size by effort. 1:1 Line for comparison
plot_ly(sim3)%>%
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

#Plot deviance between estimates using priors and site-specific estimate as a function of survey effort from simulated data. Color by eagle rate
plot_ly(sim3)%>%
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

#Scatter plot size of 80% CI as a function of survey effort for simulated data. Color by eagle rate
plot_ly(sim)%>%
  add_trace(x = ~b, y = ~UQ_F-MN_F, type = "scatter", mode = "markers",
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

#scatter plot |Standard deviance| between priors and site-specific estimates as a function of eagle activity. Color by effort
plot_ly(sim3)%>%
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


plot_ly(sim3)%>%
  add_trace(x = ~a, y = ~(UQ_F-MN_F)/MN_F, type = "scatter", mode = "markers",
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

#
plot_ly(sim3)%>%
  #x is z-score standard deviations of eagle activity above/below mean of exposure prior
  # 1.099637 is mean of exposure prior, 0.1094509 is sd (citation?)
  add_trace(x = ~(eagle_rate - 1.099637)/0.1094509,
            y = ~(MN_F-MN), type = "scatter", mode = "markers",
            color = ~ b,
            marker = list(colorbar = list(title = "Survey Effort<br>(hr*km<sup>3</sup>)")
            ),
            text = ~paste("Effort =", round((eagle_rate - 1.099637)/0.1094509, 3), "<br>Eagles =", MN_F-MN), "hr*km<sup>3</sup>",
            hoverinfo = 'text')%>%
  layout(hovermode = 'closest',
         font = list(color = 'black'),
         xaxis = list(title = "Eagle Rate Z-score"
         ),
         yaxis = list(title = "Discrepancy")
  )
glm(data = sim, (MN_F - MN) ~ (eagle_rate - 1.099637)/0.1094509)

#PLOT FOR ZERO OBSERVED EAGLES
plot_ly(data = zerosim, x = ~Effort, y = ~UQ_F,
        color = ~Hectares, type = 'scatter', mode = 'marker',
        marker = list(colorbar = list(x = 0.7, y = 1, title = "Project<br>Size (ha)")),
        hoverinfo = 'text',
        text = ~paste("Effort:", Effort, "hr*km<sup>3</sup>", "<br>Project Size:", Hectares, "ha", "<br>Fatalities:", round(UQ_F, 1), "eagles"))%>%
  layout(legend = list(x = 0.7, y = 1),
         xaxis = list(title = "Effort (hr*km2)"),
         yaxis = list(title = "Predicted Annual Eagle Fatalities"))

#COST BENEFIT ANALYSIS
#Data from West
monitor_costs <- list('annual_low_ppt' = 2000, 'annual_high_ppt' = 5000,
              'annual_low_pMW' = 300, 'annual_high_pMW' = 600)

#From Adt report
retro_costs <- list('Low' = 1040, 'High' = 2590)
electro_rates <- list('Low' = 0.0036, 'Median' = 0.0051, 'High' = 0.0066)
durations <- c(10, 20, 30, 40, 50)

#Read in table of total mitigation costs per eagle from ABT report for different durations & cost estimates
cost_table <- read.csv(file = 'data/ABT_REA_costs.csv', header = TRUE)

#low cost estimate, 20yr duration, adult golden eagle (0.0051)

15200
38000

#Can't just plug in an observed eagle activity and find the root across efforts - they are dependent.  Need to use
#Underlying eagle 'rate'

#experiment with different ways to find the minimum of the cost function
optim(par = 18.4, cost_fxn, method = "L-BFGS-B", lower = 0, upper = 10)
uniroot(cost_fxn, interval = c(0, 500), data = data.frame(activity = 1, size = 10))
optimize(cost_fxn, interval = c(0, 500), data = data.frame(a = median(df$a), size = median(Bay16$SCALE)))

#For testing purposes, assume all turbines are 200m tall w/80m blades
test_values <- expand.grid(erate = seq(0,3,0.05), size = seq(20, 500, 20)*3650*(0.2)*(0.08^2)*pi)
test_values <- mutate(test_values, mrate = 38000, srate = 167)
#test_cost_surface <- plyr::mdply(test_values, optim_fxn)

test_cost_surface <- plyr::mdply(test_values, min_cost)
colnames(test_cost_surface) <- c("erate", 'size', 'effort', 'cost')

plot_ly(type = 'contour', z = acast(test_cost_surface, erate~size, value.var = "effort"),
        y = seq(0,3,0.01), x = seq(20,500,20),
        colorbar = list(title = 'Survey<br>Effort<br>(hr*km3)'),
        autocontour = FALSE,
        contours = list(
          start = 201,
          end = 201,
          size = 1,
          coloring = 'heatmap'),
        line = list(smoothing = 5)
        )%>%
  layout(
    yaxis = list(title = 'Eagle Activity Rate (min/hr*km3)'),
    xaxis = list(title = 'Project Size (# Turbines)')
  )


plot_ly(type = 'heatmap', z = acast(low_cost_surface, erate~size, value.var = "effort"),
        y = seq(0,3,0.05), x = seq(20,500,20),
        colorbar = list(title = 'Survey<br>Effort<br>(hr*km<sup>3</sup>)'))%>%
  layout(
    yaxis = list(title = 'Eagle Activity Rate (min/hr*km<sup>3</sup>)',
                 titlefont = list(color = 'black', size = 14),
                 tickfont = list(color = 'black', size = 12)),
    xaxis = list(title = 'Project Size (# Turbines)',
                 titlefont = list(color = 'black', size = 14),
                 tickfont = list(color = 'black', size = 12))
  )

#For a given project size, how does the min effort change with eagle rate
plot_ly(type = 'scatter', mode = 'lines')%>%
  add_trace(data = filter(test_cost_surface, size == unique(test_values$size)[1]),
        x = ~erate, y = ~effort)%>%
  add_trace(data = filter(test_cost_surface, size == unique(test_values$size)[5]),
        x = ~erate, y = ~effort)%>%
  add_trace(data = filter(test_cost_surface, size == unique(test_values$size)[10]),
            x = ~erate, y = ~effort)%>%
  add_trace(data = filter(test_cost_surface, size == unique(test_values$size)[15]),
            x = ~erate, y = ~effort)%>%
  add_trace(data = filter(test_cost_surface, size == unique(test_values$size)[20]),
            x = ~erate, y = ~effort)%>%
  add_trace(data = filter(test_cost_surface, size == unique(test_values$size)[25]),
            x = ~erate, y = ~effort)


sub_test <- filter(test_values, erate %in% c(0, 0.5, 1.0, 1.5, 2.0, 2.5),
                   round(size, 4) %in% c(293.5504, 1761.3025, 3816.1554, 5871.0084, 7338.7604))


#Example curves showing effort/cost tradeoff
multiplot <- function(i){
  cst <- curve(cost(x, sub_test$erate[i], sub_test$size[i], 'Total'),
               from = 0, to = 500)
  mon <- curve(cost(x, sub_test$erate[i], sub_test$size[i], 'M'),
               from = 0, to = 500)
  surv <- curve(cost(x, sub_test$erate[i], sub_test$size[i], 'S'),
                from = 0, to = 500)
  cols <- viridis(5)
  plot_ly(type = 'scatter', mode = 'lines')%>%
    add_trace(
      x = ~cst$x, y = ~cst$y,
      #line = list(color = cols[(i-1)%/%6], width = ((i-1)%%6 +1)),
      line = list(color = 'orange'),
      showlegend = FALSE,
      name = 'Total'
      )%>%
    add_trace(
      x = ~mon$x, y = ~mon$y,
      #line = list(color = cols[(i-1)%/%6], width = ((i-1)%%6 +1), dash = 'dash'),
      line = list(color = 'grey', dash = 'dash'),
      showlegend = FALSE,
      name = "Mitigation"
    )%>%
    add_trace(
      x = ~surv$x, y = ~surv$y,
      #line = list(color = cols[(i-1)%/%6], width = ((i-1)%%6 +1), dash = 'dot'),
      line = list(color = 'blue', dash = 'dot'),
      showlegend = FALSE,
      name = 'Survey'
    )%>%
    add_trace(
      x = c(10, 10), y = c(0, max(cst$y)),
      line = list(color = 'black', width = 1),
      showlegend = FALSE
    )
}

s1 <- subplot(lapply(1:nrow(sub_test), multiplot),
              nrows = 5, shareY = TRUE,
              titleY = TRUE, shareX =TRUE,titleX= TRUE)%>%
  layout(yaxis = list(title = ''),
         yaxis2 = list(title = ''),
         yaxis3 = list(title = 'Cost ($)'),
         yaxis4 = list(title = ''),
         yaxis5 = list(title = ''),
         xaxis = list(title = ''),
         xaxis2 = list(title = ''),
         xaxis3 = list(title = 'Effort (hr*km<sup>3</sup>)'),
         xaxis4 = list(title = ''),
         xaxis5 = list(title = ''),
         xaxis6 = list(title = ''))

# pre calculate matrices representing optimized effort for different combinations of
# estimated mitigation and survey costs By default, low and high values are defined for
# median electrocution rate (0.0051 eagles/pole*yr) and 20 year retrofit duration
# TODO: incorporate different durations?
low_low <- mutate(test_values, mrate = 15200, srate = 167)%>%
  plyr::mdply(min_cost)
low_high <- mutate(test_values, mrate = 15200, srate = 417)%>%
  plyr::mdply(min_cost)
high_low <- mutate(test_values, mrate = 38000, srate = 167)%>%
  plyr::mdply(min_cost)
high_high <- mutate(test_values, mrate = 38000, srate = 417)%>%
  plyr::mdply(min_cost)

# optimization to minimize eagle death

