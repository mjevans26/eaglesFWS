library(Deriv)
library(plyr)
library(dplyr)
library(plotly)
library(reshape2)
library(stringr)
library(tidyr)
library(rv)
library(viridis)

source("C:/Users/mevans/repos/eaglesFWS/R/helper_fxns.R")
load('vignettes/data/newdata.RData')

# define plot styles
# axis format
ax <- list(
  titlefont = list(family = 'serif', color = 'black', size = 18),
  tickfont = list(family = 'serif', color = 'black', size = 14),
  zerolinewidth = 2,
  zerolinecolor = 'black',
  showgrid = FALSE
)

# colorbar format
leg <- list(
  font = list(family = 'serif', color = 'black', size = 14)
)

# define annotation for FWS minimum survey effort
a <- list(
  x = 10,
  y = 0,
  text = 'FWS minimum',
  font = list(color = 'black', size = 12),
  xref = "x",
  yref = "y",
  xanchor = 'left',
  showarrow = TRUE,
  arrowhead = 0,
  ax = 0,
  ay = -60
)

# add orca command line utility to R environmental path
Sys.setenv("PATH" = paste(Sys.getenv("PATH"), "C:\\Users\\mevans\\AppData\\Local\\Programs\\orca", sep = .Platform$path.sep))

site_preds18 <- vapply(1:nrow(Bay16), function(x) {
 #a <- sum(Bay16$FLIGHT_MIN) + Bay16$FLIGHT_MIN[x]
 #b <- sum(Bay16$EFFORT) + Bay16$EFFORT[x]
  out <- simFatal(BMin = Bay16$FLIGHT_MIN[x],
                  Fatal = -1,
                  SmpHrKm = Bay16$EFFORT[x],
                  ExpFac = Bay16$SCALE[x],
                  aPriExp = goldExposure$shape,
                  bPriExp = goldExposure$rate,
                  aPriCPr = goldCollision$shape,
                  bPriCPr = goldCollision$rate,
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
                   aPriCPr = goldCollision$shape,
                   bPriCPr = goldCollision$rate,
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

#cretae simulated eagle flight observation and survey data
#values based off of minimum survey requirements:
#1hr per plot per month for two years
#survey plot of 0.8km radius * 0.2km height

# create a range of underlying eagle activity rates (min/hr*km3)
erate <- seq(0, 3, 0.1)

# Survey time in hours - 24 to 240 hours
time <- seq(0, 240, 24)

#Survey area in km3 - 1 to five plots
area <- seq(0.402, 2.01, 0.402)

# Create dataframe including all combinations of survey time, area, and eagle activity
df <- expand.grid(TIME = time, AREA = area, erate = erate)%>%
  # Effort is time(hrs)*area(km3)
  mutate(b = TIME*AREA)%>%
  # Calculate observed eagle activity in min
  mutate(a = b*erate)

# create supplemental dataframe to illustrate behavior at low efforts
suppDf <- expand.grid(TIME = seq(0, 24, 2), AREA = area, erate = erate)%>%
  mutate(b = TIME*AREA)%>%
  mutate(a = b*erate)

df <- bind_rows(df, suppDf)%>%
  mutate(index = paste(a, b, sep = "_"))%>%
  filter(!duplicated(index))


rm(suppDf)
# save our simulated scenarios
saveRDS(df, file = 'data/simData.rds')

# generate theoretical curves of discrepancy vs. effort
p <- plot_ly(type = 'scatter', mode = 'lines')
for(i in seq(0, 2, 0.2)){
  crv <- curve(effort_discrepancy_slope(i, goldExposure$rate, goldExposure$shape, x), from = 0, to = 100)
  p <- add_trace(p, x = crv$x, y = crv$y)
}

# DEPRICATED
# return upper quantiles for simulation data
# uppers <- vapply(1:nrow(df), function(x) {
#   a <- df$a
#   b <- df$b
#   out <- prediction(10000, a+sum(Bay16$FLIGHT_MIN), b+sum(Bay16$EFFORT))
#   q80 <- quantile(out$fatality, 0.8)
#   out2 <- prediction(10000, a, b)
#   q82 <- quantile(out2$fatality, 0.8)
#   return (c("U_F" = q80, "U" = q82))
# }, USE.NAMES = FALSE, FUN.VALUE = c(0,0))

# Predict fatalities with and without using priors for simulated scenarios
sim <- plyr::mdply(df[,c(5, 4)], estimates, niters = 10000, nturbines = 200)%>%
  # add erate, TIME, AREA columns
  bind_cols(df[, 1:3])

colnames(sim) <- c("a", "b", "MN_F", "UQ_F", 'MN', "UQ", 'time', 'area', 'eagle_rate')

# save outputs
saveRDS(sim, file = 'data/simResults_95.rds')
sim <- readRDS(file = 'data/simResults_95.rds')

# Evaluate cases where there are zero eagles observed
zeroest <- function(iters, Effort, ExpFac){
  out <- simFatal(BMin = 0,
                  Fatal = -1,
                  SmpHrKm = Effort,
                  ExpFac = ExpFac,
                  aPriExp = expose$shape,
                  bPriExp = expose$rate,
                  aPriCPr = collide$shape,
                  bPriCPr = collide$rate,
                  iters = iters)
  fatality <- mean(out$fatality)
  q80 <- quantile(out$fatality, c(0.8))
  return (c("MN_F" = fatality, "UQ_F" = q80))
}

# Create a simulated dataset of varying efforts at different project sizes
zerodf <- expand.grid(Time = time, Area = area, nTurb = seq(50, 400, 50))%>%
  mutate(Effort = Time * Area, ExpFac = turbines_to_size(nTurb))

# Predict fatalities using priors assuming no eagles are observed
zerosim <- plyr::mdply(zerodf[, 4:5], zeroest, iters = 10000)
colnames(zerosim)[4] <- "UQ_F"
zerosim$ExpFac <- zerodf$ExpFac

# save zerosim data
saveRDS(zerosim, file = 'data/zeroSim_95.rds')
zerosim <- readRDS(file = 'data/zeroSim_95.rds')

# Is it better to add plots, or hours?

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
fig1a <- plot_ly(filter(sim, b > 0, b < 100))%>%
  add_trace(y = ~UQ_F, x = ~UQ, size = ~b,
            type = "scatter", mode = "markers",
            marker = list(line = list(color = "black"),
                          sizemode = "diameter"
            ),
            sizes = c(5,15), color = ~ eagle_rate,
            name = "Survey effort",
            text = ~paste("Effort =", round(b, 3), "hr*km<sup>3</sup>", "<br>Eagles =", eagle_rate),
            hoverinfo = "text")%>%
  add_trace(x = ~c(0, max(UQ_F)), y = ~c(0, max(UQ_F)),
            type = "scatter", mode = "lines", name = "1:1 Line")%>%
  colorbar(x = 0.8, y = 0.6, title = 'Eagle activity<br>rate (min/hr)',
           titlefont = list(family = 'serif', color = 'black', size = 16))%>%
  #add_trace(x = ~c(0.002, 0.006), y = ~c(0.004, 0.004), type = "scatter", mode = "lines", name = "Mean")%>%
  layout(hovermode = "closest", font = list(color = "black"),
         yaxis = append(list(title = 'Fatalities estimated with priors'), ax),
         xaxis = append(list(title = 'Fatalities estimated without priors'), ax),
         legend = list(x = 0.10, y = 0.95, bordercolor = "black", borderwidth = 1,
                       font = list(family = 'serif', size = 16, color = 'black'),
                       showgridlines = FALSE))

#scatter plot |Standard deviance| between priors and site-specific estimates as a function of eagle activity. Color by effort
fig1b <- plot_ly(filter(sim, b >0, b %% 9.648 == 0))%>%
  add_trace(x = ~eagle_rate, y = ~(UQ_F-UQ)/UQ,
            type = "scatter", mode = "markers",
            color = ~ b,
            marker = list(colorbar = list(title = "Survey Effort<br>(hr*km<sup>3</sup>)")
            ),
            text = ~paste("Effort =", round(b, 3), "hr*km<sup>3</sup><br>Eagles =", a, "(min)<br>Deviance = ", (UQ_F-UQ)/UQ),
            hoverinfo = 'text')%>%
  colorbar(x = 0.8, y = 0.8,
           title = 'Survey<br>effort (hr*km<sup>3</sup>)',
           titlefont = list(family = 'serif', color = 'black', size = 16))%>%
  layout(hovermode = 'closest',
         font = list(color = 'black'),
         xaxis = append(list(title = "Eagle Obs (min)"), ax),
         yaxis = append(list(title = "Standardized Deviance"), ax)
  )

#Plot deviance between estimates using priors and site-specific estimate as a function of survey effort from simulated data. Color by eagle rate
fig2a <- plot_ly(filter(sim, b >0, b < 100, eagle_rate%%0.2 == 0))%>%
  add_trace(x = ~b, y = ~(UQ_F-UQ), type = "scatter", mode = "markers",
            color = ~ eagle_rate,
            marker = list(colorbar = list(title = "Eagle Obs<br>(min)")),
            text = ~paste("Effort =", round(b, 3), "hr*km<sup>3</sup>", "<br>Eagles =", eagle_rate),
            hoverinfo = 'text')%>%
  colorbar(title = 'Eagle activity<br>rate (min/hr)', x = 0.8, y = 1,
           titlefont = list(family = 'serif', size = 14, color = 'black'))%>%
  layout(hovermode = 'closest',
         annotations = a,
         font = list(color = 'black'),
         xaxis = append(list(title = "Survey effort (hr*km<sup>3</sup>)"), ax),
         yaxis = append(list(title = "Difference in estimated fatalities (# eagles)"), ax)
  )

p <- plot_ly(type = 'scatter', mode = 'lines')
for(i in seq(0, 2, 0.2)){
  crv <- curve(effort_discrepancy_slope(i, goldExposure$shape, goldExposure$rate, x)*turbines_to_size(200), from = 0, to = 100)
  p <- add_trace(p, x = crv$x, y = crv$y)
}

plot_ly(d, type = 'scatter', mode = 'lines', x = ~x, y = ~y, color = ~eagle_rate)

#PLOT FOR ZERO OBSERVED EAGLES
fig2b <- plot_ly(data = filter(zerosim, Effort > 0.402, Effort <= 100),
                 x = ~Effort, y = ~UQ_F,
                color = ~ExpFac/expFac, type = 'scatter', mode = 'markers',
                #marker = list(colorbar = list(x = 0.7, y = 1, title = "Project<br>Size (ha)")),
                hoverinfo = 'text',
                text = ~paste("Effort:", Effort, "hr*km<sup>3</sup>", "<br>Project Size:", ExpFac/expFac, "turbines", "<br>Fatalities:", round(UQ_F, 1), "eagles"))%>%
  colorbar(x = 0.8, y = 1, title = 'Project size<br>(# turbines)<br>',
           font = list(family = 'serif', size = 14, color = 'black'))%>%
  layout(legend = list(x = 0.7, y = 1),
         annotations = a,
         xaxis = append(list(title = "Effort (hr*km<sup>3</sup>)"), ax),
         yaxis = append(list(title = "Estimated eagle fatalities"), ax))

h#Scatter plot size of 80% CI as a function of survey effort for simulated data. Color by eagle rate
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

#COST BENEFIT ANALYSIS
# Create a simulation dataset of project sizes and true eagle activity rates
# For testing purposes, assume all turbines are 200m tall w/80m blades
test_values <- expand.grid(erate = seq(0,3,0.05),
                           size = turbines_to_size(seq(20, 500, 20)))

#Estimated survey cost data from West Ecosystems Inc.
survey_costs <- list('annual_low_ppt' = 2000, 'annual_high_ppt' = 5000,
                     "Low" = 2000/12, 'High' = 5000/12,
                     'annual_low_pMW' = 300, 'annual_high_pMW' = 600)

#From Adt report
retro_costs <- list('Low' = 1040, 'High' = 2590)
electro_rates <- list('Low' = 0.0036, 'Median' = 0.0051, 'High' = 0.0066)
durations <- c(10, 20, 30, 40, 50)

#Read in table of total mitigation costs per eagle from ABT report for different durations & cost estimates
cost_table <- read.csv(file = 'data/ABT_REA_costs.csv', header = TRUE)

# create dataframe of levels of effort
effort_df <- data.frame(effort = seq(0, 200, 2))

#' Create line plots showing cost vs. effort curves
#' @param erate true eagle activity rate (min/hr*km3)
#' @param nturb number of turbines at hypothetical project
#' @param mcost assumed per-retrofit mitigation cost
#' @param scost assumed per-hour survey cost
#' @return plotly line plot
#' @example
#' plot_curves(2, 200, retro_costs$High, survey_costs$High)
plot_curves <- function(erate, nturb, mcost, scost){
  size <- nturb*expFac
  output <- plyr::mdply(effort_df, cost_curve, erate, size, mcost, scost)
  min_effort <- output$effort[output$T == min(output$T)]
  plot_ly(data = output, type = 'scatter', mode = 'lines')%>%
    add_trace(
      x = ~effort, y = ~T,
      #line = list(color = cols[(i-1)%/%6], width = ((i-1)%%6 +1)),
      line = list(color = 'orange'),
      showlegend = TRUE,
      name = 'Total'
    )%>%
    add_trace(
      x = ~effort, y = ~M,
      #line = list(color = cols[(i-1)%/%6], width = ((i-1)%%6 +1), dash = 'dash'),
      line = list(color = 'grey', dash = 'dash'),
      showlegend = TRUE,
      name = "Mitigation"
    )%>%
    add_trace(
      x = ~effort, y = ~S,
      #line = list(color = cols[(i-1)%/%6], width = ((i-1)%%6 +1), dash = 'dot'),
      line = list(color = 'blue', dash = 'dot'),
      showlegend = TRUE,
      name = 'Survey'
    )%>%
    add_trace(
      x = c(min_effort, min_effort), y = c(0, max(output$T)),
      line = list(color = 'black', width = 1),
      name = paste('Min cost effort (', min_effort, ' hrs)', sep = "")
    )%>%
    layout(
      xaxis = list(title = 'Survey effort (hr*km<sup>3</sup>)'),
      yaxis = list(title = 'Cost ($)'),
      #annotations = a,
      legend = list(x = 0.2, y = 1)
    )
}

#experiment with different ways to find the minimum of the cost function
# TODO: These all have problems because the stochasticity involved in gibbs sampling creates erratic curves
optim(par = 18.4, cost_fxn, method = "L-BFGS-B", lower = 0, upper = 10)
uniroot(cost_fxn, interval = c(0, 500), data = data.frame(activity = 1, size = 10))
optimize(cost_fxn, interval = c(0, 500), data = data.frame(a = median(df$a), size = median(Bay16$SCALE)))

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


# Lattice of example curves showing effort/cost relationships at a variety of scenarios
multiplot <- function(i){
  x <- seq(0, 200, 1)
  cst <- curve(cost(x, sub_test$erate[i], sub_test$size[i], retro_costs$High, survey_costs$Low)[['T']],
               from = 0, to = 200)
  mon <- curve(cost(x, sub_test$erate[i], sub_test$size[i], retro_costs$High, survey_costs$Low)[['M']],
               from = 0, to = 200)
  surv <- curve(cost(x, sub_test$erate[i], sub_test$size[i], retro_costs$High, survey_costs$Low)[['S']],
                from = 0, to = 200)
  cols <- viridis(5)
  plot_ly(type = 'scatter', mode = 'lines')%>%
    add_trace(
      x = cst$x, y = cst$y,
      #line = list(color = cols[(i-1)%/%6], width = ((i-1)%%6 +1)),
      line = list(color = 'orange'),
      showlegend = FALSE,
      name = 'Total'
      )%>%
    add_trace(
      x = mon$x, y = mon$y,
      #line = list(color = cols[(i-1)%/%6], width = ((i-1)%%6 +1), dash = 'dash'),
      line = list(color = 'grey', dash = 'dash'),
      showlegend = FALSE,
      name = "Mitigation"
    )%>%
    add_trace(
      x = surv$x, y = surv$y,
      #line = list(color = cols[(i-1)%/%6], width = ((i-1)%%6 +1), dash = 'dot'),
      line = list(color = 'blue', dash = 'dot'),
      showlegend = FALSE,
      name = 'Survey'
    )#%>%
    # add_trace(
    #   x = c(10, 10), y = c(0, max(cst$y)),
    #   line = list(color = 'black', width = 1),
    #   showlegend = FALSE
    # )
}

fig3 <- subplot(lapply(1:nrow(sub_test), multiplot),
              nrows = 5, shareY = TRUE,
              titleY = TRUE, shareX =TRUE,titleX= TRUE)%>%
  layout(yaxis = append(list(title = ''), ax),
         yaxis2 = append(list(title = ''),ax),
         yaxis3 = append(list(title = 'Cost ($)'), ax),
         yaxis4 = append(list(title = ''),ax),
         yaxis5 = append(list(title = ''),ax),
         xaxis = append(list(title = ''),ax),
         xaxis2 = append(list(title = ''),ax),
         xaxis3 = append(list(title = 'Effort (hr*km<sup>3</sup>)'), ax),
         xaxis4 = append(list(title = ''),ax),
         xaxis5 = append(list(title = ''),ax),
         xaxis6 = append(list(title = ''),ax))

# Cost Surfaces


# pre calculate matrices representing optimized effort for different combinations of
# estimated mitigation and survey costs By default, low and high values are defined for
# median electrocution rate (0.0051 eagles/pole*yr) and 20 year retrofit duration
# TODO: incorporate different durations?
low_low <- mutate(test_values, mrate = retro_costs$Low, srate = survey_costs$Low)%>%
  plyr::mdply(min_cost)
low_high <- mutate(test_values, mrate = retro_costs$Low, srate = survey_costs$High)%>%
  plyr::mdply(min_cost)
high_low <- mutate(test_values, mrate = retro_costs$High, srate = survey_costs$Low)%>%
  plyr::mdply(min_cost)
high_high <- mutate(test_values, mrate = retro_costs$High, srate = survey_costs$High)%>%
  plyr::mdply(min_cost)

save(high_high, low_low, low_high, high_low, file = 'data/cost_surfaces_95.rdata')
load(file = 'data/cost_surfaces_95.rdata')
# Create heatmap of minimum cost efforts
fig4 <- plot_ly(type = 'heatmap', z = acast(high_low, erate~size, value.var = "effort"),
                y = seq(0,2,0.05), x = seq(20,500,20),
                zmin = 0, zmax = 40, colors = colorRamp(c('black', 'white')))%>%
  colorbar(title = 'Survey effort<br>(hr*km<sup>3</sup>)',
           titlefont = list(family = 'serif', color = 'black', size = 14))%>%
  layout(
    yaxis = append(list(title = 'Eagle activity rate (min/hr*km<sup>3</sup>)'), ax),
    xaxis = append(list(title = 'Project size (# turbines)'), ax)
  )

# Write figures to file for manuscript
orca(fig1a, file = 'Fig1a.png', format = tools::file_ext('png'), scale = 20)
orca(fig1b, file = 'Fig1b.png', format = tools::file_ext('png'), scale = 20)
orca(fig2a, file = 'Fig2a.png', format = tools::file_ext('png'), scale = 20)
orca(fig2b, file = 'Fig2b.png', format = tools::file_ext('png'), scale = 20)
orca(fig3, file = 'Fig3.png', format = tools::file_ext('png'), scale = 20)
orca(fig4, file = 'Fig4.png', format = tools::file_ext('png'), scale = 20)
# optimization to minimize eagle death
# what proportion of simulated scenarios would reduce their costs by continued monitoring

# slope of discrepancy is proportional to (alpha - (rate*beta))/(beta+x)
