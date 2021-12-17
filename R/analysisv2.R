library(Deriv)
library(plyr)
library(dplyr)
library(plotly)
library(reshape2)
library(stringr)
library(tidyr)
library(rv)
library(viridis)

# load helper functions
source("R/helper_fxns.R")

# SIMULATION DATA
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

# Predict fatalities with and without using priors for simulated scenarios
sim <- plyr::mdply(df[,c(5, 4)], estimates, niters = 10000, nturbines = 200)%>%
  # add erate, TIME, AREA columns
  bind_cols(df[, 1:3])

colnames(sim) <- c("a", "b", "CRM_mean", "CRM_80", 'Survey_mean', "Survey_80", 'time', 'area', 'erate')

# save outputs
saveRDS(sim, file = 'data/simResults_5May21.rds')

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
  return (c("CRM_mean" = fatality, "CRM_80" = q80))
}

# Create a simulated dataset of varying efforts at different project sizes
zerodf <- expand.grid(Time = time, Area = area, nTurb = seq(50, 400, 50))%>%
  mutate(Effort = Time * Area, ExpFac = turbines_to_size(nTurb, 100, 50))

# Predict fatalities using priors assuming no eagles are observed
zerosim <- plyr::mdply(zerodf[, 4:5], zeroest, iters = 10000)
colnames(zerosim)[4] <- "CRM_80"
# zerosim$ExpFac <- zerodf$ExpFac
# save zerosim data
saveRDS(zerosim, file = 'data/zeroSim_5May21.rds')

#COST BENEFIT ANALYSIS
# Create a simulation dataset of project sizes and true eagle activity rates
# For testing purposes, assume all turbines are 100m tall w/50m blades
test_values <- expand.grid(erate = seq(0,3,0.05),
                           size = turbines_to_size(seq(20, 500, 20), 100, 50))

#Read in table of total mitigation costs per eagle from ABT report for different durations & cost estimates
cost_table <- read.csv(file = 'data/ABT_REA_costs.csv', header = TRUE)

# create dataframe of levels of effort
effort_df <- data.frame(effort = seq(0, 200, 2))

# create a subset of scenarios for generating a lattice plot of cost/effort curves
sub_test <- filter(test_values, erate %in% c(0, 0.5, 1.0, 1.5, 2.0, 2.5),
                   size %in% turbines_to_size(c(20, 40, 100, 200, 400), 100, 50))

#Estimated survey cost data from West Ecosystems Inc.
survey_costs <- list('annual_low_ppt' = 2000, 'annual_high_ppt' = 5000,
                     "Low" = 2000/12, 'High' = 5000/12,
                     'annual_low_pMW' = 300, 'annual_high_pMW' = 600)

#From Adt report
retro_costs <- list('Low_ppole' = 1040, 'High_ppole' = 2590)
electro_rates <- list('Low' = 0.0036, 'Median' = 0.0051, 'High' = 0.0066)

durations <- c(10, 20, 30, 40, 50)

retro_cost <- filter(cost_table, Duration == 30, Rate == 'Median')

low_low <- mutate(test_values, mrate = retro_cost[retro_cost$Cost == 'Low', 'M'], srate = survey_costs$Low)%>%
  plyr::mdply(high_high[, 1:4], wrapper)
low_high <- mutate(test_values, mrate = retro_cost[retro_cost$Cost == 'Low', 'M'], srate = survey_costs$High)%>%
  plyr::mdply(high_high[, 1:4], wrapper)
high_low <- mutate(test_values, mrate = retro_cost[retro_cost$Cost == 'High', 'M'], srate = survey_costs$Low)%>%
  plyr::mdply(high_high[, 1:4], wrapper)
high_high <- mutate(test_values, mrate = retro_cost[retro_cost$Cost == 'High', 'M'], srate = survey_costs$High)%>%
  plyr::mdply(high_high[, 1:4], wrapper)

# PLOTS
# load our output datsets
sim <- readRDS(file = 'data/simResults_5May21.rds')
zerosim <- readRDS(file = 'data/zeroSim_5May21.rds')
high_high <- read.csv(file = 'data/high_high5May21.csv', header = TRUE, stringsAsFactors = FALSE)%>%
  mutate(maxEagleMitigation = mrate*maxEagle,
         maxEagleSurvey = maxEagleEffort *srate,
         maxEagleCost = maxEagleMitigation + maxEagleSurvey)
# axis format
tickfont = list(family = 'serif', color = 'black', size = 14)
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
  font = list(family = 'serif', color = 'black', size = 12),
  xref = "x",
  yref = "y",
  xanchor = 'left',
  showarrow = TRUE,
  arrowhead = 0,
  ay = -60
)

#Scatter plot estimates with priors vs. without priors for simulated data. Color by eagle rate, size by effort. 1:1 Line for comparison
fig1a <- plot_ly(filter(sim, b > 0, b < 100))%>%
  add_trace(y = ~CRM_80, x = ~Survey_80, size = ~b,
            type = "scatter", mode = "markers",
            marker = list(line = list(color = "black"),
                          sizemode = "diameter"
            ),
            sizes = c(5,15), color = ~ erate,
            name = "Survey effort",
            text = ~paste("Effort =", round(b, 3), "hr*km<sup>3</sup>", "<br>Eagles =", erate),
            hoverinfo = "text")%>%
  add_trace(x = ~c(0, max(CRM_80)), y = ~c(0, max(CRM_80)),
            type = "scatter", mode = "lines", name = "1:1 Line")%>%
  colorbar(x = 0.8, y = 0.6, title = 'Eagle exposure<br>(min/hr*km<sup>3</sup>)',
           titlefont = list(family = 'serif', color = 'black', size = 16),
           tickfont = tickfont)%>%
  #add_trace(x = ~c(0.002, 0.006), y = ~c(0.004, 0.004), type = "scatter", mode = "lines", name = "Mean")%>%
  layout(hovermode = "closest", font = list(color = "black"),
         yaxis = append(list(title = 'Fatalities estimated with priors'), ax),
         xaxis = append(list(title = 'Fatalities estimated without priors'), ax),
         legend = list(x = 0.10, y = 0.95, bordercolor = "black", borderwidth = 1,
                       font = list(family = 'serif', size = 16, color = 'black'),
                       showgridlines = FALSE))

#scatter plot |Standard deviance| between priors and site-specific estimates as a function of eagle activity. Color by effort
fig1b <- plot_ly(filter(sim, b >0, b %% 9.648 == 0))%>%
  add_trace(x = ~erate, y = ~(CRM_80-Survey_80)/Survey_80,
            type = "scatter", mode = "markers",
            color = ~ b,
            marker = list(colorbar = list(title = "Survey Effort<br>(hr*km<sup>3</sup>)")
            ),
            text = ~paste("Effort =", round(b, 3), "hr*km<sup>3</sup><br>Eagles =", a, "(min)<br>Deviance = ", (CRM_80-Survey_80)/Survey_80),
            hoverinfo = 'text')%>%
  colorbar(x = 0.8, y = 0.8,
           title = 'Survey effort<br>(hr*km<sup>3</sup>)',
           titlefont = list(family = 'serif', color = 'black', size = 16))%>%
  layout(hovermode = 'closest',
         font = list(color = 'black'),
         xaxis = append(list(title = "Eagle activity rate (min/hr*km<sup>3</sup>)"), ax),
         yaxis = append(list(title = "Standardized deviance"), ax)
  )

#Plot deviance between estimates using priors and site-specific estimate as a function of survey effort from simulated data. Color by eagle rate
fig2a <- plot_ly(filter(sim, erate%%0.2 == 0))%>%
  add_trace(x = ~b, y = ~(CRM_80-Survey_80), type = "scatter", mode = "markers",
            color = ~ erate,
            marker = list(colorbar = list(title = "Eagle Obs<br>(min)")),
            text = ~paste("Effort =", round(b, 3), "hr*km<sup>3</sup>", "<br>Eagles =", erate, "<br>Delta =", round(CRM_80 - Survey_80, 2)),
            hoverinfo = 'text')%>%
  colorbar(title = 'Eagle exposure<br>(min/hr*km<sup>3</sup>)',
           x = 0.5, xanchor = 'left', y = 0.6, yanchor = 'bottom',
           titlefont = list(family = 'serif', size = 14, color = 'black'),
           tickfont = tickfont,
           len = 0.4)%>%
  layout(hovermode = 'closest',
         annotations = a,
         font = list(color = 'black'),
         xaxis = append(list(title = "Survey effort (hr*km<sup>3</sup>)"), ax),
         yaxis = append(list(title = "Difference in estimated fatalities (# eagles)", range = c(-2,2)), ax)
  )


#PLOT FOR ZERO OBSERVED EAGLES
fig2b <- plot_ly(data = zerosim, type = 'scatter', mode = 'markers')%>%
  add_trace(
    x = ~Effort, y = ~CRM_80,
    color = ~ExpFac/expFac,
    showlegend = FALSE,
    #marker = list(colorbar = list(x = 0.7, y = 1, title = "Project<br>Size (ha)")),
    hoverinfo = 'text',
    text = ~paste("Effort:", Effort, "hr*km<sup>3</sup>", "<br>Project Size:", ExpFac/expFac, "turbines", "<br>Fatalities:", round(CRM_80, 1), "eagles"))%>%
  add_trace(
    x = ~Effort, y = ~CRM_80,
    color = ~ExpFac/expFac,
    xaxis = 'x2',
    yaxis = 'y2',
    showlegend = FALSE
  )%>%
  colorbar(x = 0.8, y = 1, title = 'Project size<br>(# turbines)<br>',
           titlefont = list(family = 'serif', size = 14, color = 'black'),
           tickfont = tickfont)%>%
  layout(legend = list(x = 0.7, y = 1),
         annotations = append(a, list(ax = 20)),
         xaxis = append(list(title = "Effort (hr*km<sup>3</sup>)"), ax),
         yaxis = append(list(title = "Estimated eagle fatalities"), ax),
         xaxis2 = append(list(range = c(0,100), domain = c(0.2, 0.9), anchor = 'y2'), ax),
         yaxis2 = append(list(range = c(0, 0.5), domain = c(0.4, 0.7), anchor = 'x2'), ax))

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
  mitigation <- size*(erate*qbeta(0.8, collide$shape, collide$rate))
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
    add_trace(
      x = ~effort, y = ~ -abs((mitigation*mcost) - M),
      name = '$/eagle',
      yaxis = 'y2'
    )%>%
    layout(
      xaxis = list(title = 'Survey effort (hr*km<sup>3</sup>)'),
      yaxis = list(title = 'Cost ($)'),
      #annotations = a,
      legend = list(x = 0.2, y = 1),
      yaxis2 = list(overlaying = 'y', side = 'right')
    )
}

# Lattice of example curves showing effort/cost relationships at a variety of scenarios
multiplot <- function(i){
  retro_cost <- filter(cost_table, Duration == 30, Rate == 'Median', Cost == 'High')$M
  x <- seq(0, 100, 1)
  cst <- curve(cost(x, sub_test$erate[i], sub_test$size[i], retro_cost, survey_costs$Low)[['T']],
               from = 0, to = 100)
  mon <- curve(cost(x, sub_test$erate[i], sub_test$size[i], retro_cost, survey_costs$Low)[['M']],
               from = 0, to = 100)
  surv <- curve(cost(x, sub_test$erate[i], sub_test$size[i], retro_cost, survey_costs$Low)[['S']],
                from = 0, to = 100)
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
  layout(yaxis = append(list(title = '', type = 'log', range = c(0, 6)), ax),
         yaxis2 = append(list(title = '', type = 'log', range = c(0, 6)),ax),
         yaxis3 = append(list(title = 'Cost ($)', type = 'log', range = c(0, 6)), ax),
         yaxis4 = append(list(title = '', type = 'log', range = c(0, 6)),ax),
         yaxis5 = append(list(title = '', type = 'log', range = c(0, 6)),ax),
         xaxis = append(list(title = ''),ax),
         xaxis2 = append(list(title = ''),ax),
         xaxis3 = append(list(title = 'Survey effort (hr*km<sup>3</sup>)'), ax),
         xaxis4 = append(list(title = ''),ax),
         xaxis5 = append(list(title = ''),ax),
         xaxis6 = append(list(title = ''),ax))

# fig 4a - difference in mitigation between min and max eagle estimates
fig4a <- plot_ly(
    type = 'heatmap',
    z = acast(mutate(high_high, diff = ifelse(minCostEffort == 0, maxEagleMitigation - (minEagle*mrate), (maxEagleMitigation - (minEagle*mrate))*-1)), erate~size, value.var = 'diff'),
    # z = acast(mutate(high_high, diff = (minCostEffort*srate)- maxEagleSurvey), erate~size, value.var = 'diff'),
    # z = acast(mutate(high_high, diff = ifelse(minCostEffort == 0, maxEagleCost - minCost, (maxEagleCost - minCost)*-1)), erate~size, value.var = "diff"),
    y = seq(0,2,0.05),
    x = seq(20,500,20),
    # zmin = -400000, zmax = 400000,
    zmin = -250000, zmax = 1000000,
    colors = colorRamp(c('black', 'white')))%>%
  add_trace(type = 'contour',
            y = seq(0,2,0.05),
            x = seq(20,500,20),
            # z = acast(mutate(high_high, diff = ifelse(minCostEffort == 0, maxEagle-minEagle, (maxEagle - minEagle)*-1)), erate~size, value.var = "diff"),
            zmin = -250000,
            zmax = 1000000,
            autocontour = F,
            contours = list(
              end = 750000,
              start = -250000,
              size = 250000,
              # showlabels = TRUE,
              coloring = 'none',
              labelfont = list(family = 'serif', color = 'white', size = 12)
            ),
            line = list(
              smoothing = 1,color = 'white',
              width = 2),
            #colorscale = list(list(0, '#FFFFFF00'), list(0.5,'#FFFFFF00'), list(1,'#FFFFFF00')),
            showscale = FALSE,
            showlegend = FALSE)%>%
  colorbar(title = 'Mitigation<br>cost ($)',
           titlefont = list(family = 'serif', color = 'black', size = 14),
           tickfont = list(family = 'serif', color = 'black', size = 12),
           tick0 = -250000,
           dtick = 250000)%>%
  layout(
    yaxis = append(list(title = 'Eagle exposure (min/hr*km<sup>3</sup>)'), ax),
    xaxis = append(list(title = 'Facility size (# turbines)'), ax)
  )

# fig 4b - difference in survey costs between min and max eagles
fig4b <- plot_ly(
    type = 'heatmap',
    # z = acast(mutate(high_high, diff = ifelse(minCostEffort == 0, maxEagleMitigation - (minEagle*mrate), (maxEagleMitigation - (minEagle*mrate))*-1)), erate~size, value.var = 'diff'),
    z = acast(mutate(high_high, diff = (minCostEffort*srate)-maxEagleSurvey), erate~size, value.var = 'diff'),
    # z = acast(mutate(high_high, diff = ifelse(minCostEffort == 0, maxEagleCost - minCost, (maxEagleCost - minCost)*-1)), erate~size, value.var = "diff"),
    y = seq(0,2,0.05),
    x = seq(20,500,20),
    # zmin = -400000, zmax = 400000,
    zmin = -250000, zmax = 1000000,
    colors = colorRamp(c('black', 'white')))%>%
  add_trace(type = 'contour',
            y = seq(0,2,0.05),
            x = seq(20,500,20),
            # z = acast(mutate(high_high, diff = ifelse(minCostEffort == 0, maxEagle-minEagle, (maxEagle - minEagle)*-1)), erate~size, value.var = "diff"),
            zmin = -250000,
            zmax = 1000000,
            autocontour = F,
            contours = list(
              end = 1000000,
              start = -1000000,
              size = 1000000,
              # showlabels = TRUE,
              coloring = 'none',
              labelfont = list(family = 'serif', color = 'white', size = 12)
            ),
            line = list(
              smoothing = 1,color = 'white',
              width = 2),
            #colorscale = list(list(0, '#FFFFFF00'), list(0.5,'#FFFFFF00'), list(1,'#FFFFFF00')),
            showscale = FALSE,
            showlegend = FALSE)%>%
  colorbar(title = 'Survey<br>cost ($)',
           titlefont = list(family = 'serif', color = 'black', size = 14),
           tickfont = list(family = 'serif', color = 'black', size = 12),
           tick0 = -250000,
           dtick = 250000)%>%
  layout(
    yaxis = append(list(title = 'Eagle exposure (min/hr*km<sup>3</sup>)'), ax),
    xaxis = append(list(title = 'Facility size (# turbines)'), ax)
  )

# fig 4c - difference in total costs between min and max eagles
fig4c <- plot_ly(
    type = 'heatmap',
    # z = acast(mutate(high_high, diff = ifelse(minCostEffort == 0, maxEagleMitigation - (minEagle*mrate), (maxEagleMitigation - (minEagle*mrate))*-1)), erate~size, value.var = 'diff'),
    # z = acast(mutate(high_high, diff = maxEagleSurvey-(minCostEffort*srate)), erate~size, value.var = 'diff'),
    z = acast(mutate(high_high, diff = ifelse(minCostEffort == 0, maxEagleCost - minCost, (maxEagleCost - minCost)*-1)), erate~size, value.var = "diff"),
    y = seq(0,2,0.05),
    x = seq(20,500,20),
    zmin = -250000, zmax = 1000000,
    colors = colorRamp(c('black', 'white')))%>%
  add_trace(type = 'contour',
            y = seq(0,2,0.05),
            x = seq(20,500,20),
            # z = acast(mutate(high_high, diff = ifelse(minCostEffort == 0, maxEagle-minEagle, (maxEagle - minEagle)*-1)), erate~size, value.var = "diff"),
            zmin = -250000,
            zmax = 1000000,
            autocontour = F,
            contours = list(
              end = 750000,
              start = -250000,
              size = 250000,
              # showlabels = TRUE,
              coloring = 'none',
              labelfont = list(family = 'serif', color = 'white', size = 12)
            ),
            line = list(
              smoothing = 1,color = 'white',
              width = 2),
            #colorscale = list(list(0, '#FFFFFF00'), list(0.5,'#FFFFFF00'), list(1,'#FFFFFF00')),
            showscale = FALSE,
            showlegend = FALSE)%>%
  colorbar(title = 'Total<br>cost ($)',
           titlefont = list(family = 'serif', color = 'black', size = 14),
           tickfont = list(family = 'serif', color = 'black', size = 12),
           tick0 = -250000,
           dtick = 250000)%>%
  layout(
    yaxis = append(list(title = 'Eagle exposure (min/hr*km<sup>3</sup>)'), ax),
    xaxis = append(list(title = 'Facility size (# turbines)'), ax)
  )
# Fig. 4d - difference between min max eagles
fig4d <- plot_ly(
    type = 'heatmap',
    z = acast(mutate(high_high, diff = ifelse(minCostEffort == 0, maxEagle-minEagle, (maxEagle - minEagle)*-1)), erate~size, value.var = "diff"),
    y = seq(0,2,0.05),
    x = seq(20,500,20),
    zmin = -10, zmax = 25,
    colors = colorRamp(c('black', 'white')))%>%
  add_trace(type = 'contour',
            y = seq(0,2,0.05),
            x = seq(20,500,20),
            z = acast(mutate(high_high, diff = ifelse(minCostEffort == 0, maxEagle-minEagle, (maxEagle - minEagle)*-1)), erate~size, value.var = "diff"),
            zmin = -10,
            zmax = 25,
            autocontour = F,
            contours = list(
              end = 15,
              start = -5,
              size = 5,
              # showlabels = TRUE,
              coloring = 'none',
              labelfont = list(family = 'serif', color = 'white', size = 14)
            ),
            line = list(
              smoothing = 1,color = 'white',
              width = 2),
            # colorscale = list(list(0, '#FFFFFF00'), list(0.5,'#FFFFFF00'), list(1,'#FFFFFF00')),
            showscale = FALSE,
            showlegend = FALSE)%>%
colorbar(title = 'Eagle<br>fatalities',
         titlefont = list(family = 'serif', color = 'black', size = 14),
         tickfont = list(family = 'serif', color = 'black', size = 12),
         tick0 = -10,
         dtick = 5)%>%
  layout(
    yaxis = append(list(title = 'Eagle exposure (min/hr*km<sup>3</sup>)'), ax),
    xaxis = append(list(title = 'Facility size (# turbines)'), ax)
  )

# add orca command line utility to R environmental path
Sys.setenv("PATH" = paste(Sys.getenv("PATH"), "C:\\Users\\mevans\\AppData\\Local\\Programs\\orca", sep = .Platform$path.sep))

# Write figures to file for manuscript
orca(fig1a, file = 'Fig1.png', format = tools::file_ext('png'), scale = 10)
orca(fig1b, file = 'Fig1b.png', format = tools::file_ext('png'), scale = 20)
orca(fig2a, file = 'Fig2a.png', format = tools::file_ext('png'), scale = 10, height = 500, width = 1000)
orca(fig2b, file = 'Fig2b.png', format = tools::file_ext('png'), scale = 10, height = 500, width = 1000)
orca(fig3, file = 'Fig3.png', format = tools::file_ext('png'), scale = 10)
orca(fig4a, file = 'Fig4a.png', format = tools::file_ext('png'), scale = 10)
orca(fig4b, file = 'Fig4b.png', format = tools::file_ext('png'), scale = 10)
orca(fig4c, file = 'Fig4c.png', format = tools::file_ext('png'), scale = 10)
orca(fig4d, file = 'Fig4d.png', format = tools::file_ext('png'), scale = 10)
