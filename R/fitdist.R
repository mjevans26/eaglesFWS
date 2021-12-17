library(fitdistrplus)
test <- data.frame()
subset <- high_high[high_high$size == min(high_high$size),]

# TODO: use TryCatch on fitdist functions
optim_fitdist <- function(effort, size, erate){
  fatalities <- simFatal(SmpHrKm = effort,
                         ExpFac = size,
                         aPriExp = expose$shape + (erate*effort),
                         bPriExp = expose$rate + effort,
                         aPriCPr = collide$shape,
                         bPriCPr = collide$rate,
                         iters = 2000)
  # out <- quantile(fatalities$fatality, 0.8)
  # scale the fatalities between 0 and 1
  scaled <- (fatalities$fatality - min(fatalities$fatality) + 0.00001)/(max(fatalities$fatality) - min(fatalities$fatality) + 0.00002)
  # find the best distribution fitting the posterior
  aics <- c('weibull' = 5000, 'gamma' = 5000, 'beta' = 5000)
  try({
    weib_dist <- fitdist(scaled, 'weibull')
    aics[1] <- weib_dist$aic
  })
  try({
    gamma_dist <- fitdist(scaled, 'gamma')
    aics[2] <- gamma_dist$aic
  })
  try({
    beta_dist <- fitdist(scaled, 'beta')
    aics[3] <- beta_dist$aic
  })

  minaic <- names(aics)[aics == min(aics)]
  stat <- switch(minaic,
         'weibull' = qweibull(0.8, weib_dist$estimate[1], weib_dist$estimate[2]),
         'beta' = qbeta(0.8, beta_dist$estimate[1], beta_dist$estimate[2]),
         'gamma' = qgamma(0.8, gamma_dist$estimate[1], gamma_dist$estimate[2]))
  out <- stat*(max(fatalities$fatality) - min(fatalities$fatality)) + min(fatalities$fatality)
  return(out)
}

wrapper <- function(erate, size, mrate, srate){
  effort <- seq(0,500,5)
  vector <- vapply(effort, function(x){return(optim_fitdist(x, size, erate))}, FUN.VALUE = double(1))
  cost <- (mrate*vector) + (srate*effort)
  maxEagle <- max(vector)
  minEagle <- min(vector)
  maxEffort <- min(effort[vector >= maxEagle-0.1])
  minEffort <- min(effort[vector <= minEagle+0.1])
  return(data.frame(maxEagle = maxEagle,
                    minEagle = minEagle,
                    maxEagleEffort = maxEffort,
                    minEagleEffort = minEffort,
                    minCost = min(cost),
                    maxCost = max(cost),
                    minCostEffort = effort[cost == min(cost)],
                    maxCostEffort = effort[cost == max(cost)]))
}

# This analysis was run in a COlab notebook to free up memory.
# Because of stochasticity at high eagle rates, we ran scenarios with erate >1.7 x3
high_high <- mutate(test_values, retro_cost[retro_cost$Cost == 'High', 'M'], srate = survey_costs$High)
high_high <- high_high <- read.csv(file = 'data/output500.csv', header = TRUE, stringsAsFactors = FALSE)%>%
  bind_rows(read.csv(file = 'data/output3.csv', header = TRUE, stringsAsFactors = FALSE))%>%
  bind_rows(read.csv(file = 'data/output1500.csv', header = TRUE, stringsAsFactors = FALSE))

high_high <- mutate(high_high,
                    maxEagleMitigation = mrate*maxEagle,
                    maxEagleSurvey = maxEagleEffort *srate,
                    maxEagleCost = maxEagleMitigation + maxEagleSurvey)

# identify scenarios for which a) min cost is less than max eagle and b) cost of surveying
# to minimum is greater than mitigating for 1.75
high_high <- mutate(high_high,
                    surveyCostDiff = ((minCostEffort -maxEagleEffort)*srate),
                    indicator = ifelse(minCost > maxEagleMitigation, 2, ifelse(surveyCostDiff > 0, 1, 0)))

plot_ly(
  data = high_high,
  type = 'scatter',
  mode = 'markers',
  x = ~erate,
  y = ~ maxEagleEffort - minCostEffort,
  color = ~ size
)

fig4a <- plot_ly(
  type = 'heatmap',
  # z = acast(mutate(high_high, diff = ifelse(minCostEffort == 0, maxEagleMitigation - (minEagle*mrate), (maxEagleMitigation - (minEagle*mrate))*-1)), erate~size, value.var = 'diff'),
  z = acast(mutate(high_high, diff = (minCostEffort*srate)- maxEagleSurvey), erate~size, value.var = 'diff'),
  # z = acast(mutate(high_high, diff = ifelse(minCostEffort == 0, maxEagleCost - minCost, (maxEagleCost - minCost)*-1)), erate~size, value.var = "diff"),
  y = seq(0,2,0.05),
  x = seq(20,500,20),
  # zmin = -400000, zmax = 400000,
  zmin = -500000, zmax = 500000,
  colors = colorRamp(c('black', 'white'))
  # autocontour = F,
  # contours = list(
  #   end = 0,
  #   start = 0,
  #   size = 1),
  # line = list(smoothing = 0.5, color = 'white', width = 2),
  # showlegend = FALSE
  )%>%
  colorbar(title = 'Cost<br>difference<br>(USD)',
           titlefont = list(family = 'serif', color = 'black', size = 14),
           tickfont = list(family = 'serif', color = 'black', size = 12),
           tick0 = -400000,
           dtick = 200000)%>%
  layout(
    yaxis = append(list(title = 'Eagle activity rate (min/hr*km<sup>3</sup>)'), ax),
    xaxis = append(list(title = 'Project size (# turbines)'), ax)
  )

fig4b <- plot_ly(
  type = 'heatmap',
  z = acast(mutate(high_high, diff = ifelse(minCostEffort == 0, maxEagle-minEagle, (maxEagle - minEagle)*-1)), erate~size, value.var = "diff"),
  y = seq(0,2,0.05),
  x = seq(20,500,20),
  zmin = -15, zmax = 15,
  colors = colorRamp(c('black', 'white'))
  # autocontour = F,
  # contours = list(
  #   end = 1,
  #   start = -1,
  #   size = 1),
  # line = list(smoothing = 1, color = 'white', width = 2),
  # showlegend = FALSE
)%>%
  # add_trace(type = 'contour',
  #           y = seq(0,2,0.05),
  #           x = seq(20,500,20),
  #           z = acast(high_high, erate~size, value.var = 'indicator'),
  #           autocontour = F,
  #           contours = list(
  #             end = 2,
  #             start = 0,
  #             size = 1
  #           ),
  #           line = list(
#             smoothing = 0.5,color = 'white',
#             width = 2),
#           colorscale = list(list(0, '#00FFFFFF'), list(0.5,'#00FFFFFF'), list(1,'#00FFFFFF')),
#           showscale = FALSE,
#           showlegend = FALSE)%>%
colorbar(title = 'Eagles',
         titlefont = list(family = 'serif', color = 'black', size = 14),
         tickfont = list(family = 'serif', color = 'black', size = 12),
         tick0 = -15,
         dtick = 5)%>%
  layout(
    yaxis = append(list(title = 'Eagle activity rate (min/hr*km<sup>3</sup>)'), ax),
    xaxis = append(list(title = 'Project size (# turbines)'), ax)
  )

test <- simFatal(SmpHrKm = 500, ExpFac = 100*expFac, aPriExp = expose$shape + (2.5*500), bPriExp = expose$rate + 500, aPriCPr = collide$shape, bPriCPr = expose$rate, iters = 1000)
scaled <- (test$fatality - min(test$fatality) + 0.00001)/(max(test$fatality) - min(test$fatality) + 0.00002)
descdist(scaled, discrete = FALSE)
beta_dist <- fitdist(scaled, 'beta')
weib_dist <- fitdist(scaled, 'weibull')
gamma_dist <- fitdist(scaled, 'gamma')
plot(weib_dist)

cost <- function(effort, erate, size, mrate, srate){
  activity <- erate*effort
  #activity <- 1
  #size <- 10
  aExp <- expose$shape
  bExp <- expose$rate
  aCPr <- collide$shape
  bCPr <- collide$rate
  #Read in effort values (hrs*km3)
  EXP <- rvgamma(1, aExp + activity, bExp + effort)
  COL <- rvbeta(1, aCPr, bCPr)
  Fatal <- EXP * COL * size
  weib_dist <- fitdist(sims(Fatal)[,1], 'weibull')
  # the quantile of simulated fatalities is subject to a lot of noise, so use a distribution
  # E <- rvquantile(Fatal, 0.8)
  E <- qweibull(0.8, shape = weib_dist$estimate[1], scale = weib_dist$estimate[2])
  M <- E * mrate#38000
  S <- effort * srate#167
  total_cost <- M+S

  return(list('T' = total_cost, 'M' = M, 'S' = S, 'E' = E))
  #if (return == 'T'){
  #  return(total_cost[1,])
  #}else if (return == "M"){
  #  return(M[1,])
  #}else if (return == "S"){
  #  return(S)
  #}
}

cost_fxn <- function(effort, data){
  with(data, {
    costs <- cost(effort, erate, size, mrate, srate)
    return(costs$T)
  })
}

eagle_fxn <- function(effort, data){
  with(data, {
    costs <- cost(effort, erate, size, mrate, srate)
    return(costs$E)
  })
}

optim_fxn <- function(erate, size, mrate, srate){
  dat <- data.frame(erate = erate, size = size, mrate = mrate, srate = srate)
  eagles <- optimize(eagle_fxn,
                     interval = c(0,200),
                     data = dat,
                     maximum = TRUE)
  costs <- optimize(cost_fxn,
                    interval = c(0,200),
                    data = dat,
                    maximum = FALSE)
  return(data.frame(minEffort = costs$minimum,
                    minCost = costs$objective,
                    maxEffort = eagles$maximum,
                    maxEagles = eagles$objective))
}

