##NOTE
## for Gamma dist a = (u/sd)2, b = (u/sd2)
## FWS priors are mean eagle min/hr*km2 for exposure
## birds / min



# CRM priors taken from New et al (2018) https://www.fws.gov/migratorybirds/pdf/management/crmpriorsreport2018.pdf
baldExposure <- list('shape' = 0.077, 'rate' = 0.024)
baldCollision <- list('shape' = 1.61, 'rate' = 228.2)
goldExposure <- list('shape' = 0.287, 'rate' = 0.237)
goldCollision <- list('shape' = 1.29, 'rate' = 227.6)

# # CRM priors provided by E. Bjerre 4/10/20
# expose <- list('shape' = 0.968, 'rate' = 0.552)
# collide <- list('shape' = 2.31, 'rate' = 396.69)

# CRM priors updated in New et al. 2018
expose <- list('shape' = 0.287, 'rate' = 0.237)
collide <- list('shape' = 1.29, 'rate' = 227.6)

#Site expansion factor assuming 100m tall turbines w/50m rotors operating 10hrs/day
# Turbine specs: http://www.aweo.org/windmodels.html
#expFac <- 3650*(0.2)*(0.08^2)*pi
expFac <- 3650*(0.1)*(0.05^2)*pi
#' convert number of turbines to project 'size'
#'
#' this function assumes  turbines operate 365 days/year for 10hr/day
#'
#' @param n number of turbines
#' @param h turbine tower height (m)
#' @param r turbine rotor radius (m)
#' @return size (hr*km3)
turbines_to_size <- function(n, h, r){
  # convert lengths to km
  h <- h/1000
  r <- r/1000
  size = n*3650*(h)*(r^2)*pi
  #size = n*3650*(0.2)*(0.08^2)*pi
  return(size)
}

#' Calculate cost for eagle mitigation
#'
#' @param cost per pole cost
#' @param duration retrofit longevity
#' @param adult boolean indication of adult or juvenille eagle
#' @param electrocution assumed per/pole electrocution rate
#' @return estimated cost (numeric)
retrofit_cost <- function(cost, duration = 20, adult = TRUE, electrocution){
  age <- ifelse(isTRUE(adult), 10, 2)
  future_yrs <- 30 - age
  n_poles <- future_yrs/0.0051*duration
  total <- n_poles*cost
  return(total)
}


vir_col <- function(n){
  return (substr(viridis(n),1,7))
}

#' estimate fatalities from the collision risk model
#' @param BMin observed number of bird minutes
#' @param Fatal annual avian fatalities on an operational wind facility
#' @param SmpHrKm total time and area surveyed for bird minutes
#' @param ExpFac expansion factor
#' @param aPriExp alpha parameter for the prior on lambda
#' @param bPriExp beta parameter for the prior on lambda
#' @param aPriCPr alpha parameter for the prior on C
#' @param bPriCPr beta parameter for the prior on C
#'
#' The default of a negative value for BMin or Fatal indicates that no data were collected for those model inputs
#'
#' @require rv
#' @return data frame with random draws for collision rate, exposure and predicted fatalities
#' for each iteration
simFatal <- function(BMin=-1, Fatal=-1, SmpHrKm, ExpFac = 1, aPriExp=1,
                     bPriExp=1,aPriCPr=1, bPriCPr=1, iters){
  out <- data.frame(collision = rep(NA,iters),
                    expose = rep(NA, iters),
                    fatality = rep(NA, iters)
  )


  # Update the exposure prior
  if(BMin>=0){
    aPostExp <- aPriExp + BMin
    bPostExp <- bPriExp + SmpHrKm
  }else{
    aPostExp <- aPriExp
    bPostExp <- bPriExp}
  # Update the collisions prior
  if(Fatal>=0){
    aPostCPr <- aPriCPr + Fatal
    bPostCPr <- ((rvmean(Exp) * ExpFac) - Fatal) + bPriCPr
  }else{
    aPostCPr <- aPriCPr
    bPostCPr <- bPriCPr}

  for(i in 1:iters){
    Exp <- rgamma(n=1, aPostExp, bPostExp)
    CPr <- rbeta(n=1, aPostCPr, bPostCPr)
    Fatalities <- ExpFac * Exp * CPr
    out[i,] <- c(CPr, Exp, Fatalities)
  }

  #attr(Fatalities,"Exp") <- c(Mean=rvmean(Exp), SD=rvsd(Exp))
  #attr(Fatalities,"CPr") <- c(Mean=rvmean(CPr), SD=rvsd(CPr))
  return(out)
}

prediction <- function(iters, aExp, bExp, aCPr, bCPr){
  out <- data.frame(collision = rep(NA,iters),
                    expose = rep(NA, iters),
                    fatality = rep(NA, iters)
  )
  for(n in 1:iters){
    out[n,] <- simFatal(BMin=-1, Fatal=-1, SmpHrKm, ExpFac, aPriExp=1,
                        bPriExp=1,aPriCPr=1, bPriCPr=1)
    #c <- rbeta(1, shape1 = 9.28, shape2 = 3224.51)
    #e <- rgamma(1, shape = alpha, rate = beta)
    #f <- c*e
    #out[n,] <- c(c,e,f)
  }
  return(out)
}


#' calculate mean and 80% CI estimates from a predicted fatality distribution
#'
#' This function is hardwired to use updated golden eagle priors
#'
#' @param niters number of iterations
#' @param a observed eagle minutes
#' @param b survey effort (hr*km3)
#' @return named vector with mean and 80th percentile estimates calcualted with
#' and without eagle exposure priors
#'
estimates <- function(niters, a, b, nturbines){
  out <- simFatal(BMin = a,
                  Fatal = -1,
                  SmpHrKm = b,
                  ExpFac = turbines_to_size(nturbines, 100, 50),
                  aPriExp = expose$shape,
                  bPriExp = expose$rate,
                  aPriCPr = collide$shape,
                  bPriCPr = collide$rate,
                  iters = niters)
  fatality <- mean(out$fatality)
  q80 <- quantile(out$fatality, c(0.8))
  out2 <- simFatal(BMin = a,
                   Fatal = -1,
                   SmpHrKm = b,
                   ExpFac = turbines_to_size(nturbines, 100, 50),
                   aPriExp = 0,
                   bPriExp = 0,
                   aPriCPr = collide$shape,
                   bPriCPr = collide$rate,
                   iters = niters)
  fatality2 <- mean(out2$fatality)
  q82 <- quantile(out2$fatality, 0.8)
  return (c("CRM_mean" = fatality, "CRM_80" = q80, "Survey_mean" = fatality2, "Survey_80" = q82))
}

#' Calculate total mitigation and monitoring costs
#'
#' @description This function calculates the...the first argument is effort because this is what will be optimized over
#' @param effort survey effort (hrs*km3). Will be optimized when used with optimize()
#' @param data data frame with columns 'erate', 'size' 'mcost' and 'acost.' These columns must containe
#'  contain eagle activity rate, project size, per eagle mitigation cost, and hourly survey cost respectively
#'
#' @return total cost of mitigation and monitoring (numeric)
cost_fxn <- function(effort, data){
  with(data, {
    activity <- erate*effort
    #Do we need to use rv here?
    EXP <- rvgamma(1, expose$shape + activity, expose$rate + effort)
    COL <- rvbeta(1, collide$shape, collide$rate)
    Fatal <- EXP * COL * size
    E <- rvquantile(Fatal, 0.8)
    M <- E* mrate#38000
    S <- effort * srate#167
    Total <- M+S
    return(Total)
  })
}

#' Calculate mitigation, monitoring, and total costs
#'
#' @param effort survey effort (hr*km3)
#' @param erate eagle activity rate (eagle min/hr)
#' @param size project size in number of turbines
#' @param mcost cost of mitigation for 1 eagle take
#' @param scost hourly cost of pre-construction monitoring
#'
#' @return data.frame with eagles('E'), total ('T'), mitigation ('M'), survey ('S') costs
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
  E <- rvquantile(Fatal, 0.8)
  M <- E * mrate#38000
  S <- effort * srate#167
  total_cost <- M+S

  return(list('T' = total_cost[1,], 'M' = M[1,], 'S' = S, 'E' = E[1,]))
  #if (return == 'T'){
  #  return(total_cost[1,])
  #}else if (return == "M"){
  #  return(M[1,])
  #}else if (return == "S"){
  #  return(S)
  #}
}

#' Generate total, mitigation, and survey costs over a range of efforts
#' @return data frame wih columns x, T, M, S
cost_curve <- function(effort, erate, size, mrate, srate){
  output <- cost(effort, erate, size, mrate, srate)
  return(data.frame(T = output['T'], M = output['M'], S = output['S'], E = output['E']))
}


#' Find effort that equates to minimum cost
#'
#' @description identifies the amount of effort that minimized the total costs associated with
#'  mitigation and monitoring for a given eagle activity rate, project size, and assumed
#'  per eagle mitigation costs and hourly survey costs.
#' @param
optim_fxn <- function(erate, size, mrate, srate){
  opt <-optimize(cost_fxn, interval = c(0, 500), data = data.frame(erate = erate, size = size, mrate = mrate, srate = srate), tol = 0.00000001)
  return(data.frame(effort = opt$minimum, cost = opt$objective))
}

#Alternatively we use 'curve' and 'cost' to generate points, fit a line,
# then find minimum
min_cost <- function(erate, size, mrate, srate){
  crv <- curve(cost(x, erate, size, mrate, srate)$T,
               from = 0, to = 500)

  # lo <- loess(crv$y ~ crv$x, span = 0.2)
  # smoothed <- predict(lo, x = crv$x)
  #
  # min_effort <- crv$x[smoothed == min(smoothed)]
  #
  # return(data.frame(cost = min(smoothed), effort = min_effort))
  #
  min_cost <- min(crv$y)
  min_effort <- crv$x[crv$y == min_cost]
  return(data.frame(cost = min_cost, effort = min_effort))
}

#' Function calculating maximum eagles and minimum costs
#'
#'This assumes the mean of the gamma distribution is used
#' @param erate inherent rate of eagle activity (min/hr*km3)
#' @param size size of facility accounting for operation time (hr*km3)
#' @param mrate per eagle cost of mitigation
#' @param srate per hour cost of surveying
#' @return data.frame
max_eagle <- function(erate, size, mrate, srate){
  # calculate the crm exposure curve
  exposure <- curve(qgamma(0.8, expose$shape + (erate*x), expose$rate + x),
               from = 0, to = 100)
  # define multiplier for determining mitigation costs
  constant <- 0.01*size*mrate
  # create survey cost vs. effort curve
  survey <- curve(srate*x, from = 0, to = 100)
  # create total cost curve by combining exposure * contant + survey
  costs <- (exposure$y*constant) + survey$y
  # find minimum cost
  minCost <- min(costs)
  # match the index of the minimum cost value to corresponding survey effort
  minEffort <- survey$x[match(minCost, costs)]
  # find the maximum exposure
  # could be a) equal to the 'true' erate, with inifite surveying
  maxExposure <- qgamma(0.8, expose$shape + (erate*10000), expose$rate + 10000)
  # or b) equal to the estimated exposure at the services minimum
  # maxExposure <- qgamma(0.8, expose$shape + (erate*9.65), expose$rate + 9.65)
  # identify effort resulting in max exposure
  maxEffort <- exposure$x[exposure$y==maxExposure]
  # get costs associated with levels of effort producing maximum eagles and minimum costs
  minCosts <- cost(minEffort, erate, size, mrate, srate)
  maxCosts <- cost(maxEffort, erate, size, mrate, srate)
  return(data.frame(minCost = minCosts$T,
                    minEffort = minEffort,
                    minEagles = minCosts$E,
                    minSurvey = minCosts$S,
                    minMitigation = minCosts$M,
                    maxCost = maxCosts$T,
                    maxEffort = maxEffort,
                    maxSurvey = maxCosts$S,
                    maxMitigation = maxCosts$M,
                    maxEagles = maxCosts$E))
}
max_eagle <- function(erate, size, mrate, srate){
  crv <- curve(cost(x, erate, size, mrate, srate)$E,
               from = 0, to = 100)
  stableEagle <- mean(crv$y[80:100]) - 0.1
  maxEffort <- min(crv$x[which(crv$y >= stableEagle)])
  costs <- cost(maxEffort, erate, size, mrate, srate)
  # opt <-optimize(cost_fxn, interval = c(-1, 500), data = data.frame(erate = erate, size = size, mrate = mrate, srate = srate), maximum = TRUE)
  # return(data.frame(eagles = opt$objective, effort = opt$maximum))
  return(data.frame(maxEffort = maxEffort,
                    maxEagles = costs$E,
                    maxSurvey = costs$S,
                    maxMitigation = costs$M,
                    maxCost = costs$T))
}

#' Equation defining relationship between effort and discrepancy
#'
#'This assumes the mean of the gamma distribution is used
#' @param erate inherent rate of eagle activity
#' @param a shape parameter from exposure prior
#' @param b rate parameter from exposure prior
#' @param w effort
#' @return numeric value
effort_discrepancy_slope <- function(erate, a, b, w, n){
  #turbines_to_size(n)*collide$shape/(collide$shape + collide$rate)*(a-(erate*b))/(b+w)
  expFac*n/100*(a-(erate*b))/(b+w)
}

#' w = 2ca - 2crb - b
effort_needed <- function(nturbines, eagle_rate, threshold){
  constant <- 1/threshold*expFac*nturbines/100
  (constant * expose$shape) - (constant*eagle_rate*expose$rate) - expose$rate
}

difffxn <- function(nturbines, obs_min, effort){
  crm <- qgamma(0.8, expose$shape + obs_min, expose$rate + effort)
  siteonly <- qgamma(0.8, obs_min, effort)
  diff <- (crm - siteonly)*expFac*100*qbeta(0.8, collide$shape, collide$rate)
  return(diff)
}

testfxn <- function(erate, effort){
  crm <- qgamma(0.8, expose$shape + (erate * effort), expose$rate + effort)
  site <- qgamma(0.8, erate*effort, effort)
  return(list('CRM' = crm, 'SITE' = site, 'DIFF' = crm-site))
}
