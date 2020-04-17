##NOTE
## for Gamma dist a = (u/sd)2, b = (u/sd2)
## FWS priors are mean eagle min/hr*km2 for exposure
## birds / min



# CRM priors taken from New et al (2018) https://www.fws.gov/migratorybirds/pdf/management/crmpriorsreport2018.pdf
baldExposure <- list('shape' = 0.077, 'rate' = 0.024)
baldCollision <- list('shape' = 1.61, 'rate' = 228.2)
goldExposure <- list('shape' = 0.287, 'rate' = 0.237)
goldCollision <- list('shape' = 1.29, 'rate' = 227.6)

# CRM priors provided by E. Bjerre 4/10/20
expose <- list('shape' = 0.968, 'rate' = 0.552)
collide <- list('shape' = 2.31, 'rate' = 396.69)

#Site expansion factor assuming 200m tall turbines w/80m rotors operating 10hrs/day
expFac <- 3650*(0.2)*(0.08^2)*pi
#' convert number of turbines to project 'size'
#'
#' this function assumes 200m tall turbines with 80m rotors
#'
#' @param n number of turbines
#' @return size (km3)
turbines_to_size <- function(n){
  size = n*3650*(0.2)*(0.08^2)*pi
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

#'
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
                  ExpFac = turbines_to_size(nturbines),
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
                   ExpFac = turbines_to_size(nturbines),
                   aPriExp = 0,
                   bPriExp = 0,
                   aPriCPr = collide$shape,
                   bPriCPr = collide$rate,
                   iters = niters)
  fatality2 <- mean(out2$fatality)
  q82 <- quantile(out2$fatality, 0.8)
  return (c("MN_F" = fatality, "U_F" = q80, "MN" = fatality2, "U" = q82))
}

#' Calculate total mitigation and monitoring costs
#'
#' @description This function calculates the...the first argument is effort because this is what will be optimized over
#' @param effort survey effort (hrs*km3). Will be optimized when used with optimize()
#' @param data data frame with columns 'a', 'size' 'mcost' and 'acost.' These columns must containe
#'  contain eagle activity rate, project size, per eagle mitigation cost, and hourly survey cost respectively
#'
#' @return total cost of mitigation and monitoring (numeric)
cost_fxn <- function(effort, data){
  with(data, {
    activity <- a*effort
    #activity <- 1
    #size <- 10
    aExp <- goldExposure$shape
    bExp <- goldExposure$rate
    aCPr <- goldCollision$shape
    bCPr <- goldCollision$rate
    #Read in effort values (hrs*km3)
    #Do we need to use rv here?
    EXP <- rvgamma(1, aExp + activity, bExp + effort)
    COL <- rvbeta(1, aCPr, bCPr)
    Fatal <- EXP * COL * size
    M <- rvquantile(Fatal, 0.8) * mrate#38000
    S <- effort * srate#167
    total_cost <- M+S
    return(total_cost)
  })
}

#' Calculate mitigation, monitoring, and total costs
#'
#' @param effort survey effort (hr*km3)
#' @param a eagle activity rate (eagle min/hr)
#' @param size project size in number of turbines
#' @param mcost cost of mitigation for 1 eagle take
#' @param scost hourly cost of pre-construction monitoring
#'
#' @return data.frame with eagles('E'), total ('T'), mitigation ('M'), survey ('S') costs
cost <- function(effort, a, size, mrate, srate){
  activity <- a*effort
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
  opt <-optimize(cost_fxn, interval = c(0, 500), data = data.frame(a = erate, size = size, mrate = mrate, srate = srate), tol = 0.00000001)
  return(data.frame(effort = opt$minimum, cost = opt$objective))
}

#Alternatively we use 'curve' and 'cost' to generate points, fit a line,
# then find minimum
min_cost <- function(erate, size, mrate, srate){
  crv <- curve(cost(x, erate, size, mrate, srate)$T,
               from = 0, to = 500)

  lo <- loess(crv$y ~ crv$x, span = 0.2)
  smoothed <- predict(lo, x = crv$x)

  min_effort <- crv$x[smoothed == min(smoothed)]

  return(data.frame(cost = min(smoothed), effort = min_effort))
}

#' Equation defining relationship between effort and discrepancy
#'
#' @param erate inherent rate of eagle activity
#' @param a shape parameter from exposure prior
#' @param b rate parameter from exposure prior
#' @param w effort
#' @return numeric value
effort_discrepancy_slope <- function(erate, a, b, w){
  (erate*b - a)/(b+w)
}
