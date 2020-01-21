##NOTE
## for Gamma dist a = (u/sd)2, b = (u/sd2)
## FWS priors are mean eagle min/hr*km2 for exposure
## birds / min

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

vir_col <- function(n){
  return (substr(viridis(n),1,7))
}

simFatal <- function(BMin=-1, Fatal=-1, SmpHrKm, ExpFac = 1, aPriExp=1,
                     bPriExp=1,aPriCPr=1, bPriCPr=1, iters){
  out <- data.frame(collision = rep(NA,iters),
                    expose = rep(NA, iters),
                    fatality = rep(NA, iters)
  )
  # BMin:     observed number of bird minutes
  # Fatal:    annual avian fatalities on an operational wind facility
  # SmpHrKm:  total time and area surveyed for bird minutes
  # ExpFac:   expansion factor
  # aPriExp:  alpha parameter for the prior on lambda
  # bPriExp:  beta parameter for the prior on lambda
  # aPriCPr:  alpha parameter for the prior on C
  # bPriCPr:  beta parameter for the prior on C

  # The default of a negative value for BMin or Fatal indicates that no data were collected for those model inputs

  #require(rv)

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


#function to return mean and 80% CI estimates from a predicted fatality distribution
#hardwired to use updated golden eagle priors
estimates <- function(niters, a, b){
  out <- simFatal(BMin = a,
                  Fatal = -1,
                  SmpHrKm = b,
                  ExpFac = mean(Bay16$SCALE),
                  aPriExp = 11.81641,
                  bPriExp = 9.7656250,
                  aPriCPr = 1.638029,
                  bPriCPr = 290.0193,
                  iters = 10000)
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
                   iters = 10000)
  fatality2 <- mean(out2$fatality)
  q82 <- quantile(out2$fatality, 0.8)
  return (c("MN_F" = fatality, "U_F" = q80, "MN" = fatality2, "U" = q82))
}

