prediction <- function(iters, alpha, beta){
 out <- data.frame(collision = rep(NA,iters),
                   expose = rep(NA, iters),
                   fatality = rep(NA, iters)
                   )
 for(n in 1:iters){
   c <- rbeta(1, shape1 = 32.56, shape2 = 8641.52)
   e <- rgamma(1, shape = alpha, rate = beta)
   f <- c*e
   out[n,] <- c(c,e,f)
 }
 return(out)
}

vir_col <- function(n){
  return (substr(viridis(n),1,7))
}

#function to return mean and 80% CI estimates from a predicted fatality distribution
#hardwired to use Bay16 data as exposure priors
estimates <- function(niters, a, b){
  out <- prediction(niters, a+mean(Bay16$FLIGHT_MIN), b+mean(Bay16$EFFORT))
  fatality <- mean(out$fatality)
  q80 <- quantile(out$fatality, c(0.1, 0.9))
  out2 <- prediction(10000, a, b)
  fatality2 <- mean(out2$fatality)
  q82 <- quantile(out2$fatality, c(0.1, 0.9))
  return (c("MN_F" = fatality, "LQ_F" = q80[1], "UQ_F" = q80[2],
            "MN" = fatality2, "LQ" = q82[1], "UQ" = q82[2]))
}

