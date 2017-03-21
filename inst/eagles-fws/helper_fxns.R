prediction <- function(iters, alpha, beta){
 out <- data.frame(collision = rep(NA,iters),
                   expose = rep(NA, iters),
                   fatality = rep(NA, iters)
                   )
 for(n in 1:iters){
   c <- rbeta(1, shape1 = 9.38, shape2 = 3224.51)
   e <- rgamma(1, shape = alpha, rate = beta)
   f <- c*e
   out[n,] <- c(c,e,f)
 }
 return(out)
}

vir_col <- function(n){
  return (substr(viridis(n),1,7))
}

