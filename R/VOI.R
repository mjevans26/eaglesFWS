library(dplyr)
library(reshape2)
source(file='R/helper_fxns.R')

## Calculation of expected values of surveying vs. mitigating for eagles under the US Fish & Wildlife
## collision risk model used to predict eagle fatalities at proposed wind energy facilities.
## Based on Bennet et al. (2018).

## STEP 1: SETUP
# define possible eagle activity states - a discretized approximation
S <- seq(0.1, 3, 0.1)
len <- length(S)

# define mitigation rates for a hypothetical project
cost_table <- read.csv(file = 'data/ABT_REA_costs.csv', header = TRUE)
nturb <- 200
size <- nturb * expFac
mitigation <- filter(cost_table, Duration == 30, Rate == "Median", Cost == 'High')$M * size
#Estimated survey cost data from West Ecosystems Inc.
survey_costs <- list('annual_low_ppt' = 2000, 'annual_high_ppt' = 5000,
                     "Low" = 2000/12, 'High' = 5000/12,
                     'annual_low_pMW' = 300, 'annual_high_pMW' = 600)
# probability of each state is based on the eagle activity prior ~gamma(0.968, 0.552)
pS <- dgamma(S, expose$shape, expose$rate)

# The other component predicting eagle fatalities is the probability of collision with
# wind turbines, given exposure.  This is a fixed distribution
# For simplicity, we'll use the mean of the collision prior to predict fatalties
# TODO: calculate 80th percentile?
meanCollision <- collide$shape/(collide$shape + collide$rate)

# our current belief of eagle activity is the mean of the prior distribution to start
curBelief <- expose$shape/expose$rate
# our confidence in this belief is represented by the variance of the distribution
confidence <- expose$shape/(expose$rate^2)

# STEP 2: DEFINE VALUES MATRIX
# SxSx2 array of action values. cells are abs[discrepancy between mitigation cost of true eagle rate and
# cost of current belief]
# true states are rows, beliefs are columns
# TODO: currently using the mean of the collision prior. could/should update to represent 80th?
values <- expand.grid(truth = S, belief = S)%>%
  mutate(value = -abs((belief - truth)*meanCollision*mitigation))%>%
  acast(truth ~ belief)

values <- array(c(values, rep(-survey_costs$Low*10, length(S)^2)), dim = c(len, len, 2))
dimnames(values)[[3]]<-c('mitigate', 'survey')

# STEP 3: EXPECTED VALUES UNDER UNCERTAINTY

# sum of value of current belief relative to true state * probability of states
valUncertainMitigate <- sum(pS * -abs((curBelief - S)*meanCollision*mitigation))
valUncertainSurvey <- -survey_costs$Low*10*len
valUncertain <- max(valUncertainMitigate, valUncertainSurvey)

## STEP 4: EXPECTED VALUE UNDER CERTAINAY

# value of each action given current belief relative to each state
valCertainMitigate <- -abs((curBelief - S)*meanCollision*mitigation)
valCertainSurvey <- -survey_costs$Low*10

# optimal action for each state
maxValsCertain <- data.frame('mitigate' = valCertainMitigate, 'survey' = valCertainSurvey)%>%
  apply(1, max)

# sum of optimal action * probabilty of each state
valCertain <- sum(pS * maxValsCertain)


# STEP 5: EXPECTED VALUE AFTER MONTORING
# P(s|y) = P(y|s)*P(s)/P(y)
# we calculate P(s|y) by directly updating the posterior distribution based on a set amount of survey effort
# and possible survey outcomes as in New et al. (2015)

# P(y) probability of observing y eagles is Poisson with rate s
# is this the joint distribution of observing a given number of eagles and a given true state?
#
# TODO: test alternative we make these prob of greater, less, equal curbelief +/- 0.2 with ppois
pObs <- expand.grid(truth = S, observed = S)%>%
  mutate(value = dpois(observed*10, truth*10)*dgamma(truth, expose$shape, expose$rate))%>%
  acast(truth~observed)%>%
  colSums()

#or is this the predictive posterior (negative binomial)? Posterior predictive is the probability of
# new observations, given hyperparameters and data, integrating over possible states (poisson rates)
pObs <- dnbinom(S*10, mu = expose$shape + (curBelief*10), size = 1/(1+expose$rate+10))

# P(s|y) - we can estimate directly due to conjugativity
# we assume we monitor for 10 hours
# creates an S x S matrix, rows are S, columns are Y
pSY <- sapply(S, function(x){dgamma(S, expose$shape + x*10, expose$rate + 10)})

# expected values for each management action after surveying
# these will be the value of an action given true state * probability of that state after a result y
expValsMitigate <- colSums(pSY * values[,,'mitigate'])
expValsSurvey <- colSums(pSY * values[,,'survey'])

expVals <- data.frame('mitigate' = expValsMitigate, 'survey' = expValsSurvey)
rownames(expVals) <- S

# value of optimal actions for each survey outcome
maxVals <- apply(expVals, 1, max)

# sum of maximum expected value for each survey outcom * probability of outcomes
valPostMonitor <- sum(maxVals*pObs)

valPerfect <- valCertain - valUncertain
voi <- valPostMonitor - valUncertain

#' calculate the value of monitoring information
#'
#' this function assumes  turbines operate 365 days/year for 10hr/day
#'
#' @param S vector of possible eagle activity rates
#' @param shape shape parameter of gamma exposure distribution
#' @param rate rate parameter of gamma exposure distribution
#' @return size (hr*km3)
calc_voi <- function(S, shape, rate){

  pS <- dgamma(S, shape, rate)

  # The other component predicting eagle fatalities is the probability of collision with
  # wind turbines, given exposure.  This is a fixed distribution
  # For simplicity, we'll use the mean of the collision prior to predict fatalties
  meanCollision <- 2.31/(2.31 + 396.69)

  # our current belief of eagle activity is the mean of the prior distribution to start
  curBelief <- shape/rate
  # our confidence in this belief is represented by the variance of the distribution
  confidence <- shape/rate^2

  # STEP 2: DEFINE VALUES MATRIX
  # SxSx2 array of action values. cells are abs[discrepancy between mitigation cost of true eagle rate and
  # cost of current belief]
  # true states are rows, beliefs are columns
  # TODO: currently using the mean of the collision prior. could/should update to represent 80th?
  values <- expand.grid(truth = S, belief = S)%>%
    mutate(value = -abs((belief - truth)*meanCollision*mitigation))%>%
    acast(truth ~ belief)

  values <- array(c(values, rep(-survey_costs$Low*10, length(S)^2)), dim = c(len, len, 2))
  dimnames(values)[[3]]<-c('mitigate', 'survey')

  # STEP 3: EXPECTED VALUES UNDER UNCERTAINTY

  # sum of value of current belief relative to true state * probability of states
  valUncertainMitigate <- sum(pS * -abs((curBelief - S)*meanCollision*mitigation))
  valUncertainSurvey <- -survey_costs$Low*10*len
  valUncertain <- max(valUncertainMitigate, valUncertainSurvey)

  ## STEP 4: EXPECTED VALUE UNDER CERTAINAY

  # value of each action given current belief relative to each state
  valCertainMitigate <- -abs((curBelief - S)*meanCollision*mitigation)
  valCertainSurvey <- -survey_costs$Low*10

  # optimal action for each state
  maxValsCertain <- data.frame('mitigate' = valCertainMitigate, 'survey' = valCertainSurvey)%>%
    apply(1, max)

  # sum of optimal action * probabilty of each state
  valCertain <- sum(pS * maxValsCertain)


  # STEP 5: EXPECTED VALUE AFTER MONTORING
  # P(s|y) = P(y|s)*P(s)/P(y)
  # we calculate P(s|y) by directly updating the posterior distribution based on a set amount of survey effort
  # and possible survey outcomes as in New et al. (2015)

  # P(y) probability of observing y eagles is Poisson with rate s
  # pObs <- expand.grid(truth = S, observed = S)%>%
  #   mutate(value = dpois(observed*10, truth*10)*dgamma(truth, expose$shape, expose$rate))%>%
  #   acast(truth~observed)%>%
  #   colSums()

  # or is this the predictive posterior (negative binomial)? Posterior predictive is the probability of
  # new observations, given hyperparameters and data, integrating over possible states (poisson rates)
  pObs <- dnbinom(S*10, mu = shape + (curBelief*10), size = 1/(1+rate+10))

  # P(s|y) - we can estimate directly due to conjugativity
  # we assume we monitor for 10 hours
  # creates an S x S matrix, rows are S, columns are Y
  pSY <- sapply(S, function(x){dgamma(S, shape + x*10, rate + 10)})

  # expected values for each management action after surveying
  # these will be the value of an action given true state * probability of that state after a result y
  expValsMitigate <- colSums(pSY * values[,,'mitigate'])
  expValsSurvey <- colSums(pSY * values[,,'survey'])

  expVals <- data.frame('mitigate' = expValsMitigate, 'survey' = expValsSurvey)
  rownames(expVals) <- S

  # value of optimal actions for each survey outcome
  maxVals <- apply(expVals, 1, max)

  # sum of maximum expected value for each survey outcom * probability of outcomes
  valPostMonitor <- sum(maxVals*pObs)

  valPerfect <- valCertain - valUncertain
  voi <- valPostMonitor - valUncertain
  return(list('voi' = voi, 'perfect' = valPerfect))
}

niters = 1000
hypothetical <- data.frame(erate = rep(NA, niters),
                           voi = rep(NA, niters),
                           perfect = rep(NA, niters),
                           shape = rep(NA, niters),
                           rate = rep(NA, niters))
S <- seq(0.1, 3, 0.1)
shape <- expose$shape
rate <- expose$rate
erate <- 3
for(i in 1:niters){
  outcome <- calc_voi(S, shape, rate)
  if(outcome$voi > 0)
    newObs <- rpois(1, erate*10)
  aprime <- shape+newObs
  bprime <- rate + 4.02
  shape <- aprime
  rate <- bprime
  hypothetical[i,] <- c(erate, outcome$voi, outcome$perfect, shape, rate)
}
