library(dplyr)
library(reshape2)
library(MDPtoolbox)

# FWS eagle activity (min/hr*km3) prior distribution is Gamma(0.968, 0.552)
expose <- list('shape' = 0.968, 'rate' = 0.552)
# FWS collision probability distribution is Beta(2.31, 396.69)
collide <- list('shape1' = 2.31, 'shape2' = 396.69)

meanCollision <- collide$shape1/collide$shape2

# Collision rate (fatalities/min) and eagle activity (min/hr*km3) are multiplied by an expansion factor
# determined by the size of a wind facility (hr*km3) to produce a final eagle fatality estimate
expFac <- 3650*(0.1)*(0.05^2)*pi
nturb <- 200
size <- nturb * expFac
mitigation <- filter(cost_table, Duration == 30, Rate == "Median", Cost == 'High')$M * size

## S: possible states of the system:
# one approach is that states represent our possible beliefs or true activity rates (?)
S<- seq(0.5, 3, 0.5)
# len <- length(S)

# another approach is states represent possible combos of true eagle activity rate and beliefs
s <- expand.grid(truth = S, belief = S)%>%
  mutate(state = paste(truth, belief, sep = "_"))
len <- nrow(s)

## R: SxSx2 array
# difference between the current state discrepancy between believed and true number of eagles
# and the discrepancy of any other state. We assume a facility with 100 turbines
transitions <- expand.grid(state1 = s$state, state2 = s$state)%>%
  left_join(s, by = c('state1' = 'state'))%>%
  left_join(s, by = c('state2' = 'state'))%>%
  mutate(value = -abs((truth.x - belief.x)*meanCollision*mitigation) - -abs((truth.y - belief.y)*meanCollision*mitigation))
values <- acast(transitions, state1 ~ state2, value.var = 'value')

R <- array(c(values, rep(survey_costs$Low*10, len^2)), dim = c(len, len, 2))
dimnames(R)[[3]]<-c('mitigate', 'survey')

## P: probability transition array
# probability of going from state S to state S(t+1) for each action

# probability of transitioning if we mitigate is 0. therefore identity matrix
# TODO: if state incorporates our strength of belief (i.e. survey efffort) then this is no longer identity
Pm <- diag(nrow = len, ncol = len)

# probability of transitioning from S to S(t+1) after surveying for x hours...

# S x S matrix holding probabilities of observing each state given each possible state is true
Ps <- mutate(transitions, prob = dpois(belief.y *10, truth.x*10))%>%
  mutate(prob = ifelse(truth.x == truth.y, prob, 0))%>%
  acast(state1 ~ state2, value.var = 'prob')

P <- array(c(Pm, Ps), dim = c(len, len, 2))
dimnames(P)[[3]]<-c('mitigate', 'survey')

analysis <- mdp_value_iteration(P, R, discount = 0.9, max_iter = 1000, epsilon = 0.02)
