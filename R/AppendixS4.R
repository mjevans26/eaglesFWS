## Appendix S4 - R code walkthrough of case studies - Bennett et al. Multi-unit VOI


require(utils) # for making a data frame out of all combinations of priors and outcomes later
rm(list=ls())

########################################################################################################################################################
########################################################################################################################################################
#CASE STUDY 1
########################################################################################################################################################
########################################################################################################################################################




############################################################################################
#basic parameters
#patch=seq(1:16)
prob.pres<-c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5)
#priors=data.frame(patch,p)
prob.abs<-1-prob.pres

budget<-8
mon.cost<-2 #cost monitoring ALL units
prot.cost<-1 #cost protecting ONE unit
############################################################################################

############################################################################################
#decision consequences

conseq.table<-data.frame(c(1,0),c(0,0))
rownames(conseq.table)<-c("protect","don't protect")
colnames(conseq.table)<-c("present","absent")
############################################################################################

###################################################
######expected value of best action under uncertainty
ex.val.protect<-prob.pres*conseq.table["protect","present"]+prob.abs*conseq.table["protect","absent"]   #EQUATION 1 - EXPECTED VALUE OF ACTION "PROTECT" FOR ALL PARCELS
ex.val.dont<-prob.pres*conseq.table["don't protect","present"]+prob.abs*conseq.table["don't protect","absent"]  #EQUATION 1 - EXPECTED VALUE OF ACTION "DON'T PROTECT" FOR ALL PARCELS
ex.val.all.uncert<-cbind(ex.val.protect,ex.val.dont)

ex.val.dec.uncert<-apply(ex.val.all.uncert,1,max)  #EQUATION 2 - expected value decision under uncertainty all patches
###################################################

###################################################
######expected value of best action under certainty

ex.val.all.cert<-cbind(ex.val.protect,ex.val.dont)
ex.val.dec.cert<-rowSums(ex.val.all.cert)  #EQUATION 3 - expected value decisions under certainty for all patches (sums best action for all possibilities)- note here it's same as uncertainty
#this is because only the action "protect" can have a positive consequence - the others have zero (so you would do the same thing under certainty or uncertainty)
#so expected value of perfect info in initial scenario would be zero if only considered one patch
#see below for expected value of perfect info, one patch (actual calculation)
###################################################


###################################################
######expected value best action after monitoring

####
#monitoring accuracy table (confusion matrix)

mon.acc<-data.frame(c(0.8,0.2),c(0,1))
#mon.acc<-data.frame(c(0.7,0.3),c(0,1))  #an alternate
rownames(mon.acc)<-c("found","not found")
colnames(mon.acc)<-c("present","absent")    #this creates a confusiong matrix of probability found, not found when it's present or absent
####


#updated priors
p.pres.if.found<-mon.acc["found","present"]*prob.pres/((mon.acc["found","present"]*prob.pres)+(mon.acc["found","absent"]*prob.abs))    #probability present if found (true pos)
p.abs.if.found<-1-p.pres.if.found  #alternate: mon.acc["found","absent"]*prob.pres/((mon.acc["found","present"]*prob.pres)+(mon.acc["found","absent"]*prob.abs))    #probability absent if found (false pos)

p.pres.if.notfound<- mon.acc["not found","present"]*prob.pres/((mon.acc["not found","present"]*prob.pres)+(mon.acc["not found","absent"]*prob.abs))    #probability present if NOT found (false neg)
p.abs.if.notfound<- 1-p.pres.if.notfound


ex.val.protect.found<-p.pres.if.found*conseq.table["protect","present"]+p.abs.if.found*conseq.table["protect","absent"]
ex.val.dont.found<-p.pres.if.found*conseq.table["don't protect","present"]+p.abs.if.found*conseq.table["don't protect","absent"]

ex.val.found<-cbind(ex.val.protect.found,ex.val.dont.found)

ex.val.protect.notfound<-p.pres.if.notfound*conseq.table["protect","present"]+p.abs.if.notfound*conseq.table["protect","absent"]
ex.val.dont.notfound<-p.pres.if.notfound*conseq.table["don't protect","present"]+p.abs.if.notfound*conseq.table["don't protect","absent"]

ex.val.notfound<-cbind(ex.val.protect.notfound,ex.val.dont.notfound)

###
#outcome after survey

#prob of survey results

prob.found<-(mon.acc["found","present"]*prob.pres)+(mon.acc["found","absent"]*prob.abs)    #probability you find the species in each patch
prob.not.found<- 1-prob.found #alternate: (mon.acc["not found","present"]*prob.pres)+(mon.acc["not found","absent"]*prob.abs)


optimal.outcome.found<-apply(ex.val.found, 1, max)  #optimal outcomes if it's found
optimal.outcome.notfound<-apply(ex.val.notfound, 1, max)  #optimal outcomes if it's NOT found

expected.val.dec.after.mon<- optimal.outcome.found*prob.found + optimal.outcome.notfound*prob.not.found      #EQUATION 5 - expected value of decisions after monitoring
###################################################


###################################################
#expected values of perfect info and monitoring info - one patch only
exp.val.perfect.info<- ex.val.dec.cert - ex.val.dec.uncert   #EQUATION 4 - expected value perfect information if you were dealing with patches individually (or you had budget to protect all)
exp.val.mon.info<- expected.val.dec.after.mon - ex.val.dec.uncert #EQUATION 6 - expected value monitoring information if you were dealing with patches individually (or you had budget to protect all)
###################################################

###################################################
#summarized individual values among all patches

summarized.indiv.values<-as.data.frame(cbind(prob.pres,ex.val.found,ex.val.notfound,prob.found,prob.not.found,optimal.outcome.found,optimal.outcome.notfound,expected.val.dec.after.mon))              ##
###################################################


###################################################
#aggregated values
summ.agg<-aggregate(summarized.indiv.values, list(summarized.indiv.values$prob.pres), FUN = "mean")
###################################################



######################################################################################################
###########
###########calculations for multiple decisions with limited budget
##########

####ranking outcomes and predicting number of them

#########################################
#setting up the table of possible outcomes
unique.priors<-unique(prob.pres)
unique.outcomes<-rownames(mon.acc)
possible.combinations<-expand.grid(prior = unique.priors, survey.result = unique.outcomes)   #creates all combinations unique outcomes
#########################################


#########################################
###create a blank vector length = # bind it to Unique priors

name.vect<-c("prob.result","optimal.outcome","expected.value","number.patches","expected.total", "cost","rank")  #adding blank columns
possible.combinations[,name.vect] <- NA
#########################################

#########################################
#separating the survey outcomes and value of decisions for the two priors
prior.0.1<-subset(summ.agg, prob.pres == unique.priors[1])
prior.0.5<-subset(summ.agg, prob.pres == unique.priors[2])
#########################################


#if found and prior of 0.1
possible.combinations <- within(possible.combinations, prob.result[prior==0.1 & survey.result == 'found'] <- prior.0.1$prob.found)  #this fills in the probability of the result (in this case found with prior of 0.1), from the prior 0.1 subset above
possible.combinations <- within(possible.combinations, optimal.outcome[prior==0.1 & survey.result == 'found'] <- prior.0.1$optimal.outcome.found)

#if not found and prior of 0.1
possible.combinations <- within(possible.combinations, prob.result[prior==0.1 & survey.result == 'not found'] <- prior.0.1$prob.not.found)
possible.combinations <- within(possible.combinations, optimal.outcome[prior==0.1 & survey.result == 'not found'] <- prior.0.1$optimal.outcome.notfound)


#if found and prior of 0.5
possible.combinations <- within(possible.combinations, prob.result[prior==0.5 & survey.result == 'found'] <- prior.0.5$prob.found)
possible.combinations <- within(possible.combinations, optimal.outcome[prior==0.5 & survey.result == 'found'] <- prior.0.5$optimal.outcome.found)

#if not found and prior of 0.5
possible.combinations <- within(possible.combinations, prob.result[prior==0.5 & survey.result == 'not found'] <- prior.0.5$prob.not.found)
possible.combinations <- within(possible.combinations, optimal.outcome[prior==0.5 & survey.result == 'not found'] <- prior.0.5$optimal.outcome.notfound)

###############expected values for each combination of prior and found/not
possible.combinations$expected.value<-possible.combinations$prob.result*possible.combinations$optimal.outcome

###############numbers of patches corresponding to each prior
#if 0.1 prior
possible.combinations <- within(possible.combinations, number.patches[prior==0.1] <- 10)

#if 0.5 prior
possible.combinations <- within(possible.combinations, number.patches[prior==0.5] <- 10)

############### expected TOTAL value of information across all the patches corresponding to each prior
possible.combinations$expected.total<-possible.combinations$expected.value*possible.combinations$number.patches

############### cost for each combination
possible.combinations$cost<-1 #for this simulation assume costs all 1

############### rank of each combination by outcome
possible.combinations$rank<-possible.combinations$optimal.outcome/possible.combinations$cost   #for this scenario assume costs all 1
possible.combinations<-possible.combinations[order(-possible.combinations$rank),]

################################################
############expected values of info all patches
############

################################################
#current info - this orders the patches so the top current values are picked up to the budget
summarized.indiv.values<-summarized.indiv.values[order(-prob.pres),]  #note cost is 1 here so we can order the units in terms of probability of presence (because cost-effectiveness is prob presence divided by 1)
voi.current<-sum(summarized.indiv.values$prob.pres[1:budget])    #EQUATION 7: expected value of optimal group of actions with current information in the first simulation - sums up the current values up to the budget


################################################
#prefect info
summed.vals.perf<-sum(ex.val.dec.cert) #this sums up the values of perfect info for all the patches

if (summed.vals.perf < budget) {val.perfect<-summed.vals.perf} else       #EQUATION 8: EXPECTED VALUE OF OPTIMAL GROUP OF ACTIONS WITH PERFECT INFO
  #if we can afford to manage all our average possible outcomes (if we expect fewer positive outcomes than budget), VOI perfect is the                                                                           sum of all their perfect info values
{val.perfect<-budget}                                                      #but if we can't, expected value of optimal group of decisions with perfect info is just the budget (because if we would expect to manage our outcomes up to our budget and in this case the cost of managing the best outcome is 1)

#################################################
#expected value of monitoring info

#best action
exp.value.best.action1<-possible.combinations[1,]$expected.total     #we're looking for the best actions - we have only a few here, but can make more general using if/else statements

#because we protect what we see, cost is same as outcome for survey outcomes 'found'
remaining.budget<-budget-exp.value.best.action1

#for larger problems, checking whether we're within the busget can be automated with an if statement

#second best action
exp.value.best.action2<-possible.combinations[2,]$expected.total

remaining.budget<-remaining.budget-exp.value.best.action2

#third best action
exp.value.best.action3<-possible.combinations[3,]$optimal.outcome*remaining.budget    #here we use the remaining budget on next best outcome

########value of optimal gorup of actions after monitoring all patches
value.decision.monitoring<-sum(exp.value.best.action1,exp.value.best.action2,exp.value.best.action3)   #EQUATION 9: EXPECTED VALUE OF OPTIMAL GROUP OF ACTIONS WITH MONITORING INFO



############################################
##values of perfect and monitoring information, before considering monitoring cost
voi.perfect<-val.perfect-voi.current
voi.monitoring<-value.decision.monitoring-voi.current
############################################


#################adding monitoring cost
#######################################
#######################################


sac.occurrences<-possible.combinations[3,]$optimal.outcome*mon.cost #sacrificing the patch with the lowest outcome
total.managed.after.mon<-value.decision.monitoring - sac.occurrences
voi.monitoring.inc.cost<-total.managed.after.mon - voi.current   #EQUATION 9 AFTER ACCOUNTING FOR MONITORING COSTS


final.results<-cbind(voi.perfect,voi.monitoring,voi.monitoring.inc.cost)






############################################################################################################################################
############################################################################################################################################
#loop to explore effect of different accuracy levels

rm(list=ls())

require(utils) # for making a data frame out of all combinations of priors and outcomes later


############################################################################################
#basic parameters
#patch=seq(1:20)
prob.pres<-c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5)
#priors=data.frame(patch,p)
prob.abs<-1-prob.pres

budget<-8
mon.cost<-2 #cost monitoring ALL
prot.cost<-1 #cost protecting ONE
############################################################################################

############################################################################################
#decision consequences

conseq.table<-data.frame(c(1,0),c(0,0))
rownames(conseq.table)<-c("protect","don't protect")
colnames(conseq.table)<-c("present","absent")
############################################################################################

###################################################
######expected value of decision under uncertainty
ex.val.protect<-prob.pres*conseq.table["protect","present"]+prob.abs*conseq.table["protect","absent"]
ex.val.dont<-prob.pres*conseq.table["don't protect","present"]+prob.abs*conseq.table["don't protect","absent"]
ex.val.all.uncert<-cbind(ex.val.protect,ex.val.dont)

ex.val.dec.uncert<-apply(ex.val.all.uncert,1,max)  #expected value decision under uncertainty all patches
###################################################

###################################################
######expected value under certainty

ex.val.all.cert<-cbind(ex.val.protect,ex.val.dont)
ex.val.dec.cert<-rowSums(ex.val.all.cert)  #expected value decisions under certainty for all patches - note in initial scenario it's same as uncertainty
#this is because only the action "protect" can have a positive consequence - the others have zero (so you would do the same thing under certainty or uncertainty)
#so expected value of perfect info in initial scenario would be zero if only considered one patch
#see below for expected value of perfect info, one patch (actual calculation)
###################################################


###################################################
######expected value after monitoring

####
#monitoring accuracy table (confusion matrix)

### ### ### ###
acc.levels<-seq(0.05,0.95, 0.05)  ### ### ### ###

summ.all.table<-data.frame(
  true.pos = numeric(0),
  false.neg = numeric(0),
  voi.perfect = numeric(0),
  voi.monitoring = numeric(0),
  voi.monitoring.inc.cost =numeric (0)
)

for (i in 1:length(acc.levels))      ### ### ### ###

{

  true.pos<-acc.levels[i]  #percent found if there      ### ### ### ###
  false.neg<-1-true.pos     #percent not found if there   ### ### ### ###

  mon.acc<-data.frame(c(true.pos,false.neg),c(0,1))
  #mon.acc<-data.frame(c(0.8,0.2),c(0,1))
  rownames(mon.acc)<-c("found","not found")
  colnames(mon.acc)<-c("present","absent")    #this creates a confusiong matrix of probability found, not found when it's present or absent
  ####


  #updated priors
  p.pres.if.found<-mon.acc["found","present"]*prob.pres/((mon.acc["found","present"]*prob.pres)+(mon.acc["found","absent"]*prob.abs))    #probability present if found (true pos)
  p.abs.if.found<-1-p.pres.if.found  #alternate: mon.acc["found","absent"]*prob.pres/((mon.acc["found","present"]*prob.pres)+(mon.acc["found","absent"]*prob.abs))    #probability absent if found (false pos)

  p.pres.if.notfound<- mon.acc["not found","present"]*prob.pres/((mon.acc["not found","present"]*prob.pres)+(mon.acc["not found","absent"]*prob.abs))    #probability present if NOT found (false neg)
  p.abs.if.notfound<- 1-p.pres.if.notfound


  ex.val.protect.found<-p.pres.if.found*conseq.table["protect","present"]+p.abs.if.found*conseq.table["protect","absent"]
  ex.val.dont.found<-p.pres.if.found*conseq.table["don't protect","present"]+p.abs.if.found*conseq.table["don't protect","absent"]

  ex.val.found<-cbind(ex.val.protect.found,ex.val.dont.found)

  ex.val.protect.notfound<-p.pres.if.notfound*conseq.table["protect","present"]+p.abs.if.notfound*conseq.table["protect","absent"]
  ex.val.dont.notfound<-p.pres.if.notfound*conseq.table["don't protect","present"]+p.abs.if.notfound*conseq.table["don't protect","absent"]

  ex.val.notfound<-cbind(ex.val.protect.notfound,ex.val.dont.notfound)

  ###
  #outcome after survey

  #prob of survey results

  prob.found<-(mon.acc["found","present"]*prob.pres)+(mon.acc["found","absent"]*prob.abs)    #probability you find the species in each patch
  prob.not.found<- 1-prob.found #alternate: (mon.acc["not found","present"]*prob.pres)+(mon.acc["not found","absent"]*prob.abs)


  optimal.outcome.found<-apply(ex.val.found, 1, max)  #optimal outcomes if it's found
  optimal.outcome.notfound<-apply(ex.val.notfound, 1, max)  #optimal outcomes if it's NOT found

  expected.val.dec.after.mon<- optimal.outcome.found*prob.found + optimal.outcome.notfound*prob.not.found      #expected value of decisions after monitoring
  ###################################################


  ###################################################
  #expected values of perfect info and monitoring info - one patch only
  exp.val.perfect.info<- ex.val.dec.cert - ex.val.dec.uncert   #expected value perfect information if you were dealing with patches individually (or you had budget to protect all)
  exp.val.mon.info<- expected.val.dec.after.mon - ex.val.dec.uncert #expected value monitoring information if you were dealing with patches individually (or you had budget to protect all)
  ###################################################

  ###################################################
  #summarized individual values among all patches

  summarized.indiv.values<-as.data.frame(cbind(prob.pres,ex.val.found,ex.val.notfound,prob.found,prob.not.found,optimal.outcome.found,optimal.outcome.notfound,expected.val.dec.after.mon))              ##
  ###################################################


  ###################################################
  #aggregated values
  summ.agg<-aggregate(summarized.indiv.values, list(summarized.indiv.values$prob.pres), FUN = "mean")
  ###################################################



  ######################################################################################################
  ###########
  ###########calculations for multiple decisions with limited budget
  ##########

  ####ranking outcomes and predicting number of them

  #########################################
  #setting up the table of possible outcomes
  unique.priors<-unique(prob.pres)
  unique.outcomes<-rownames(mon.acc)
  possible.combinations<-expand.grid(prior = unique.priors, survey.result = unique.outcomes)   #creates all combinations unique outcomes
  #########################################


  #########################################
  ###create a blank vector length = # bind it to Unique priors

  name.vect<-c("prob.result","optimal.outcome","expected.value","number.patches","expected.total", "cost","rank")  #adding blank columns
  possible.combinations[,name.vect] <- NA
  #########################################

  #########################################
  #separating the survey outcomes and value of decisions for the two priors
  prior.0.1<-subset(summ.agg, prob.pres == unique.priors[1])
  prior.0.5<-subset(summ.agg, prob.pres == unique.priors[2])
  #########################################


  #if found and prior of 0.1
  possible.combinations <- within(possible.combinations, prob.result[prior==0.1 & survey.result == 'found'] <- prior.0.1$prob.found)  #this fills in the probability of the result (in this case found with prior of 0.1), from the prior 0.1 subset above
  possible.combinations <- within(possible.combinations, optimal.outcome[prior==0.1 & survey.result == 'found'] <- prior.0.1$optimal.outcome.found)

  #if not found and prior of 0.1
  possible.combinations <- within(possible.combinations, prob.result[prior==0.1 & survey.result == 'not found'] <- prior.0.1$prob.not.found)
  possible.combinations <- within(possible.combinations, optimal.outcome[prior==0.1 & survey.result == 'not found'] <- prior.0.1$optimal.outcome.notfound)


  #if found and prior of 0.5
  possible.combinations <- within(possible.combinations, prob.result[prior==0.5 & survey.result == 'found'] <- prior.0.5$prob.found)
  possible.combinations <- within(possible.combinations, optimal.outcome[prior==0.5 & survey.result == 'found'] <- prior.0.5$optimal.outcome.found)

  #if not found and prior of 0.5
  possible.combinations <- within(possible.combinations, prob.result[prior==0.5 & survey.result == 'not found'] <- prior.0.5$prob.not.found)
  possible.combinations <- within(possible.combinations, optimal.outcome[prior==0.5 & survey.result == 'not found'] <- prior.0.5$optimal.outcome.notfound)

  ###############expected values for each combination of prior and found/not
  possible.combinations$expected.value<-possible.combinations$prob.result*possible.combinations$optimal.outcome

  ###############numbers of patches corresponding to each prior
  #if 0.1 prior
  possible.combinations <- within(possible.combinations, number.patches[prior==0.1] <- 10)

  #if 0.5 prior
  possible.combinations <- within(possible.combinations, number.patches[prior==0.5] <- 10)

  ############### expected TOTAL value of information across all the patches corresponding to each prior
  possible.combinations$expected.total<-possible.combinations$expected.value*possible.combinations$number.patches

  ############### cost for each combination
  possible.combinations$cost<-1 #for this simulation assume costs all 1

  ############### rank of each combination by outcome
  possible.combinations$rank<-possible.combinations$optimal.outcome/possible.combinations$cost   #for this simulation assume costs all 1
  possible.combinations<-possible.combinations[order(-possible.combinations$rank),]

  ################################################
  ############values info all patches
  ############

  ################################################
  #current info - this orders the patches so the top current values are picked up to the budget
  summarized.indiv.values<-summarized.indiv.values[order(-prob.pres),]  #will have to make this more generic later - so we include cost and select up to budget
  voi.current<-sum(summarized.indiv.values$prob.pres[1:budget])    #value of current information in the first simulation - sums up the current values up to the budget


  ################################################
  #prefect info
  summed.vals.perf<-sum(ex.val.dec.cert) #this sums up the values of perfect info for all the patches

  if (summed.vals.perf < budget) {val.perfect<-summed.vals.perf} else       #if we can afford to manage all our average possible outcomes (if we expect fewer positive outcomes than budget), VOI perfect is the sum of all their perfect info values
  {val.perfect<-budget}                                                      #but if we can't, VOI of perfect info is just the budget (because if we would expect to manage our outcomes up to our budget)

  #################################################
  #expected value of monitoring info

  #best action
  exp.value.best.action1<-possible.combinations[1,]$expected.total     #will need to make this more generalizable, and into a series of if...else statament so budget fills up

  #because we protect what we see, cost is same as outcome for survey outcomes 'found'
  remaining.budget<-budget-exp.value.best.action1

  ### ### ###
  #add an if statement checking if remaining budget >0 - if we're not, then we need to only do the action above up to the budget (so the else part of the statement can be value.decision.monitoring - it's finished here)
  ### ### ###

  #second best action
  exp.value.best.action2<-possible.combinations[2,]$expected.total

  remaining.budget<-remaining.budget-exp.value.best.action2

  #third best action
  exp.value.best.action3<-possible.combinations[3,]$optimal.outcome*remaining.budget    #using remaining budget on next best outcome

  ########value of the decision after monitoring all patches
  value.decision.monitoring<-sum(exp.value.best.action1,exp.value.best.action2,exp.value.best.action3)



  ############################################
  ##values of perfect and monitoring information, before considering monitoring cost
  voi.perfect<-val.perfect-voi.current
  voi.monitoring<-value.decision.monitoring-voi.current
  ############################################


  #################adding monitoring cost
  #######################################
  #######################################


  sac.occurrences<-possible.combinations[3,]$optimal.outcome*mon.cost #sacrificing the patch with the lowest outcome
  total.managed.after.mon<-value.decision.monitoring - sac.occurrences
  voi.monitoring.inc.cost<-total.managed.after.mon - voi.current


  final.results<-cbind(true.pos, false.neg, voi.perfect,voi.monitoring,voi.monitoring.inc.cost)

  summ.all.table<-rbind(summ.all.table, final.results)    ### ### ### ###

} ### ### ### ### end of loop


summ.all.table


write.csv(summ.all.table, file = "C:/Users/josep/Dropbox/VOI/survey_accuracy_test.csv")

plot(voi.monitoring.inc.cost~true.pos, data = summ.all.table)






############################################################################################################################################
############################################################################################################################################
#loop to explore different monitoring costs

mon.costs<-seq(0,6,0.5)
mon.dollars<-seq(0,1500,125)
expected.units<-possible.combinations$prob.result*possible.combinations$number.patches
possible.combinations<-cbind(possible.combinations, expected.units)

#jj<-5

voi.diff.mon.costs<-c()

for (jj in 1:length(mon.costs))      ### ### ### ###

{

  mon.cost<-mon.costs[jj]

  remaining.budget<-budget-mon.cost

  #coult loop this instead of specifying column numbers - in Calla's case you'd fund whole units until budget was reached

  #protecting in order until remaining budget is reached

  if (possible.combinations$expected.units[1] > remaining.budget) {val.mon.seq.test<-remaining.budget*possible.combinations$optimal.outcome[1]
  remaining.budget<-0
  }
  else       #if we can afford to manage all our average possible
  {
    remaining.budget<-remaining.budget-possible.combinations$expected.units[1]
    val.mon.seq.test<-possible.combinations$expected.total[1]
  }


  if (possible.combinations$expected.units[2] > remaining.budget) {val.mon.seq.test<-val.mon.seq.test+remaining.budget*possible.combinations$optimal.outcome[2]
  remaining.budget<-0

  } else       #if we can afford to manage all our average possible
  {
    remaining.budget<-remaining.budget-possible.combinations$expected.units[2]
    val.mon.seq.test<-val.mon.seq.test+possible.combinations$expected.total[2]
  }


  if (possible.combinations$expected.units[3] > remaining.budget) {val.mon.seq.test<-val.mon.seq.test+remaining.budget*possible.combinations$optimal.outcome[3]} else       #if we can afford to manage all our average possible
  {
    remaining.budget<-remaining.budget-possible.combinations$expected.units[3]
    val.mon.seq.test<-val.mon.seq.test+possible.combinations$expected.total[3]
  }

  voi.diff.mon.costs[jj]<-val.mon.seq.test-4 ##because in this case study the value of current information is 4

}

voi.mon.costs.df<-data.frame(mon.costs,voi.diff.mon.costs,mon.dollars)

plot(voi.diff.mon.costs~mon.costs, data = voi.mon.costs.df)
plot(voi.diff.mon.costs~mon.dollars, data = voi.mon.costs.df)









########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
#CASE STUDY 2
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################




############################################################################################
#basic parameters
#patch=seq(1:16)
#priors=data.frame(patch,p)

budget<-17
############################################################################################

############################################################################################
#decision consequences

conseq.table<-data.frame(c(2,1,0),c(0,0,0))
rownames(conseq.table)<-c("endangered","threatened","not threatened")
colnames(conseq.table)<-c("manage","don't manage")
############################################################################################

############################################################################################
#monitoring accuracy

survey.acc<-data.frame(c(0,0.75,0.25),c(0.25,0.5,0.25),c(0.25,0.75,0))
rownames(survey.acc)<-c("prob too low","prob correct","prob too high")
colnames(survey.acc)<-c("endangered","threatened","not threatened")


##############################################################################################################
##############################################################################################################
#single decision


###################################################
######expected value of decision under uncertainty - for initial designations as threatened
ex.val.manage<-survey.acc["prob too low", "threatened"]*conseq.table["endangered","manage"]+survey.acc["prob correct", "threatened"]*conseq.table["threatened","manage"]+survey.acc["prob too high", "threatened"]*conseq.table["not threatened","manage"]  #EQUATION 1: EXPECTED VALUE OF ACTION "MANAGE" WITH CURRENT INFO
ex.val.dont<-survey.acc["prob too low", "threatened"]*conseq.table["endangered","don't manage"]+survey.acc["prob correct", "threatened"]*conseq.table["threatened","don't manage"]+survey.acc["prob too high", "threatened"]*conseq.table["not threatened","don't manage"]   #EQUATION 1: EXPECTED VALUE OF ACTION "DO NOT MANAGE" WITH CURRENT INFO
ex.val.all.uncert<-cbind(ex.val.manage,ex.val.dont)

ex.val.dec.uncert<-apply(ex.val.all.uncert,1,max)  #EQUATION 2: expected value decision under uncertainty
###################################################


###################################################
######expected value of decision under certainty

ex.val.all.cert<-cbind(ex.val.manage,ex.val.dont)
ex.val.dec.cert<-rowSums(ex.val.all.cert)  #EQUATION 3: expected value decision under certainty for all species - note in initial scenario it's same as uncertainty
#this is because only the action "manage" can have a positive consequence - the others have zero (so you would do the same thing under certainty or uncertainty)
#so expected value of perfect info in initial scenario would be zero if only considered one species
#see below for expected value of perfect info, one species (actual calculation)
###################################################


###################################################
######expected value decision after monitoring

####
####survey accuracy table is above


####probabilities of results:
#result = endangered
prob.result.endang<-survey.acc["prob too low", "threatened"] * survey.acc["prob correct", "endangered"] + survey.acc["prob correct", "threatened"] * survey.acc["prob too high", "endangered"]
#this is prior prob it's endangered * prob you'll get the correct result if endangered + prior prob it's threatened * prob it gets misclassified as endangered

#result = threatened
prob.result.threatened<-survey.acc["prob too high", "threatened"] * survey.acc["prob too high", "endangered"] + survey.acc["prob correct", "threatened"] * survey.acc["prob correct", "threatened"] + survey.acc["prob too low", "threatened"] * survey.acc["prob too low", "not threatened"]

#result - not threatened
prob.result.not<-survey.acc["prob too high", "threatened"] * survey.acc["prob correct", "not threatened"] + survey.acc["prob correct", "threatened"] * survey.acc["prob too low", "not threatened"]


####################
#updated priors and expected values
####################

#############after result = endangered

#prob endangered
prob.endangered.res.endangered<-survey.acc["prob too low", "threatened"] * survey.acc["prob correct", "endangered"] / (survey.acc["prob too low", "threatened"] * survey.acc["prob correct", "endangered"]+ survey.acc["prob correct", "threatened"] * survey.acc["prob too high", "endangered"])
#prob endangered | result endangered = prior prob endangered * prob result endangered | endangered / (prob result endangered)

#prob threatened
prob.threatened.res.endangered<-survey.acc["prob correct", "threatened"] * survey.acc["prob too high", "endangered"] / (survey.acc["prob too low", "threatened"] * survey.acc["prob correct", "endangered"]+ survey.acc["prob correct", "threatened"] * survey.acc["prob too high", "endangered"])

#expected value management after result = endangered
exp.val.res.endangered<- prob.endangered.res.endangered * conseq.table["endangered","manage"] + prob.threatened.res.endangered * conseq.table["threatened","manage"]


############after result = threatened

#prob endangered
prob.endangered.res.threatened<-survey.acc["prob too low", "threatened"] * survey.acc["prob too low", "threatened"] / (survey.acc["prob too low", "threatened"] * survey.acc["prob too low", "threatened"] + survey.acc["prob correct", "threatened"] * survey.acc["prob correct", "threatened"] + survey.acc["prob too high", "threatened"] * survey.acc["prob too high", "threatened"])

#prob threatened
prob.threatened.res.threatened<-survey.acc["prob correct", "threatened"] * survey.acc["prob correct", "threatened"] / (survey.acc["prob too low", "threatened"] * survey.acc["prob too low", "threatened"] + survey.acc["prob correct", "threatened"] * survey.acc["prob correct", "threatened"] + survey.acc["prob too high", "threatened"] * survey.acc["prob too high", "threatened"])

#prob not threatened
prob.not.res.threatened<-survey.acc["prob too high", "threatened"] * survey.acc["prob too high", "threatened"] / (survey.acc["prob too low", "threatened"] * survey.acc["prob too low", "threatened"] + survey.acc["prob correct", "threatened"] * survey.acc["prob correct", "threatened"] + survey.acc["prob too high", "threatened"] * survey.acc["prob too high", "threatened"])

#expected value management after result = threatened
exp.val.res.threatened<-prob.endangered.res.threatened * conseq.table["endangered","manage"] + prob.threatened.res.threatened * conseq.table["threatened","manage"]


############after result = not threatened

#prob threatened
prob.threatened.res.not<-survey.acc["prob correct", "threatened"] * survey.acc["prob too low", "not threatened"] / (survey.acc["prob too high", "threatened"] * survey.acc["prob correct", "not threatened"] + survey.acc["prob correct", "threatened"] * survey.acc["prob too low", "not threatened"])


#prob not threatened
prob.not.res.not<-survey.acc["prob too high", "threatened"] * survey.acc["prob correct", "not threatened"] / (survey.acc["prob too high", "threatened"] * survey.acc["prob correct", "not threatened"] + survey.acc["prob correct", "threatened"] * survey.acc["prob too low", "not threatened"])

#expected value management after result = not threatened
exp.val.res.not<- prob.threatened.res.not * conseq.table["threatened","manage"]   #no value if not threatened


#################expected value optimal action with monitoring info - not considering cost
exp.val.dec.mon<-exp.val.res.endangered*prob.result.endang + exp.val.res.threatened*prob.result.threatened + exp.val.res.not*prob.result.not
#EQUATION 5: EXPECTED VALUE DECISION AFTER MONITORING (sum of expected value of optimal actions given result * probability of getting results)


#################expected value optimal action with monitoring info - considering cost
#################cost is 10% probability that an endangered species will go extinct
exp.val.dec.mon.with.cost<-exp.val.res.endangered*prob.result.endang + exp.val.res.threatened*prob.result.threatened + exp.val.res.not*prob.result.not - 0.1 * survey.acc["prob too low", "threatened"] * conseq.table["endangered","manage"]
#EQUATION 5: EXPECTED VALUE DECISION AFTER MONITORING - ACCOUNTING FOR MONITORING COST




########################################################################################################################################################
########################################################################################################################################################
###########calculations for multiple decisions with limited budget


#######################################################################
#expected number of species for each category and expected total values
#endangered
exp.num.endangered<-survey.acc["prob too low", "threatened"] *20
exp.tot.value.endangered<-exp.num.endangered*conseq.table["endangered","manage"]

#threatened
exp.num.threatened<-survey.acc["prob correct", "threatened"] *20
exp.tot.value.threatened<-exp.num.threatened*conseq.table["threatened","manage"]

#not threatened
exp.num.not<-survey.acc["prob too high", "threatened"] *20
exp.tot.value.not<-exp.num.not*0 #no value in not threatened, in this case
#######################################################################


#######################################################################

########expected value decision with current info - if manage 17

exp.value.dec.curr<-17*(survey.acc["prob too low", "threatened"] * conseq.table["endangered","manage"] + survey.acc["prob correct", "threatened"] * conseq.table["threatened","manage"] + survey.acc["prob too high", "threatened"] * conseq.table["not threatened","manage"])  #EQUATION 7: EXPECTED VALUE OF DECISIONS WITH CURRENT INFORMATION (MULTIPLE UNITS)
#if they all have same prior, can use expected value current for one species - but calculations are provided for illustration


#######################################################################

########expected value decison with perfect info - if manage 17
exp.value.dec.perfect<-exp.num.endangered*conseq.table["endangered","manage"] + exp.num.threatened * conseq.table["threatened","manage"] #EQUATION 8: EXPECTED VALUE DECISIONS WITH PERFECT INFORMATION (MULTIPEL UNITS)
#first one is best action, use up budget for 5, then second one is second best, use up budget for 10 - after that you can stop because no value for not threatened


#############expected value of perfect information
exp.val.perfect.info<-exp.value.dec.perfect-exp.value.dec.curr    #EQUATION 4: EXPECTED VALUE OF PERFECT INFORMATION (MULTIPLE UNITS)
###############

#######################################################################


#######################################################################

########expected value decision with monitoring info

#expected number with result endangered
exp.num.res.endangered<-prob.result.endang*20

#total expected value result endangered
exp.tot.value.res.endangered<-exp.num.res.endangered*exp.val.res.endangered

#leftover budget after we manage species with result endangered
leftover.budget<-17-exp.num.res.endangered
#leftover is 10.75

#expected number with result threatened
exp.num.res.threatened<-prob.result.threatened*20
#expected number with this result is 7.5, which is lower than leftover budget

#total expected value result threatened
exp.tot.value.res.threatened<-exp.num.res.threatened*exp.val.res.threatened

#leftover budget after we manage species with result threatened
leftover.budget<-leftover.budget-exp.num.res.threatened
#leftover is 3.25

#expected number with result not
exp.num.res.not<-prob.result.not*20
#expected number with this result is 7.5, which is HIGHER than the leftover budget of 3.25, so we can only expect to manage 3.25

#total expected value result not
exp.tot.value.res.not<-exp.num.res.not*exp.val.res.not

#total value we'd manage result not
exp.value.managed.not<-leftover.budget*exp.val.res.not


#####expected total value across results
exp.tot.value.managed<-exp.tot.value.res.endangered+exp.tot.value.res.threatened+exp.value.managed.not   #EQUATION 9: EXPECTED VALUE OF DECISIONS WITH MONITORING INFORMATION (MULTIPLE UNITS)


#############expected value monitoring info - no monitoring loss
exp.val.monitoring.info.no.loss<-exp.tot.value.managed-exp.value.dec.curr   #EQUATION 6: EXPECTED VALUE OF MONITORING INFORMATION (MULTIPLE UNITS)
###############

#######################################################################


####################now considering loss when monitoring - 10% prob endangered species will go extinct

exp.loss.endangered.spp<-0.1*exp.num.endangered #10% of prior probability endangered (not result endangered)

#expected number with result endangered
exp.num.res.endangered<-prob.result.endang*20-exp.loss.endangered.spp
#this makes the assumption that we'd discover which species went extinct and not manage it; also that we'd lose it from 'endangered' catgegory because it would not be misclassified as threatened

#total expected value result endangered
exp.tot.value.res.endangered<-exp.num.res.endangered*exp.val.res.endangered

#leftover budget after we manage species with result endangered
leftover.budget<-17-exp.num.res.endangered
#leftover is now 11.25

#expected number with result threatened
exp.num.res.threatened<-prob.result.threatened*20
#expected number with this result is 7.5, which is lower than leftover budget

#total expected value result threatened
exp.tot.value.res.threatened<-exp.num.res.threatened*exp.val.res.threatened

#leftover budget after we manage species with result threatened
leftover.budget<-leftover.budget-exp.num.res.threatened
#leftover is now 3.75

#expected number with result not
exp.num.res.not<-prob.result.not*20
#expected number with this result is 7.5, which is HIGHER than the leftover budget of 3.75, so we can only expect to manage 3.75

#total expected value result not
exp.tot.value.res.not<-exp.num.res.not*exp.val.res.not

#total value we'd manage result not
exp.value.managed.not<-leftover.budget*exp.val.res.not


#####expected total value across results
exp.tot.value.managed<-exp.tot.value.res.endangered+exp.tot.value.res.threatened+exp.value.managed.not    #EQUATION 9: EXPECTED VALUE OF DECISIONS WITH MONITORING INFORMATION (MULTIPLE UNITS) - considering monitoring cost



#############expected value monitoring info - WITH monitoring loss
exp.val.monitoring.info.with.loss<-exp.tot.value.managed-exp.value.dec.curr    #EQUATION 6: EXPECTED VALUE OF MONITORING INFORMATION (MULTIPLE UNITS) - considering monitoring cost
###############
