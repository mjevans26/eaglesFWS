source('R/helper_fxns.R')
# define probabilities of each state, for us
prob.pres <- rep(c(0.1, 0.5), each = 10)
prob.abs <- 1-prob.pres

# costs of monitoring (survey) and protection (mitigation)
#mon.cost <- 2
#prot.cost <- 1

# decision consequences - benefits of actions given possible states
conseq.table <- data.frame('present' = c(1,0), 'absent' = c(0,0))
rownames(conseq.table) <- c('protect', 'dont protect')

# the benefit of surveying another hour or mitigating, given potential underlying eagle rates
valueTable <- data.frame('survey' = 0,
                         'mitigate' = 1)


# expected value of best action under uncertainty
#EQUATION 1 - EXPECTED VALUE OF ACTION "MITIGATE"
ex.val.protect <- prob.pres*conseq.table['protect', 'present']+prob.abs*conseq.table["protect", 'absent']
# ex.val.protect <- sum(eagleRates*valueTable[1,'mitigate'])
#EQUATION 1 - EXPECTED VALUE OF ACTION "DON'T PROTECT" FOR ALL PARCELS
ex.val.dont<-prob.pres*conseq.table["dont protect","present"]+prob.abs*conseq.table["dont protect","absent"]
# ex.val.dont <- sum(eagleRates*valueTable[1, 'survey'])

# this is single expected value per action (columns)
ex.val.all.uncert<-cbind(ex.val.protect,ex.val.dont)

# this is single maximum expected value
ex.val.dec.uncert<-apply(ex.val.all.uncert,1,max)

######expected value of best action under certainty

ex.val.all.cert<-cbind(ex.val.protect,ex.val.dont)

# sum of all
ex.val.dec.cert<-rowSums(ex.val.all.cert)
#EQUATION 3 - expected value decisions under certainty for all patches (sums best action for all possibilities)- note here it's same as uncertainty
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
# P(s|y) = P(y|s)*P(s)/P(y)
#probability present if found (true pos)
p.pres.if.found<-mon.acc["found","present"]*prob.pres/((mon.acc["found","present"]*prob.pres)+(mon.acc["found","absent"]*prob.abs))
#probability absent if found (false pos)
p.abs.if.found<-1-p.pres.if.found  #alternate: mon.acc["found","absent"]*prob.pres/((mon.acc["found","present"]*prob.pres)+(mon.acc["found","absent"]*prob.abs))

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
# sum of probability of outcomes across all possible states
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
