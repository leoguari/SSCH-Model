##INDIVIDUAL INTERVENTIONS


##Food system
## just changing SSB
ssb.auxs <- base.auxs
ssb.auxs["aSSBPriceChange"] <- 20
ssb.20 <- data.frame(ode(y=stocks, times=simtime, func = model, 
                         parms=ssb.auxs, method='euler'))

# Subsidies to FV
lower.fv.auxs <- base.auxs
lower.fv.auxs["aPriceChangeFV"] <- -20
lower.fv.20 <- data.frame(ode(y=stocks, times=simtime, func = model, 
                              parms=lower.fv.auxs, method='euler'))

#front of package labeling for nutrient warning
fopl.auxs <- base.auxs
fopl.auxs["aLabels.switch"] <- 1
fopl <- data.frame(ode(y=stocks, times=simtime, func = model, 
                       parms=fopl.auxs, method='euler'))

# price increase on ultra-processed foods, no SSBs
upf.tax.auxs <- base.auxs
upf.tax.auxs["aPriceChangeUH"] <- 20
upf.20 <- data.frame(ode(y=stocks, times=simtime, func = model, 
                         parms=upf.tax.auxs, method='euler'))

## compare outcomes to baseline
ind.food <- data.frame(base.case$time, base.case$dmprev, ssb.20$dmprev, upf.20$dmprev, lower.fv.20$dmprev,
                       fopl$dmprev)
names(ind.food) <- c("time", "baseline", "ssb.20", "upf.20", "fv.20", "fopl")

ind.food.ob <- data.frame(base.case$time, base.case$Obesity, ssb.20$Obesity, upf.20$Obesity, lower.fv.20$Obesity,
                       fopl$Obesity)
names(ind.food.ob) <- c("time", "baseline", "ssb.20", "upf.20", "fv.20", "fopl")

## combined price changes on food
food.fiscal.auxs <- base.auxs
food.fiscal.auxs["aPriceChangeUH"] <- 20
food.fiscal.auxs["aSSBPriceChange"] <- 20
food.fiscal.auxs["aPriceChangeFV"] <- -20

food.fiscal.20 <- data.frame(ode(y=stocks, times=simtime, func = model, 
                                 parms=food.fiscal.auxs, method='euler'))

campaign.auxs <- base.auxs
campaign.auxs["aFVPH.switch"] <- 1
campaign.auxs["aLabels.switch"] <- 1
food.campaigns <- data.frame(ode(y=stocks, times=simtime, func = model, 
                                 parms=campaign.auxs, method='euler'))

## combined food interventions
food.combined <- data.frame(ode(y=stocks, times=simtime, func = model, 
                                parms=food.auxs, method='euler'))

### Comparing food interventions
food.multi.plot <- ggplot() +
  geom_line(data=base.case[base.case$time %in% c(2020:2050),], aes(x=time, y=Obesity, color='Baseline')) +
  geom_line(data=fopl[fopl$time %in% c(2020:2050),], aes(x=time, y=Obesity, color='Front of package labeling only')) +
  geom_line(data=lower.fv.20[lower.fv.20$time %in% c(2020:2050),], aes(x=time, y=Obesity, color='20% price decrease on F&V')) +
  geom_line(data=food.campaigns[food.campaigns$time %in% c(2020:2050),], aes(x=time, y=Obesity, color='Information campaigns only')) +
  geom_line(data=food.fiscal.20[food.fiscal.20$time %in% c(2020:2050),], aes(x=time, y=Obesity, color='Price inc. SSB and UPF, dec. F&V - 20%')) +
  geom_line(data=food.combined[food.combined$time %in% c(2020:2050),], aes(x=time, y=Obesity, color='Fiscal interventions, campaigns, and labelling')) +
  labs(x="Year", y="Obesity prevalence (%)", color = 'Scenarios', title = "Comparison of food system interventions on obesity prevalence")
food.multi.plot + theme_bw()

food.multi.dm.plot <- ggplot() +
  geom_line(data=base.case[base.case$time %in% c(2020:2050),], aes(x=time, y=dmprev, color='Baseline')) +
  geom_line(data=fopl[fopl$time %in% c(2020:2050),], aes(x=time, y=dmprev, color='Front of package labeling only')) +
  geom_line(data=lower.fv.20[lower.fv.20$time %in% c(2020:2050),], aes(x=time, y=dmprev, color='20% price decrease on F&V')) +
  geom_line(data=food.campaigns[food.campaigns$time %in% c(2020:2050),], aes(x=time, y=dmprev, color='Information campaigns only')) +
  geom_line(data=food.fiscal.20[food.fiscal.20$time %in% c(2020:2050),], aes(x=time, y=dmprev, color='Price inc. SSB and UPF, dec. F&V - 20%')) +
  geom_line(data=food.combined[food.combined$time %in% c(2020:2050),], aes(x=time, y=dmprev, color='Fiscal interventions, campaigns, and labelling')) +
  labs(x="Year", y="Diabetes prevalence (%)", color = 'Scenarios', title = "Comparison of food system interventions on diabetes prevalence")
food.multi.dm.plot + theme_bw()

############################################################################################################
## WEIGHT LOSS and OBESITY FOR DM CHANGES
## hold obesity steady
wt.auxs <- base.auxs
wt.auxs["ob.freeze.switch"] <- 1
wt.auxs["obese.year.level"] <- base.case$Obesity[base.case$time == 2020]
freeze.obesity <- data.frame(ode(y=stocks, times=simtime, func = model, 
                                 parms=wt.auxs, method='euler'))

# lose 500g for men and women
wt.auxs["ob.freeze.switch"] <- 0
wt.auxs["ob.decrease.switch"] <- 1
wt.auxs["weight.loss.M"]<- -0.5
wt.auxs["weight.loss.W"] <- -0.5
decrease.obesity.all <- data.frame(ode(y=stocks, times=simtime, func = model, 
                                       parms=wt.auxs, method='euler'))
# lose 500g for women
wt.auxs["ob.decrease.switch"] <- 1
wt.auxs["weight.loss.M"] <- 0
wt.auxs["weight.loss.W"] <- -0.5
decrease.obesity.w <- data.frame(ode(y=stocks, times=simtime, func = model, 
                                     parms=wt.auxs, method='euler'))


## DPP style relative reduction in incidence
dpp.auxs <- base.auxs
dpp.auxs["dpp.switch"] <- 1
dpp<- data.frame(ode(y=stocks, times=simtime, func = model, 
                     parms=dpp.auxs, method='euler'))

## prevent predm
prev.predm.auxs <- base.auxs
prev.predm.auxs["predm.switch"] <- 1
prev.predm.auxs["predm.red.size"] <- 0.8
prev.predm <- data.frame(ode(y=stocks, times=simtime, func = model, 
                             parms=prev.predm.auxs, method='euler'))


obese.decrease.dm.plot <- ggplot() +
  geom_line(data=base.case[base.case$time %in% c(2020:2050),],  aes(x=time, y=dmprev, color='Baseline')) +
  geom_line(data=decrease.obesity.all[decrease.obesity.all$time %in% c(2020:2050),],  aes(x=time, y=dmprev, color='500g decrease per year - everyone')) +
  geom_line(data=freeze.obesity[freeze.obesity$time %in% c(2020:2050),],  aes(x=time, y=dmprev, color='No increase in obesity after 2020')) +
  geom_line(data=decrease.obesity.w[decrease.obesity.w$time %in% c(2020:2050),],  aes(x=time, y=dmprev, color='500g decrease per year - women only')) +
  labs(x="Year", y="Diabetes prevalence (%)", color = 'Scenarios', title = "Effects of weight loss on diabetes prevalence")
obese.decrease.dm.plot + theme_bw()

obese.decrease.inc.plot <- ggplot() +
  geom_line(data=base.case[base.case$time %in% c(2020:2050),],  aes(x=time, y=dminc, color='Baseline')) +
  geom_line(data=decrease.obesity.all[decrease.obesity.all$time %in% c(2020:2050),],  aes(x=time, y=dminc, color='500g decrease per year - everyone')) +
  geom_line(data=freeze.obesity[freeze.obesity$time %in% c(2020:2050),],  aes(x=time, y=dminc, color='No increase in obesity after 2020')) +
  geom_line(data=prev.predm[prev.predm$time %in% c(2020:2050),],  aes(x=time, y=dminc, color='Reduce PreDM onset by 50%')) +
  geom_line(data=decrease.obesity.w[decrease.obesity.w$time %in% c(2020:2050),],  aes(x=time, y=dminc, color='500g decrease per year - women only')) +
  labs(x="Year", y="Diabetes new cases (in 1000s)", color = 'Scenarios', title = "Effects of weight loss on diabetes incidence")
obese.decrease.inc.plot + theme_bw()

########################################################################################################################
#PHYSICAL ACTIVITY
source("Assumptions.R")
## public health campaign on PA, improvement of infrastructure for active travel, investment in public transport
pa.case<-data.frame(ode(y=stocks, times=simtime, func = model, 
                        parms=pa.auxs, method='euler'))

mvpa15.auxs <- base.auxs
mvpa15.auxs["MVPA.switch"] <- 1
mvpa15.auxs["mvpa.increase"] <- 15 # 15 of MVPA added per day
#combo.auxs["aPAPH.Campaign"] <- 1 #PA public information campaign

mvpa15 <- data.frame(ode(y=stocks, times=simtime, func = model, 
                            parms=mvpa15.auxs, method='euler'))

pac.auxs <- base.auxs
pac.auxs["aPAPH.Campaign"] <- 1

campaign.pa <- data.frame(ode(y=stocks, times=simtime, func = model, 
                              parms=pac.auxs, method='euler'))


## replace 4 hours sedentary time with light PA
ltpa.auxs <- base.auxs
ltpa.auxs["MVPA.switch"] <- 0
ltpa.auxs["Sedentary.switch"] <- 1
halve.sedentary <- data.frame(ode(y=stocks, times=simtime, func = model, 
                                  parms=ltpa.auxs, method='euler'))


#women as active as men
women.auxs <- base.auxs
women.auxs["PA.women.switch"] <- 1
women.to.men <- data.frame(ode(y=stocks, times=simtime, func = model, 
                               parms=women.auxs, method='euler'))

pa.diff <- data.frame(base.case$time, base.case$dmprev, mvpa15$dmprev, campaign.pa$dmprev, halve.sedentary$dmprev, women.to.men$dmprev)
names(pa.diff) <- c("time", "baseline", "15+ mvpa", "campaign", "sedentary", "women.as.men")

pa.ob.diff <- data.frame(base.case$time, base.case$Obesity, mvpa15$Obesity, campaign.pa$Obesity, halve.sedentary$Obesity, women.to.men$Obesity)
names(pa.ob.diff) <- c("time", "baseline", "15+ mvpa", "campaign", "sedentary", "women.as.men")

mvpa.plot <- ggplot() +
  geom_line(data=base.case[base.case$time %in% c(2020:2050),], aes(x=time, y=Obesity, color='Baseline - declining MVPA trends')) +
  geom_line(data=ltmvpa.30[ltmvpa.30$time %in% c(2020:2050),], aes(x=time, y=Obesity, color='Leisure time MVPA to 30 min/day (average population)')) +
  geom_line(data=mv.impact[mv.impact$time %in% c(2020:2050),], aes(x=time, y=Obesity, color='Double MVPA')) +
  geom_line(data=halve.sedentary[halve.sedentary$time %in% c(2020:2050),], aes(x=time, y=Obesity, color='Replace 4h sedentary time with light PA')) +
  geom_line(data=women.to.men[women.to.men$time %in% c(2020:2050),], aes(x=time, y=Obesity, color='Women as active as men')) +
  labs(x="Year", y="Obesity Prevalence", color = 'Scenarios', title = "Effect of changing MVPA on obesity prevalence")
mvpa.plot + theme_bw() + scale_color_brewer(palette="Set1", direction=1)

mvpa.dm.plot <- ggplot() +
  geom_line(data=base.case[base.case$time %in% c(2020:2050),], aes(x=time, y=dmprev, color='Baseline - declining MVPA trends')) +
  geom_line(data=ltmvpa.30[ltmvpa.30$time %in% c(2020:2050),], aes(x=time, y=dmprev, color='Leisure time MVPA to 30 min/day (average population)')) +
  geom_line(data=mv.impact[mv.impact$time %in% c(2020:2050),], aes(x=time, y=dmprev, color='Double MVPA')) +
  geom_line(data=halve.sedentary[halve.sedentary$time %in% c(2020:2050),], aes(x=time, y=dmprev, color='Replace 4h sedentary time with light PA')) +
  geom_line(data=women.to.men[women.to.men$time %in% c(2020:2050),], aes(x=time, y=dmprev, color='Women as active as men')) +
  labs(x="Year", y="Diabetes prevalence (%)", color = 'Scenarios', title = "Effect of changing MVPA on diabetes prevalence")
mvpa.dm.plot + theme_bw() + scale_color_brewer(palette="Set1", direction=1)

####################################################################################################################################
## Changing food intake patterns

#double FV consumption
fv.auxs <- base.auxs
fv.auxs["double.FV.switch"] <- 1
double.fv <- data.frame(ode(y=stocks, times=simtime, func = model, 
                            parms=fv.auxs, method='euler'))

#replacing all UPF with fruits and vegetables
# estimate grams of UPF to replace as F&V
no.upf.auxs <- base.auxs
no.upf.auxs["replace.UPF.switch"] <- 1

UPFCalories.new <- c(UPFCalories[1:30], rep(0, 31))
ffUHCalories <- approxfun(years.all, UPFCalories.new)

no.upf <- data.frame(ode(y=stocks, times=simtime, func = model, 
                         parms=no.upf.auxs, method='euler'))

## reduce UPF and SSB consumption by half
upf.auxs <- base.auxs
upf.auxs["replace.UPF.switch"] <- 0
upf.auxs["double.FV.switch"] <- 0

UPF.50.new <- c(UPFCalories[1:30], UPFCalories[31:61]*0.5)
ffUHCalories <- approxfun(years.all, UPF.50.new)

SSB.trend.new <- c(SSB.trend[1:30], SSB.trend[31:61]*0.5) 
ffSSB <- approxfun(years.all, SSB.trend.new)

half.SSB.UPF <- data.frame(ode(y=stocks, times=simtime, func = model, 
                               parms=upf.auxs, method='euler'))

## reduce only SSB consumption by half
ffUHCalories <- approxfun(years.all, UPFCalories)
half.SSB.only <- data.frame(ode(y=stocks, times=simtime, func=model,
                                parms=upf.auxs, method='euler'))

#reset SSBs
ffSSB <- approxfun(years.all, SSB.trend)

upf.decrease.plot <- ggplot() +
  geom_line(data=base.case[base.case$time %in% c(2020:2050),], aes(x=time, y=Obesity, color='Baseline')) +
  geom_line(data=double.fv[double.fv$time %in% c(2020:2050),], aes(x=time, y=Obesity, color='Double fruit and vegetable consumption')) +
  geom_line(data=no.upf[no.upf$time %in% c(2020:2050),], aes(x=time, y=Obesity, color='Replace all UPFs with Fruits and Vegetables')) +
  geom_line(data=half.SSB.only[half.SSB.only$time %in% c(2020:2050),], aes(x=time, y=Obesity, color='Half SSB consumption only')) +
  geom_line(data=half.SSB.UPF[half.SSB.UPF$time %in% c(2020:2050),], aes(x=time, y=Obesity, color='Half SSB and Ultraprocessed Foods')) +
  labs(x="Year", y="Obesity prevalence (%)", color = 'Scenarios', title = "Effect of changing dietary patterns on obesity prevalence")
upf.decrease.plot + theme_bw()

upf.decrease.dm.plot <- ggplot() +
  geom_line(data=base.case[base.case$time %in% c(2020:2050),], aes(x=time, y=dmprev, color='Baseline')) +
  geom_line(data=double.fv[double.fv$time %in% c(2020:2050),], aes(x=time, y=dmprev, color='Double fruit and vegetable consumption')) +
  geom_line(data=no.upf[no.upf$time %in% c(2020:2050),], aes(x=time, y=dmprev, color='Replace all UPFs with Fruits and Vegetables')) +
  geom_line(data=half.SSB.only[half.SSB.only$time %in% c(2020:2050),], aes(x=time, y=dmprev, color='Half SSB consumption only')) +
  geom_line(data=half.SSB.UPF[half.SSB.UPF$time %in% c(2020:2050),], aes(x=time, y=dmprev, color='Half SSB and Ultraprocessed Foods')) +
  labs(x="Year", y="Diabetes prevalence (%)", color = 'Scenarios', title = "Effect of changing dietary patterns on diabetes prevalence")
upf.decrease.dm.plot + theme_bw()

## IF NO INCREASE IN OBESITY FROM 1990
source("Assumptions.R")
auxs.1990 <- base.auxs
auxs.1990["aInterventionYear"] <- 1991
auxs.1990["ob.freeze.switch"] <- 1
auxs.1990["obese.year.level"] <- base.case$Obesity[base.case$time == 1990]

#holding obesity steady
obese.1990 <- data.frame(ode(y=stocks, times=simtime, func=model,
                             parms=auxs.1990, method='euler'))

auxs.1990.low <- base.auxs
auxs.1990.low["aInterventionYear"] <- 1991
auxs.1990.low["ob.freeze.switch"] <- 1
auxs.1990.low["obese.year.level"] <- 10

obese.1990.low <- data.frame(ode(y=stocks, times=simtime, func=model,
                                 parms=auxs.1990.low, method='euler'))

obese.1990.plot <- ggplot() +
  geom_line(data=base.case[base.case$time %in% c(1990:2050),],  aes(x=time, y=dmprev, color='Baseline')) +
  geom_line(data=obese.1990[obese.1990$time %in% c(1990:2050),],  aes(x=time, y=dmprev, color='Obesity prevalence steady since 1990')) +
  geom_line(data=obese.1990.low[obese.1990.low$time %in% c(1990:2050),],  aes(x=time, y=dmprev, color='Obesity prevalence half of 1990 - no increase')) +
  labs(x="Year", y="Diabetes prevalence (%)", color = 'Scenarios', title = "Effect of holding obesity steady since 1990 on diabetes prevalence")
obese.1990.plot

## Combinations
#upstream interventions
auxs <- upstream.auxs
upstream.combo.case<-data.frame(ode(y=stocks, times=simtime, func = model, 
                                    parms=auxs, method='euler'))

#pa interventions
pa.case<-data.frame(ode(y=stocks, times=simtime, func = model, 
                        parms=pa.auxs, method='euler'))

#food system interventions
food.case<-data.frame(ode(y=stocks, times=simtime, func = model, 
                          parms=food.auxs, method='euler'))

#fiscal and investment only
fiscal.only<-data.frame(ode(y=stocks, times=simtime, func = model, 
                            parms=fiscal.auxs, method='euler'))

#all interventions at once
allin.auxs["hc.switch"] <- 1
allin.auxs["dpp.switch"] <- 1
allin.auxs["prevention.reduction"] <- 0.9275 # equivalent to a quarter of people with preDM adhering to dpp-type intervention
allin.auxs["RR.hc.over55"] <- 0.083 # equivalent with a third of the diabetes population adhering to benefits from DSME
allin.auxs["RR.hc.under55"] <- 0.083

allin.case <- data.frame(ode(y=stocks, times=simtime, func=model,
                             parms=allin.auxs, method='euler'))

ff.leisure.mvpa <- approxfun(years.all, Leisure.MVPA.Function)
ffUHCalories <- approxfun(years.all, UPFCalories)
ffSSB <- approxfun(years.all, SSB.trend)

intervention.case.plot <- ggplot() +
  geom_line(data=base.case[base.case$time %in% c(2020:2050),],  aes(x=time, y=dmprev, color='Baseline')) +
  geom_line(data=allin.case[allin.case$time %in% c(2020:2050),], aes(x=time, y=dmprev, color='Combined PA, Food, and Health system interventions')) +
  geom_line(data=pa.case[pa.case$time %in% c(2020:2050),], aes(x=time, y=dmprev, color='Invest in infrastructure, transport, and PH campaign, (PA only)')) +
  geom_line(data=food.case[food.case$time %in% c(2020:2050),],  aes(x=time, y=dmprev, color='Price increase SSB and UPF, decrease F&V, PH campaign, FOPL (Food only)')) +
  geom_line(data=upstream.combo.case[upstream.combo.case$time %in% c(2020:2050),],  aes(x=time, y=dmprev, color='Upstream only (PA and Food)')) +
  geom_line(data=dpp.predm[dpp.predm$time %in% c(2020:2050),],  aes(x=time, y=dmprev, color='Reduction in RR of ACM, increase PreDM remission (Health system)')) +
  labs(x="Year", y="Diabetes prevalence (%)", color = 'Scenarios', title = "Effects of intervention strategies on diabetes prevalence - effect sizes from the literature")
intervention.case.plot

intervention.obesity.plot <- ggplot() +
  geom_line(data=base.case[base.case$time %in% c(2020:2050),],  aes(x=time, y=Obesity, color='Baseline')) +
  geom_line(data=allin.case[allin.case$time %in% c(2020:2050),], aes(x=time, y=Obesity, color='Combined PA, Food, and Health system interventions')) +
  geom_line(data=pa.case[pa.case$time %in% c(2020:2050),], aes(x=time, y=Obesity, color='Invest in infrastructure, transport, and PH campaign, (PA only)')) +
  geom_line(data=food.case[food.case$time %in% c(2020:2050),],  aes(x=time, y=Obesity, color='Price increase SSB and UPF, decrease F&V, PH campaign, FOPL (Food only)')) +
  geom_line(data=upstream.combo.case[upstream.combo.case$time %in% c(2020:2050),],  aes(x=time, y=Obesity, color='Upstream only (PA and Food)')) +
  #geom_line(data=dpp.predm[dpp.predm$time %in% c(2020:2050),],  aes(x=time, y=Obesity, color='Reduction in RR of ACM, increase PreDM remission (Health system)')) +
  labs(x="Year", y="Obesity prevalence (%)", color = 'Scenarios', title = "Effects of intervention on obesity prevalence (%)")
intervention.obesity.plot

 