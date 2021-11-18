source("Assumptions.R")
source("SSCHMod.R")

#upload data
auxs.file <- read.csv("auxs.csv", header=TRUE)

base.auxs <- auxs.file$base
names(base.auxs) <- as.character(auxs.file$variable)

#BASE CASE MODEL RUN
# Set up simulation and time step #
START<-1990; FINISH<-2050; STEP<-1

# Create time vector
simtime <- seq(START, FINISH, by=STEP)

# Create stock and auxs

stocks  <- c(sNG=Baseline.NG,
             spreDM=Baseline.preDM,
             sDM=Baseline.DM,
             sAvgBdWt.M=aInitAvgWt.M,
             sAvgBdWt.W=aInitAvgWt.W) 

#BASELINE
auxs <- base.auxs
base.case<-data.frame(ode(y=stocks, times=simtime, func = model, 
                          parms=auxs, method='euler'))

##########################################################################################################################
## Magnitude needed for targets
source("Assumptions.R")

mixed.auxs <- base.auxs
mixed.auxs["PA.switch"] <- 1
mixed.auxs["pa.magnitude"] <- 2
mixed.auxs["tot.kcal.intake.switch"] <- 1
mixed.auxs["intake.reduction"] <- 0.8

int.20.dmv <- data.frame(ode(y=stocks, times=simtime, func = model, 
                             parms=mixed.auxs, method='euler'))

mixed.auxs["intake.reduction"] <- 0.7
int.SSB.dmv <- data.frame(ode(y=stocks, times=simtime, func = model, 
                               parms=mixed.auxs, method='euler'))

mixed.auxs["pa.magnitude"] <- 3
int.30.tmv <- data.frame(ode(y=stocks, times=simtime, func = model, 
                                 parms=mixed.auxs, method='euler'))

mixed.auxs["intake.reduction"] <- 0.8
int.20.tmv <- data.frame(ode(y=stocks, times=simtime, func = model, 
                             parms=mixed.auxs, method='euler'))

pa.only.auxs <- base.auxs
pa.only.auxs["PA.switch"] <- 1
pa.only.auxs["pa.magnitude"] <- 3
pa.only <- data.frame(ode(y=stocks, times=simtime, func = model, 
                          parms=pa.only.auxs, method='euler'))

single.plot <- ggplot() +
  geom_line(data=base.case[base.case$time %in% c(2020:2050),], aes(x=time, y=Obesity, color='Baseline')) +
  geom_line(data=pa.only[pa.only$time %in% c(2020:2050),], aes(x=time, y=Obesity, color='double MVPA')) +
  labs(x="Year", y="Diabetes prevalence (%)", color = '', title = "Stopping the rise in diabetes prevalence")
single.plot

mixed.ob.plot <- ggplot() +
  geom_line(data=base.case[base.case$time %in% c(2020:2050),], aes(x=time, y=Obesity, color='Baseline')) +
  geom_line(data=int.20.dmv[int.20.dmv$time %in% c(2020:2050),], aes(x=time, y=Obesity, color='20kcal, 2xMVPA')) +
  geom_line(data=int.SSB.dmv[int.SSB.dmv$time %in% c(2020:2050),], aes(x=time, y=Obesity, color='30kcal, 2xMPVA')) +
  geom_line(data=int.20.tmv[int.30.tmv$time %in% c(2020:2050),], aes(x=time, y=Obesity, color='20kcal, 3xMVPA')) +
  geom_line(data=int.30.tmv[int.30.tmv$time %in% c(2020:2050),], aes(x=time, y=Obesity, color='30kcal, 3xMVPA')) +
  labs(x="Year", y="Obesity prevalence (%)", color = '', title = "Stopping the rise in obesity prevalence") + 
  theme_bw() + scale_color_brewer(palette="Set1", direction=-1, 
                                  breaks = c("Baseline", "20kcal, 2xMVPA", "30kcal, 2xMPVA", "20kcal, 3xMVPA", "30kcal, 3xMVPA"))
mixed.ob.plot 

mixed.dm.plot <- ggplot() +
  geom_line(data=base.case[base.case$time %in% c(2020:2050),], aes(x=time, y=dmprev, color='Baseline')) +
  geom_line(data=int.20.dmv[int.20.dmv$time %in% c(2020:2050),], aes(x=time, y=dmprev, color='20kcal, 2xMVPA')) +
  geom_line(data=int.SSB.dmv[int.SSB.dmv$time %in% c(2020:2050),], aes(x=time, y=dmprev, color='30kcal, 2xMPVA')) +
  geom_line(data=int.20.tmv[int.30.tmv$time %in% c(2020:2050),], aes(x=time, y=dmprev, color='20kcal, 3xMVPA')) +
  geom_line(data=int.30.tmv[int.30.tmv$time %in% c(2020:2050),], aes(x=time, y=dmprev, color='30kcal, 3xMVPA')) +
  labs(x="Year", y="Diabetes prevalence (%)", color = '', title = "Stopping the rise in diabetes prevalence") + 
  theme_bw() + scale_color_brewer(palette="Set1", direction=-1, 
                                  breaks = c("Baseline", "20kcal, 2xMVPA", "30kcal, 2xMPVA", "20kcal, 3xMVPA", "30kcal, 3xMVPA")) +
  theme(legend.position = "bottom") 
mixed.dm.plot 

mixed.inc.plot <- ggplot() +
  geom_line(data=base.case[base.case$time %in% c(2020:2050),], aes(x=time, y=dminc, color='Baseline')) +
  geom_line(data=int.20.dmv[int.20.dmv$time %in% c(2020:2050),], aes(x=time, y=dminc, color='20% reduction in caloric intake, double MVPA')) +
  geom_line(data=int.SSB.dmv[int.SSB.dmv$time %in% c(2020:2050),], aes(x=time, y=dminc, color='50% reduction in SSB intake, double MVPA')) +
  geom_line(data=int.30.tmv[int.30.tmv$time %in% c(2020:2050),], aes(x=time, y=dminc, color='30% reduction in caloric intake, triple MVPA')) +
  labs(x="Year", y="Diabetes new cases (1000s)", color = 'Scenarios', title = "Decreasing diabetes incidence")
mixed.inc.plot + theme_bw() + scale_color_brewer(palette="Set1", direction=-1)


source("Assumptions.R")
##################################################################################################
## Realistic and desired combinations

## dpp type intervention to 1/4 of population with pre-dm only
hc.auxs <- base.auxs
hc.auxs["dpp.switch"] <- 1
hc.auxs["hc.switch"] <- 1
hc.auxs["prevention.reduction"] <- 0.9275 # equivalent to a quarter of people with preDM adhering to dpp-type intervention
hc.auxs["RR.hc.over55"] <- 0.083 # equivalent with a third of the diabetes population adhering to benefits from DSME
hc.auxs["RR.hc.under55"] <- 0.083
dpp.predm<- data.frame(ode(y=stocks, times=simtime, func = model, 
                           parms=hc.auxs, method='euler'))

#modest intervention

source("Assumptions.R")
combo.auxs <- base.auxs
combo.auxs["MVPA.switch"] <- 1
combo.auxs["mvpa.increase"] <- 15 # 15 of MVPA added per day
combo.auxs["aPAPH.Campaign"] <- 1 #PA public information campaign
combo.auxs["aLabels.switch"]  <- 1 # FOPL

SSB.15 <- c(SSB.trend[1:30], SSB.trend[31:61]*0.75)
ffSSB <- approxfun(years.all, SSB.15) # reduce SSB intake by 15%

UPF.15 <- c(UPFCalories[1:30], UPFCalories[31:61]*0.75)
ffUHCalories <- approxfun(years.all, UPF.15) # reduce UPF intake by 15%

combo.auxs["FV.intake.switch"] <- 1
combo.auxs["FV.intake.increase"] <- 200*0.15 # increase FV intake by 15%

upstream.light <- data.frame(ode(y=stocks, times=simtime, func = model, 
                           parms=combo.auxs, method='euler'))


#intensive intervention
## 30 min of MVPA
source("Assumptions.R")
combo.auxs <- base.auxs
combo.auxs["MVPA.switch"] <- 1
combo.auxs["mvpa.increase"] <- 30 # 30 of MVPA added per day
combo.auxs["aPAPH.Campaign"] <- 1 #PA public information campaign
combo.auxs["aLabels.switch"]  <- 1 # FOPL

SSB.25 <- c(SSB.trend[1:30], SSB.trend[31:61]*0.75)
ffSSB <- approxfun(years.all, SSB.25) # reduce SSB intake by 25%

UPF.25 <- c(UPFCalories[1:30], UPFCalories[31:61]*0.75)
ffUHCalories <- approxfun(years.all, UPF.25) # reduce UPF intake by 25%

combo.auxs["FV.intake.switch"] <- 1
combo.auxs["FV.intake.increase"] <- 200*0.25 # increase FV intake by 25%

upstream <- data.frame(ode(y=stocks, times=simtime, func = model, 
                                  parms=combo.auxs, method='euler'))

## combine with high risk intervention
up.hi.auxs <- combo.auxs
up.hi.auxs["dpp.switch"] <- 1
up.hi.auxs["prevention.reduction"] <- 0.9275
up.hi.auxs["hc.switch"] <- 1
up.hi.auxs["RR.hc.over55"] <- 0.083
up.hi.auxs["RR.hc.under55"] <- 0.083
upstream.hirisk <- data.frame(ode(y=stocks, times=simtime, func = model, 
                             parms=up.hi.auxs, method='euler'))

source("Assumptions.R")
campaigns <- base.auxs
campaigns["aLabels.switch"] <- 1
campaigns["aPAPH.Campaign"] <- 1

only.info <- data.frame(ode(y=stocks, times=simtime, func = model, 
                            parms=campaigns, method='euler'))

scenarios <- c("Baseline", "Campaigns and labelling", "Health system intervention", 
               "Moderate upstream interventions", "Intensive upstream interventions", 
               "Intensive upstream interventions + health system")

needed.dm <- data.frame(base.case$time, base.case$dmprev,
                        only.info$dmprev, dpp.predm$dmprev,
                        upstream.light$dmprev, upstream$dmprev, upstream.hirisk$dmprev)
names(needed.dm) <- c("time", "baseline", "campaigns", "health", "moderate", "intensive", "combo")
data.needed.dm <- melt(needed.dm, id="time")

needed.ob <- data.frame(base.case$time, base.case$Obesity,
                        only.info$Obesity, 
                        upstream.light$Obesity, upstream$Obesity)
names(needed.ob) <- c("time", "baseline", "campaigns", "moderate", "intensive")

data.needed.ob <- melt(needed.ob, id="time")

cbp2 <- c("#000000", "#e6ab02","#66a61e", "#d95f02","#7570b3", "#e7298a")

upstream.needed.plot <- ggplot(data = data.needed.dm[data.needed.dm$time %in% 2020:2050,], aes(x=time, y=value, color = variable)) +
  geom_line() +
  labs(x="Year", y="Diabetes prevalence (%)", color = '', 
       title = "Plausible scenarios to shift diabetes prevalence") +
  theme_bw() + scale_color_manual(values=cbp2, labels = scenarios) + 
  theme(legend.position = "bottom")
upstream.needed.plot

cbp3 <- c("#000000", "#e6ab02","#d95f02", "#7570b3")

upstream.needed.ob.plot <- ggplot(data = data.needed.ob[data.needed.ob$time %in% 2020:2050,], aes(x=time, y=value, color = variable)) +
  geom_line() +
  labs(x="Year", y="Obesity prevalence (%)", color = '', 
       title = "Plausible scenarios to shift obesity prevalence") +
  theme_bw() + scale_color_manual(values=cbp3, labels = scenarios) + 
  theme(legend.position = "bottom")
upstream.needed.ob.plot

upstream.needed.deaths.plot <- ggplot() +
  geom_line(data=base.case[base.case$time %in% c(2020:2050),],  aes(x=time, y=DMMortality, color='Baseline')) +
  geom_line(data=dpp.predm[dpp.predm$time %in% c(2020:2050),],  aes(x=time, y=DMMortality, color='Health system intervention for high risk + PWD')) +
  geom_line(data=upstream.light[upstream.light$time %in% c(2020:2050),],  aes(x=time, y=DMMortality, color='Modest upstream interventions')) +
  geom_line(data=upstream[upstream$time %in% c(2020:2050),],  aes(x=time, y=DMMortality, color='Intensive combined upstream interventions')) +
  geom_line(data=upstream.hirisk[upstream.hirisk$time %in% c(2020:2050),],  aes(x=time, y=DMMortality, color='Intensive combined upstream interventions and high-risk + PWD')) +
  labs(x="Year", y="Deaths in people with diabetes (1000s)", color = '', title = "Scenarios for high magnitude of upstream effect on mortality in PWD") + theme_bw()
upstream.needed.deaths.plot 


## baseline prediction plots
time <- c(2001, 2007, 2017)
dm <- c(7.2, 7.9, 11.9)
low <- c(6.0, 6.7, 10.5)
hi <- c(8.3, 9.1, 13.4)

dm.hist <- data.frame(time, dm, low, hi)

load("uncertainty.dm.RData")
dm.uncert.limits <- data.frame(min.df.plot$time, min.df.plot$dmprev, max.df.plot$dmprev)
names(dm.uncert.limits) <- c("time", "min.dm", "max.dm")

dm.plot <- base.case[, c("time", "dmprev")]

diabetes.prediction.plot <- ggplot() +
  geom_line(data=dm.plot,  aes(x=time, y=dmprev, color='Diabetes prevalence in adults 20+ years'), size = 1.1) +
  geom_point(data=dm.hist, aes(x=time, y=dm), size = 1.5) +
  geom_errorbar(data=dm.hist, aes(x=time, ymin=low, ymax=hi)) +
  #geom_ribbon(data=dm.uncert.limits[dm.uncert.limits$time %in% c(2020:2050),], aes(x=time, ymin = min.dm, ymax = max.dm), alpha=0.2) +
  labs(x="Year", y="Diabetes prevalence (%)", title = "Diabetes prevalence prediction (20+ years)\n and survey measured estimates") +
  theme_bw() + theme(legend.position = "none")
diabetes.prediction.plot 


# bmi plots
time <- c(1990, 1996, 2001, 2007, 2017)
bmi.w <- c(27.9, 28, 27.8, 28.5, 29.3)
w.low <-  c(27.6, 27.6, NA, 27.6, 28.8)
w.hi <- c(28.2, 28.4, NA, 29.0, 29.8)
bmi.m <- c(23.8, 23.8, 24.2, 25, 24.8)
m.low <- c(23.4, 23.2, NA, 24.3, 24.3)
m.hi <- c(24.2, 24.2, NA, 25.3, 25.3)

bmi.hist <- data.frame(time, bmi.w, w.low, w.hi, bmi.m, m.low, m.hi)

bmi.uncert <- data.frame(min.bmi.w.plot$time, 
                         min.bmi.w.plot$BMI.W, max.bmi.w.plot$BMI.W,
                         min.bmi.m.plot$BMI.M, max.bmi.m.plot$BMI.M)
names(bmi.uncert) <- c("time", "w.low", "w.hi", "m.low", "m.hi")

BMI.prediction.plot <- ggplot() +
  geom_line(data=base.case[base.case$time %in% c(1990:2050),],  aes(x=time, y=BMI.W, color='BMI in Women'), size = 1.1) +
  geom_point(data=bmi.hist, aes(x=time, y=bmi.w), size = 1.5) + 
  geom_errorbar(data=bmi.hist, aes(x=time, ymin=w.low, ymax=w.hi)) +
  #geom_ribbon(data=bmi.uncert, aes(x=time, ymin=w.low, ymax=w.hi), alpha=0.2) +
  geom_line(data=base.case[base.case$time %in% c(1990:2050),],  aes(x=time, y=BMI.M, color='BMI in Men'), size = 1.1) +
  geom_point(data=bmi.hist, aes(x=time, y=bmi.m), size = 1.5) + 
  geom_errorbar(data=bmi.hist, aes(x=time, ymin=m.low, ymax=m.hi)) +
  #geom_ribbon(data=bmi.uncert, aes(x=time, ymin=m.low, ymax=m.hi), alpha=0.2) +
  labs(x="Year", y="Average BMI kg/m^2", color = '', title = "Body mass index (BMI) prediction (20+ years)\n and survey measured estimates") +
  theme_bw() + #annotate("rect", xmin = 2020, xmax=2050, ymin= 23, ymax=31, alpha=0.2) + #annotate("text", x = 2035, y=23, label = "Prediction") +
  theme(legend.position = c(.15, .90))
BMI.prediction.plot 


### information for magnitude changes
dm.diff <- data.frame(base.case$time, base.case$dmprev, dpp.predm$dmprev, only.info$dmprev, upstream.light$dmprev, upstream$dmprev, upstream.hirisk$dmprev)
names(dm.diff) <- c("time","baseline", "healthcare", "only.info", "moderate", "intensive", "combined")

deaths.diff <- data.frame(base.case$time, base.case$DMMortality, dpp.predm$DMMortality, only.info$DMMortality, upstream.light$DMMortality, upstream$DMMortality, upstream.hirisk$DMMortality)
names(deaths.diff) <- c("time","baseline", "healthcare", "only.info", "moderate", "intensive", "combined")

ob.diff <- data.frame(base.case$time, base.case$Obesity, dpp.predm$Obesity, only.info$Obesity, upstream.light$Obesity, upstream$Obesity, upstream.hirisk$Obesity)
names(ob.diff) <- names(dm.diff)

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

#scale_factor <- 1.5
pdf(file = "SSCH_Results.pdf", height = 8.3 , width = 11.7 )

#baseline predictions
grid.arrange(diabetes.prediction.plot, BMI.prediction.plot, ncol=2)


#magnitude of change needed
legend <- get_legend(mixed.dm.plot)

dm.plot <- mixed.dm.plot + theme(legend.position = "none")
ob.plot <- mixed.ob.plot + theme(legend.position = "none")

grid.arrange(dm.plot,ob.plot, legend, ncol = 2, nrow = 2,
             layout_matrix = rbind(c(1,2), c(3,3)),
             widths = c(2.7, 2.7), heights = c(2.5, 0.2))

#interventions of interest
legend2 <- get_legend(upstream.needed.plot)
needed.plot <- upstream.needed.plot + theme(legend.position = "none")
needed.ob.plot <- upstream.needed.ob.plot + theme(legend.position = "none")

grid.arrange(needed.plot, needed.ob.plot, legend2, ncol = 2, nrow = 2,
             layout_matrix = rbind(c(1,2), c(3,3)),
             widths = c(2.7, 2.7), heights = c(2.5, 0.2))

dev.off()

