source("Assumptions.R")
source("SSCHMod.R")
library(cowplot)
library(patchwork)

#colors for figures
cbp2 <- c("green","purple", "orange","blue", "red", "#000000")
cbp3 <- c("green","orange", "blue", "#000000")

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
##freeze MVPA - no decreases


mixed.auxs <- base.auxs
mixed.auxs["aInterventionYear"] <- 2010
mixed.auxs["PA.switch"] <- 1
mixed.auxs["pa.magnitude"] <- 2
mixed.auxs["tot.kcal.intake.switch"] <- 1
mixed.auxs["intake.reduction"] <- 0.8

int.20.dmv <- data.frame(ode(y=stocks, times=simtime, func = model, 
                             parms=mixed.auxs, method='euler'))

mixed.auxs["intake.reduction"] <- 0.7
int.30.dmv <- data.frame(ode(y=stocks, times=simtime, func = model, 
                               parms=mixed.auxs, method='euler'))

mixed.auxs["pa.magnitude"] <- 3
int.30.tmv <- data.frame(ode(y=stocks, times=simtime, func = model, 
                                 parms=mixed.auxs, method='euler'))

mixed.auxs["intake.reduction"] <- 0.8
int.20.tmv <- data.frame(ode(y=stocks, times=simtime, func = model, 
                             parms=mixed.auxs, method='euler'))

pa.only.auxs <- base.auxs
pa.only.auxs["aInterventionYear"] <- 2010
pa.only.auxs["PA.switch"] <- 1
pa.only.auxs["pa.magnitude"] <- 3
pa.only <- data.frame(ode(y=stocks, times=simtime, func = model, 
                          parms=pa.only.auxs, method='euler'))

scenarios <- c("-20% caloric intake, double MVPA", "-30%caloric intake, double MVPA",
               "-20% caloric intake, triple MVPA", "-30% caloric intake, triple MVPA", "Baseline" )

mixed.ob <- data.frame(base.case$time, int.20.dmv$Obesity, int.30.dmv$Obesity,
                       int.20.tmv$Obesity, int.30.tmv$Obesity, base.case$Obesity)
names(mixed.ob) <- c("time", "20dmv", "30dmv", "20tmv", "30tmv", "baseline")
data.mixed.ob <- melt(mixed.ob, id="time")

mixed.ob.plot <- ggplot(data=data.mixed.ob, aes(x=time, y=value, color=variable)) +
    geom_line() +
    labs(x="Year", y="Prevalence (%)", color="") +
    geom_vline(xintercept = 2025, color = "grey") +
    annotate(geom = "text", x=2024, y=36, label = "WHO GAP Target", size = 3, angle=90)+
    theme_bw() + scale_color_manual(values = cbp2[2:6], labels=scenarios) +
    theme(legend.position = "bottom")
mixed.ob.plot

mixed.dm <- data.frame(base.case$time, int.20.dmv$dmprev, int.30.dmv$dmprev,
                       int.20.tmv$dmprev, int.30.tmv$dmprev, base.case$dmprev)
names(mixed.dm) <-  names(mixed.ob)

data.mixed.dm <- melt(mixed.dm, id="time")

mixed.dm.plot <- ggplot(data=data.mixed.dm, aes(x=time, y=value, color=variable)) +
  geom_line() +
  labs(x="Year", y="Prevalence (%)", color="") +
  geom_vline(xintercept = 2025, color = "grey") +
  annotate(geom = "text", x=2024, y=18, label = "WHO GAP Target", size = 3, angle=90)+
  theme_bw() + scale_color_manual(values = cbp2[2:6], labels=scenarios) +
  theme(legend.position = "bottom")
mixed.dm.plot

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

SSB.15 <- c(SSB.trend[1:30], SSB.trend[31:61]*0.85)
ffSSB <- approxfun(years.all, SSB.15) # reduce SSB intake by 15%

UPF.15 <- c(UPFCalories[1:30], UPFCalories[31:61]*0.85)
ffUHCalories <- approxfun(years.all, UPF.15) # reduce UPF intake by 15%

combo.auxs["FV.intake.switch"] <- 1
combo.auxs["FV.intake.increase"] <- 200*0.15 # increase FV intake by 15%

upstream.light <- data.frame(ode(y=stocks, times=simtime, func = model, 
                           parms=combo.auxs, method='euler'))


#intensive intervention
## 30 min of MVPA
source("Assumptions.R")
intensive.auxs <- base.auxs
intensive.auxs["MVPA.switch"] <- 1
intensive.auxs["mvpa.increase"] <- 30 # 30 of MVPA added per day
intensive.auxs["aPAPH.Campaign"] <- 1 #PA public information campaign
intensive.auxs["aLabels.switch"]  <- 1 # FOPL

SSB.25 <- c(SSB.trend[1:30], SSB.trend[31:61]*0.75)
ffSSB <- approxfun(years.all, SSB.25) # reduce SSB intake by 25%

UPF.25 <- c(UPFCalories[1:30], UPFCalories[31:61]*0.75)
ffUHCalories <- approxfun(years.all, UPF.25) # reduce UPF intake by 25%

intensive.auxs["FV.intake.switch"] <- 1
intensive.auxs["FV.intake.increase"] <- 200*0.25 # increase FV intake by 25%

upstream.int <- data.frame(ode(y=stocks, times=simtime, func = model, 
                                  parms=intensive.auxs, method='euler'))

## combine with high risk intervention
up.hi.auxs <- intensive.auxs
up.hi.auxs["dpp.switch"] <- 1
up.hi.auxs["prevention.reduction"] <- 0.9275
up.hi.auxs["hc.switch"] <- 1
up.hi.auxs["RR.hc.over55"] <- 0.083
up.hi.auxs["RR.hc.under55"] <- 0.083
upstream.hirisk <- data.frame(ode(y=stocks, times=simtime, func = model, 
                             parms=up.hi.auxs, method='euler'))

# source("Assumptions.R")
# campaigns <- base.auxs
# campaigns["aLabels.switch"] <- 1
# campaigns["aPAPH.Campaign"] <- 1
# 
# only.info <- data.frame(ode(y=stocks, times=simtime, func = model, 
#                             parms=campaigns, method='euler'))

scenarios <- c("Health system intervention", 
               "Modest upstream interventions", "Intensive upstream interventions", 
               "Combined intensive upstream and downstream", "Baseline")

scenarios2 <- c("Moderate upstream interventions", "Intensive upstream interventions", 
                 "Baseline")

needed.dm <- data.frame(base.case$time, dpp.predm$dmprev, upstream.light$dmprev, upstream.int$dmprev, upstream.hirisk$dmprev, base.case$dmprev)
names(needed.dm) <- c("time", "health", "moderate", "intensive", "combo", "baseline")
data.needed.dm <- melt(needed.dm, id="time")

needed.ob <- data.frame(base.case$time, upstream.light$Obesity, upstream.int$Obesity,  base.case$Obesity)
names(needed.ob) <- c("time", "moderate", "intensive", "baseline")
data.needed.ob <- melt(needed.ob, id="time")

upstream.needed.plot <- ggplot(data = data.needed.dm, aes(x=time, y=value, color = variable)) +
  geom_line() +
  labs(x="Year", y="Prevalence (%)", color = '', 
       title = "4A - Diabetes") +
  theme_bw() + scale_color_manual(values=cbp2[2:6], labels = scenarios) + 
  theme(legend.position = "bottom")
upstream.needed.plot

upstream.needed.ob.plot <- ggplot(data = data.needed.ob, aes(x=time, y=value, color = variable)) +
  geom_line() +
  labs(x="Year", y="Prevalence (%)", color = '', 
       title = "4B - Obesity") +
  theme_bw() + scale_color_manual(values=cbp3[2:6], labels = scenarios2) + 
  theme(legend.position = "bottom")
upstream.needed.ob.plot


source("Assumptions.R")
## baseline prediction plots
time <- c(2001, 2007, 2017)
dm <- c(7.2, 7.9, 11.9)
low <- c(6.0, 6.7, 10.5)
hi <- c(8.3, 9.1, 13.4)

dm.hist <- data.frame(time, dm, low, hi)

load("sens.df.RData")
dm.uncert.limits <- data.frame(df$time, df$dmprev, df$run)
names(dm.uncert.limits) <- c("time", "dmprev", "run")

min.dm <- min(dm.uncert.limits$dmprev[dm.uncert.limits$time == 2050])
min.run <- dm.uncert.limits$run[dm.uncert.limits$dmprev == min.dm & dm.uncert.limits$time == 2050]
max.dm <- max(dm.uncert.limits$dmprev[dm.uncert.limits$time == 2050])
max.run <- dm.uncert.limits$run[dm.uncert.limits$dmprev == max.dm & dm.uncert.limits$time == 2050]

min.dm.plot <- dm.uncert.limits[dm.uncert.limits$run == min.run,]
max.dm.plot <- dm.uncert.limits[dm.uncert.limits$run == max.run,]

dm.uncert.plot <- data.frame(min.dm.plot$time, min.dm.plot$dmprev, max.dm.plot$dmprev)
names(dm.uncert.plot) <- c("time", "min.dm", "max.dm")

dm.plot <- base.case[, c("time", "dmprev")]

diabetes.prediction.plot <- ggplot() +
  geom_line(data=dm.plot,  aes(x=time, y=dmprev), size = 1.1) +
  geom_point(data=dm.hist, aes(x=time, y=dm), size = 1.5) +
  geom_errorbar(data=dm.hist, aes(x=time, ymin=low, ymax=hi)) +
  #geom_ribbon(data=dm.uncert.plot, aes(x=time, ymin = min.dm, ymax = max.dm), alpha=0.2) +
  labs(x="Year", y="Prevalence (%)") +
  #ggtitle("2A") +
  theme_bw() + theme(legend.position = "none")
diabetes.prediction.plot 

# bmi plots
time <- c(1990, 1996, 2000, 2007, 2017)
bmi.w <- c(27.9, 28, 27.8, 28.5, 29.3)
w.low <-  c(27.6, 27.6, 27.4, 27.6, 28.8)
w.hi <- c(28.2, 28.4, 28.2, 29.0, 29.8)
bmi.m <- c(23.8, 23.8, 24.2, 25, 24.8)
m.low <- c(23.4, 23.2, 23.8, 24.3, 24.3)
m.hi <- c(24.2, 24.2, 24.6, 25.3, 25.3)

bmi.hist <- data.frame(time, bmi.w, w.low, w.hi, bmi.m, m.low, m.hi)

bmi.lab <- expression(Mean ~ kg/m^2)

BMI.prediction.plot <- ggplot() +
  geom_line(data=base.case,  aes(x=time, y=BMI.W, color='BMI in Women'), size = 1.1) +
  geom_point(data=bmi.hist, aes(x=time, y=bmi.w), size = 1.5) + 
  geom_errorbar(data=bmi.hist, aes(x=time, ymin=w.low, ymax=w.hi)) +
  #geom_ribbon(data=bmi.uncert, aes(x=time, ymin=w.low, ymax=w.hi), alpha=0.2) +
  geom_line(data=base.case,  aes(x=time, y=BMI.M, color='BMI in Men'), size = 1.1) +
  geom_point(data=bmi.hist, aes(x=time, y=bmi.m), size = 1.5) + 
  geom_errorbar(data=bmi.hist, aes(x=time, ymin=m.low, ymax=m.hi)) +
  #geom_ribbon(data=bmim.uncert.plot, aes(x=time, ymin=min.bmi, ymax=max.bmi), alpha=0.2) +
  labs(x="Year", y=bmi.lab, color = '') +
  theme_bw() + #annotate("rect", xmin = 2020, xmax=2050, ymin= 23, ymax=31, alpha=0.2) + #annotate("text", x = 2035, y=23, label = "Prediction") +
  theme(legend.position = c(0.75, 0.1), legend.text = element_text(size=8), 
        legend.key.size = unit(2, 'mm'), legend.background = element_blank())
BMI.prediction.plot 

#obesity plots
time <- c(1994, 2000, 2007, 2017)
ob <- c(24, 19.8, 25.3, 28.9)
ob.l <- c(21.8, 18.06, 23.7, 27.2)
ob.h <- c(26.2, 21.54, 26.9, 30.6)
ob.hist <- data.frame(time, ob, ob.l, ob.h)

ob.prediction.plot <- ggplot() +
  geom_line(data=base.case, aes(x=time, y=Obesity), size=1.1) +
  geom_point(data=ob.hist, aes(x=time, y=ob), size = 1.5) +
  geom_errorbar(data=ob.hist, aes(x=time, ymin=ob.l, ymax=ob.h)) +
  labs(x="Year", y="Prevalence (%)", color='') +
  theme_bw() + 
  theme(legend.position = "none")
ob.prediction.plot

### information for magnitude changes
dm.diff <- data.frame(base.case$time, base.case$dmprev, dpp.predm$dmprev, upstream.light$dmprev, upstream.int$dmprev, upstream.hirisk$dmprev)
names(dm.diff) <- c("time","baseline", "healthcare", "moderate", "intensive", "combined")

deaths.diff <- data.frame(base.case$time, base.case$DMMortality, dpp.predm$DMMortality, upstream.light$DMMortality, upstream.int$DMMortality, upstream.hirisk$DMMortality)
names(deaths.diff) <- c("time","baseline", "healthcare", "moderate", "intensive", "combined")

ob.diff <- data.frame(base.case$time, base.case$Obesity, dpp.predm$Obesity, upstream.light$Obesity, upstream.int$Obesity, upstream.hirisk$Obesity)
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
diab.pred <- diabetes.prediction.plot + labs(title= "Diabetes", tag = "A") +
  theme(plot.tag = element_text(face = "bold"), plot.title = element_text(hjust = 0.5, size = 10),
        plot.tag.position = "topleft", axis.title= element_text(size=10))

ob.pred <- ob.prediction.plot + labs(title= "Obesity", tag = "B", y= "") +
  theme(plot.tag = element_text(face = "bold"), plot.title = element_text(hjust = 0.5, size = 10),
        plot.tag.position = "topleft", axis.title= element_text(size=10))

BMI.pred <- BMI.prediction.plot + labs(title= "BMI", tag = "C") +
  theme(plot.tag = element_text(face = "bold"), plot.title = element_text(hjust = 0.5, size = 10),
        plot.tag.position = "topleft", axis.title= element_text(size=10))

combined.pred <- plot_grid(diab.pred, ob.pred, BMI.pred,  ncol=3, nrow=2, rel_heights = c(1, 0.4))
combined.pred


#magnitude of change needed
legend <- get_legend(mixed.dm.plot)

dm.fig <- mixed.dm.plot + labs(title= "Diabetes", tag="A") +
  theme(plot.tag = element_text(face="bold"), plot.title = element_text(hjust=0.5, size=10),
        plot.tag.position = "topleft")

ob.fig <- mixed.ob.plot + labs(title= "Obesity", tag="B", y = "") +
  theme(plot.tag = element_text(face="bold"), plot.title = element_text(hjust=0.5, size=10),
        plot.tag.position = "topleft")

combined.change <- dm.fig + ob.fig & theme(legend.position = "bottom")
combined.change + plot_layout(guides = "collect")


#interventions of interest
legend2 <- get_legend(upstream.needed.plot)
needed.plot <- upstream.needed.plot + theme(legend.position = "none")
needed.ob.plot <- upstream.needed.ob.plot + theme(legend.position = "none")

grid.arrange(needed.plot, needed.ob.plot, legend2, ncol = 2, nrow = 2,
             layout_matrix = rbind(c(1,2), c(3,3)),
             widths = c(2.7, 2.7), heights = c(2, 0.5))

dev.off()

