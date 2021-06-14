### System Science for Caribbean Health Model of Diabetes ###

# load necessary packages #
library(deSolve)
library(ggplot2)
library(FME)
library(plyr)
library(grid)
library(gridExtra)

#upload information on assumptions and inputs for various runs
auxs.file <- read.csv("auxs.csv", header=TRUE)

base.auxs <- auxs.file$base
names(base.auxs) <- as.character(auxs.file$variable)

pa.auxs <- auxs.file$PA
names(pa.auxs) <- as.character(auxs.file$variable)

food.auxs <- auxs.file$food
names(food.auxs) <- as.character(auxs.file$variable)

upstream.auxs <- auxs.file$upstream
names(upstream.auxs) <- as.character(auxs.file$variable)

hc.auxs <- auxs.file$hc
names(hc.auxs) <- as.character(auxs.file$variable)

allin.auxs <- auxs.file$allin
names(allin.auxs) <- as.character(auxs.file$variable)

hc.switch <- 0

model <- function(time, stocks, auxs){
  
  with(as.list(c(stocks, auxs)),{
  #  browser()
    # simple function implement an intervention year
    
  intervention.year <- function(x){
      x.time <- c(rep(0, FINISH - aInterventionYear), rep(x, aInterventionYear + 1 - START))
    }
    
    #SSB calories
    ## function for calculating SSB price change from intervention start date
    SSB.price <- intervention.year(aSSBPriceChange)
    ffSSB.price.change <- approxfun(years.all, SSB.price)
    
    aEffectSSBPriceChange <- (aElasticity.SSB + aEffectSSB.Campaign)*ffSSB.price.change(time)
    aAvgSSBConsumption <- ffSSB(time) + (aEffectSSBPriceChange/100)
    aCaloriesSSB <- aAvgSSBConsumption*aSSBperUnitCal
     #Food calories
  
    #inflow of Fruit and Vegetable production
    FV.increase.int <- intervention.year(aIncreaseinFV)
    ffFV.increase <- approxfun(years.all, FV.increase.int)
    
    aFVProduction <- ((ffInitialFVProduction(time)*1000) + 
                        ((ffFV.increase(time))/100*ffInitialFVProduction(time)*1000))/ffTotalPopulation(time)
    
    aFVImports <- ffTotalkgImports(time)/ffTotalPopulation(time)
    fFVSupply <- (ffTotalkgImports(time) - (ffTotalkgImports(time)*aImportsTourism/100)) + 
      (aFVProduction - (aFVProduction*aLocalTourism/100))
    
    #outflow of waste and export
    fFVExport <- ffFVExport(time)/ffTotalPopulation(time) #per capita export per year
    fFVWaste <- sFV * .3
    
    #currently modeling an increase in price for FV - if a decrease needed, then needs to be (-)
    Price.change.FV <- intervention.year(aPriceChangeFV)
    ffprice.change.FV <- approxfun(years.all, Price.change.FV)
    
    aEffectFVPrice <- ffprice.change.FV(time)*aElasFVPrice
    
    Price.change.UH <- intervention.year(aPriceChangeUH)
    ffprice.change.UH <- approxfun(years.all, Price.change.UH)
    
    aEffectUHPrice <- ffprice.change.UH(time) * aUHFVCrossPrice
    
     #outflow of fruit and veg consumption in kg/capita/yr
    FVPH <- intervention.year(aFVPH.switch)
    ffFV.PH <- approxfun(years.all, FVPH)
    
    fFVConsumption <- aInitFVIntake + (aInitFVIntake * aEffectFVPrice/100) + 
      (aInitFVIntake*aEffectUHPrice/100) + ((aEffectFVPH/1000)*365)*ffFV.PH(time)
    
    aFVkcalperday <- aCalperFV*((fFVConsumption/365)*1000)
    
    UH.price.change <- intervention.year(aPriceChangeUH)
    ffUH.price.change <- approxfun(years.all, UH.price.change)
    
    aEffectUHTax <- ffUH.price.change(time) * aElasUHFoods #elasticity needs to be negative
    
    UHPH <- intervention.year(aUHPH.switch)
    ffUHPH <- approxfun(years.all, UHPH)
    
    aUHFoods <- ffUHCalories(time) + (ffUHCalories(time)*aEffectUHTax/100) + 
      (ffUHCalories(time)*(aEffectUHPH/100)*ffUHPH(time))
    
    aDailyIntake.Total <- aUHFoods + aFVkcalperday + OtherIntake + aCaloriesSSB
    Ratio.Intake <- 1.17 #ratio of calorie intake in Men to Women
    aDailyIntake.W <- aDailyIntake.Total/((per.women/100) + (per.men/100)*Ratio.Intake)
    aDailyIntake.M <- aDailyIntake.W*Ratio.Intake
    
    #Fruit and vegetable stock equation
    dFV_dt <-  fFVSupply - fFVWaste - fFVExport - fFVConsumption
    
    #Physical activity
    aWorkMVPA <- ff.work.mvpa(time)
    aTravelMVPA <- ff.travel.mvpa(time)
    aLTMVPA <- ff.leisure.mvpa(time)
    aDomMVPA <- ff.domestic.mvpa(time)
    
    infra.change <- intervention.year(aInfra.switch)
    ffinfra.change <- approxfun(years.all, infra.change)
    
    public.transport <- intervention.year(aPublic.switch)
    ffpublic.transport <- approxfun(years.all, public.transport)
    
    aActiveTransport <- aTravelMVPA + (aTravelMVPA*aEffectInfra/100)*ffinfra.change(time) + 
      ffpublic.transport(time)*aPublic.Transport
    aLTMVPA.with.infra <- aLTMVPA + aLTMVPA*(aEffectInfra/100)*ffinfra.change(time)
    aTotalMVPA.noPH <- aActiveTransport + aWorkMVPA + aLTMVPA.with.infra + aDomMVPA
    
    PA.campaign <- intervention.year(aPAPH.Campaign)
    ffPA.campaign <- approxfun(years.all, PA.campaign)
    
    aTotalMVPA <- aTotalMVPA.noPH + (aRRPACampaign-1)*(aTotalMVPA.noPH)*ffPA.campaign(time)
    Ratio.PA <- 3.5
    aMVPA.W <- aTotalMVPA/(per.women/100 + per.men/100*Ratio.PA)
    aMVPA.M <- aMVPA.W * Ratio.PA
    
    aActiveMets.W <- aMVPA.W*5.2
    aActiveMets.M <- aMVPA.M*5.2
    
    Sedentary.T.W <- Total.minutes - aMVPA.W - Sleep.time - LightPA.time
    SedentaryMets.W <- Sedentary.T.W*1.5
    
    Sedentary.T.M <- Total.minutes - aMVPA.M - Sleep.time - LightPA.time
    SedentaryMets.M <- Sedentary.T.M*1.5
    
    aPAL.W <- (SedentaryMets.W + aActiveMets.W + SleepMets + LightPAMets)/Total.minutes
    aPAL.M <-(SedentaryMets.M + aActiveMets.M + SleepMets + LightPAMets)/Total.minutes
 
  
    ##Obesity module
    # Men
    aMod.BMI.M <- sAvgBdWt.M / (aAvgHeight.M^2)
    #aFatFrac.M <- ((3.05*BMI.M) - (0.028*BMI.M^2) -36.61)/100

    #Harris-Benedict
    aBasalCalories.M <- 13.397*sAvgBdWt.M + 4.799*aAvgHeight.M - 5.677*aAvgAge.M(time) + 88.362
    aTDEE.M <- aBasalCalories.M * aPAL.M
    aCaloricBalance.M <- aDailyIntake.M - aTDEE.M
    fChange.wt.M <- aCaloricBalance.M/8050
    dAvg.wt_dt.M <- fChange.wt.M
    aObeseTotal.M <- 146 + -15.1*aMod.BMI.M + 0.39*aMod.BMI.M^2
    
    #Women
    aMod.BMI.W <- sAvgBdWt.W / (aAvgHeight.W^2)
    #aFatFrac.W <- ((3.65*BMI.W) - (0.042*BMI.W^2) - 30.81)/100
    #Harris-Benedict
    aBasalCalories.W <- 447.593 + 9.247*sAvgBdWt.W + 3.098*aAvgHeight.W - 4.33*aAvgAge.W(time)
    aTDEE.W <- aBasalCalories.W * aPAL.W
    aCaloricBalance.W <- aDailyIntake.W - aTDEE.W
    fChange.wt.W <- aCaloricBalance.W/8050
    dAvg.wt_dt.W <- fChange.wt.W
    aObeseTotal.W <- 28.6 + -5.6*aMod.BMI.W + 0.205*aMod.BMI.W^2
    
    aMod.BMI <- aMod.BMI.M*per.men/100 + aMod.BMI.W*per.women/100
    
    aObeseTotal <- 40.3 - 6.45*aMod.BMI + 0.219*aMod.BMI^2
      
    aObesefractNGT <- aObeseTotal*0.8
    aObesefractIGT <- aObeseTotal*1.2
    
    ## Core Model
    hc.prep <- intervention.year(hc.switch)
    hc.intervention <- approxfun(years.all, hc.prep)
    
    # NGT
    # Inflow of adult population
    fAdultGrowth <- pop.inflow(time)
    
    # Outflow NGT mortality
    under.55 <- 100-over.55(time)
    fNGTMortality <- (sNGT*(aMortalityNGTrate.under55/1000)*(under.55/100)) +
      (sNGT*(aMortalityNGTrate.over55/1000)*(over.55(time)/100))
      
    # Flow of new cases to IGT/IFG
    aIGTObese <- sNGT*(aObesefractNGT/100)*(aIGTincidenceNO/1000)*aRRofIGTinObese
    aIGTNonObese <- sNGT*((100-aObesefractNGT)/100)*(aIGTincidenceNO/1000)
    fIGTOnset <- aIGTObese + aIGTNonObese
    
    # Inflow of IGT recovery
    # create an option for modifying the rate of recovery as a HC intervention
    
    fIGTRecovery <- (aIGTrecovery + IGT.effect*hc.intervention(time))/100*sIGT
    
    # NGT stock equation
    dNGT_dt <- fAdultGrowth - fNGTMortality - fIGTOnset + fIGTRecovery 
    
    # IGT
    # Outflow IGT mortality
    fIGTMortality <- (sIGT*(aMortalityNGTrate.under55/1000)*(under.55/100)) +
      (sIGT*(aMortalityNGTrate.over55/1000)*(over.55(time)/100))
    
    # Outflow DM Onset
    aDMinNonObese <- sIGT*((100-aObesefractIGT)/100)*(aDMincidenceNO/1000)
    aDMinObese <- (aObesefractIGT/100)*sIGT*(aDMincidenceNO/1000)*aRRofDMinObese
    aDMOnsetObesity <- aDMinObese + aDMinNonObese
    aDMOnsetPA<- aDMOnsetObesity*ff.LTPA.RR(aLTMVPA.with.infra*7/60)
    aDMOnsetSSB <- aDMOnsetPA + (aDMOnsetPA*(aRRofSSBs/100)*aAvgSSBConsumption)
    
    FV.Daily <- aInitFVIntake*1000/365
    
    if (FV.Daily > 400) {
      aDMwithFV <- 0.95*aDMOnsetSSB
    } else {
      aDMwithFV <- aDMOnsetSSB
    }
    
    aDMOnsetAging <- ((over.65(time)/100)*aRRofDMinElderly*aDMwithFV) +
      (((100-over.65(time))/100)*aDMwithFV)
    fDMOnset <- aDMOnsetAging
     
    # IGT Stock equation
    dIGT_dt <- fIGTOnset - fIGTRecovery - fIGTMortality - fDMOnset
    
    # DM
    # Outflow DM mortality
    hc.rr.mortality.under55 <- aRRofMortalityDM.under55 - (hc.intervention(time)*RR.hc.under55*aRRofMortalityDM.under55)
    hc.rr.mortality.over55 <- aRRofMortalityDM.over55 - (hc.intervention(time)*RR.hc.over55*aRRofMortalityDM.over55)
    
    fDMMortality <- (sDM*(under.55/100)*hc.rr.mortality.under55*(aMortalityNGTrate.under55/1000)) +
      (sDM*hc.rr.mortality.over55*(aMortalityNGTrate.over55/1000)*(over.55(time)/100))
   
    # DM Stock equation
    dDM_dt <- fDMOnset - fDMMortality
    
    #estimate prevalence and incidence
    dmprev <- sDM*100/(sDM + sIGT + sNGT)
    dminc <- fDMOnset * 1000/(sIGT + sNGT)
    
    # All the results for the time step
    ans <- list(c(dNGT_dt, dIGT_dt, dDM_dt, dAvg.wt_dt.M, dAvg.wt_dt.W, dFV_dt),
                NGTOnset=fAdultGrowth,
                NGTMortality=fNGTMortality,
                NGTNetFlow=dNGT_dt,
                IGTOnset=fIGTOnset,
                IGTMortality=fIGTMortality,
                IGTNetFlow=dIGT_dt,
                DMOnset=fDMOnset,
                DMMortality=fDMMortality,
                DMNetFlow=dDM_dt,
                BWtAvg.M=dAvg.wt_dt.M,
                BWtAvg.W=dAvg.wt_dt.W,
                Obesity=aObeseTotal,
                dmprev=dmprev,
                dminc=dminc,
                PAL.W=aPAL.W,
                Intake.W=aDailyIntake.W,
                TDEE.W=aTDEE.W,
                stock.bw.W=sAvgBdWt.W)
    
  })
}

#BASE CASE MODEL RUN

  # Set up simulation and time step #
  START<-1990; FINISH<-2050; STEP<-1
  
  # Create time vector
  simtime <- seq(START, FINISH, by=STEP)
  
  # Create stock and auxs
  
  stocks  <- c(sNGT=Baseline.NGT,
               sIGT=Baseline.IGT,
               sDM=Baseline.DM,
               sAvgBdWt.M=aInitAvgWt.M,
               sAvgBdWt.W=aInitAvgWt.W,
               sFV=aInitFVStock) 
  
  #base run
  auxs <- base.auxs
    
  base.case<-data.frame(ode(y=stocks, times=simtime, func = model, 
                    parms=auxs, method='euler'))

  # Set up simulation and time step #
  #START <- 2015; FINISH <- 2050; STEP <- 1
  
  # Create time vector
  simtime <- seq(START, FINISH, by=STEP)
  
  # # Create stock and auxs
  # stocks <- c(sNGT=base.case$sNGT[base.case$time == 2015],
  #             sIGT=base.case$sIGT[base.case$time == 2015],
  #             sDM=base.case$sDM[base.case$time ==2015],
  #             sAvgBdWt.M=base.case$sAvgBdWt.M[base.case$time == 2015],
  #             sAvgBdWt.W=base.case$sAvgBdWt.W[base.case$time == 2015],
  #             sFV=base.case$sFV[base.case$time == 2015])
  # 
  #physical activity interventions
  auxs <- pa.auxs
  
  pa.case<-data.frame(ode(y=stocks, times=simtime, func = model, 
                            parms=auxs, method='euler'))
  
 
  #food system interventions
  auxs <- food.auxs
 
  food.case<-data.frame(ode(y=stocks, times=simtime, func = model, 
                         parms=auxs, method='euler'))
 
  #upstream interventions
  auxs <- upstream.auxs
  
  upstream.combo.case<-data.frame(ode(y=stocks, times=simtime, func = model, 
                           parms=auxs, method='euler'))
 
  #healthcare
  hc.switch <- 1
  auxs <- hc.auxs
  
  healthcare.case <- data.frame(ode(y=stocks, times=simtime, func = model, 
                                    parms=auxs, method='euler'))
  #all interventions at once
  auxs <- allin.auxs
  
  allin.case <- data.frame(ode(y=stocks, times=simtime, func=model,
                               parms=auxs, method='euler'))

intervention.case.plot <- ggplot() +
  geom_line(data=allin.case[allin.case$time %in% c(2015:2050),], aes(x=time, y=dmprev, color='All interventions')) +
  geom_line(data=pa.case[pa.case$time %in% c(2015:2050),], aes(x=time, y=dmprev, color='PA only')) +
  geom_line(data=food.case[food.case$time %in% c(2015:2050),],  aes(x=time, y=dmprev, color='Food system only')) +
  geom_line(data=upstream.combo.case[upstream.combo.case$time %in% c(2015:2050),],  aes(x=time, y=dmprev, color='Upstream combined')) +
  geom_line(data=healthcare.case[healthcare.case$time %in% c(2015:2050),],  aes(x=time, y=dmprev, color='Health system')) +
  geom_line(data=base.case[base.case$time %in% c(2015:2050),],  aes(x=time, y=dmprev, color='Baseline')) +
  labs(x="Year", y="Diabetes prevalence (%)", color = 'Scenarios', title = "Effects of intervention strategies on diabetes prevalence")
intervention.case.plot

intervention.incidence.plot <- ggplot() +
  geom_line(data=allin.case[allin.case$time %in% c(2015:2050),], aes(x=time, y=DMOnset, color='All interventions')) +
  geom_line(data=pa.case[pa.case$time %in% c(2015:2050),], aes(x=time, y=DMOnset, color='PA only')) +
  geom_line(data=food.case[food.case$time %in% c(2015:2050),],  aes(x=time, y=DMOnset, color='Food system only')) +
  geom_line(data=upstream.combo.case[upstream.combo.case$time %in% c(2015:2050),],  aes(x=time, y=DMOnset, color='Upstream combined')) +
  geom_line(data=healthcare.case[healthcare.case$time %in% c(2015:2050),],  aes(x=time, y=DMOnset, color='Health system')) +
  geom_line(data=base.case[base.case$time %in% c(2015:2050),],  aes(x=time, y=DMOnset, color='Baseline')) +
  labs(x="Year", y="Diabetes new cases (in 1000s)", color = 'Scenarios', title = "Effects of intervention strategies new diabetes cases")
intervention.incidence.plot


intervention.deaths.plot <- ggplot() +
  geom_line(data=allin.case[allin.case$time %in% c(2015:2050),], aes(x=time, y=DMMortality, color='All interventions')) +
  geom_line(data=pa.case[pa.case$time %in% c(2015:2050),], aes(x=time, y=DMMortality, color='PA only')) +
  geom_line(data=food.case[food.case$time %in% c(2015:2050),],  aes(x=time, y=DMMortality, color='Food system only')) +
  geom_line(data=upstream.combo.case[upstream.combo.case$time %in% c(2015:2050),],  aes(x=time, y=DMMortality, color='Upstream combined')) +
  geom_line(data=healthcare.case[healthcare.case$time %in% c(2015:2050),],  aes(x=time, y=DMMortality, color='Health system')) +
  geom_line(data=base.case[base.case$time %in% c(2015:2050),],  aes(x=time, y=DMMortality, color='Baseline')) +
  labs(x="Year", y="Deaths in PWD in 1000s)", color = 'Scenarios', title = "Effects of intervention strategies on deaths in PWD")
intervention.deaths.plot


intervention.obesity.plot <- ggplot() +
  geom_line(data=allin.case[allin.case$time %in% c(2015:2050),], aes(x=time, y=Obesity, color='All interventions')) +
  geom_line(data=pa.case[pa.case$time %in% c(2015:2050),], aes(x=time, y=Obesity, color='PA only')) +
  geom_line(data=food.case[food.case$time %in% c(2015:2050),],  aes(x=time, y=Obesity, color='Food system only')) +
  geom_line(data=upstream.combo.case[upstream.combo.case$time %in% c(2015:2050),],  aes(x=time, y=Obesity, color='Upstream combined')) +
  geom_line(data=healthcare.case[healthcare.case$time %in% c(2015:2050),],  aes(x=time, y=Obesity, color='Health system')) +
  geom_line(data=base.case[base.case$time %in% c(2015:2050),],  aes(x=time, y=Obesity, color='Baseline')) +
  labs(x="Year", y="Obesity prevalence (%)", color = 'Scenarios', title = "Effects of intervention strategies on obesity prevalence")
intervention.obesity.plot



