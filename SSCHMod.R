### System Science for Carribean Health Model of Diabetes ###

# load necessary packages #
library(deSolve)
library(ggplot2)
library(FME)
library(plyr)
library(grid)
library(gridExtra)

model <- function(time, stocks, auxs){
  
  with(as.list(c(stocks, auxs)),{
  #  browser()
    # simple function implement an intervention year
    intervention.year <- function(x){
      x.time <- c(rep(0, 2050-aInterventionYear), rep(x, 61-(2050-aInterventionYear)))
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
    
    aDailyIntake <- aUHFoods + aFVkcalperday + ffOtherIntake(time) + aCaloriesSSB
  
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
    aTotalMETs <- aTotalMVPA*aMETsMVPA
    aDailyCalBurned <- (aTotalMETs*3.5*sAvgBdWt)/200
    
    ##Obesity module
    aMod.BMI <- sAvgBdWt / (aAvgHeight^2)
    BMI.M <- aMod.BMI/(per.men + per.women*BMI.ratio)*100
    BMI.W <- BMI.ratio * BMI.M
    aFatFrac.M <- ((3.76*BMI.M) - (0.04*BMI.M^2) - 47.8)/100
    aFatFrac.W <- ((4.35*BMI.W) - (0.05*BMI.W^2) - 46.24)/100
    aFatFrac <- (aFatFrac.W + aFatFrac.M)/2
   
    aBasalCalories <- (sAvgBdWt * (0.024 * aFatFrac + 0.102 * (1-aFatFrac)) + 0.85) * 238.7
    aCaloricBalance <- aDailyIntake - aDailyCalBurned - aBasalCalories - (aFracCalDigestion * aDailyIntake)
    aFracBalanceFat <- 1/(1 +((10.4/(sAvgBdWt*aFatFrac))*(4100/9300)))
    fChange.wt <- aCaloricBalance * ((aFracBalanceFat/9300) + ((1-aFracBalanceFat)/4100))
    dAvg.wt_dt <- fChange.wt
    aObeseTotal <- 43 + -6.65*aMod.BMI + 0.233*aMod.BMI^2
    aObesefractNGT <- aObeseTotal*0.8
    aObesefractIGT <- aObeseTotal*1.2
    
    ## Core Model
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
    
    fIGTRecovery <- aIGTrecovery/100*sIGT
    
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
    fDMMortality <- (sDM*(under.55/100)*aRRofMortalityDM.under55*(aMortalityNGTrate.under55/1000)) +
      (sDM*aRRofMortalityDM.over55*(aMortalityNGTrate.over55/1000)*(over.55(time)/100))
   
    # DM Stock equation
    dDM_dt <- fDMOnset - fDMMortality
    
    #estimate prevalence and incidence
    dmprev <- sDM*100/(sDM + sIGT + sNGT)
    dminc <- fDMOnset * 1000/(sIGT + sNGT)
    
    # All the results for the time step
    ans <- list(c(dNGT_dt, dIGT_dt, dDM_dt, dAvg.wt_dt, dFV_dt),
                NGTOnset=fAdultGrowth,
                NGTMortality=fNGTMortality,
                NGTNetFlow=dNGT_dt,
                IGTOnset=fIGTOnset,
                IGTMortality=fIGTMortality,
                IGTNetFlow=dIGT_dt,
                DMOnset=fDMOnset,
                DMMortality=fDMMortality,
                DMNetFlow=dDM_dt,
                BWtAvg=dAvg.wt_dt,
                Obesity=aObeseTotal,
                dmprev=dmprev,
                dminc=dminc)
    
  })
}

#MODEL RUN

  # Set up simulation and time step #
  START<-1990; FINISH<-2050; STEP<-1
  
  # Create time vector
  simtime <- seq(START, FINISH, by=STEP)
  
  # Create stock and auxs
  
  stocks  <- c(sNGT=Baseline.NGT,
               sIGT=Baseline.IGT,
               sDM=Baseline.DM,
               sAvgBdWt=aInitAvgWt,
               sFV=aInitFVStock) 
  
  auxs <- c(aInterventionYear=2020,
    
    #interventions
    aSSBPriceChange=0, #intervention point
    aIncreaseinFV=0, #intervention point for increase in production as a percentage
    aPriceChangeFV=0,#intervention point change as a percentage (+ for increase, - for decrease)
    aPriceChangeUH=0,#intervention point change as a percentage
    aFVPH.switch=0, #set to 1 for intervention, fruit and veg consumption PH campaign
    aUHPH.switch=0, #set to 1 for intervention, traffic light labeling of food
    aInfra.switch=0, #change to 1 for intervention, infrastructure for active transport
    aPublic.switch=0, #change to 1 for interventions to increase public transportation
    aPAPH.Campaign=0, #change to 1 for intervention, PA awareness campaign
    
    #ssb assumptions
    aElasticity.SSB=-1.3,
    aEffectSSB.Campaign=-0.5,
    aSSBperUnitCal=130,
    
    #Calories from food assumptions
    aImportsTourism=60,
    aLocalTourism=15,
    aElasFVPrice=-.65,
    aUHFVCrossPrice=0.07,
    aInitFVIntake=73,
    aEffectFVPH=6.2,
    aCalperFV=1,
    aElasUHFoods=-0.09,
    aEffectUHPH=-3.59,#traffic light system of food labeling
    
    #Physical activity calories
    aPublic.Transport=4.3, # min/day added from better public transport
    aEffectInfra=16, 
    aRRPACampaign=1.28, 
    aMETsMVPA=4.0,
    
    #Obesity
    aFracCalDigestion=0.1,
    aAvgHeight=1.65,
    
    #Core model
    aMortalityNGTrate.under55=2,
    aMortalityNGTrate.over55=32,
    aIGTincidenceNO=20,
    aRRofIGTinObese=1.32,
    aIGTrecovery=10, 
    aDMincidenceNO=35,
    aRRofDMinObese=2.3,
    aRRofSSBs=13,
    aRRofDMinElderly=1.5,
    aRRofMortalityDM.over55=1.65, 
    aRRofMortalityDM.under55=3.0)

  
  base.case<-data.frame(ode(y=stocks, times=simtime, func = model, 
                    parms=auxs, method='euler'))

  
### Physical activity interventions
  
  auxs <- c(aInterventionYear=2020,
                    
                    #interventions
                    aSSBPriceChange=0, #intervention point
                    aIncreaseinFV=0, #intervention point for increase in production as a percentage
                    aPriceChangeFV=0,#intervention point change as a percentage (+ for increase, - for decrease)
                    aPriceChangeUH=0,#intervention point change as a percentage
                    aFVPH.switch=0, #set to 1 for intervention, fruit and veg consumption PH campaign
                    aUHPH.switch=0, #set to 1 for intervention, traffic light labeling of food
                    aInfra.switch=1, #change to 1 for intervention, infrastructure for active transport
                    aPublic.switch=1, #change to 1 for interventions to increase public transportation
                    aPAPH.Campaign=1, #change to 1 for intervention, PA awareness campaign
                    
                    #ssb assumptions
                    aElasticity.SSB=-1.3,
                    aEffectSSB.Campaign=-0.5,
                    aSSBperUnitCal=130,
                    
                    #Calories from food assumptions
                    aImportsTourism=60,
                    aLocalTourism=15,
                    aElasFVPrice=-.65,
                    aUHFVCrossPrice=0.07,
                    aInitFVIntake=73,
                    aEffectFVPH=6.2,
                    aCalperFV=1,
                    aElasUHFoods=-0.09,
                    aEffectUHPH=-3.59,#traffic light system of food labeling
                    
                    #Physical activity calories
                    aPublic.Transport=4.3, # min/day added from better public transport
                    aEffectInfra=16, 
                    aRRPACampaign=1.28, 
                    aMETsMVPA=4.0,
                    
                    #Obesity
                    aFracCalDigestion=0.1,
                    aAvgHeight=1.65,
                    
                    #Core model
                    aMortalityNGTrate.under55=2,
                    aMortalityNGTrate.over55=32,
                    aIGTincidenceNO=24,
                    aRRofIGTinObese=1.32,
                    aIGTrecovery=10, 
                    aDMincidenceNO=35,
                    aRRofDMinObese=2.3,
                    aRRofSSBs=13,
                    aRRofDMinElderly=1.5,
                    aRRofMortalityDM.over55=1.65, 
                    aRRofMortalityDM.under55=3.0)

  pa.case<-data.frame(ode(y=stocks, times=simtime, func = model, 
                            parms=auxs, method='euler'))


 ## Food system interventions
 
 auxs <- c(aInterventionYear=2020,
           
           #interventions
           aSSBPriceChange=20, #intervention point
           aIncreaseinFV=5, #intervention point for increase in production as a percentage
           aPriceChangeFV=-10,#intervention point change as a percentage (+ for increase, - for decrease)
           aPriceChangeUH=10,#intervention point change as a percentage
           aFVPH.switch=1, #set to 1 for intervention, fruit and veg consumption PH campaign
           aUHPH.switch=1, #set to 1 for intervention, traffic light labeling of food
           aInfra.switch=0, #change to 1 for intervention, infrastructure for active transport
           aPublic.switch=0, #change to 1 for interventions to increase public transportation
           aPAPH.Campaign=0, #change to 1 for intervention, PA awareness campaign
           
           #ssb assumptions
           aElasticity.SSB=-1.3,
           aEffectSSB.Campaign=-0.5,
           aSSBperUnitCal=130,
           
           #Calories from food assumptions
           aImportsTourism=60,
           aLocalTourism=15,
           aElasFVPrice=-.65,
           aUHFVCrossPrice=0.07,
           aInitFVIntake=73,
           aEffectFVPH=6.2,
           aCalperFV=1,
           aElasUHFoods=-0.09,
           aEffectUHPH=-3.59,#traffic light system of food labeling
           
           #Physical activity calories
           aPublic.Transport=4.3, # min/day added from better public transport
           aEffectInfra=16, 
           aRRPACampaign=1.28, 
           aMETsMVPA=4.0,
           
           #Obesity
           aFracCalDigestion=0.1,
           aAvgHeight=1.65,
           
           #Core model
           aMortalityNGTrate.under55=2,
           aMortalityNGTrate.over55=32,
           aIGTincidenceNO=20,
           aRRofIGTinObese=1.32,
           aIGTrecovery=10, 
           aDMincidenceNO=35,
           aRRofDMinObese=2.3,
           aRRofSSBs=13,
           aRRofDMinElderly=1.5,
           aRRofMortalityDM.over55=1.65, 
           aRRofMortalityDM.under55=3.0)
 
 food.case<-data.frame(ode(y=stocks, times=simtime, func = model, 
                         parms=auxs, method='euler'))
 
 
 ## upstream
 auxs <- c(aInterventionYear=2020,
           
           #interventions
           aSSBPriceChange=20, #intervention point
           aIncreaseinFV=5, #intervention point for increase in production as a percentage
           aPriceChangeFV=-10,#intervention point change as a percentage (+ for increase, - for decrease)
           aPriceChangeUH=10,#intervention point change as a percentage
           aFVPH.switch=1, #set to 1 for intervention, fruit and veg consumption PH campaign
           aUHPH.switch=1, #set to 1 for intervention, traffic light labeling of food
           aInfra.switch=1, #change to 1 for intervention, infrastructure for active transport
           aPublic.switch=1, #change to 1 for interventions to increase public transportation
           aPAPH.Campaign=1, #change to 1 for intervention, PA awareness campaign
           
           #ssb assumptions
           aElasticity.SSB=-1.3,
           aEffectSSB.Campaign=-0.5,
           aSSBperUnitCal=130,
           
           #Calories from food assumptions
           aImportsTourism=60,
           aLocalTourism=15,
           aElasFVPrice=-.65,
           aUHFVCrossPrice=0.07,
           aInitFVIntake=73,
           aEffectFVPH=6.2,
           aCalperFV=1,
           aElasUHFoods=-0.09,
           aEffectUHPH=-3.59,#traffic light system of food labeling
           
           #Physical activity calories
           aPublic.Transport=4.3, # min/day added from better public transport
           aEffectInfra=16, 
           aRRPACampaign=1.28, 
           aMETsMVPA=4.0,
           
           #Obesity
           aFracCalDigestion=0.1,
           aAvgHeight=1.65,
           
           #Core model
           aMortalityNGTrate.under55=2,
           aMortalityNGTrate.over55=32,
           aIGTincidenceNO=20,
           aRRofIGTinObese=1.32,
           aIGTrecovery=10, 
           aDMincidenceNO=35,
           aRRofDMinObese=2.3,
           aRRofSSBs=13,
           aRRofDMinElderly=1.5,
           aRRofMortalityDM.over55=1.65, 
           aRRofMortalityDM.under55=3.0)
 
 upstream.combo.case<-data.frame(ode(y=stocks, times=simtime, func = model, 
                           parms=auxs, method='euler'))


# healthcare only
## Need to modify the time of running the model to be able to intervene with changes 
 # in RR for mortality and Pre-DM recovery

 ## healthcare
 # Set up simulation and time step #
 START<-2020; FINISH<-2050; STEP<-1
 
 # Create time vector
 simtime <- seq(START, FINISH, by=STEP)
 
 # Create stock and auxs
 stocks <- c(sNGT=base.case$sNGT[base.case$time == 2020],
             sIGT=base.case$sIGT[base.case$time == 2020],
             sDM=base.case$sDM[base.case$time ==2020],
             sAvgBdWt=base.case$sAvgBdWt[base.case$time == 2020],
             sFV=base.case$sFV[base.case$time == 2020])
 
  auxs <- c(aInterventionYear=2020,
          
          #interventions
          aSSBPriceChange=0, #intervention point
          aIncreaseinFV=0, #intervention point for increase in production as a percentage
          aPriceChangeFV=0,#intervention point change as a percentage (+ for increase, - for decrease)
          aPriceChangeUH=0,#intervention point change as a percentage
          aFVPH.switch=0, #set to 1 for intervention, fruit and veg consumption PH campaign
          aUHPH.switch=0, #set to 1 for intervention, traffic light labeling of food
          aInfra.switch=0, #change to 1 for intervention, infrastructure for active transport
          aPublic.switch=0, #change to 1 for interventions to increase public transportation
          aPAPH.Campaign=0, #change to 1 for intervention, PA awareness campaign
          
          #ssb assumptions
          aElasticity.SSB=-1.3,
          aEffectSSB.Campaign=-0.5,
          aSSBperUnitCal=130,
          
          #Calories from food assumptions
          aImportsTourism=60,
          aLocalTourism=15,
          aElasFVPrice=-.65,
          aUHFVCrossPrice=0.07,
          aInitFVIntake=73,
          aEffectFVPH=6.2,
          aCalperFV=1,
          aElasUHFoods=-0.09,
          aEffectUHPH=-3.59,#traffic light system of food labeling
          
          #Physical activity calories
          aPublic.Transport=4.3, # min/day added from better public transport
          aEffectInfra=16, 
          aRRPACampaign=1.28, 
          aMETsMVPA=4.0,
          
          #Obesity
          aFracCalDigestion=0.1,
          aAvgHeight=1.65,
          
          #Core model
          aMortalityNGTrate.under55=2,
          aMortalityNGTrate.over55=32,
          aIGTincidenceNO=20,
          aRRofIGTinObese=1.32,
          aIGTrecovery=12, #base is 10, increase to 12 for high-risk approach 
          aDMincidenceNO=35,
          aRRofDMinObese=2.3,
          aRRofSSBs=13,
          aRRofDMinElderly=1.5,
          aRRofMortalityDM.over55=1.32,#reduction in RR mortality of PWD by 20 percent
          aRRofMortalityDM.under55=2.4)

healthcare.case<-data.frame(ode(y=stocks, times=simtime, func = model, 
                                    parms=auxs, method='euler'))


intervention.case.plot <- ggplot() +
  geom_line(data=pa.case[pa.case$time %in% c(2020:2050),], aes(x=time, y=dmprev, color='PA only')) +
  geom_line(data=food.case[food.case$time %in% c(2020:2050),],  aes(x=time, y=dmprev, color='Food system only')) +
  geom_line(data=upstream.combo.case[upstream.combo.case$time %in% c(2020:2050),],  aes(x=time, y=dmprev, color='Upstream combined')) +
  geom_line(data=healthcare.case,  aes(x=time, y=dmprev, color='Health system')) +
  geom_line(data=base.case[base.case$time %in% c(2020:2050),],  aes(x=time, y=dmprev, color='Baseline')) +
  labs(x="Year", y="Diabetes prevalence (%)", color = 'Scenarios', title = "Effects of intervention strategies on diabetes prevalence")
intervention.case.plot

intervention.incidence.plot <- ggplot() +
  geom_line(data=pa.case[pa.case$time %in% c(2020:2050),], aes(x=time, y=DMOnset, color='PA only')) +
  geom_line(data=food.case[food.case$time %in% c(2020:2050),],  aes(x=time, y=DMOnset, color='Food system only')) +
  geom_line(data=upstream.combo.case[upstream.combo.case$time %in% c(2020:2050),],  aes(x=time, y=DMOnset, color='Upstream combined')) +
  geom_line(data=healthcare.case,  aes(x=time, y=DMOnset, color='Health system')) +
  geom_line(data=base.case[base.case$time %in% c(2020:2050),],  aes(x=time, y=DMOnset, color='Baseline')) +
  labs(x="Year", y="Diabetes new cases (in 1000s)", color = 'Scenarios', title = "Effects of intervention strategies new diabetes cases")
intervention.incidence.plot


intervention.deaths.plot <- ggplot() +
  geom_line(data=pa.case[pa.case$time %in% c(2020:2050),], aes(x=time, y=DMMortality, color='PA only')) +
  geom_line(data=food.case[food.case$time %in% c(2020:2050),],  aes(x=time, y=DMMortality, color='Food system only')) +
  geom_line(data=upstream.combo.case[upstream.combo.case$time %in% c(2020:2050),],  aes(x=time, y=DMMortality, color='Upstream combined')) +
  geom_line(data=healthcare.case,  aes(x=time, y=DMMortality, color='Health system')) +
  geom_line(data=base.case[base.case$time %in% c(2020:2050),],  aes(x=time, y=DMMortality, color='Baseline')) +
  labs(x="Year", y="Deaths in PWD in 1000s)", color = 'Scenarios', title = "Effects of intervention strategies on deaths in PWD")
intervention.deaths.plot


intervention.obesity.plot <- ggplot() +
  geom_line(data=pa.case[pa.case$time %in% c(2020:2050),], aes(x=time, y=Obesity, color='PA only')) +
  geom_line(data=food.case[food.case$time %in% c(2020:2050),],  aes(x=time, y=Obesity, color='Food system only')) +
  geom_line(data=upstream.combo.case[upstream.combo.case$time %in% c(2020:2050),],  aes(x=time, y=Obesity, color='Upstream combined')) +
  geom_line(data=healthcare.case,  aes(x=time, y=Obesity, color='Health system')) +
  geom_line(data=base.case[base.case$time %in% c(2020:2050),],  aes(x=time, y=Obesity, color='Baseline')) +
  labs(x="Year", y="Obesity prevalence (%)", color = 'Scenarios', title = "Effects of intervention strategies on obesity prevalence")
intervention.obesity.plot
