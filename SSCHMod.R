### System Science for Carribean Health Model of Diabetes ###

# load necessary packages #
library(deSolve)
library(ggplot2)
library(FME)

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
    aAvgSSBConsumption <- aSSB.init + (aEffectSSBPriceChange/100)
    aCaloriesSSB <- aAvgSSBConsumption*aSSBperUnitCal
     #Food calories
  
    #inflow of Fruit and Vegetable production
    FV.increase.int <- intervention.year(aIncreaseinFV)
    ffFV.increase <- approxfun(years.all, FV.increase.int)
    
    aFVProduction <- ((ffInitialFVProduction(time)*1000) + 
                        ((ffFV.increase(time))/100*ffInitialFVProduction(time)*1000))/ffTotalPopulation(time)
    
    aFVImports <- ffTotalkgImports(time)/ffTotalPopulation(time)
    fFVProduction <- (ffTotalkgImports(time) - (ffTotalkgImports(time)*aImportsTourism/100)) + 
      (aFVProduction - (aFVProduction*aLocalTourism/100))
    
    #outflow of waste and export
    fFVExport <- ffFVExport(time)/ffTotalPopulation(time) #per capita export per year
    fFVWaste <- fFVProduction * .3
    
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
    
    aEffectUHTax <- ffUH.price.change(time) * aElasUHFoods #elasticity should be negative
    
    UHPH <- intervention.year(aUHPH.switch)
    ffUHPH <- approxfun(years.all, UHPH)
    
    aUHFoods <- ffUHCalories(time) + (ffUHCalories(time)*aEffectUHTax/100) - 
      (ffUHCalories(time)*(aEffectUHPH/100)*ffUHPH(time))
    
    aDailyIntake <- aUHFoods + aFVkcalperday + ffOtherIntake(time) + aCaloriesSSB
  
    #Fruit and vegetable stock equation
    dFV_dt <-  fFVProduction - fFVWaste - fFVExport - fFVConsumption
    
    #Physical activity
    bus.fare.change <- intervention.year(aChange.in.Bus.Fare)
    ffbus.fare.change <- approxfun(years.all, bus.fare.change)
    
    aNew.Bus.Use <- ffbus.to.car(time) + (aElasticity.Bus.Fare*ffbus.fare.change(time))
    aBusMinutes <- ffBusMinutes.calc(aNew.Bus.Use)
    aWorkMVPA <- ff.work.mvpa(time)
    aTravelMVPA <- ff.travel.mvpa(time)
    aLTMVPA <- ff.leisure.mvpa(time)
    
    infra.change <- intervention.year(aInfra.switch)
    ffinfra.change <- approxfun(years.all, infra.change)
    
    aActiveTransport <- aTravelMVPA + (aTravelMVPA*aEffectInfra/100)*ffinfra.change(time) + aBusMinutes
    aLTMVPA.with.infra <- aLTMVPA + aLTMVPA*(aEffectInfra/100)*ffinfra.change(time)
    aTotalMVPA.noPH <- aActiveTransport + aWorkMVPA + aLTMVPA.with.infra
    
    PA.campaign <- intervention.year(aPAPH.Campaign)
    ffPA.campaign <- approxfun(years.all, PA.campaign)
    
    aTotalMVPA <- aTotalMVPA.noPH + (aRRPACampaign-1)*(aTotalMVPA.noPH)*ffPA.campaign(time)
    aTotalMETs <- aTotalMVPA*aMETsMVPA
    aDailyCalBurned <- (aTotalMETs*3.5*sAvgBdWt)/200
    
    ##Obesity module
    aBasalCalories <- (sAvgBdWt * (0.024 * aFatFrac + 0.102 * (1-aFatFrac)) + 0.85) * 238.7
    aCaloricBalance <- aDailyIntake - aDailyCalBurned - aBasalCalories - (aFracCalDigestion * aDailyIntake)
    aFracBalanceFat <- 1/(1 +((10.4/(sAvgBdWt*aFatFrac))*(4100/9300)))
    fChange.wt <- aCaloricBalance * ((aFracBalanceFat/9300) + ((1-aFracBalanceFat)/4100))
    dAvg.wt_dt <- fChange.wt
    
    aMod.BMI <- sAvgBdWt / (aAvgHeight^2)
    aObeseTotal <- Obesity.fraction.calc(aMod.BMI)*100
    aObesefractNGT <- aObeseTotal*0.8
    aObesefractIGT <- aObeseTotal*1.2
    
    ## Core Model
    # NGT
    # Inflow of adult population
    fAdultGrowth <- Total.pop(time)
    
    # Outflow NGT mortality
    under.50 <- 100-over.50(time)
    fNGTMortality <- (sNGT*(aMortalityNGTrate.under50/1000)*(under.50/100)) +
      (sNGT*(aMortalityNGTrate.over50/1000)*((over.50(time)/100)))
      
    # Outflow of new cases of IGT/IFG
    fIGTOnset <- (sNGT*(aObesefractNGT/100)*(aIGTincidenceNO/100)*aRRofIGTinObese) +
      (sNGT*((100-aObesefractNGT)/100)*(aIGTincidenceNO/100))
    
    # Inflow of IGT recovery
    # create an option for modifying the rate of recovery as a HC intervention
    #HC.IGT <-  c(rep(10, 2050-aInterventionYear), rep(aIGTrecovery, 61-(2050-aInterventionYear)))
    #ffHC.IGT <- approxfun(years.all, HC.IGT)
    #aIGTrecovery <- ffHC.IGT(time)
    
    fIGTRecovery <- aIGTrecovery/100*sIGT
    
    # NGT stock equation
    dNGT_dt <- fAdultGrowth + fIGTRecovery - fIGTOnset - fNGTMortality
    
    # IGT
    # Outflow IGT mortality
    fIGTMortality <- (sIGT*(aMortalityNGTrate.under50/1000)*(under.50/100)) +
      (sIGT*(aMortalityNGTrate.over50/1000)*(over.50(time)/100))
    
    # Outflow DM Onset
    aDMinNonObese <- sIGT*((100-aObesefractIGT)/100)*(aDMincidenceNO/100)
    aDMinObese <- (aObesefractIGT/100)*sIGT*(aDMincidenceNO/100)*aRRofDMinObese
    aDMOnsetObesity <- aDMinObese + aDMinNonObese
    aDMOnsetPA<- aDMOnsetObesity*ff((aTotalMVPA/60)*7)
    aDMOnsetSSB <- aDMOnsetPA + (aDMOnsetPA*(aRRofSSBs/100)*aAvgSSBConsumption)
    aDMOnsetAging <- ((over.65(time)/100)*aRRofDMinElderly*aDMOnsetSSB) +
      (((100-over.65(time))/100)*aDMOnsetSSB)
    fDMOnset <- aDMOnsetAging
     
    # IGT Stock equation
    dIGT_dt <- fIGTOnset - fIGTRecovery - fIGTMortality - fDMOnset
    
    # DM
    # Outflow DM mortality
    fDMMortality <- (sDM*(under.50/100)*aRRofMortalityDM.under50*(aMortalityNGTrate.under50/1000)) +
      (sDM*aRRofMortalityDM.over50*(aMortalityNGTrate.over50/1000)*(over.50(time)/100))
   
    # DM Stock equation
    dDM_dt <- fDMOnset - fDMMortality
    
    
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
                under.50=under.50)
    
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
  
  auxs    <- c(aInterventionYear=2020,
               
               #interventions
               aSSBPriceChange=0, #intervention point
               aIncreaseinFV=0, #intervention point for increase in production as a percentage
               aPriceChangeFV=0,#intervention point change as a percentage (+ for increase, - for decrease)
               aPriceChangeUH=0,#intervention point change as a percentage
               aFVPH.switch=0, #set to 1 for intervention, fruit and veg consumption PH campaign
               aUHPH.switch=0, #set to 1 for intervention, traffic light labeling of food
               
               aChange.in.Bus.Fare=0,#intervention point, change as a percentage
               aInfra.switch=0, #change to 1 for intervention, infrastructure for active transport
               aPAPH.Campaign=0, #change to 1 for intervention, PA awareness campaign
               
               #ssb assumptions
               aElasticity.SSB=-1.3,
               aEffectSSB.Campaign=-0.5,
               aSSBperUnitCal=130,
               aSSB.init=2,
               
               #Calories from food assumptions
               aImportsTourism=60,
               aLocalTourism=15,
               aElasFVPrice=-.65,
               aUHFVCrossPrice=0.07,
               aInitFVIntake=109.5,
               aEffectFVPH=6.2,
               aCalperFV=1,
               aElasUHFoods=-0.9,
               aEffectUHPH=3.59,#traffic light system of food labeling
               
               #Physical activity calories
               aElasticity.Bus.Fare=0.15, 
               #aWork.init=200,
               #aTravel.init=60,
               #aLT.init=25,
               aEffectInfra=16, 
               aRRPACampaign=1.28, 
               aMETsMVPA=4.0,
               
               #Obesity
               aFatFrac=0.3,
               aFracCalDigestion=0.1,
               aAvgHeight=1.65,
               
               #Core model
               aMortalityNGTrate.under50=7.6,
               aMortalityNGTrate.over50=48.7,
               aIGTincidenceNO=4.5,
               aRRofIGTinObese=1.5,
               aIGTrecovery=10, 
               aDMincidenceNO=9,
               aRRofDMinObese=1.6,
               aRRofSSBs=13,
               aRRofDMinElderly=1.5,
               aRRofMortalityDM.over50=1.65, 
               aRRofMortalityDM.under50=3.0)
  
  #browser()
  base.case<-data.frame(ode(y=stocks, times=simtime, func = model, 
                    parms=auxs, method='euler'))
  base.case$dmprev <- base.case$sDM/(base.case$sDM + base.case$sIGT + base.case$sNGT)*100
  #qplot(x=time, y=(sDM/(sDM+sIGT+sNGT)*100), xlab = "Year", ylab="Diabetes Prevalence (%)", data=base.case) + geom_line()

  ggplot(data=base.case, mapping=aes(x=time, y=dmprev)) + 
    geom_smooth() +
    labs(x="Years", y="Diabetes prevalence (%)")

#############################################################
# SENSITIVITY ANALYSIS
#baseline mid values
base.midpoints <- data.frame(mid.point=c(SSB.Campaign.base =-0.5, SSBperUnitCal.base=130, SSB.init.base=2, ImportsTourism.base=60, LocalTourism.base=15,
                               ElasFVPrice.base=-.65, InitFVIntake.base=109.5, EffectFVPH.base=6.2, CalperFV.base=1, ElasUHFoods.base=-0.9,
                               EffectUHPH.base=3.59, Elasticity.Bus.Fare.base=0.15, 
                               EffectInfra.base=16, RRPACampaign.base=1.28, MortalityNGTrate.under50.base=7.6, MortalityNGTrate.over50.base=48.7,
                               IGTincidenceNO.base=4.5, RRofIGTinObese.base=1.5, IGTrecovery.base=10, DMincidenceNO.base=9, RRofDMinObese.base=1.6,
                               RRofSSBs.base=13, RRofDMinElderly.base=1.5, RRofMortalityDM.over50.base=1.65, RRofMortalityDM.under50.base=3.0))

rownames(base.midpoints) <- c("aEffectSSB.Campaign", "aSSBperUnitCal", "aSSB.init", "aImportsTourism", "aLocalTourism", "aElasFVPrice", 
                          "aInitFVIntake", "aEffectFVPH", "aCalperFV", "aElasUHFoods", "aEffectUHPH", "aElasticity.Bus.Fare", "aEffectInfra", 
                          "aRRPACampaign", "aMortalityNGTrate.under50", "aMortalityNGTrate.over50", "aIGTincidenceNO", "aRRofIGTinObese", 
                          "aIGTrecovery", "aDMincidenceNO", "aRRofDMinObese", "aRRofSSBs", "aRRofDMinElderly", "aRRofMortalityDM.over50",
                          "aRRofMortalityDM.under50")
  
#min and max for Latin Hypercube sampling

UHF.PH.min=-8.9; UHF.PH.max=1.72; #traffic light system
Elas.bus.fare.min=-0.3; Elas.bus.fare.max=-0.65;
#Work.init.min=1; Work.init.max=1440.5;
#Travel.init.min=1; Travel.init.max=557;
#LT.init.min=2; LT.init.max=480;
Effect.infra.min=0; Effect.infra.max=25;
RRPA.min=1.03; RRPA.max=1.3;
MortalityNGTrate.under50.min= 6.6; MortalityNGTrate.under50.max= 8.0; #range from NHANES paper for non-diabetic, black population
MortalityNGTrate.over50.min= 39.05; MortalityNGTrate.over50.max= 58.35;#range from NHANES
IGT.incidence.min=3.0; IGT.incidence.max=6.0;
RR.IGT.Obese.min=1.5; RR.IGT.Obese.max=2.0;
IGT.recovery.min=5; IGT.recovery.max=15;
DM.incidence.NO.min=5; DM.incidence.NO.max=12; #no data from the DPP on confidence intervals
RR.DM.Obese.min=1.2; RR.DM.Obese.max=5.0; #no data from DPP on confidence, 1.6 for those with IGT; maybe better to use nurse's health values
RR.SSBs.min=6; RR.SSBs.max=21; #Fumiaki
RR.DM.Elderly.min=1.0; RR.DM.Elderly.max=1.8; #not sure this is necessary
RR.Mortality.DM.Over50.min=1.4; RR.Mortality.DM.Over50.max=2.6; #unwin and roglic from NHANES
RR.Mortality.DM.Under50.min=2.0; RR.Mortality.DM.Under50.max=4.6; #unwin and roglic from NHANES

parRange <- data.frame(
  min=c(SSB.Camp.min=-1.299, SSB.per.unit.min=110, SSB.init.min=1.2, Imports.Tourism.min=50, Local.Tourism.min=10, Elas.FV.min=-0.71,
        Init.FV.min=247.8, FV.PH.min=11.24, Cal.per.FV.min=0.95, Elas.UHF.min=-6, UHF.PH.min, Elas.bus.fare.min, 
        Effect.infra.min, RRPA.min, MortalityNGTrate.under50.min,
        MortalityNGTrate.over50.min, IGT.incidence.min, RR.IGT.Obese.min, IGT.recovery.min,
        DM.incidence.NO.min, RR.DM.Obese.min, RR.SSBs.min, RR.DM.Elderly.min, RR.Mortality.DM.Over50.min,
        RR.Mortality.DM.Under50.min),
  max=c(SSB.Camp.max=1.089, SSB.per.unit.max=150, SSB.init.max=3.0, Imports.Tourism.max=70, Local.Tourism.max=20, Elas.FV.max=-0.59,
        Init.FV.max=376.5, FV.PH.max=24.66, Cal.per.FV.max=1.05, Elas.UHF.max=-12, UHF.PH.max, Elas.bus.fare.max,  Effect.infra.max, RRPA.max, MortalityNGTrate.under50.max, 
        MortalityNGTrate.over50.max, IGT.incidence.max, RR.IGT.Obese.max, IGT.recovery.max,
        DM.incidence.NO.max, RR.DM.Obese.max, RR.SSBs.max, RR.DM.Elderly.max, RR.Mortality.DM.Over50.max,
        RR.Mortality.DM.Under50.max))

rownames(parRange) <- c("aEffectSSB.Campaign", "aSSBperUnitCal", "aSSB.init", 
                        "aImportsTourism", "aLocalTourism", "aElasFVPrice", 
                        "aInitFVIntake", "aEffectFVPH", "aCalperFV", "aElasUHFoods", "aEffectUHPH",
                        "aElasticity.Bus.Fare", "aEffectInfra", 
                        "aRRPACampaign", "aMortalityNGTrate.under50", "aMortalityNGTrate.over50",
                        "aIGTincidenceNO", "aRRofIGTinObese", "aIGTrecovery", "aDMincidenceNO",
                        "aRRofDMinObese", "aRRofSSBs", "aRRofDMinElderly", "aRRofMortalityDM.over50",
                        "aRRofMortalityDM.under50")

 
## define function for sensitivity run
sensRun <- function(x){
  
  g.SimsRuns <<-list(length=nrow(x))
  
  for (i in 1:nrow(x)){
    #init <- p[i, "xyz"]
    
    auxs <- c(aInterventionYear=2020,
              aElasticity.SSB=-1.3,
              aSSBPriceChange=0,
              aIncreaseinFV=0,
              aPriceChangeFV=0,#intervention point
              aPriceChangeUH=0,#intervention point
              aUHFVCrossPrice=0.07,
              aFVPH.switch=0, #set to 1 for intervention
              aUHPH.switch=0, #set to 1 for intervention
              aOtherIntake=2100, 
              aChange.in.Bus.Fare=0,#intervention point
              aInfra.switch=0, #change to 1 for intervention
              aPAPH.Campaign=0, #change to 1 for intervention
              aMETsMVPA=4.0,
              aFatFrac=0.3,
              aFracCalDigestion=0.1,
              aAvgHeight=1.65, p[i, 1:28])
    
    stocks <- c(sNGT=1270*95,
                sIGT=1270*0.03,
                sDM=1270*0.02,
                sAvgBdWt=71.7,
                sFV=250)
    
    o<-data.frame(ode(y=stocks, times=simtime, func = model, 
                      parms=auxs, method='euler'))
    o$run <- i
    g.SimsRuns[[i]] <<- o
  }
}

#create random parameter estimates within ranges for sensitivity - latin hypercube
p <- data.frame(Latinhyper(parRange, 100)) #need to determine number of runs

#univariate sensitivity




#multivariate sensitivity
sensRun(p)

#set the time parameters for the simulation of sensitivity
START<-2020; FINISH<-2050; STEP<-1

# Create time vector
simtime <- seq(START, FINISH, by=STEP)
library(plyr)
df<-rbind.fill(g.SimsRuns)

p1 <- ggplot(df, aes(x=time, y=(sDM/(sDM+sIGT+sNGT)*100), color=run, group=run)) +
  geom_line()

