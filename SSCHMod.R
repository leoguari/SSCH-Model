### System Science for Carribean Health Model of Diabetes ###

# load necessary packages #
library(deSolve)
library(ggplot2)

model <- function(time, stocks, auxs){
  
  with(as.list(c(stocks, auxs)),{
  #  browser()
    #SSB calories
    aEffectSSBPriceChange <- (aElasticity.SSB + aEffectSSB.Campaign + aEffectSSB.Counter)*aSSBPriceChange
    aAvgSSBConsumption <- aSSB.init + (aEffectSSBPriceChange/100)
    aCaloriesSSB <- aAvgSSBConsumption*aSSBperUnitCal
     #Food calories
  
    #inflow of Fruit and Vegetable production
    aFVProduction <- ((ffInitialFVProduction(time)*1000) + 
                        (aIncreaseinFV/100*ffInitialFVProduction(time)*1000))/ffTotalPopulation(time)
    aFVImports <- ffTotalkgImports(time)/ffTotalPopulation(time)
    fFVProduction <- (ffTotalkgImports(time) - (ffTotalkgImports(time)*aImportsTourism/100)) + 
      (aFVProduction - (aFVProduction*aLocalTourism/100))
    
    #outflow of waste and export
    fFVExport <- ffFVExport(time)/ffTotalPopulation(time) #per capita export per year
    fFVWaste <- fFVProduction * .3
    
    aEffectFVPrice <- aPriceChangeFV * aElasFVPrice
    aEffectUHPrice <- aPriceChangeUH * aUHFVCrossPrice
    
     #outflow of fruit and veg consumption in kg/capita/yr
    fFVConsumption <- aInitFVIntake + (aInitFVIntake * aEffectFVPrice/100) + 
      (aInitFVIntake*aEffectUHPrice/100) + ((aEffectFVPH/1000)*365)
    
    aFVkcalperday <- aCalperFV*(fFVConsumption/365)*1000
    aEffectUHTax <- aPriceChangeUH * aElasUHFoods
    aUHFoods <- ffUHCalories(time) + (ffUHCalories(time)*aEffectUHTax/100) - (ffUHCalories(time)*aEffectUHPH/100)
    aDailyIntake <- aUHFoods + aFVkcalperday + aOtherIntake + aCaloriesSSB
    
    #Fruit and vegetable stock equation
    dFV_dt <-  fFVProduction - fFVWaste - fFVExport - fFVConsumption
    
    #Physical activity
    aNew.Bus.Use <- aFraction.Bus.Use + (aElasticity.Bus.Fare*aChange.in.Bus.Fare)
    aBusMinutes <- ffBusMinutes.calc(aNew.Bus.Use)
    aWorkMVPA <- aWork.init - aWork.init*aWork.decline
    aTravelMVPA <- aTravel.init - aTravel.init*aTravel.decline
    aLTMVPA <- aLT.init + aLT.init*aLT.change
    aActiveTransport <- aTravelMVPA + (aTravelMVPA*aEffectInfra/100) + aBusMinutes
    aPHLTPA <- aLTMVPA + aLTMVPA*aEffectInfra/100
    aTotalMVPA <- aActiveTransport + aWorkMVPA + aPHLTPA*aRRPACampaign
    aTotalMETs <- aTotalMVPA*aMETsMVPA
    aDailyCalBurned <- (aTotalMETs*3.5*sAvgBdWt)/200
    
    ##Obesity module
    aBasalCalories <- (sAvgBdWt * (0.024 * aFatFrac + 0.102 * (1-aFatFrac)) + 0.85) * 238.7
    aCaloricBalance <- aDailyIntake - aDailyCalBurned - aBasalCalories - (aFracCalDigestion * aDailyIntake)
    aFracBalanceFat <- 1/(1 +(10.4/sAvgBdWt*aFatFrac)*(4100/9300))
    aChange.per.day <- aCaloricBalance * (aFracBalanceFat/9300 + (1-aFracBalanceFat)/4100)
    fChange.wt <- aChange.per.day * 365
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
    fNGTMortality <- aMortalityNGTrate/1000*sNGT
    
    # Outflow of new cases of IGT/IFG
    fIGTOnset <- ((aObesefractNGT/100)*sNGT*(aIGTincidenceNO/100)*aRRofIGTinObese) +
      (((100-aObesefractNGT)/100)*sNGT*aIGTincidenceNO/100)
    
    # Inflow of IGT recovery
    fIGTRecovery <- aIGTrecovery/100*sIGT
    
    # NGT stock equation
    dNGT_dt <- fAdultGrowth + fIGTRecovery - fIGTOnset - fNGTMortality
    
    # IGT
    # Outflow IGT mortality
    fIGTMortality <- aMortalityNGTrate/1000*sIGT
    
    # Outflow DM Onset
    aDMinNonObese <- ((100-aObesefractIGT)/100)*sIGT*(aDMincidenceNO/100)
    aDMinObese <- (aObesefractIGT/100)*sIGT*(aDMincidenceNO/100)*aRRofDMinObese
    aDMOnsetObesity <- aDMinObese + aDMinNonObese
    aDMOnsetPA<- aDMOnsetObesity*ff(aTotalMVPA/60*7)
    aDMOnsetSSB <- aDMOnsetPA + (aDMOnsetPA*aRRofSSBs*aAvgSSBConsumption/100)
    aDMOnsetAging <- ((over.65(time)/100)*aRRofDMinElderly*aDMOnsetSSB) +
      (((100-over.65(time))/100)*aDMOnsetSSB)
    fDMOnset <- aDMOnsetAging
     
    # IGT Stock equation
    dIGT_dt <- fIGTOnset - fIGTRecovery - fIGTMortality - fDMOnset
    
    # DM
    # Outflow DM mortality
    fDMMortality <- (((over.50(time)/100)*aRRofMoratlityDM.Over50*aMortalityNGTrate) +
                    (((100-over.50(time))/100)*aRRofMortalityDM.Under50*aMortalityNGTrate))/1000*sDM
    
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
                FVNetFlow=dFV_dt)
    
  })
}
# Plot results
#qplot(x=time,y=sDM,data=o) + geom_line()

