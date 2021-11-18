### System Science for Caribbean Health Model of Diabetes ###
source("Assumptions.R")


model <- function(time, stocks, auxs){
  
  with(as.list(c(stocks, auxs)),{
  #  browser()
    # simple function implement an intervention year
    
  intervention.year <- function(x){
      x.time <- c(rep(0, length(START:(aInterventionYear-1))), rep(x, length(aInterventionYear:FINISH)))
    }
    
    #estimate prevalence and incidence
    dmprev <- sDM*100/(sDM + spreDM + sNG)
    ngprev <- sNG*100/(sDM + spreDM + sNG)
    predmprev <- spreDM*100/(sDM + spreDM + sNG)
  
    #SSB calories
    ## function for calculating SSB price change from intervention start date
    SSB.price <- intervention.year(aSSBPriceChange)
    ffSSB.price.change <- approxfun(START:FINISH, SSB.price)
    
    aEffectSSBPriceChange <- (aElasticity.SSB + aEffectSSB.Campaign)*ffSSB.price.change(time)
    aAvgSSBConsumption <- ffSSB(time) + (aEffectSSBPriceChange/100)
    aCaloriesSSB <- aAvgSSBConsumption*aSSBperUnitCal
     #Food calories
    
    #currently modeling an increase in price for FV - if a decrease needed, then needs to be (-)
    Price.change.FV <- intervention.year(aPriceChangeFV)
    ffprice.change.FV <- approxfun(START:FINISH, Price.change.FV)
    
    aEffectFVPrice <- ffprice.change.FV(time)*aElasFVPrice
    
    Price.change.UH <- intervention.year(aPriceChangeUH)
    ffprice.change.UH <- approxfun(START:FINISH, Price.change.UH)
    
    aEffectUHPrice <- ffprice.change.UH(time) * aUHFVCrossPrice
    
     #fruit and veg consumption in g/day
    FVPH <- intervention.year(aFVPH.switch)
    ffFV.PH <- approxfun(START:FINISH, FVPH)
    
    replace.UPF.w.FV <- intervention.year(replace.UPF.switch)
    ff.UPF.FV <- approxfun(START:FINISH, replace.UPF.w.FV)
    
    intake.FV <- intervention.year(FV.intake.switch)
    ffintake.FV <- approxfun(START:FINISH, intake.FV)
    
    Labels <- intervention.year(aLabels.switch)
    ffLabels <- approxfun(START:FINISH, Labels)
    
     DailyFFV <- aInitFVIntake + (aInitFVIntake * aEffectFVPrice/100) + 
        (aInitFVIntake*aEffectUHPrice/100) + aEffectFVPH*ffFV.PH(time) + 
        ff.UPF.FV(time)*UPF.to.FV.g(time) + ffintake.FV(time)*FV.intake.increase + 
       ffLabels(time)*(aInitFVIntake*0.13)
  
    #aFVkcalperday <- aCalperFV*DailyFFV
    
    UH.price.change <- intervention.year(aPriceChangeUH)
    ffUH.price.change <- approxfun(START:FINISH, UH.price.change)
    
    aEffectUHTax <- ffUH.price.change(time) * aElasUHFoods #elasticity needs to be negative
    
    
    if(ffLabels(time) == 1){
      aUHFoods <- (ffUHCalories(time) + (ffUHCalories(time)*aEffectUHTax/100))*(1-(aEffectLabels/100)) 
    }
    else
    {
      aUHFoods <- ffUHCalories(time) + (ffUHCalories(time)*aEffectUHTax/100)
    }

    if(ffLabels(time) == 1){
      aDailyIntake.Total <- (aUHFoods + OtherIntake + aCaloriesSSB)*(0.94)
    }
    else{
      aDailyIntake.Total <- aUHFoods + OtherIntake + aCaloriesSSB
    }
     
     
    red.intake <- intervention.year(tot.kcal.intake.switch)
    ffred.intake <- approxfun(START:FINISH, red.intake)
    
    if(ffred.intake(time) == 1) {
      aDailyIntake.Total <- aDailyIntake.Total*intake.reduction
    }
     else{
       aDailyIntake.Total <- aDailyIntake.Total
     }
    
    aDailyIntake.W <- aDailyIntake.Total/((per.women/100) + (per.men/100)*Ratio.Intake)
    aDailyIntake.M <- aDailyIntake.W*Ratio.Intake
    
    #Physical activity
    aWorkMVPA <- ff.work.mvpa(time)
    aTravelMVPA <- ff.travel.mvpa(time)
    aLTMVPA <- ff.leisure.mvpa(time)
    aDomMVPA <- ff.domestic.mvpa(time)
    
    infra.change <- intervention.year(aInfra.switch)
    ffinfra.change <- approxfun(START:FINISH, infra.change)
    
    public.transport <- intervention.year(aPublic.switch)
    ffpublic.transport <- approxfun(START:FINISH, public.transport)
    
    aActiveTransport <- aTravelMVPA + (aTravelMVPA*aEffectInfra/100)*ffinfra.change(time) + 
      ffpublic.transport(time)*aPublic.Transport
    aLTMVPA.with.infra <- aLTMVPA + aLTMVPA*(aEffectInfra/100)*ffinfra.change(time)
    aTotalMVPA.noPH <- aActiveTransport + aWorkMVPA + aLTMVPA.with.infra + aDomMVPA
    
    PA.campaign <- intervention.year(aPAPH.Campaign)
    ffPA.campaign <- approxfun(START:FINISH, PA.campaign)
    
    aTotalMVPA.pre <- aTotalMVPA.noPH + (aRRPACampaign-1)*(aTotalMVPA.noPH)*ffPA.campaign(time)
    
    mvpa.inc <- intervention.year(MVPA.switch)
    ffmvpa <- approxfun(START:FINISH, mvpa.inc)
    
    aTotalMVPA <- aTotalMVPA.pre + ffmvpa(time)*mvpa.increase
    
    Ratio.PA <- 3.5
    
    aMVPA.W <- aTotalMVPA/(per.women/100 + per.men/100*Ratio.PA)
    aMVPA.M <- aMVPA.W * Ratio.PA
    
    aActiveMets.W <- aMVPA.W*5.2
    aActiveMets.M <- aMVPA.M*5.2
    
    #intervention to increase mvpa
    pa.double <- intervention.year(PA.switch)
    ffpa.double <- approxfun(START:FINISH, pa.double)
    
    if (ffpa.double(time) == 1) {
      aActiveMets.W <- aActiveMets.W*pa.magnitude
      aActiveMets.M <- aActiveMets.M*pa.magnitude
    }
    else
    {
      aActiveMets.W <- aActiveMets.W
      aActiveMets.M <- aActiveMets.M
    }
    
    sedentary.reduction <- intervention.year(Sedentary.switch)
    ffsedentary <- approxfun(START:FINISH, sedentary.reduction)
    
    if (ffsedentary(time) == 1) {
      LightPA.time.new <- LightPA.time + ffsedentary(time)*hours.LPA.increase*60
    }
    else {
      LightPA.time.new <- LightPA.time
    }
    
    Sedentary.T.W <- Total.minutes - aMVPA.W - Sleep.time - LightPA.time.new
    SedentaryMets.W <- Sedentary.T.W*1.5
    
    Sedentary.T.M <- Total.minutes - aMVPA.M - Sleep.time - LightPA.time.new
    SedentaryMets.M <- Sedentary.T.M*1.5
    
    LightPAMets <- LightPA.time.new*2
    
    TotalMETs.M <- SedentaryMets.M + aActiveMets.M + SleepMets + LightPAMets
    
    mvpa.women <- intervention.year(PA.women.switch)
    ffwomen <- approxfun(START:FINISH, mvpa.women)
    
    if (ffwomen(time) == 1) {
      TotalMETs.W <- TotalMETs.M
    }
    else
    {
      TotalMETs.W <- SedentaryMets.W + aActiveMets.W + SleepMets + LightPAMets
    }
    
    TotalMETs.all <- TotalMETs.W*per.women/100 + TotalMETs.M*per.men/100
    
    aPAL.W <- TotalMETs.W/Total.minutes
    aPAL.M <- TotalMETs.M/Total.minutes
    
    ##Obesity module
    obese.decrease <- intervention.year(ob.decrease.switch)
    ffobese.decrease <- approxfun(START:FINISH, obese.decrease)
    
    # Men
    aMod.BMI.M <- sAvgBdWt.M / (aAvgHeight.M^2)
    #aFatFrac.M <- ((3.05*BMI.M) - (0.028*BMI.M^2) -36.61)/100

    #Harris-Benedict
    aBasalCalories.M <- 13.397*sAvgBdWt.M + 4.799*aAvgHeight.M - 5.677*aAvgAge.M(time) + 88.362
    aTDEE.M <- aBasalCalories.M * aPAL.M
    aCaloricBalance.M <- aDailyIntake.M - aTDEE.M
    
    
    if(ffobese.decrease(time) == 1)
    {
      fChange.wt.M <- weight.loss.M
    }
    else
    {
      fChange.wt.M <- aCaloricBalance.M/8050
    }
    
    dAvg.wt_dt.M <- fChange.wt.M
    aObeseTotal.M <- 146 + -15.1*aMod.BMI.M + 0.39*aMod.BMI.M^2
    
    
    #Women
    aMod.BMI.W <- sAvgBdWt.W / (aAvgHeight.W^2)
    #aFatFrac.W <- ((3.65*BMI.W) - (0.042*BMI.W^2) - 30.81)/100
    #Harris-Benedict
    aBasalCalories.W <- 447.593 + 9.247*sAvgBdWt.W + 3.098*aAvgHeight.W - 4.33*aAvgAge.W(time)
    aTDEE.W <- aBasalCalories.W * aPAL.W
    aCaloricBalance.W <- aDailyIntake.W - aTDEE.W
    
     if(ffobese.decrease(time) == 1)
     {
       fChange.wt.W <- weight.loss.W
     }
    else {
      fChange.wt.W <- aCaloricBalance.W/8050
    }
    
    dAvg.wt_dt.W <- fChange.wt.W
    aObeseTotal.W <- 28.6 + -5.6*aMod.BMI.W + 0.205*aMod.BMI.W^2
    
    
    aMod.BMI <- aMod.BMI.M*per.men/100 + aMod.BMI.W*per.women/100
    
    BMI.rate <- (fChange.wt.W/aAvgHeight.W^2)*per.women/100 + (fChange.wt.M/aAvgHeight.M^2)*per.men/100
    
    #aObeseTotal <- 40.3 - 6.46*aMod.BMI + 0.222*aMod.BMI^2
    
    obese.freeze <- intervention.year(ob.freeze.switch)
    ffobese.freeze <- approxfun(START:FINISH, obese.freeze)
    
    if(ffobese.freeze(time) == 1){
      aObeseTotal <- obese.year.level
    }
    else {
      aObeseTotal <- 40.3 - 6.46*aMod.BMI + 0.222*aMod.BMI^2
    }
    ## Core Model
    # NG
    # Inflow of adult population
    fAdultGrowth <- pop.inflow(time)
    
    # Outflow NG mortality
    # Obese fractions
    obese.NG <- aObeseTotal/(ngprev/100 + predmprev/100*aRRofpreDMinObese + dmprev/100*aRRofDMinObese)
    obese.preDM <- obese.NG*aRRofpreDMinObese*100/(aRRofpreDMinObese*obese.NG + (100 - obese.NG))
    obese.DM <- obese.preDM*aRRofDMinObese*100/(aRRofDMinObese*obese.preDM + (100 - obese.preDM))
    
    #estimate prevalence and incidence
    under.55 <- 100-over.55(time)
    over.55ng <- over.55(time)/(dmprev/100*aRRofDMinElderly + predmprev/100*aRRofpreDMinElderly + ngprev/100)
    over.55predm <- over.55ng*aRRofpreDMinElderly*100/(aRRofpreDMinElderly*over.55ng + (100 - over.55ng))
    over.55dm <- over.55predm*aRRofDMinElderly*100/(aRRofDMinElderly*over.55predm + (100 - over.55predm))
    under.55ng <- (100-over.55ng)
    under.55predm <- (100-over.55predm)
    under.55dm <- (100-over.55dm)
    
    #under 55 deaths
    dmprev.under55 <- (sDM*under.55dm)*100 / ((sDM + spreDM + sNG)*under.55)
    
    aMortalityNGrate.under55 <- aMortalityTotalrate.under55/ 
                                      ((100 - dmprev.under55)/100 + aRRofMortalityDM.under55 * dmprev.under55/100)
    
    #over 55 deaths
    dmprev.over55 <- (sDM*over.55dm)*100 / ((sDM + spreDM + sNG)*over.55(time))
    
    aMortalityNGrate.over55 <- aMortalityTotalrate.over55/ 
                                    ((100 - dmprev.over55)/100 + aRRofMortalityDM.over55 * dmprev.over55/100)
    
    fNGMortality <- (sNG*(aMortalityNGrate.under55/1000)*(under.55ng/100)) +
      (sNG*(aMortalityNGrate.over55/1000)*(over.55ng/100))
      
    # Flow of new cases to preDM/IFG
    EffectObesityPredm <- ((1 - obese.NG/100) + (obese.NG/100*aRRofpreDMinObese)) / 
                             ((1 - obese.NG.init/100) + (obese.NG.init/100*aRRofpreDMinObese))
    
    EffectAgingPredm <- ((1 - over.55ng/100) + (over.55ng/100*aRRofpreDMinElderly)) / 
                                ((1 - over.55ng.init/100) + (over.55ng.init/100*aRRofpreDMinElderly))
    
    ##Relative reduction in preDM incidence rate from DPP style intervention - 50%
    predm.prev <- intervention.year(predm.switch)
    predm.reduction <- approxfun(START:FINISH, predm.prev)
    
    if(predm.reduction(time) == 1){
      preDMonsetrate <- apreDMincidence*EffectObesityPredm*EffectAgingPredm*predm.red.size
    }
    else{
      preDMonsetrate <- apreDMincidence*EffectObesityPredm*EffectAgingPredm
    }
    
    fpreDMOnset <- preDMonsetrate/1000*sNG
    
    # Inflow of preDM recovery
    
    if (BMI.rate >= 0) {
      wt.red.recovery <- 1
      } 
    else {
      wt.red.recovery <- 1 + (0.43*(-BMI.rate)*obese.preDM/100)
      }
   
      fpreDMRecovery <- (apreDMrecovery*wt.red.recovery/100)*spreDM
    
    # NG stock equation
    dNG_dt <- fAdultGrowth - fNGMortality - fpreDMOnset + fpreDMRecovery 
    
    # preDM
    # Outflow preDM mortality
  
    fpreDMMortality <- (spreDM*(aMortalityNGrate.under55/1000)*(under.55predm/100)) +
      (spreDM*(aMortalityNGrate.over55/1000)*(over.55predm/100))

    # Outflow DM Onset
    EffectObesityDM <- ((1 - obese.preDM/100) + (obese.preDM/100*aRRofDMinObese)) / 
                          ((1 - obese.preDM.init/100) + (obese.preDM.init/100*aRRofDMinObese))
    
    EffectAgingDM <- ((1 - over.55predm/100) + (over.55predm/100*aRRofDMinElderly)) / 
                      ((1 - over.55predm.init/100) + (over.55predm.init/100*aRRofDMinElderly))
    
    METs.per.wk.init <- ((aTotalMETs.init/60)*7)
    METs.per.wk <- ((TotalMETs.all/60)*7)
    
    EffectPADM <- 1 - ((METs.per.wk/10 - METs.per.wk.init/10) * PA.RR/100)
    
    EffectFVDM <- 1 - (DailyFFV - aInitFVIntake)/200 * 0.02
    
    EffectSSBDM <- 1 + (aAvgSSBConsumption - aAvgSSBConsumption.init) * aRRofSSBs/100 
    
    ##DPP type intervention to reduce incidence
    dpp.effect <- intervention.year(dpp.switch)
    rr.inc <- approxfun(START:FINISH, dpp.effect)
    
    if (rr.inc(time)==1){
      DMOnsetRate <- aDMincidence * EffectObesityDM * EffectAgingDM * EffectFVDM * EffectSSBDM*prevention.reduction * EffectPADM
    }
    else{
      DMOnsetRate <- aDMincidence * EffectObesityDM * EffectAgingDM * EffectFVDM * EffectSSBDM * EffectPADM
    }
    
    fDMOnset <- DMOnsetRate/1000*spreDM
     
    # preDM Stock equation
    dpreDM_dt <- fpreDMOnset - fpreDMRecovery - fpreDMMortality - fDMOnset
    
    # DM
    # Outflow DM mortality
    hc.prep <- intervention.year(hc.switch)
    hc.intervention <- approxfun(START:FINISH, hc.prep)
    
    hc.rr.mortality.under55 <- aRRofMortalityDM.under55 - (hc.intervention(time)*RR.hc.under55*aRRofMortalityDM.under55)
    hc.rr.mortality.over55 <- aRRofMortalityDM.over55 - (hc.intervention(time)*RR.hc.over55*aRRofMortalityDM.over55)

    fDMMortality <- (sDM*(under.55dm/100)*hc.rr.mortality.under55*(aMortalityNGrate.under55/1000)) +
      (sDM*hc.rr.mortality.over55*(aMortalityNGrate.over55/1000)*(over.55dm/100))
   
    # DM Stock equation
    dDM_dt <- fDMOnset - fDMMortality
    
    dminc <- fDMOnset
    
    
    # All the results for the time step
    ans <- list(c(dNG_dt, dpreDM_dt, dDM_dt, dAvg.wt_dt.M, dAvg.wt_dt.W),
                NGOnset=fAdultGrowth,
                NGMortality=fNGMortality,
                NGNetFlow=dNG_dt,
                preDMOnset=fpreDMOnset,
                preDMMortality=fpreDMMortality,
                preDMRecovery=fpreDMRecovery,
                preDMNetFlow=dpreDM_dt,
                DMOnset=fDMOnset,
                DMMortality=fDMMortality,
                DMNetFlow=dDM_dt,
                BWtAvg.M=dAvg.wt_dt.M,
                BWtAvg.W=dAvg.wt_dt.W,
                Obesity=aObeseTotal,
                BMI.W=aMod.BMI.W,
                BMI.M=aMod.BMI.M,
                dmprev=dmprev,
                dminc=dminc,
                aDailyIntake.Total=aDailyIntake.Total)
    
  })
}



  
