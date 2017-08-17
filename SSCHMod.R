### System Science for Carribean Health Model of Diabetes ###

# load necessary packages #
library(deSolve)
library(ggplot2)

model <- function(time, stocks, auxs){
  
  with(as.list(c(stocks, auxs)),{
    
    # NGT
    # Inflow of adult population
    fAdultGrowth <- aAdultPopGrowth
    
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
    fDMOnset <- ((aObesefractIGT/100)*sIGT*(aDMincidenceNO/100)*aRRofDMinObese) +
      (((100-aObesefractIGT)/100)*sNGT*aDMincidenceNO/100)
    
    # IGT Stock equation
    dIGT_dt <- fIGTOnset - fIGTRecovery - fIGTMortality - fDMOnset
    
    # DM
    # Outflow DM mortality
    fDMMortality <- aMortalityNGTrate/1000*sDM*aRRofMortalityDM
    
    # DM Stock equation
    dDM_dt <- fDMOnset - fDMMortality
    
    
    # All the results for the time step
    ans <- list(c(dNGT_dt, dIGT_dt, dDM_dt),
                NGTOnset=fAdultGrowth, 
                NGTMortality=fNGTMortality,
                NGTNetFlow=dNGT_dt,
                IGTOnset=fIGTOnset, 
                IGTMortality=fIGTMortality,
                IGTNetFlow=dIGT_dt,
                DMOnset=fDMOnset, 
                DMMortality=fDMMortality,
                DMNetFlow=dDM_dt)
    
  })
}
# Plot results
##qplot(x=time,y=sDM,data=o) + geom_line()

