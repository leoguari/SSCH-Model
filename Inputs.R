#Creating different input sets to be used for sensitivity analyses

#No interventions, low,  mid, high, male-mid, female-mid
aEffectSSB.Campaign=c(0, -0.5) ## may want to discard this - a tax needs a PH campaign to work
aElasticity.SSB= c(-1.3, -1.089, -1.3, -1.5) # -- data drawn from meta-analysis, Cabrera Escobar et al
#background SSB consumption seems to be decreasing
aEffectFVPH=c(0, 2.2, 4.0, 6.4) #scenario -- intervention - we could add VAT from the same paper
aEffectUHPH=c(0, 3.6, 8.9, 3.6, 3.6) #scenario -- intervention on food labeling -- from meta-analysis data on processed foods one year follow up
aEffectInfra=c(0, 0.032, 0.05, 0.032, 0.032) #scenario -- intervention study from New Orleans - no CI -- 5 percent from Auchincloss et al
aRRPACampaign=c(0, 1.25) #scenario -- intervention -- meta-analysis on mass media campaigns
aIGTrecovery=c(5, 10, 15, 10, 10) #intervention -- assumption/intervention

aElasUHFoods=c(0.882, 0.6, 0.882, 1.16) #range - Afshin & Penalvo et al
aUHFVCrossPrice=c(0, 0.07, 0.07, 0.07)  # no CI taken from study in Brazil
aElasFVPrice=c(1.2, 1.0, 1.2, 1.5) #range - Afshin & Penalvo et al

aSSB.init=c(2.5, 2.0, 2.7, 3.5) #from NCD Risk factor group for Jamaica 2010 -- possible increases but uncertainty is high

#Fruits and Vegetables
aInitFVIntake=c(300, 200, 300, 400) #range? -- assumption
aOtherIntake=c(2000, 2000, 2000, 2000) #this needs to be updated to a curve -- assumption

#Physical activity
aFraction.Bus.Use=c(50, 50, 75, 80) # this needs to be a curve -- assumption
aElasticity.Bus.Fare=0.21 #range? -- assumption

#Core model
aMortalityNGTrate=7.6 #range? -- assumption
aIGTincidenceNO=1.2 #range? -- assumption
aRRofIGTinObese=1.5 #range? -- assumption
aDMincidenceNO=10 # -- assumption
aRRofDMinObese=9.9 # -- assumption
aRRofSSBs=13 # -- assumption
aRRofDMinElderly=1.5 # -- assumption
aRRofMoratlityDM.Over50=1.5 # -- assumption
aRRofMortalityDM.Under50=3.5  # -- assumption
