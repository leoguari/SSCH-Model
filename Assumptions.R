years <-c(1990, 1995, 2000, 2005, 2010, 2015, 2020,
          2025, 2030, 2035, 2040, 2045, 2050)
years.all <- 1990:2050

#Estimating the RR reduction in DM from PA
PaRRs <- c(1, 0.99, 0.93, 0.87, 0.76, 0.74, 0.64, 0.47)
PAHrs <- c(0,1, 2, 4, 10, 11, 22, 60)
ff <- approxfun(PAHrs, PaRRs)

#Assuming a baseline share of prev of DM in adults 2%, IGT 5%, 97% NGT
Baseline.Pop <- 1200 #in 1000s so 1.2 million is baseline
Baseline.NGT <- .93*Baseline.Pop
Baseline.IGT <-.05*Baseline.Pop
Baseline.DM <- .02*Baseline.Pop

#Estimating population inflow, number of adults added
# in 1000s
people <-c(1000,1130, 1000, 990, 1190, 1390, 1120,
           580, 360, 170, 370, 140, -140)
Total.pop <- approxfun(years, people)

# fraction of population of 65 - for use in RR of mortality
Fract.Over65 <- c(.14, .13, .14, .13, .13, .14, .15, .16, .19, .21, .23, 
                  .25, .27)
over.65 <- approxfun(years, Fract.Over65)
Fract.Over50 <- c(29.22, 28.23, 28.68, 30.40, 31.90, 34.03, 36.16,
                  38.62, 41.09, 44.31, 47.59, 51.08, 54.48)
over.50 <- approxfun(years, Fract.Over50)


##Estimating obesity prevalence from average population BMI
BMI <- c(20:35)
Ob.prev <- c(0.17, 0.17, 0.18, 0.22, 0.27, 0.3, 0.36, 0.4, 0.43, 
             0.47, 0.52, 0.59, 0.62, 0.66, 0.68, 0.7)
Obesity.fraction.calc <- approxfun(BMI, Ob.prev)

aAvgHeight <- 1.69
aInitAvgWt <- 62.8 #in kg
aFatFrac <- 0.3
aFracCalDigestion <- 0.1
#SSB Calories
aRRofSSBs <- 1.13 #13% increase in risk per unit consumed per day
aSSB.init <- 3 #average units consumed per day
aSSBperUnitCal <- 130
aElasticity.SSB <- -1.3
aEffectSSB.Campaign <- -0.5#NEED EVIDENCE FOR THIS FIGURE
aEffectSSB.Counter <- 2#NEED EVIDENCE FOR THIS FIGURE
aSSBPriceChange <- 10#percentage that should be input by the model
#Food Calories
aOtherIntake <- 2000

UHCalories <- c(200,200, 211, 233, 244, 278, 311, 333,
                 356, 356, 356, 367, 378)
aUHCalories <- approxfun(years, UHCalories)
aElasUHFoods <- 0.725
aPriceChangeUH <- 20 #percentage
aEffectUHPH <- 10 #percentage
aCalperFV <- 1 #kcal/g
aInitFVIntake <- 40 #g/day
aEffectFVPH <- 6.2 #grams per day increase consumption FV
aUHFVCrossPrice <- 0.07 #cross price elasticity from UH foods to FV
aPriceChangeFV <- 10 #percentage
aElasFVPrice <- 1.65 #assuming a price decrease
FVExport.full <- c(14000000, 14387875, 17329309, 18232364, 18250000,
               18250000, 18289920, 15154392, 16252016, 17176887,
               12402018, 15042028, 13400867, 15645654, 13753094,
               8422733, 12384329, 13168322, 10444653, 10443921,
               12557787, 12858187, 13481384, 13403537, 13797638,
               11922301, 15589345, 15954861, 15954861, 15954861,
               15954861, 15732639, 15732639, 15732639, 15732639,
               16420139, 16420139, 16642361, 16642361, 16864583,
               16864583, 16864583, 16864583, 17086806, 17086806,
               17086806, 17086806, 17086806, 17086806, 17086806,
               17086806, 17309028, 17309028, 17309028, 17531250,
               17531250, 17531250, 17531250, 17531250, 17531250,
               17753472)
aFVExport <- approxfun(years.all, FVExport.full)
aTotalPopulation <- 2800000 #NEED TO UPDATE THIS TO A YEARLY DISTRIBUTION
aImportsTourism <- 60 #percentage
aLocalTourism <- 15 #percentage
TotalkgImports.full <- c(2500000, 2897861, 2709680, 6395833, 7729167,
                     8173611, 14975153, 23684661, 29272795, 30399675,
                     37334477, 30088706, 32777478, 30088706, 35296545,
                     33330301, 36034691, 30824522, 29553210, 27366174,
                     23125787, 24016223, 23024000, 21104533, 21651381,
                     23242060,  19099595, 20618056, 20173611, 20173611,
                     20173611, 20173611, 20173611, 19284722, 19729167,
                     19729167, 19729167, 19729167, 19729167, 19729167,
                     19729167, 19729167, 19729167,  19729167, 19729167,
                     19729167,  19729167, 19729167, 19729167, 19729167,
                     19729167, 19729167, 19729167, 19729167, 19729167,
                     19729167, 19729167, 19729167, 19729167, 19729167,
                     19729167)
aTotalkgImports <- approxfun(years.all, TotalkgImports.full)
aIncreaseinFV <- 10 #percentage

InitialFVProduction.Full <- c(503976, 511753, 511753, 519531, 527309, 542865,
                          542865, 542865, 542865, 553157, 450530, 490296,
                          431579, 491473, 414790, 391707, 467802, 427305,
                          400110, 489671, 500304, 592108, 610138, 614912,
                          579092, 571441, 668501, 643976, 659531, 659531,
                          659531, 667309, 667309, 667309, 667309, 667309,
                          667309, 667309, 682865, 682865, 690642, 690642,
                          690642, 690642, 690642, 690642, 690642, 690642,
                          690642, 690642, 690642, 690642, 690642, 690642,
                          690642, 690642, 690642, 690642, 690642, 690642,
                          690642)
aInitialFVProduction <- approxfun(years.all, InitialFVProduction.Full)

# Physical activity
aMETsMVPA <- 4.0 # average METS for MVPA
aWork.init <- 200 # minutes in MVPA at work in 1990
aWork.decline <- .03 # rate of decrease of MVPA at work from Ng and Popkin
aTravel.init <- 60
aTravel.decline <- .017
aLT.init  <- 15
aLT.change <- .019
Bus.Minutes.Function <- c(0.25, 0.5, 1.00, 2, 5, 7, 10)
bus.years <- c(1990, 2000, 2010, 2020, 2030, 2040, 2050)
aBusMinutes.calc <- approxfun(bus.years, Bus.Minutes.Function)
aFraction.Bus.Use <- 75 # needs to be a curve. Right now only latest data
aElasticity.Bus.Fare <- 0.15
aChange.in.Bus.Fare <- 10