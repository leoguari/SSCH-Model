years <-c(1990, 1995, 2000, 2005, 2010, 2015, 2020,
          2025, 2030, 2035, 2040, 2045, 2050)
years.all <- 1990:2050
aInterventionYear <- 2020

#Baseline assumptions
#Assuming a baseline share of prev of DM in adults 2%, IGT 5%, 93% NGT
Baseline.Pop <- 1317 #in 1000s so 1.9 million is baseline
Baseline.IGT <- .07*Baseline.Pop
Baseline.DM <- .03*Baseline.Pop
Baseline.NGT <- Baseline.Pop - (Baseline.IGT + Baseline.DM)
aInitAvgWt <- 72 #men 71, women 73
aInitFVStock <- 250
per.women <-52
per.men <- 48
BMI.ratio <- 1.17

#Functions for the model
#Estimating the RR reduction in DM from PA
PaRRs <- c(1, 0.99, 0.93, 0.87, 0.76, 0.74, 0.64, 0.47)
PAHrs <- c(0, 1, 2, 4, 10, 11, 22, 60)
ff <- approxfun(PAHrs, PaRRs)

#Estimating population inflow, number of adults added
# in 1000s
people <-c(37, 37.6, 37.6, 39.6, 34.6, 35.6, 40.8, 38.8,
           34.8, 35.8, 34.8, 38.6, 38.6, 37.6, 40.6,
           33.6, 41.8, 44.8, 41.8, 42.8, 44.8, 44.8,
           48.8, 44.8, 48.8, 44.8, 47, 47, 48, 44,
           41, 45.2, 41.2, 38.2, 36.2, 35.2, 38.8,
           35.8, 33.8, 36.8, 34.8, 36.8, 34.8, 35.8,
           35.8, 35.8, 37.2, 37.2, 33.2, 33.2, 35.2,
           35.4, 32.4, 33.4, 32.4, 29.4, 33.4, 32.4, 28.4,
           29.4, 27.4)
pop.inflow <- approxfun(years.all, people)

# fraction of population of 65 - for use in RR of mortality
Fract.Over65 <- c(13, 13, 13, 13, 13, 13, 13, 13, 13,
                  13, 13, 13, 13, 13, 13, 13, 13, 13,
                  13, 13, 13, 13, 13, 13, 13, 13, 13,
                  13, 13, 13, 13, 13, 14, 14, 14, 15,
                  15, 16, 16, 16, 17, 17, 18, 18, 18,
                  19, 19, 20, 20, 20, 21, 21, 21, 21,
                  22, 22, 22, 23, 23, 24, 24)
over.65 <- approxfun(years.all, Fract.Over65)

Fract.Over55 <- c(23.23, 23.12, 22.86, 22.79, 22.38, 22.32, 22.33,
                  22.44, 22.47, 22.62, 22.85, 22.85, 22.84, 22.92,
                  23.07, 23.14, 23.30, 23.59, 23.74, 23.87, 24.08,
                  24.19, 24.41, 24.62, 24.88, 25.08, 25.31, 25.64,
                  25.99, 26.29, 26.68, 27.11, 27.63, 28.09, 28.57,
                  29.11, 29.65, 30.09, 30.56, 31.02, 31.46, 31.84,
                  32.10, 32.40, 32.64, 33.11, 33.50, 33.93, 34.46,
                  34.98, 35.52, 36.04, 36.61, 37.16, 37.77, 38.38,
                  39.00, 39.64, 40.31, 41.00, 41.64)
over.55 <- approxfun(years.all, Fract.Over55)

##Estimating obesity prevalence from average population BMI - NOT USING THIS ANYMORE
# BMI <- c(20:35)
# Ob.prev <- c(0, 0, 0, 0.03, 0.09, 0.1495, 0.2097, 0.2699, 0.3301, 0.3903,
#              0.4505, 0.5107, 0.5709, 0.6311, 0.6913, 0.7515)
# Obesity.fraction.calc <- approxfun(BMI, Ob.prev)

#estimating the number of unhealthy calories per year
UHCalories <- c(199.9, 204, 208.1, 212.4, 216.7, 221.1,
                225.6, 230.2, 234.9, 239.7, 244.6, 249.6,
                254.7, 259.9, 265.2, 270.6, 276.1, 281.8,
                287.5, 293.4, 300.1, 305.1, 309.7, 312.8, 
                317.7, 321.8, 327.8, 333.9, 340.6, 346.8,
                353.7, 360.8, 368, 375.3, 382.8, 390.5, 398.3,
                406.3, 414.4, 422.7, 431.1, 439.8, 448.6, 457.5,
                466.7, 476, 485.5, 495.2, 505.2, 515.3, 525.6,
                536.1, 546.8, 557.7, 568.9, 580.3, 591.9, 603.7,
                615.8,  628.1, 640.7)
ffUHCalories <- approxfun(years.all, UHCalories)

#estimating calories from other food sources
OtherIntake <- c(1914, 1952, 1982, 2030, 2079, 2101, 2131, 2173, 2221,
                 2263, 2302, 2341, 2380)
ffOtherIntake <- approxfun(years, OtherIntake)

#exports of Fruits and Vegetables from Jamaica Agriculture database
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
ffFVExport <- approxfun(years.all, FVExport.full)

#WPP estimates of total population from 2017
Total.Pop.All.Ages <- c(2424, 2537, 2657, 2745, 2817, 2872, 2913, 2934, 2933, 2908, 2858, 2789, 2704) #WPP 2017
ffTotalPopulation <- approxfun(years, Total.Pop.All.Ages)

#estimates of Fruit and Vegetable imports from Jamaica Agriculture database
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
ffTotalkgImports <- approxfun(years.all, TotalkgImports.full)

#estimates of total fruit and vegetable production
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
ffInitialFVProduction <- approxfun(years.all, InitialFVProduction.Full)
## baseline SSB consumption
SSB.trend <- c(1.5, 2.0, 2.5, 3.5)
ffSSB <- approxfun(c(1990, 2005, 2020, 2050), SSB.trend)


## PHYSICAL ACTIVITY
#chart for fraction of people moving from car to bus
# bus.to.car <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, -2, -2, -2, -3, -3,
#                 -3, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -3, -2, -1,
#                 -1, -1, 0, 1, 1, 2, 3, 3, 3, 4, 5, 5, 5, 6, 7, 8, 8, 9, 9,
#                 9, 10, 10, 11, 11, 13, 13, 13, 14, 14, 15, 16)
# ffbus.to.car <- approxfun(years.all, bus.to.car)
# #Physical activity - function for bus use contributing to minutes in MVPA
# Bus.Minutes.Function <- c(0, 10)
# bus.users <- c(0, 100)
# ffBusMinutes.calc <- approxfun(bus.users, Bus.Minutes.Function, yleft=0)

##Occupational MVPA decline function
#assuming an annualized reduction in occupational PA of 1.2% from Ng and Popkin and initial occupational MVPA of 50 min per day
Initial.Work <- 75
Work.MVPA.Function <- c(Initial.Work, 32.31)
ff.work.mvpa <- approxfun(c(1990, 2050), Work.MVPA.Function)
ff.work.mvpa.stable <- 75

##Travel MVPA decline function
Initial.Travel <- 17.5
Travel.MVPA.Function <- c(Initial.Travel, 7.5)
ff.travel.mvpa <- approxfun(c(1990, 2050), Travel.MVPA.Function)
ff.travel.mvpa.stable <- 17.5

#Leisure MVPA decline function
Initial.Leisure <- 5
Leisure.MVPA.Function <- c(Initial.Leisure, 12.9)
ff.leisure.mvpa <- approxfun(c(1990, 2050), Leisure.MVPA.Function)
ff.leisure.mvpa.stable <- 5

#Domestic MVPA decline function
Initial.Domestic <- 25
Domestic.MVPA.Function <- c(Initial.Domestic, 8.5)
ff.domestic.mvpa <- approxfun(c(1990, 2050), Domestic.MVPA.Function)
ff.domestic.mvpa.stable <- 25

## Effect of LTPA on diabetes onset function
LTPA.RR <- c(1, 0.99, 0.93, 0.87, 0.76, 0.74, 0.64, 0.47)
minutes.LT <- c(0, 1, 2, 4, 10, 11, 22, 60)

ff.LTPA.RR <- approxfun(minutes.LT, LTPA.RR)

