years <-c(1990, 1995, 2000, 2005, 2010, 2015, 2020,
          2025, 2030, 2035, 2040, 2045, 2050)
years.all <- 1990:2050
aInterventionYear <- 2020

#Baseline assumptions
#Assuming a baseline share of prev of DM in adults 2%, IGT 5%, 93% NGT
Baseline.Pop <- 1270 #in 1000s so 1.9 million is baseline
Baseline.NGT <- .95*Baseline.Pop
Baseline.IGT <-.03*Baseline.Pop
Baseline.DM <- .02*Baseline.Pop
aInitAvgWt <- 71.7
aInitFVStock <- 250

#Functions for the model
#Estimating the RR reduction in DM from PA
PaRRs <- c(1, 0.99, 0.93, 0.87, 0.76, 0.74, 0.64, 0.47)
PAHrs <- c(0, 1, 2, 4, 10, 11, 22, 60)
ff <- approxfun(PAHrs, PaRRs)

#Estimating population inflow, number of adults added
# in 1000s
people <-c(104, 109, 111, 118, 123, 128, 133, 138, 140, 148,
           153, 155, 157, 160, 162, 162, 165, 165, 165, 165,
           165, 167, 167, 170, 172, 172, 172, 175, 175, 175,
           175, 175, 172, 170, 167, 167, 165, 165, 162, 162,
           162, 162, 160, 160, 155, 150, 150, 145, 140, 135,
           131, 126, 121, 111, 99,84, 72, 57, 43, -1, -6)
Total.pop <- approxfun(years.all, people)

# fraction of population of 65 - for use in RR of mortality
Fract.Over65 <- c(13, 13, 13, 13, 13, 13, 13, 15, 
                  17, 19, 21, 22, 24)
over.65 <- approxfun(years, Fract.Over65)

Fract.Over50 <- c(29, 28, 29, 31, 32, 33, 35,
                  37, 39, 42, 45, 48, 51)
over.50 <- approxfun(years, Fract.Over50)

##Estimating obesity prevalence from average population BMI
BMI <- c(20:35)
Ob.prev <- c(0, 0, 0, 0.03, 0.09, 0.1495, 0.2097, 0.2699, 0.3301, 0.3903,
             0.4505, 0.5107, 0.5709, 0.6311, 0.6913, 0.7515)
Obesity.fraction.calc <- approxfun(BMI, Ob.prev)

#estimating the number of unhealthy calories per year
UHCalories <- c(200,200, 211, 233, 244, 278, 311, 333,
                 356, 356, 356, 367, 378)
ffUHCalories <- approxfun(years, UHCalories)

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

#chart for fraction of people moving from car to bus
bus.to.car <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, -2, -2, -2, -3, -3,
                -3, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -3, -2, -1,
                -1, -1, 0, 1, 1, 2, 3, 3, 3, 4, 5, 5, 5, 6, 7, 8, 8, 9, 9,
                9, 10, 10, 11, 11, 13, 13, 13, 14, 14, 15, 16)
ffbus.to.car <- approxfun(years.all, bus.to.car)
#Physical activity - function for bus use contributing to minutes in MVPA
Bus.Minutes.Function <- c(0, 10)
bus.users <- c(0, 100)
ffBusMinutes.calc <- approxfun(bus.users, Bus.Minutes.Function, yleft=0)

##Work MVPA decline function
Work.MVPA.Function <- c(198, 196, 191, 191, 189, 180, 176, 173,
                        171, 169, 164, 162, 158, 153, 151, 149,
                        144, 138, 133, 131, 129, 122, 118, 116,
                        113, 109, 107, 106, 104, 100, 100, 100,
                        100, 98, 96, 96, 93, 93, 91, 89, 89, 87,
                        87, 84, 82, 82, 80, 78, 78, 73, 73, 71,
                        67, 62, 62, 60, 58, 56, 56, 53, 51)
ff.work.mvpa <- approxfun(years.all, Work.MVPA.Function)
##Travel MVPA decline function
Travel.MVPA.Function <- c(61, 61, 60, 60, 59, 59, 58, 57, 56, 
                          56, 55, 54, 54, 53, 52, 52, 51, 50,
                          49, 49, 48, 47, 47, 45, 44, 44, 43,
                          42, 42, 41, 40, 40, 39, 39, 38, 37,
                          36, 36, 36, 35, 35, 34, 34, 34, 32,
                          31, 31, 31, 31, 30, 29, 29, 27, 27,
                          26, 26, 25, 25, 25, 25, 24)
ff.travel.mvpa <- approxfun(years.all, Travel.MVPA.Function)

#Leisure MVPA decline function
Leisure.MVPA.Function <- c(25, 25, 25, 25, 24, 24, 24, 24, 23,
                           23, 23, 23, 22, 22, 22, 21, 21, 21,
                           20, 20, 19, 19, 19, 18, 18, 17, 17,
                           17, 16, 16, 16, 16, 16, 15, 15, 15,
                           14, 14, 14, 14, 13, 13, 13, 12, 12,
                           11, 11, 11, 10, 10, 10, 9, 9, 9, 8,
                           8, 7, 7, 6, 6, 6)
ff.leisure.mvpa <- approxfun(years.all, Leisure.MVPA.Function)
## Intervention variables, starting point of intervention 2020
SSB.price.change.input=0
SSB.price <- c(rep(0, 61-length(aInterventionYear)), rep(SSB.price.change.input, length(aInterventionYear)))
#ffSSB.price.change <- approxfun(years.all, SSB.price)



