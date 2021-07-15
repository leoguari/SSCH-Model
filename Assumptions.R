# load necessary packages #
library(deSolve)
library(ggplot2)
library(FME)
library(plyr)
library(grid)
library(gridExtra)

years <-c(1990, 1995, 2000, 2005, 2010, 2015, 2020,
          2025, 2030, 2035, 2040, 2045, 2050)
years.all <- 1990:2050
aInterventionYear <- 2020

#Baseline assumptions
#Assuming a baseline share of prev of DM in adults 2%, IGT 5%, 93% NGT
Baseline.Pop <- 1317 #in 1000s so 1.9 million is baseline
Baseline.IGT <- .07*Baseline.Pop
Baseline.DM <- .035*Baseline.Pop
Baseline.NGT <- Baseline.Pop - (Baseline.IGT + Baseline.DM)
aInitFVStock <- 250
aInitAvgWt.M <- 70
aInitAvgWt.W <- 71
per.women <-52
per.men <- 48
BMI.ratio <- 1.17

# average age from WPP
age.m <- c(40.7, 41.0, 41.9, 42.5, 42.8, 42.9, 43.4, 44.4, 45.5, 46.5, 47.5, 48.5, 49.4)
aAvgAge.M <- approxfun(years, age.m)

age.w <- c(41.5, 41.6, 42.2, 43.0, 43.3, 43.5, 44.1, 45.2, 46.5, 47.7, 48.8, 49.9, 51.0)
aAvgAge.W <-  approxfun(years, age.w)

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

UHCalories <- c(239.88, 244.8, 249.72, 254.88, 260.04, 265.32, 270.72,
                276.24, 281.88, 287.64, 293.52, 299.52, 305.64, 311.88,
                318.24, 324.72, 331.32, 338.16, 345, 352.08, 360.12,
                366.12, 371.64, 375.36, 381.24, 386.16, 393.36, 400.68,
                408.72, 416.16, 424.44, 432.96, 441.6, 450.36, 459.36, 468.6,
                477.96, 487.56, 497.28, 507.24, 517.32, 527.76, 538.32, 549,
                560.04, 571.2, 582.6, 594.24, 606.24, 618.36, 630.72, 643.32,
                656.16, 669.24, 682.68, 696.36, 710.28, 724.44, 738.96, 753.72,
                768.84)
ffUHCalories <- approxfun(years.all, UHCalories)
replace.UPF.switch <- 0

UPF.kcal <- c(rep(0, 30), 424.44, 432.96, 441.6, 450.36, 459.36, 468.6,
              477.96, 487.56, 497.28, 507.24, 517.32, 527.76, 538.32, 549,
              560.04, 571.2, 582.6, 594.24, 606.24, 618.36, 630.72, 643.32,
              656.16, 669.24, 682.68, 696.36, 710.28, 724.44, 738.96, 753.72,
              768.84)/2
UPF.to.FV.g <- approxfun(years.all, UPF.kcal)

#estimating calories from other food sources
OtherIntake <- 1550
#ffOtherIntake <- approxfun(years.all, OtherIntake)

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
SSB.trend <- c(2.0, 2.1, 2.1, 2.1, 2.1, 2.2, 2.2)
ffSSB <- approxfun(c(1990, 1995, 2000, 2005, 2010, 2015, 2050), SSB.trend)


##Occupational MVPA decline function
#assuming an annualized reduction in occupational PA of 1.2% from Ng and Popkin and initial occupational MVPA of 50 min per day
Work.MVPA.Function <- c(45, 44.3, 43.7, 43, 42.4, 41.7, 41.1,
                        40.5, 39.9, 39.3, 38.7, 38.1, 37.5,
                        37, 36.4, 35.9, 35.3, 34.8, 34.3, 33.8,
                        33.3, 32.8, 32.3, 31.8, 31.3, 30.8, 30.4,
                        29.9, 29.5, 29, 28.6, 28.2, 27.7, 27.3, 26.9,
                        26.5, 26.1, 25.7, 25.3, 25, 24.6, 24.2, 23.9,
                        23.5, 23.1, 22.8, 22.5, 22.1, 21.8, 21.5, 21.1,
                        20.8, 20.5, 20.2, 19.9, 19.6, 19.3, 19, 18.7, 18.4, 18.2)
ff.work.mvpa <- approxfun(years.all, Work.MVPA.Function)
ff.work.mvpa.stable <- 45

##Travel MVPA decline function
Travel.MVPA.Function <- c(15, 14.7, 14.5, 14.2, 14, 13.8, 13.5, 13.3, 13.1,
                          12.9, 12.6, 12.4, 12.2, 12, 11.8, 11.6, 11.4, 11.2,
                          11, 10.8, 10.6, 10.5, 10.3, 10.1, 9.9, 9.8, 9.6,
                          9.4, 9.3, 9.1, 9, 8.8, 8.7, 8.5, 8.4, 8.2, 8.1,
                          8, 7.8, 7.7, 7.6, 7.4, 7.3, 7.2, 7.1, 6.9, 6.8,
                          6.7, 6.6, 6.5, 6.4, 6.3, 6.1, 6, 5.9, 5.8, 5.7,
                          5.6, 5.5,5.5, 5.4)
ff.travel.mvpa <- approxfun(years.all, Travel.MVPA.Function)
ff.travel.mvpa.stable <- 15

#Leisure MVPA decline function
Leisure.MVPA.Function <- c(5, 5.1, 5.2, 5.3, 5.4, 5.5, 5.6, 5.7, 5.9,
                           6, 6.1, 6.2, 6.3, 6.5, 6.6, 6.7, 6.9, 7,
                           7.1, 7.3, 7.4, 7.6, 7.7, 7.9, 8, 8.2, 8.4,
                           8.5, 8.7, 8.9, 9.1, 9.2, 9.4, 9.6, 9.8,
                           10, 10.2, 10.4, 10.6, 10.8, 11, 11.3, 11.5,
                           11.7, 12, 12.2, 12.4, 12.7, 12.9, 13.2,
                           13.5, 13.7, 14, 14.3, 14.6, 14.9, 15.2, 15.5,
                           15.8, 16.1, 16.4)
ff.leisure.mvpa <- approxfun(years.all, Leisure.MVPA.Function)
ff.leisure.mvpa.stable <- 5

#Domestic MVPA decline function
Domestic.MVPA.Function <- c(25, 24.6, 24.2, 23.9, 23.5, 23.1,
                            22.8, 22.4, 22.1, 21.7, 21.4, 21.1,
                            20.8, 20.4, 20.1, 19.8, 19.5, 19.2,
                            18.9, 18.6, 18.3, 18.1, 17.8, 17.5,
                            17.2, 17, 16.7, 16.5, 16.2, 16, 15.7,
                            15.5, 15.2, 15, 14.8, 14.5, 14.3, 14.1,
                            13.9, 13.7, 13.5, 13.3, 13.1, 12.9, 12.7,
                            12.5, 12.3, 12.1, 11.9, 11.7, 11.5, 11.4,
                            11.2, 11, 10.8, 10.7, 10.5, 10.3, 10.2, 10, 9.9)
ff.domestic.mvpa <- approxfun(years.all, Domestic.MVPA.Function)
ff.domestic.mvpa.stable <- 25

## Effect of LTPA on diabetes onset function
LTPA.RR <- c(1, 0.99, 0.93, 0.87, 0.76, 0.74, 0.64, 0.47)
minutes.LT <- c(0, 1, 2, 4, 10, 11, 22, 60)

ff.LTPA.RR <- approxfun(minutes.LT, LTPA.RR)

## Total PAL calculations and assumptions

Total.minutes <- 1440
Sleep.time <- 8*60
LightPA.time <- 6*60
SleepMets <- Sleep.time*0.95
Sedentary.switch <- 0
hours.LPA.increase <- 4

#healthcare intervention effect sizes
# percentage point increase in remission from pre-diabetes, evidence derived from DPP
IGT.effect <- .5

# % reduction in RR of mortality by age group
RR.hc.over55 <- .0625
RR.hc.under55 <- .0625 # HR of 0.75 from meta-anlysis of in-person self-management education doi: 10.1007/s12020-016-1168-2


