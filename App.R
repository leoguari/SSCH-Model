## Set up Shiny interface
source("SSCHMod.R")

library(shiny)

dm_options <- c("Diabetes Incidence", "Diabetes Mortality")

## set up server file
server <- function(input, output) {
  output$distPlot <- renderPlot({
    ##hist(rnorm(input$obs), col = 'darkgray', border = 'white')
    
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
    
    auxs    <- c(aEffectSSBPriceChange=0, #SSB calories
                 aElasticity.SSB=-1.3,
                 aEffectSSB.Campaign=-0.5,
                 aEffectSSB.Counter=2,
                 aSSBPriceChange=10,
                 aAvgSSBConsumption=3.02,
                 aSSBperUnitCal=140,
                 aSSB.init=3.1, #change this to a distribution
                 
                 #Fruits and Vegetables
                 aIncreaseinFV=0,
                 aImportsTourism=60,
                 aLocalTourism=15,
                 aPriceChangeFV=0,
                 aElasFVPrice=1.65,
                 aPriceChangeUH=20,
                 aUHFVCrossPrice=0.07,
                 aInitFVIntake=40,
                 aEffectFVPH=6.2,
                 aCalperFV=1,
                 aElasUHFoods=0.725,
                 aEffectUHPH=10,
                 aOtherIntake=2000, #this needs to be updated to a curve
                 
                 #Physical activity
                 aFraction.Bus.Use=75, # this needs to be a curve
                 aElasticity.Bus.Fare=0.15,
                 aChange.in.Bus.Fare=0,
                 aWork.init=200,
                 aWork.decline=0.03,
                 aTravel.init=60,
                 aTravel.decline=0.017,
                 aLT.init=15,
                 aLT.change=0.019,
                 aEffectInfra=0.05,
                 aRRPACampaign=0.05,
                 aMETsMVPA=4.0,
                 
                 #Obesity
                 aFatFrac=0.3,
                 aFracCalDigestion=0.1,
                 aAvgHeight=1.69,
                 
                 #Core model
                 aMortalityNGTrate=7.6,
                 aIGTincidenceNO=1.2,
                 aRRofIGTinObese=1.5,
                 aIGTrecovery=10,
                 aDMincidenceNO=7,
                 aRRofDMinObese=9.9,
                 aRRofSSBs=13,
                 aRRofDMinElderly=1.5,
                 aRRofMoratlityDM.Over50=1.5,
                 aRRofMortalityDM.Under50=3.5             
    )
    browser()
    o<-data.frame(ode(y=stocks, times=simtime, func = model, 
                      parms=auxs, method='euler'))
    
    qplot(x=time,y=sDM,data=o) + geom_line()
  })
  
}

## design user interface
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = "DM_options", label = "Select Diabetes Disease Measures:", choices = dm_options),
      sliderInput("incDM", "Incidence in 1000s:", min = 1, max = 100, value = 10),
      sliderInput("mort", "All-cause mortality in 1000s:", min = 1, max = 100, value = 20)
    ),
    mainPanel(plotOutput("distPlot"))
  )
)

shinyApp(ui = ui, server = server)


