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
                 sDM=Baseline.DM)
    
    auxs    <- c(aAdultPopGrowth = 200, 
                 aMortalityNGTrate = 7.6, ## per 1000 adults
                 aIGTincidenceNO = 1.2, ## baseline incidence rate of IGT in people without obesity as %
                 aDMincidenceNO = input$incDM, ## 7, ## percentage from IGT --
                 aIGTrecovery = 10, ## rate of recovery from IGT to NGT, percentage
                 aRRofIGTinObese = 1.5,
                 aRRofDMinObese = 9.9,
                 aRRofMortalityDM.Under50 = 3.5,
                 aRRofMortalityDM.Over50 = 1.5,
                 aFractOver50=over.50, # curve taken from assumptions, UNPoP
                 aRRofDMElderly= 1.5, #Over 65
                 aObesefractNGT=20, ## percentage
                 aObesefractIGT=60, ## percentage
                 aFractOver65=over.65) ## curve taken from assumptions, UNPoP Div
    
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


