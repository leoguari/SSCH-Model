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
    stocks  <- c(sNGT=1200,
                 sIGT=102,
                 sDM=52.8)
    
    auxs    <- c(aAdultPopGrowth = 200, 
                 aMortalityNGTrate = input$obs, ## 7.6, --
                 aIGTincidenceNO = 1.2, ## percentage from NGT
                 aDMincidenceNO = input$obs2, ## 7, ## percentage from IGT --
                 aIGTrecovery = 10, ## percentage
                 aRRofIGTinObese = 1.5,
                 aRRofDMinObese = 9.9,
                 aRRofMortalityDM = 1.8,
                 aRRofDMElderly= 1.5,
                 aObesefractNGT=20, ## percentage
                 aObesefractIGT=60, ## percentage
                 aElderlyfract=30) ## percentage
    
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
      sliderInput("obs", "Incidence in 1000s:", min = 1, max = 100, value = 10),
      sliderInput("obs2", "All-cause mortality in 1000s:", min = 1, max = 100, value = 20)
    ),
    mainPanel(plotOutput("distPlot"))
  )
)

shinyApp(ui = ui, server = server)


