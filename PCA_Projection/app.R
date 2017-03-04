#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Principal Component Analysis"),
   
   navbarPage("", 
              
              
      tabPanel("Spectra",
        sidebarLayout(
          sidebarPanel(
            # input data
            fileInput("data", "CSV data:")
            ),
          
          
          mainPanel(
            plotOutput("plot1")
            
            
          )
        )
               
               ),
      tabPanel("Images"),
      tabPanel("Audio")
              
              
   
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  dataSet <- reactive({
    infile <- input$data
    if (is.null(infile)) {
      # User has not uploaded a file yet
      data <- faithful
    } else {
      data <- read.csv(infile$datapath)
    }
    
    meas <- data[,1:4]
    data <- data[,-c(1:4)]
    info <- list(meas = meas, data = data)
  })
  
  output$plot1 <- renderPlot({
    info <- dataSet()
    matplot(t(info$data), type = "l", xlab = "Channel", ylab = "Intensity", main = "Unprocessed Spectra")
  })
  
  output$plot2 <- 
  
}

# Run the application 
shinyApp(ui = ui, server = server)

