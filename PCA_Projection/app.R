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
            fileInput("data", "CSV data:"),
            htmlOutput("PCAColumn1"),
            htmlOutput("PCAColumn2"),
            htmlOutput("DropPCs"),
            textOutput("text1")
            ),
          
          
          mainPanel(
            plotOutput("plot1"),
            plotOutput("plot2"),
            plotOutput("plot3"),
            plotOutput("plot4")
            
            
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
    
    # create color list for plotting
    color <- rainbow(n = (length(data[,1]) + length(data[,1])/10))[1:length(data[,1])]
    # organize by spectra order at in middle channel
    channel <- floor(length(data[1,])/2)
    value <- data[,channel]
    value <- cbind(value, 1:length(value))
    value2 <- value[order(value[,1]),]
    value2 <- cbind(value2, 1:length(value2[,1]))
    value2 <- value2[order(value2[,2]),]
    color <- color[value2[,3]]
    
    info <- list(meas = meas, data = data, color = color)
  })
  
  output$plot1 <- renderPlot({
    info <- dataSet()
    
    matplot(t(info$data), type = "l", xlab = "Channel", ylab = "Intensity", main = "Unprocessed Spectra", lty = 1, col = info$color)
  })
  
  
  
  
  output$plot2 <- renderPlot({
    info <- dataSet()
    
    pca1 <- prcomp(info$data, center = T, scale. = T)
    plot(pca1)
    
  })
  
  pcaComps <- reactive({
    info <- dataSet()
    pca1 <- prcomp(info$data)
    per_var <- round(100*pca1$sdev^2 / sum(pca1$sdev^2),2)
    per_var <- paste0("Comp ",1:length(per_var), ". ", per_var, "% variance")
  })
  
  
  output$PCAColumn1 <- renderUI({
    selectInput("PCA1", "X Variable:", pcaComps(), selected = pcaComps()[1])
  })
  output$PCAColumn2 <- renderUI({
    selectInput("PCA2", "Y Variable:", pcaComps(), selected = pcaComps()[2])
  })
  
  
  output$plot3 <- renderPlot({
    info <- dataSet()
    pca1 <- prcomp(info$data, center = T, scale. = T)
    
    # convert PCA1 and PCA2 to column number
    col1 <- input$PCA1
    a <- strsplit(col1, " ")[[1]][2]
    a <- strsplit(a,"")[[1]]
    a <- a[1:(length(a) - 1)]
    col1 <- paste0(a, collapse = "")
    col1 <- as.numeric(col1)
    col2 <- input$PCA2
    a <- strsplit(col2, " ")[[1]][2]
    a <- strsplit(a,"")[[1]]
    a <- a[1:(length(a) - 1)]
    col2 <- paste0(a, collapse = "")
    col2 <- as.numeric(col2)
    
    plot(pca1$x[,col1], pca1$x[,col2], pch = 20, col = info$color)
    
  })
  
  
  output$DropPCs <- renderUI({
    selectInput("dropPC", "PCs to Drop: ", pcaComps(), multiple = T, selectize = T)
  })
  
  output$plot4 <- renderPlot({
    
    # get col numbers to drop from reconstruction
    out <- NULL
    for (value in input$dropPC) {
      col1 <- value
      a <- strsplit(col1, " ")[[1]][2]
      a <- strsplit(a,"")[[1]]
      a <- a[1:(length(a) - 1)]
      col1 <- paste0(a, collapse = "")
      col1 <- as.numeric(col1)
      out <- c(out, col1)
    }
    
    info <- dataSet()
    pca1 <- prcomp(info$data, center = T, scale. = T)
    newdata <- info$data
    if (!is.null(out)) {
      newdata <- pca1$x[,-out] %*% t(pca1$rotation[,-out])
      if(pca1$scale != FALSE){
        newdata <- scale(newdata, center = FALSE , scale=1/pca1$scale)
      }
      if(pca1$center != FALSE){
        newdata <- scale(newdata, center = -1 * pca1$center, scale=FALSE)
      }
    }
    
    matplot(t(newdata), type = "l", xlab = "Channel", ylab = "Intensity", main = "Unprocessed Spectra", lty = 1, col = info$color)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

