
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {
  
  library(class)
  data <- faithful
  
  
  # get data loaded via fileInput
  dataSet <- reactive({
    infile <- input$data
    if (is.null(infile)) {
      # User has not uploaded a file yet
      data <- faithful
    } else {
      data <- read.csv(infile$datapath, row.names = 1)
    }
    
    
    
    sample <- 1:length(data[,1])
    sample <- sample(sample, length(data[,1]))
    
    
    
    pro <- input$trainpor
    pro <- pro * length(data[,1])
    pro <- round(pro)
    sample <- sample[1:pro]
    train <- data[sample,]
    test <- data[-sample,]
    
    scale <- input$scale
    center <- input$center
    
    info <- list(data = data, sample = sample, train = train, test = test, scale = scale, center = center)
    return(info)
  })
  
  
  
  
  # get column names
  datacolumns <- reactive({
    colnames(dataSet()$data)
  })
  
  # output column names for selection
  
  output$selectColumn1 <- renderUI({
    selectInput("column1", "X Variable:", datacolumns(), selected = 1)
  })
  output$selectColumn2 <- renderUI({
    selectInput("column2", "Y Variable:", datacolumns(), selected = 2)
  })
  
  output$selectClass <- renderUI({
    selectInput("columnclass", "Class Variable:", datacolumns(), selected = 2)
  })
  
  
  # PCA tab
  minNcomp <- reactive({
    info <- dataSet()
    info <- dim(info$data)
    min(info)
  })
  
  output$ncompSlider <- renderUI({
    info <- dataSet()
    info <- dim(info$data)
    sliderInput("ncomp", "Number of Components", min = 1, max = minNcomp(), value = 1)
  })
  
  output$PCAColumn1 <- renderUI({
    selectInput("PCAcolumn1", "X Variable:", datacolumns(), selected = 1)
  })
  output$PCAColumn2 <- renderUI({
    selectInput("PCAcolumn2", "Y Variable:", datacolumns(), selected = 2)
  })
  
  
  
  # generate histogram depending on column selected
  output$distPlot <- renderPlot({
    info <- dataSet()
    # generate bins based on input$bins from ui.R
    data <- scale(info$data, scale = info$scale, center = info$center)
    x <- data[, input$column1] 
    #x <- scale(x, scale = input$scale, center = input$center)
    
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = input$color, border = 'white', main = "Histogram of Variable X")
  })
  
  output$dist2Plot <- renderPlot({
    info <- dataSet()
    # generate bins based on input$bins from ui.R
    data <- scale(info$data, scale = info$scale, center = info$center)
    x <- data[, input$column2] 
    #x <- scale(x, scale = input$scale, center = input$center)
    
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = input$color, border = 'white', main = "Histogram of Variable Y")
  })
  
  
  output$twodPlot <- renderPlot({
    info <- dataSet()
    
    train <- data[info$sample,]
    test <- data[-info$sample,]
    
    train <- scale(train, scale = info$scale, center = info$center)
    test <- scale(test, scale = attributes(train)$"scaled:scale", center = attributes(train)$"scaled:center")
    
    
    plot(train[,input$column1], train[,input$column2], pch = 20, col = 1, 
         xlim = c(min(data[,input$column1]), max(data[,input$column1])), ylim = c(min(data[,input$column2]), max(data[,input$column2])),
         xlab = input$column1, ylab = input$column2)
    points(test[,input$column1], test[,input$column2], pch = 20, col = 2)
  })
  
  output$knnPlot <- renderPlot({
    info <- dataSet()
    data <- info$data
    sample <- info$sample
    train <- info$train
    test <- info$test
    
    nam <- 1:length(info$data[1,])
    names(nam) <- colnames(info$data)
    cl1 <- nam[input$columnclass]
    
    
    cl <- data[,cl1]
    cltrain <- cl[sample]
    cltest <- cl[-sample]
    data <- data[,-(cl1)]
    
    
    train <- data[sample,]
    test <- data[-sample,]
    
    train <- scale(train, scale = info$scale, center = info$center)
    test <- scale(test, scale = attributes(train)$"scaled:scale", center = attributes(train)$"scaled:center")
    
    knn1 <- class:::knn(train = train, test = test, cl = cltrain, k = input$k)
    plot(1:length(cltest), knn1, pch = 20, col = cltest,
         xlab = "Sample No", ylab = "Predicted Class")
    
  })
  
  
  output$PCAPlot <- renderPlot({
    info <- dataSet()
    data <- info$data
    sample <- info$sample
    train <- info$train
    test <- info$test
    
    nam <- 1:length(info$data[1,])
    names(nam) <- colnames(info$data)
    cl1 <- nam[input$columnclass]
    
    
    cl <- data[,cl1]
    cltrain <- cl[sample]
    cltest <- cl[-sample]
    data <- data[,-(cl1)]
    
    
    train <- data[sample,]
    test <- data[-sample,]
    
    if (info$scale == T | info$center == T) {
    train <- scale(train, scale = info$scale, center = info$center)
    test <- scale(test, scale = attributes(train)$"scaled:scale", center = attributes(train)$"scaled:center")
    }
    
    pca1 <- prcomp(train)
    
    
    
  })
  
}
)
