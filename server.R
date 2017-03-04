
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
  
  

  


  
  
  
  # generate histogram depending on column selected
  output$distPlot <- renderPlot({
    info <- dataSet()
    # generate bins based on input$bins from ui.R
    data <- scale(info$data, scale = info$scale, center = info$center)
    x <- data[, input$column1] 
    #x <- scale(x, scale = input$scale, center = input$center)
    
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = input$color, border = 'white', main = paste0("Histogram of ", input$column1))
  })
  
  output$dist2Plot <- renderPlot({
    info <- dataSet()
    # generate bins based on input$bins from ui.R
    data <- scale(info$data, scale = info$scale, center = info$center)
    x <- data[, input$column2] 
    #x <- scale(x, scale = input$scale, center = input$center)
    
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = input$color, border = 'white', main = paste0("Histogram of ", input$column2))
  })
  
  
  output$twodPlot <- renderPlot({
    info <- dataSet()
    data <- scale(info$data, scale = info$scale, center = info$center)
    train <- data[info$sample,]
    test <- data[-info$sample,]
    
    if (info$scale == T | info$center == T) {
      train <- scale(train, scale = info$scale, center = info$center)
      test <- scale(test, scale = attributes(train)$"scaled:scale", center = attributes(train)$"scaled:center")
    }
    
    plot(data[,input$column1], data[,input$column2])
  })
  
  
  
  
  # knn
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
    
    if (info$center == T) {
      train <- scale(train, scale = F, center = info$center)
      test <- scale(test, scale = F, center = attributes(train)$"scaled:center")
    }
    if (info$scale == T) {
    train <- scale(train, scale = info$scale, center = F)
    test <- scale(test, scale = attributes(train)$"scaled:scale", center = F)
    }
    
    
    knn1 <- class:::knn(train = train, test = test, cl = cltrain, k = input$k)
    plot(1:length(cltest), knn1, pch = 20, col = cltest,
         xlab = "Sample No", ylab = "Predicted Class")
    
  })
  
  
  
  
  
  
  
  # PCA
  
  # PCA tab
  minNcomp <- reactive({
    info <- dataSet()
    info <- dim(info$data[,-1])
    min(info)
  })
  
  output$ncompSlider <- renderUI({
    info <- dataSet()
    info <- dim(info$data)
    sliderInput("ncomp", "Number of Components", min = 1, max = minNcomp(), value = minNcomp(), step = 1, round = T)
  })
  
  output$PCAncompPlot <- renderPlot({
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
    
    plot(pca1)
  })
  
  pcaComps <- reactive({
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
    per_var <- round(100*pca1$sdev^2 / sum(pca1$sdev^2),2)
    per_var <- paste0("Comp ",1:length(per_var), ". ", per_var, "% variance")
  })
  

  
  
  output$PCAColumn1 <- renderUI({
    selectInput("PCAselect1", "X Variable:", pcaComps(), selected = pcaComps()[1])
  })
  output$PCAColumn2 <- renderUI({
    selectInput("PCAselect2", "Y Variable:", pcaComps(), selected = pcaComps()[2])
  })
  
  
  output$selectPCAColumn1 <- renderUI({
    selectInput("pcacolumn1", "X Variable:", datacolumns(), selected = datacolumns()[1])
  })
  output$selectPCAColumn2 <- renderUI({
    selectInput("pcacolumn2", "Y Variable:", datacolumns(), selected = datacolumns()[2])
  })
  
  output$PCAPlot <- renderPlot({
    
    # get chosen columns
    col1 <- input$PCAselect1
    a <- strsplit(col1, " ")[[1]][2]
    a <- strsplit(a,"")[[1]]
    a <- a[1:(length(a) - 1)]
    col1 <- paste0(a, collapse = "")
    col1 <- as.numeric(col1)
    

    
    col2 <- input$PCAselect2
    a <- strsplit(col2, " ")[[1]][2]
    a <- strsplit(a,"")[[1]]
    a <- a[1:(length(a) - 1)]
    col2 <- paste0(a, collapse = "")
    col2 <- as.numeric(col2)
   
    
    
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
    per_var <- round(100*pca1$sdev^2 / sum(pca1$sdev^2),2)
    per_var <- paste0("Comp ",1:length(per_var), ". ", per_var, "% variance")
    
    plot(pca1$x[,col1], pca1$x[,col2], xlab = per_var[col1], ylab = per_var[col2], col = as.factor(cltrain))
    
  })
  
  
  output$PCAProj <- renderPlot({

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
    
    pca1 <- prcomp(train, center = F, scale. = F)
    
    backproj <- pca1$x[,1:input$ncomp] %*% t(pca1$rotation[,1:input$ncomp])
    if (info$scale == T) {
      backproj <- scale(backproj, center = FALSE , scale=1/attributes(train)$"scaled:scale")
    }
    if (info$center == T) {
      backproj <- scale(backproj, center = -1 * attributes(train)$"scaled:center", scale=FALSE)
    }
    
    
    colnames(backproj) <- colnames(data)
    
    knn1 <- class:::knn(train = train, test = test, cl = cltrain, k = input$k)
    newdata <- pca1$r%*% 
    plot(backproj[,input$pcacolumn1], backproj[,input$pcacolumn2], pch = 20, col = as.factor(cltrain),
         xlab = input$pcacolumn1, ylab = input$pcacolumn2)
    
    
    
    
    
  })
  

  
}
)
