
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)


shinyUI(fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  navbarPage("Overview of Classification Methods", 
             
             
             tabPanel("Data", 
                      
                      # inputs
                      # Sidebar with a slider input for number of bins 
                      sidebarLayout(
                        sidebarPanel(
                          
                          # input data
                          fileInput("data", "CSV data:"),
                          
                          # center and scale data
                          "Center and Scale Data",
                          checkboxInput("center", "Center", value = FALSE),
                          checkboxInput("scale", "Scale", value = FALSE),
                          
                          # explore data
                          htmlOutput("selectClass"),
                          htmlOutput("selectColumn1"),
                          htmlOutput("selectColumn2"),
                          sliderInput("trainpor", "Proportion of Data in Training Set",
                                      min = 0,
                                      max = 1,
                                      value = 0.5)
                        ),
                        
                        #outputs
                        # Show a plot of the generated distribution
                        mainPanel(
                          #tabsetPanel(
                          #  tabPanel("Histogram", plotOutput("distPlot")),
                          #  tabPanel("2D Plot", plotOutput("twodPlot"))
                          #),
                          #plotOutput("distPlot"),
                          #plotOutput("dist2Plot"),
                          plotOutput("boxPlot"),
                          plotOutput("twodPlot"),
                          plotOutput("twodclassPlot")
                          #plotOutput("twodPlot")
                          
                        )
                      )
             ), # end Input tab
             
             tabPanel("knn",
                      sidebarLayout(
                        sidebarPanel(
                          
                          
                          sliderInput("k", "Number of Neighbors",
                                      min = 1,
                                      max = 30,
                                      value = 1)
                        ),
                        mainPanel(
                          plotOutput("knnPlot")
                        )
                      )
                      
             ), # end knn tab
             
             
             
             
             
             tabPanel("PCA",
               sidebarLayout(
                 sidebarPanel(
                   htmlOutput("ncompSlider"),
                   htmlOutput("PCAColumn1"),
                   htmlOutput("PCAColumn2"),
                   htmlOutput("selectPCAColumn1"),
                   htmlOutput("selectPCAColumn2")
                 ),
                 mainPanel(plotOutput("PCAncompPlot"),
                           textOutput("text"),
                           plotOutput("PCAPlot"),
                           plotOutput("PCAProj"),
                           plotOutput("PCAknnplot")
                           ),
                fluid = TRUE
               )       
                      
                      
             ), # end PCA tab 
             tabPanel("PLSDA"), 
             tabPanel("CART"))
)
)
