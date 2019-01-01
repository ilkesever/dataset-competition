#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library("clusterGeneration")
library("factoextra")
library("fpc")
library('clValid')
library(kernlab)

generetaWellSeperated <- function(numOfClusters,sepVal,numOfSamples,numOfDimensions) {
  data1 <- genRandomClust(
    numClust = numOfClusters,
    sepVal = sepVal,
    numNonNoisy = numOfDimensions,
    clustszind = 1,
    clustSizeEq =  numOfSamples / numOfClusters
  )
  data1 <- data1$datList$test_1
  return(data1)
}

getRandomData <- function(numOfSamples,numOfVariables) {
  
  x <- matrix(runif(numOfSamples * numOfVariables), numOfSamples, numOfVariables)
  return(x)
}

getExternalData <- function(path = "iris.csv", h = TRUE) {
  data = read.csv(file = path, header = h)
  
  return(data)
}

getInternalData <- function(a='datasets', b='iris'){
  library(a,character.only = TRUE)
  b <- eval(parse(text = b))
  return(b)
}

excludeColumn <- function(a) {
  return(a[, sapply(a, is.numeric)])
}

calculateKMeans <- function(a, b) {
  kmeans_obj <- kmeans(a, b)
  return(kmeans_obj$cluster)
}

calculateHierarchical <- function(dataset,linkage){
  
  d <- dist(dataset,method = "euclidean")
  hcl <- hclust(d,method = linkage)
  return(hcl)
}

findSilhoutte <- function(data,clustering){
  
  if(!require(clusterSim))
  {
    install.packages("cluster")
  }
  library(clusterSim)
  
  d <- dist(data)
  silohuetteResult <- silhouette(clustering,d)
  return(silohuetteResult)
}
spectralClustering <-
  function(data ,
           numofClusters = 2,
           for_swissroll=FALSE) {
     if (!require(kernlab))
     {
       install.packages("kernlab")
     }
      
    
    
    clusteringResult <- specc(data, numofClusters)
    
    return(ClusteringResult)
  }

spectralClustering <- function(data,numofClusters = 2) {
    
    clusteringResult <- specc(data, numofClusters)
   
    return(clusteringResult)
}
# Define UI for application that draws a histogram
ui <- fluidPage(
   
  # Project Title
  titlePanel("Ceng 574 Project"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      selectInput("datatype", label = "Select Data Set Type", 
                  choices = c("Generated" = "generated","Predefined" = "predefined","Upload a Data" = "upload"),
                  selected = "generated"),
      conditionalPanel(
        condition = "input.datatype == 'generated'",
        
        selectInput(
          "generationType",label = "Select Generated Type",
          choices = c("Well Seperated" = "seperated","Noise Data" = "noise"),selected= "seperated")
      ),
      conditionalPanel(
        condition = "input.datatype == 'predefined'",
        selectInput(
          "predefinedDataset",label = "Select Predefined Dataset",
          choices = c("USArrests" = "USArrests","Iris" = "iris"),selected= "iris")
      ),
      conditionalPanel(
        condition = "input.datatype == 'upload'",
        fileInput("fileInput", "Choose CSV File",
                  multiple = FALSE,
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")),
        checkboxInput("uploadHeader", "Header", TRUE)
      ),
      conditionalPanel(
        condition = "input.datatype=='generated' && input.generationType=='seperated'",
        numericInput(
          "wellSeperatedNumberOfClusters",
          "Number of Clusters:",
          value = 2,
          min = 1
        ),
        numericInput(
          "wellSeperatedSeperationValue",
          "Seperation Value:",
          value = 0,
          min = -0.999,
          max = 0.999
        ),
        numericInput(
          "wellSeperatedNumberOfSamples",
          "Number Of Samples:",
          value = 60,
          min = 1
        ),
        numericInput(
          "wellSeperatedNumberOfDimensions",
          "Number Of Dimensions:",
          value = 2,
          min = 2
        )
      ),
      conditionalPanel(
        condition = "input.datatype=='generated' && input.generationType=='noise'",
        numericInput(
          "randomNumberOfSamples",
          "Number Of Samples:",
          value = 60,
          min = 1
        ),
        numericInput(
          "randomNumberOfVariables",
          "Number Of Variables:",
          value = 4,
          min = 2
        )
      ),
      hr(),
      selectInput("clusteringMethod","Select Clustering Method",
                  c("K-Means" = "kmeans","Hierarchical Clustering" = "hierarchical","Spectral Clustering"="spectral") ,selected = "kmeans"
      ),
      conditionalPanel(
        condition = "input.clusteringMethod == 'kmeans'",
        numericInput(
          "kNumberOfClusters",
          "Number Of Clusters",
          value = 3,
          min = 2,
          max = 10
        )
      ),
      conditionalPanel(
        condition = "input.clusteringMethod == 'hierarchical'",
        selectInput("linkageMethod","Linkage Method",c("Complete" = "complete","Average" = "average","Single"="single")),
        selectInput("hPlotType","Plot Type",c("Show Dendogram" = "dendogram","Show Clusters" = "clusterplot")),
        
        numericInput(
          "hNumberOfClusters",
          "Number Of Clusters",
          value = 3,
          min = 2,
          max = 10
        )
      ),
      conditionalPanel(
        condition = "input.clusteringMethod == 'spectral'",
        numericInput(
          "sNumberOfClusters",
          "Number of Clusters",
          value = 2,
          min = 1
        )
      ),
      hr(),
      selectInput("validationIndex","Select Validation Index",
                  c("Silhoutte Index" = "silhoutte","Dunn's Index" = "dunn")
      ),
      actionButton("run", "Find Clusters")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      conditionalPanel(
        condition = "!is.null(output.distPlot)",
        plotOutput("distPlot")
      ),
      conditionalPanel(
        condition = "!is.null(output.dunn)",
        textOutput("dunn")
      ),
      conditionalPanel(
        condition = "!is.null(output.silhouttePlot)",
        plotOutput("silhouttePlot")
      )
      
      
    
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  values <- reactiveValues(
    datatype = NA,
    generationType = NA,
    predefinedDataset = NA,
    fileInput = NA,
    uploadHeader = NA,
    wellSeperatedNumberOfClusters = NA,
    wellSeperatedSeperationValue = NA,
    wellSeperatedNumberOfSamples=NA,
    wellSeperatedNumberOfDimensions = NA,
    randomNumberOfSamples =NA,
    randomNumberOfVariables=NA,
    kNumberOfClusters = NA,
    hNumberOfClusters = NA,
    linkageMethod = NA,
    clusteringMethod = NA,
    hPlotType = NA,
    validationIndex = NA,
    sNumberOfClusters = NA
    
  )
  changingValues <- reactiveValues(dataset = NA,changed = NA)
  
  
  observeEvent(input$run,{
    
    values$datatype <- input$datatype;
    values$generationType <- input$generationType;
    values$predefinedDataset <- input$predefinedDataset;
    values$uploadHeader <- input$uploadHeader;
    values$fileInput <- input$fileInput;
    
    
    values$wellSeperatedNumberOfClusters <- input$wellSeperatedNumberOfClusters;
    values$wellSeperatedSeperationValue <- input$wellSeperatedSeperationValue;
    values$wellSeperatedNumberOfSamples <- input$wellSeperatedNumberOfSamples;
    values$wellSeperatedNumberOfDimensions <- input$wellSeperatedNumberOfDimensions;
    
    values$randomNumberOfSamples <- input$randomNumberOfSamples;
    values$randomNumberOfVariables <- input$randomNumberOfVariables;
    values$kNumberOfClusters <- input$kNumberOfClusters;
    values$clusteringMethod <- input$clusteringMethod;
    
    values$hNumberOfClusters <- input$hNumberOfClusters;
    values$linkageMethod <- input$linkageMethod;
    values$hPlotType <- input$hPlotType;
    
    values$sNumberOfClusters <- input$sNumberOfClusters;
    values$validationIndex <- input$validationIndex;
    
    ## Controls
    
    if (values$datatype == "generated") {
      if (values$generationType == "seperated") {
        changingValues$dataset = generetaWellSeperated(
          values$wellSeperatedNumberOfClusters,
          values$wellSeperatedSeperationValue,
          values$wellSeperatedNumberOfSamples,
          values$wellSeperatedNumberOfDimensions
        )
      }
      else if (values$generationType == "noise"){
        changingValues$dataset = getRandomData(values$randomNumberOfSamples,values$randomNumberOfVariables);
        
      }
    }
    else if (values$datatype == "upload") {
      
      changingValues$dataset = excludeColumn(getExternalData(values$fileInput$datapath,values$uploadHeader))
    }else if (values$datatype == "predefined") {
      changingValues$dataset = excludeColumn(getInternalData(b=values$predefinedDataset))
    }
    
    
  })
  
  output$distPlot <- renderPlot({
    if (values$clusteringMethod == "kmeans") {
      
      
      clusteringResult = calculateKMeans(changingValues$dataset, values$kNumberOfClusters);
      
      plot(changingValues$dataset, col=clusteringResult)
    }else if (values$clusteringMethod == "hierarchical") {
      clusteringResult = calculateHierarchical(changingValues$dataset,values$linkageMethod)
      
      if (values$hPlotType == "dendogram") {
        plot(clusteringResult,cex = 0.6,hang = -1,main = "Well Seperated Data Dendogram")
        rect.hclust(clusteringResult,values$hNumberOfClusters,border = 2:5)
      }else {
        sub_grp <- cutree(clusteringResult, k = values$hNumberOfClusters)
        fviz_cluster(list(data = changingValues$dataset, cluster = sub_grp))
      }
      
      
    }
    else if (values$clusteringMethod == "spectral") {
      clusteringResult <- spectralClustering(changingValues$dataset, numofClusters = values$sNumberOfClusters)
      plot(changingValues$dataset, col=clusteringResult)
    }
    
  })
  # Silhoutte
  output$silhouttePlot <- renderPlot({
    if (values$validationIndex == "silhoutte") {
      
      if (values$clusteringMethod == "kmeans") {
        clusteringResult = calculateKMeans(changingValues$dataset, values$kNumberOfClusters);
        plot(findSilhoutte(changingValues$dataset,clusteringResult))
      }
      else if (values$clusteringMethod == "hierarchical") {
        clusteringResult = calculateHierarchical(changingValues$dataset,values$linkageMethod)
        sub_grp <- cutree(clusteringResult, k = values$hNumberOfClusters)
        
        plot(findSilhoutte(changingValues$dataset,sub_grp))
      }
      else if (values$clusteringMethod == "spectral") {
        
        clusteringResult <- spectralClustering(changingValues$dataset, numofClusters = values$sNumberOfClusters)
        plot(findSilhoutte(changingValues$dataset,clusteringResult))
      }
    }
  })

  # Dunn Index
  
  output$dunn <- renderText({
    if (values$validationIndex == "dunn") {
      
      if (values$clusteringMethod == "kmeans") {
      clusteringResult = calculateKMeans(changingValues$dataset, values$kNumberOfClusters);
      d <- dist(changingValues$dataset)
      
      indexValue = dunn(d,clusteringResult)
      paste(c("Dunn Index Value ",indexValue," (K-Means) for k Value = ",values$kNumberOfClusters),collapse = "")
      
      }else if (values$clusteringMethod == "hierarchical") {
        clusteringResult = calculateHierarchical(changingValues$dataset,values$linkageMethod);
        sub_grp <- cutree(clusteringResult, k = values$hNumberOfClusters )
        d <- dist(changingValues$dataset)
        
        indexValue = dunn(d,sub_grp)
        paste(c("Dunn Index Value ",indexValue," (Hierarchical) for k Value = ",values$hNumberOfClusters),collapse = "")
      }
      else if (values$clusteringMethod == "spectral") {
        clusteringResult <- spectralClustering(changingValues$dataset, numofClusters = values$sNumberOfClusters)
        d <- dist(changingValues$dataset)
        indexValue = dunn(d,clusteringResult)
        paste(c("Dunn Index Value ",indexValue," (Spectral) for k Value = ",values$sNumberOfClusters),collapse = "")
        
      }
    }
    
    
    
  })
  
   
}

# Run the application 
shinyApp(ui = ui, server = server)

