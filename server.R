library(shiny)
library(vcd)
library(MASS)
library(RColorBrewer)
library(dendextend)  # High- Clust
library(jsonlite)
library(datasets)
library(corrgram)
library(visdat)
library(forecast)
library(tidytext)
library(janeaustenr)
library(stringr)
library(wordcloud)
library(reshape2)
library(pls)
library(dplyr)
library(shinyjs)
library(ggplot2)
library(ggmosaic)
library(scales)
library(plotly)
library(psych)
library(DT)
library(highcharter)
library(VIM)
library(dendextend)
library(sqldf)
library(ggplot2)
library(plotly)
library(shinydashboard)
library(lubridate)
library(reshape2)
library(gridExtra)
library(gtable)
library(grid)
library(plyr)
library(qcc)
library(corrplot)
library(caret)






#####################
library(visNetwork)  # network clustering for mind maps, js api, although its not used for mind maps.....


#####--------------------------------------------------------------------importing libraries for the clustering------------------######### 
library(skmeans) #kmean clustering
library(ClusterR)#kmean clustering 
library(NbClust) #kmean clustering
library(kernlab)
library(factoextra)#kmean clustering - Get clust_tendency() assees hopkins test
library(vegan)##For silhoutte()





#library() #EM Clustering for Gaussian process
#library() #Agglomerative clustering
#library() #Density-Based Spatial Clustering of Applications with 

####------------------------------------------------------------Pre load the required Data-------------------------------------------#####




###-------------------------------------------------------------Console output function work-------------------------------------------------    

# longfunc <- function(){
#   message("I'm working....")
#   Sys.sleep(1)
#   message("I'm still working....")
#   Sys.sleep(1)
#   message("Done")
#   
# }


# withConsoleRedirect <-function(contianderID, expr){
#   
#   txt <- capture.output(results<-expr,type = "output")
#   if(length(text) >0){
#     insertUI(paste0("#",contianderID), where ="beforeEnd",
#       ui = paste0(txt,"\n", collapse = "" ))
#     
#   }
#   
#   results
#   
# }


## I'm trying to work on to capature the console outputs if time permits, written a basic function need to work on it. 

# H Clustering Function to call in 





###---------------------------------------------------mind map for clustering setup------------------------------------------------------------------ 

nodes <- structure(list(id = 1:22, label = structure(c(14L, 20L, 19L, 
                                                       16L, 12L, 18L, 2L, 17L, 22L, 8L, 13L, 3L, 4L, 5L, 6L, 7L, 21L, 
                                                       15L, 9L, 1L, 10L, 11L), .Label = c("Original Query-based Algorithm\n", 
                                                                                          "K-Means Clustering \n", "Model depends on unobserved latent variables\n", "Gaussian Mixture Models (GMMs) give us more flexibility than K-Means\n", " EM is an iterative method to find maximum likelihood\n", "we have two parameters to describe the shape of the clusters: the mean and the standard deviation\n", 
                                                                                          
                                                                                          "With GMMs we assume that the data points are Gaussian distributed", "Based on these classified points, we recompute the group center by taking the mean of all the vectors in the group.\n", "Uses a specified distance to separate dense clusters from sparser noise.\n", 
                                                                                          
                                                                                          "Agglomerative Hierarchical Clustering\n", "method of cluster analysis which seeks to build a hierarchy of clusters", "Treats clustering as a graph partitioning problem \n", 
                                                                                          
                                                                                          "Expectation-Maximization(EM) Clustering \n", "Clustering Methods\n", "Works by detecting areas where points are concentrated\n", 
                                                                                          
                                                                                          "Cluster points using eigenvectors of matrices derived from the data .\n", "select # of classes or groups\n", 
                                                                                          
                                                                                          "Data mapped to alow-dimensional space \n", "Easy to implement.\n", "Spectral clustering\n", "Density-Based Spatial Clustering(DBSCAN)\n", 
                                                                                          
                                                                                          "Each data point is classified by computing the distance between that point and each group center\n"), class = "factor"), shape = structure(c(1L, 
                                                                                                                                                        1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                                                                                                                                        1L, 1L, 1L, 1L, 1L), class = "factor", .Label = "box")), .Names = c("id", 
                                                                                                                                                                                                                  "label", "shape"), row.names = c(NA, -22L), class = "data.frame")
edges <- structure(list(from = c(1L, 2L, 2L, 2L, 2L, 1L, 7L, 7L, 7L, 1L, 
                                 11L, 11L, 11L, 11L, 11L, 1L, 17L, 17L, 17L, 1L, 21L), to = 2:22, 
                        arrows = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                             1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), .Label = "to", class = "factor")), .Names = c("from", 
                                                                                                                                            "to", "arrows"), row.names = c(NA, 21L), class = "data.frame")
color = c("darkred", "grey", "orange", "darkblue", "purple")

visNetwork(nodes, edges) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 1)

mynetwork <- visNetwork(nodes, edges) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 1) %>%
  visPhysics(enabled = FALSE) # disable physics to move nodes





###------------------------------------pre operational work for clustering ---------------------------------------------------------------------####

df <- iris[, -5]

genx<-function(x){
  # Create uniform random numbers between min and max
  #  no of random numbers generated : length(x)
  runif(length(x), min(x), (max(x)))
}




#######---------------------------------------------------------------SERVER----------------------------------------------------------####### 

# Server starts
shinyServer(function(input,output,session){


  
#####Output network######  
  
  
  
output$network <- renderVisNetwork({
    mynetwork
  })
  
vals <- reactiveValues(coords=NULL)
  
  output$view <- renderPrint({
    write.table(vals$coords, file = "save_coordinates.csv", sep = ";")
    vals$coords
  })
  
observe({
    input$getcoord
    visNetworkProxy("network") %>% visGetPositions()
    vals$coords <- if (!is.null(input$network_positions)) 
      do.call(rbind, input$network_positions)
  })
  








###------------------------------------------------------------DATA DISPLAY--------------------------------------------------------#
  
  
  
#raw-data for iris for K mean clustering 
  

  
 output$table<- DT::renderDT(
    iris, options = list(
      pageLength = 5,
      initComplete = JS('function(setting, json) { alert("Completed"); }')
    )
  )
  

  
####------------------------------------------------------- K mean Clustering work----------------------------------------------####
  
# Step 1----------------------Compute Hopkins statistic for iris dataset-------------------------------------------

  

output$Hstat <- renderPrint({
  res<-get_clust_tendency(iris[,-5],
                       n = nrow(iris) -1 ,  
                       graph = FALSE)
        res$hopkins_stat
  })  
  
#------- Need to think of a way to put up a button at the UI to re-run the  the Hopkins Stats ( When have time ) since we need to get a value >0.5
  
  
# Step 2 ------------------------------Plot iris data set - Summary Plots
output$plot2 <-renderPlot({
  
  fviz_dist(dist(df), show_labels = FALSE)+
    labs(title = "Iris data")
  
    
  })
  

output$plot22 <-renderPlot({
    fviz_pca_ind(prcomp(df), title = "Cluster tendency check on - Iris data", 
               habillage = iris$Species,  palette = "jco",
               geom = "point", ggtheme = theme_classic(),
               legend = "bottom")

})


output$datat <-renderPrint({
  str(iris)
  
})

output$summ1 <- renderPrint({
  summary(iris)
  
})  

###-------------------Summary data works end for the K mean clustering using IRIS data-----------------------








#Step 3 ----- Cluster methods

output$plot3 <- renderPlot({
  
  data<-iris[,-c(5)] 
  par(mar = c(2,2,2,2))
  nb <- NbClust(data, method = "kmeans")
  
})

output$plot4 <-renderPlot({
  data<-iris[,-c(5)] 
  model <- cascadeKM(data, 1, 10, iter = 100)
  plot(model, sortg = TRUE)
  
})


##-------------------------------------calinski

output$calinski <- renderPrint({
  data<-iris[,-c(5)] 
  model <- cascadeKM(data, 1, 10, iter = 100)
  model$results[2,]
  
})  

output$desc1 <- renderPrint({
  data<-iris[,-c(5)] 
  model <- cascadeKM(data, 1, 10, iter = 100)
  which.max(model$results[2,])
  
})  

output$desc2 <- renderPrint({
  summary(sensordata)
  
})  

#--------------------------------------kmeanclustering


output$VariableSelect1 <- renderUI({
  selectInput("var1", label="Select first variable for clustering:",
              choices=names(dataset()), selected=names(dataset())[1])  
})
output$VariableSelect2 <- renderUI({
  selectInput("var2", label="Select second variable for clustering:",
              choices=names(dataset()), selected=names(dataset())[2])  
})

dataset <- reactive({
  if (input$dataset=="iris") {
    data(iris)
    iris[,-5]
  } else if (input$dataset=="dat1") {
    read.table("data/self_test.data", skip=1, col.names = c("x","y")) 
  } else data.frame()
})

compute <- reactive({
  
  data   <- subset(dataset(), select=c(input$var1,input$var2))
  colnames(data) <- c("x","y")
  
  if(input$k>nrow(unique(data))) updateNumericInput(session,"k", value = nrow(unique(data)))
  if(input$k<1)                  updateNumericInput(session,"k", value = 1)
  
  if (input$kernel=="linear") {
    Kclust <- kmeans(data ,input$k)
    list(kmean.result = data.frame(data, cluster=as.factor(Kclust$cluster)),
         centers = as.data.frame(Kclust$centers))
  } else if (input$kernel=="RBF") {
    Kclust <- kkmeans(as.matrix(data), input$k, kernel="rbfdot")
    list(kmean.result = data.frame(data, cluster=as.factor(Kclust@.Data)),
         centers = data.frame(x=Kclust@centers[,1],
                              y=Kclust@centers[,2]))
  }
})

output$plot11 <- renderPlot({
  data=compute()
  ggplot(data=data$kmean.result, aes(x=x, y=y, color=cluster)) +
    geom_point(size=3) + geom_point(data=data$centers,
                                    aes(x=x, y=y, color='Center'), pch=17, size=7) +
    ggtitle("Clustering result") + xlab(input$var1) + ylab(input$var2)
})



#--------------------------------------Combine the selected variables into a new data frame

  

  
### Step 4--------------------------------------Sila
  
  
output$plot5 <- renderPlot({
  cl <- kmeans(iris[,-5], 2)
  dis <- dist(iris[,-5])^2
  sil = silhouette (cl$cluster, dis)
  plot(sil,main = "Silhouette Plot for 2 clusters")
  
})  


output$plot6 <- renderPlot({
  cl <- kmeans(iris[,-5], 3)
  dis <- dist(iris[,-5])^2
  sil = silhouette (cl$cluster, dis)
  plot(sil,main = "Silhouette Plot for 3 clusters")
  
}) 

output$plot7 <- renderPlot({
  cl <- kmeans(iris[,-5], 10)
  dis <- dist(iris[,-5])^2
  sil = silhouette (cl$cluster, dis)
  plot(sil,main = "Silhouette Plot for 10 clusters")
  
})  

##---------------------------------------------------End of K mean Clustering Work-----------------------------------------#


##-------------------------------------------------mean shift clustering---------------------------------------------------#



### Couldnt work on this as pkg is out of date, planning to move on to Spectral ( check with the time frame)





###-----------------------------------------------mean shift ends---------------------------------------------------------




#### ------------------------------------------hierarchical-clustering-R--------------------------------------------------


vec_is_sorted <- function(v) {
  return(sum(sort(v) == v) == length(v))
}



points <- reactive({
  json <- input$jsonPoints
  
  if(json == "[]") return(data.frame())
  
  df <- jsonlite::fromJSON(json)
  return(df)
})


h <- reactive({
  if(nrow(points()) <= 2) return(NULL)
  
  # special case for squared euclidian distance
  if(input$metric == "euclidian2") {
    d <- dist(points()[,c("x","y")], method="euclidian")^2
  } else {
    d <- dist(points()[,c("x","y")], method=input$metric)
  }
  
  h <- hclust(
    d, 
    method=input$hclustMethod
  )
  
  h$labels <- points()$id
  
  return(h)
})

Cllust<- reactive({
  if(is.null(h())) return(NULL)
  
  # if vec_is_sorted return TRUE then this implies a not-inverted tree (otherwise no splitting)
  # if minimal maximum gap is larger than the set minimal value then splitting is applied
  if(vec_is_sorted(h()$height) && max(diff(h()$height)) >= input$minDistance) {
    c <- stats::cutree(h(),h=split_height())
  } else {
    # all points belong to same trivial single cluster
    c <- rep(1,nrow(points()))
  }
  return(c)
})

split_height <- reactive({
  if(is.null(h())) return(NULL)
  
  if(is.numeric(input$splitTreeAt) && input$splitTreeAt > 0) {
    split_height <- input$splitTreeAt
  } else {
    i <- which.max(diff(h()$height))
    # 0.7*a+0.3*b instead of (a+b)/2 to set red splitting line apart
    # from dashed cluster boxes
    split_height <- (h()$height[i]*0.7+h()$height[i+1]*0.3)
  }
  return(split_height)
})

output$treePlot <- renderPlot({
  if(is.null(h()) || is.null(Cllust())) return(NULL)
  
  hghts <- h()$height
  
  if(vec_is_sorted(hghts)) {
    max_branch_gap <- max(diff(hghts))
    
    dend <- as.dendrogram(h())
    plot(dend, main=sprintf("tree split at %.2f - maximum branching height gap is %.2f",split_height(),max_branch_gap))
    
    
# no dashed boxes if only one cluster found--------------------------------------------------------------------------------------------------------Cllus
k<-length(unique(Cllust()))
    if(k > 1 && k < nrow(points())) {
    rect.dendrogram(dend, k=k, border = 8, lty = 5, lwd = 2)}
    
    if(max_branch_gap >= input$minDistance) {
      abline(h = split_height(), col="red", lty=2)
    }
  } else {
    plot(h(), main="inversions present - hence no splitting performed", xlab="", sub="")
  }
})

# generates the CSS for points according to cluster index
output$cssForPoints <- renderUI({
  cols <- c("red","green","blue","orange","pink","brown","violet","gray","black")
  css <- paste(sprintf("#%s{fill:%s}", points()$id, cols[Cllust()]), collapse="\n")
  
  return(tags$style(css))
})

output$heights <- renderPlot({
  if(is.null(h())) return(NULL)
  
  hghts <- h()$height
  
  par(mfrow=c(1,2))
  plot(density((h()$height)), main="density of branching heights", xlab="", ylab="")
  abline(v = split_height(), col="red", lty=2)
  
  # only plot if dendrogram is not inverted
  if(vec_is_sorted(hghts)) {
    seq <- max(0,floor(min(hghts))):floor(max(hghts))
    num <- sapply(seq, function(x){length(unique(stats::cutree(h(),h=x)))})
    plot(seq, num, ylim=c(0,max(num)), xaxt="n", yaxt="n",
         main="num of clusters (y) when cutting at height (x)",
         xlab="", ylab="")
    axis(1,at=seq)
    axis(2,at=0:max(num))
    abline(v = split_height(), col="red", lty=2)
  } else {
    plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="")
  }
})





#### ------------------------------------------hierarchical-clustering-R  Ends--------------------------------------------------###



###----------------------------------------------Console Output work----------------------------------------------------------#

# observeEvent(input$clusters,{
#   
#   withConsoleRedirect("console",{
#     
#     longfunc()
#     
#     
#     
#   })
#   
#   
#   
# })

###----------------------------------------------Console out put ends-------------------------------------------------------#



# Data Summary Work 

output$Summary <- renderPrint(if(input$Go){
  
  #if(input$Go){
  DF<-iris
  #print((DF))
  if(isolate(input$ExploreWays)==1){
    summary(DF)
  }else if(isolate(input$ExploreWays)==2){
    str(DF)
  }else if(isolate(input$ExploreWays)==3){
    #round(colMeans(is.na(DF))*100,2)[order(-round(colMeans(is.na(DF))*100,2))]
    sort(colMeans(is.na(DF))[colMeans(is.na(DF))>0]*100,decreasing = T)
  }else if(isolate(input$ExploreWays)==4){
    round(cor(DF[,sapply(DF, is.numeric)], use="pairwise", method="pearson"),3)
  }else if(isolate(input$ExploreWays)==5){
    #sort(round(skewness(DF[,sapply(DF, is.numeric)]),3))
    t<-sapply(DF,is.numeric)
    sort(sapply(DF[,t], skewness),decreasing = T)
  }else if(isolate(input$ExploreWays)==6){
    #sort(round(kurtosis(DF[,sapply(DF, is.numeric)]),3))
    t<-sapply(DF,is.numeric)
    sort(sapply(DF[,t], kurtosis),decreasing = T)
    
  }else{}
  #}
}) 


#---------------------------------------------------- Plot works ------------------------------------------------------#









output$PlotTypes = renderUI({
  selectInput(
    "PlotType",
    label = "Select Plot",
    "",selectize=TRUE,multiple=FALSE,choices=c("Missing","Histogram","Box","Scatter","Scatter Matrix")
  )
})


# Missing Pattern Plot - Sorted works fine!
output$MissingPattern <-renderPlot({
   aggr(iris, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(iris), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
  #}
})

# Correlation Plot - 
output$CorPlot<-renderPlot(if(input$PlotType=="Correlation"){
  M <- cor(na.omit(iris[, sapply(iris, is.numeric)]))
  row_indic <- apply(M, 1, function(x) sum(x > input$CorRange[2] | x < input$CorRange[1]) > 1)
  correlations<- M[row_indic ,row_indic ]
  corrplot(correlations, method="number")
  
  #}
})

# Scatter Plot All Pairs
output$ScatterAllPairs<-renderPlot({
  # Create a Progress object
  progress <- shiny::Progress$new()
  # Make sure it closes when we exit this reactive, even if there's an error
  on.exit(progress$close())
  
  progress$set(message = "Plot will be displayed..Kindly wait")
  
  # DF<-DataTypeConversion()
  #if(input$Go & isolate(input$ExploreWays)==5){
  
  pairs(iris[, sapply(iris, is.numeric)], 
        main="Simple Scatterplot Matrix")      
  #}
})




output$XaxisTypes = renderUI(if(input$PlotType=="Scatter"){
  nums <- sapply(iris, is.numeric)
  numericcols<-as.list(colnames(iris[,nums]))
  selectInput(
    "Xaxis", 
    label = "Select Xaxis",
    "",selectize=TRUE,multiple=FALSE,choices=numericcols
  )
}) 

# Dropdown to select y-axis for plot

output$YaxisTypes = renderUI(if(input$PlotType=="Scatter"){
  nums <- sapply(iris, is.numeric)
  numericcols<-as.list(colnames(iris[,nums]))
  selectInput(
    "Yaxis", 
    label = "Select Yaxis",
    "",selectize=TRUE,multiple=FALSE,choices=numericcols
  )
})







# Scatter Plot Single Pairs
output$ScatterSinglePair<-renderPlotly(if(input$PlotType=="Scatter"){
    ggplot(iris, aes_string(x=input$Xaxis, y=input$Yaxis)) +
    geom_jitter(size=2)+ xlab(paste(input$Xaxis))+ylab(input$Yaxis)+geom_smooth(method = lm )+
    ggtitle(paste(input$Xaxis," Vs ",input$Yaxis))+theme(plot.title = element_text(size = 15, face = "bold"),axis.title = element_text(face="bold",size=12),
                                                         axis.text.x  = element_text(vjust=0.5, size=10,face="bold"),axis.text.y  = element_text(size=10,face="bold"),legend.text=element_text(size=12)) 
  
})


output$HistParams = renderUI(if(input$PlotType=="Histogram"){
  nums <- sapply(iris, is.numeric)
  numericcols<-as.list(colnames(iris[,nums]))
  selectInput(
    "HistParam", 
    label = "Plot Histogram",
    "",selectize=TRUE,multiple=FALSE,choices=numericcols
  )
})


#Group By para




# Histogram
output$Hist<-renderPlotly(if(input$PlotType=="Histogram"){
  H <- hist(iris[,input$HistParam], plot = FALSE)
  minimum<-min(H$breaks,na.rm=TRUE)
  maximum<-max(H$breaks,na.rm=TRUE)
  step<-H$breaks[2]-H$breaks[1]
  
  ggplot(iris,aes_string(x=input$HistParam)) + 
    stat_bin(binwidth=step,colour="blue",fill="pink") +  
    stat_bin(binwidth=step, geom="text", aes(label=scales::percent((..count../sum(..count..)))), vjust=-1.5)+
    scale_x_continuous(breaks=seq(minimum,maximum, by=step))+theme_bw()
  
  
  
})






# Box Parameter

output$BoxParams = renderUI(if(input$PlotType=="Box"){
  nums <- sapply(iris, is.numeric)
  numericcols<-as.list(colnames(iris[,nums]))
  selectInput(
    "BoxParam", 
    label = "Box Plot",
    "",selectize=TRUE,multiple=FALSE,choices=numericcols
  )
})


#Group by para

output$GrByBox = renderUI(if(input$PlotType=="Box"){
  nonumerical <- as.list(iris%>%(-(1:4)))
  selectInput(
    "GrByBoxs", 
    label = "Group By",
    "",selectize=TRUE,multiple=FALSE,choices=nonumerical)
})





# Box Plot
output$BoxPlot<-renderPlotly(if(input$PlotType=="Box"){
 #  
 # ggplot(data=iris, aes(x=Species, y=Sepal.Length)) +
 #    geom_boxplot(aes(fill=Species)) + 
 #    ylab("Sepal Length") +
 #    ggtitle("Iris Boxplot") +
 #    stat_summary(fun.y=mean, geom="point", shape=5, size=4) 
 #  print(box)
  
ggplot(iris,aes_string(x=input$GrByBoxs,y=input$BoxParam,fill=input$GrByBoxs)) +geom_boxplot()+theme_bw()
  
})


# tabPlot - R version has a confilict with this one
output$tabPlot<-renderPlot(if(input$PlotType=="Tabular"){
    tableplot(iris,select_string =isolate(input$SelectTabPlot),sortCol=isolate(input$SortTabPlot),from=as.numeric(isolate(input$FilterTabPlot[1])),to=as.numeric(isolate(input$FilterTabPlot[2])))
  
})


#Mosaic work  - tried with library(memsic) too but doesn't work , some other library  could be masking the work. 



output$MosaicFirst = renderUI(if(input$PlotType=="Mosaic"){
  factors<- sapply(iris , is.factor)
  factorcols<-as.list(iris, choices=c(1:4))
  selectInput(
    "Mosaic1st", 
    label = "Select First Variable",
    "",selectize=TRUE,multiple=FALSE,choices=factorcols
  )
}) 


output$MosaicSecond = renderUI(if(input$PlotType=="Mosaic"){
  factors<- sapply(iris, is.factor)
  factorcols<-as.list(iris ,choices=c(1:4))
  selectInput(
    "Mosaic2nd", 
    label = "Select Second Variable",
    "",selectize=TRUE,multiple=FALSE,choices=factorcols
  )
}) 




#Mosaic Plot
output$MosaicPlot<-renderPlot(if(input$PlotType=="Mosaic"){

  #DF<-DataTypeConversion()
  x<-table(iris[,input$Mosaic1st],iris[,input$Mosaic2nd])
  mosaicplot(x,shade=T,legend=T,xlab = input$Mosaic1st, ylab = input$Mosaic2nd,main ="")


})


# Association Plot
output$AssocPlot<-renderPlot(if(input$PlotType=="Mosaic"){

  # DF<-DataTypeConversion()
  x<-table(iris[,input$Mosaic1st],iris[,input$Mosaic2nd])
  assoc(x,xlab = input$Mosaic1st,ylab = input$Mosaic2nd,main ="",shade=T)


})


output$Plots<- renderUI({
  
  progress <- shiny::Progress$new()  # progress object

  on.exit(progress$close())

  progress$set(message = "Processing is going on..Kindly wait") # progress

  if(input$PlotType=="Box"){
    box(uiOutput("BoxParams"),
        uiOutput("GrByBox"),
        plotlyOutput("BoxPlot",height=520,width=1200),title="",status = "primary",width=100,solidHeader = T)
  }else if(input$PlotType=="Histogram"){
    box(uiOutput("HistParams"),
        #fluidRow(column(6,actionButton("Go1", "Plot"))),
        plotlyOutput("Hist",height=520,width=1200),title="",status = "primary",width=100,solidHeader = T)
  }
  
  else if(input$PlotType=="Scatter"){
    box(uiOutput("XaxisTypes"),
        uiOutput("YaxisTypes"),
        #fluidRow(column(6,actionButton("Go1", "Plot"))),
        plotlyOutput("ScatterSinglePair",height=520,width=1200),title="",status = "primary",width=100,solidHeader = T)
  }else if(input$PlotType=="Tabular"){
    box(uiOutput("SelectTabPlots"),
        uiOutput("SortTabPlots"),
        uiOutput("FilterTabPlots"),
        fluidRow(column(6,actionButton("Go1", "Plot"))),
        plotOutput("tabPlot",height=500,width=1200),title="",status = "primary",width=100,solidHeader = T)
  }else if(input$PlotType=="Scatter Matrix"){
    box(fluidRow(plotOutput("ScatterAllPairs",height=1000,width=1000)),title="",status = "primary",width=100,solidHeader = T)
  }else if(input$PlotType=="Correlation"){
    box(uiOutput("CorRanges"),
        fluidRow(column(6,actionButton("Go1", "Plot"))),
        plotOutput("CorPlot",height=1000,width=1000),title="",status = "primary",width=100,solidHeader = T)
  }else if(input$PlotType=="Missing"){
    box(
      #fluidRow(column(6,actionButton("Go1", "Plot"))),
      plotOutput("MissingPattern",height=500,width=1200),title="",status = "primary",width=100,solidHeader = T)
  }else if(input$PlotType=="Mosaic"){
    box(uiOutput("MosaicFirst"),
        uiOutput("MosaicSecond"),
        #fluidRow(column(6,actionButton("Go1", "Plot"))),
        plotOutput("MosaicPlot",height=500,width=1200),
        plotOutput("AssocPlot",height=500,width=1200),title="",status = "primary",width=100,solidHeader = T)
  }
  
  
  else {}
})











  
}) # Server End 

####-----------------------------------------------Server Ends-------------------------------------------------------------------------------


