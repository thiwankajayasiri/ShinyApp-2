library(shiny)
library(MASS)
library(datasets)

library(dendextend)  # High- Clust
library(jsonlite)
library(shinythemes)
library(ggplot2)
library(shinyjs)
library(dplyr)
library(plotly)
library(psych)
library(visNetwork)
library(DT)
library(sqldf)
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
library(visNetwork) 

#####----------------------------------------------------------Clustering libraries-----------------------------------------------######


library(skmeans) #kmean clustering
library(ClusterR)#kmean clustering 
library(NbClust) #kmean clustering
library(factoextra)#kmean clustering - Get clust_tendency() assees hopkins test
library(cluster)##For silhoutte()
library(vegan)##For silhoutte()
library(kernlab)




####----------------------------------------------------------------Pre load the required Data----------------------------------------------------------------#####


####---------Pre load the function for the www files for tagging

file_content <- function(file) {
  return(readChar(file, file.info(file)$size))
}


  
########----------------------------------------------------------------ShinyUI Starts----------------------------------------------------------------########

shinyUI(
  
  (fluidPage
   
   (theme = shinytheme("spacelab"),
     useShinyjs(),

   titlePanel("A2_Data423 - Thiwanka Jayasiri: Clustering"),
     
     tags$style(HTML("
                    .sidebar { height: 110vh; overflow-y: auto; }
                     " )),
     
     
     
      tabsetPanel( 
        type ="tab",
#-------------------------------------------------------------------------Tab 01----------------------------------------------------------Intro cluster map           
        tabPanel("Intro",
                 h1("An introduction to Clustering "),
                   tags$div(
                     tags$p("Clustering is one of the most common exploratory data analysis technique used to get an intuition about the structure of the data. It can be defined as the task of identifying subgroups in the data such that data points in the same subgroup (cluster) are very similar while data points in different clusters are very different."),
                      tags$p("In other words, we try to find homogeneous subgroups within the data such that data points in each cluster are as similar as possible according to a similarity measure such as euclidean-based distance or correlation-based distance. 
                            The decision of which similarity measure to use is application-specific.")
                    ,
                    
                    tags$p("Use the below Viz Net to galance through the content")
                    
                    
                    
                     ),
                fluidPage(visNetworkOutput("network", height = "800px"),
                       actionButton("getcoord", "View & Save Coordinates"),
                       verbatimTextOutput("view"))  
                      ), #intromind map
        
#----------------------------------------------------------------------Tab 02 -------------------------------------------------------------- K means algorithm     
## Notes, Welcome! Related Books: `Practical Guide To Cluster Analysis in R` at https://goo.gl/13EFCZ       



  tabPanel( "K_Mean_Clustering ",
                      tags$div(
                        tags$p(
                            "Kmeans algorithm is an iterative algorithm that tries to partition the dataset into Kpre-defined distinct non-overlapping subgroups (clusters) where each data point belongs to only one group. 
                             It tries to make the inter-cluster data points as similar as possible while also keeping the clusters as different (far) as possible. 
                            It assigns data points to a cluster such that the sum of the squared distance between the data points and the cluster's centroid (arithmetic mean of all the data points that belong to that cluster) is at the minimum. 
                            The less variation we have within clusters, the more homogeneous (similar) the data points are within the same cluster. ") ,
                        
                        tags$p(
                          
                          tags$li("Specify number of clusters K."), 
                          tags$li("Initialize centroids by first shuffling the dataset and then randomly selecting K data points for the centroids without replacement."),
                          tags$li	("Keep iterating until there is no change to the centroids. i.e assignment of data points to clusters isn't changing.")
                         
                        ), 
                        tags$p("Below is demostration of how K-Mean clustering works using the famous IRIS data set")
                          ),
                          
              
                tabsetPanel(type ="tab",
                      
                            
                      tabPanel(" Step 1 : Check the data set",
                         fluidPage(
                            fluidRow(
                                column(12,
                                  dataTableOutput('table'))
                                       ),
                            
                            tags$div(
                              
                              tags$p("Data Type "),
                              verbatimTextOutput("datat"),
                              tags$p("Summary Stats"),
                              verbatimTextOutput("summ1"),
                              tags$br()
                              
                            )
                            
                            
                            )
                                    
                           ),      
                        
                      
                      
                      
                      tabPanel("Step 2 - Plot the data ",
                               
                               uiOutput('PlotTypes'),
                               uiOutput('Plots') 
                               
                      ),
                      
                      
                      
                      
                      
                      tabPanel("Step 3 - Check Cluster Tendency ",
                               fluidPage(
                                 headerPanel('Measure Tedency before clustering the data'),
                                 
                                 
                                 tags$p("Cluster tendency :") ,
                                 
                                 tags$p("Before applying cluster methods, the first step is to assess whether the data is clusterable, a process defined as the assessing of clustering tendency.")
                                 ,tags$p("The Hopkins statistic is used to assess the clustering tendency of a data set by measuring the probability that a given data set is generated by a uniform data distribution. 
                                     Check Hopkins Statistic Value #####If the value of Hopkins statistic is greater than 0.5 (H>0.5) then data set is clusterable and if value is less than 0.5 (H<0.5) then data is non clusterable."),
                      
                                          verbatimTextOutput("Hstat"),
                                          plotOutput("plot22"),
                                          tags$br(),
                                          # tags$p("missing pattern"),
                                          # plotOutput("MissingPattern"),
                                          tags$br(),
                                          tags$p("Visual methods for clustering tendency"),
                                          tags$p( "It follows these steps",
                                                  tags$li("Dissimilarity (DM) matrix using Euclidean distance measure between data set objects"),
                                                  tags$li("Rearrange the DM so that similar objects are close to one another.It creates an ordered dissimilarity matrix (ODM)"),
                                                  tags$li("The ODM is displayed as an ordered dissimilarity image (ODI), which is the visual output of VAT")
                                                  
                                          ),
                                          plotOutput("plot2"),
                                          tags$p("Cluster Methods"),
                                          tags$br(),
                                          plotOutput("plot3"),
                                          tags$br(),
                                          tags$p("Decide on number of clusters basis of the above graph and move to the next tab : K mean Clustering"),
                                          tags$p("The Hubert index is a graphical method of determining the number of clusters.In the plot of Hubert index, we seek a significant knee that corresponds to asignificant increase of the value of the measure i.e the significant peak in Hubert  index second differences plot. "),
                                          tags$br(),
                                          tags$p("Calinski criterion is similar to finding ratio of between-cluster-variance/within-cluster variance "),
                                          plotOutput("plot4"),
                                          tags$br(),
                                          tags$p("check the results of the model"),
                                          verbatimTextOutput("calinski"),
                                          tags$p("which of these values is maximum?"),
                                          verbatimTextOutput("desc1")
                                    
                                 
                               )       
                                   
                      ),
#--------------------------------------#Kmean clustering VIZ  tab----------------------------------------------------------------------------------------------------------
                            
                      tabPanel("Step 4 - ViZ and check how Kmean Clsutering works ",
                          fluidPage(
                              headerPanel('Iris k-means clustering'),
                                    sidebarPanel(
                                      selectInput('dataset','Select a dataset:',c("iris","dat1")),
                                      uiOutput("VariableSelect1"),
                                      uiOutput("VariableSelect2"),
                                      numericInput('k', 'Number of clusters', value = 3,
                                                   min=1, step=1),
                                      selectInput('kernel','Type of kernel:',c("linear","radial (RBF)"="RBF"))
                                          ),
                          mainPanel( 
                            
                                      h2("Instructions:"),
                                      p("Select a dataset and variables to be used for clustering."),
                                      p("Indicate the desired number of clusters and the type of kernel to be used."),
                                      p("Kernel::https://en.wikipedia.org/wiki/Kernel_method "),
                                      h2("Result:"),
                                      plotOutput('plot11')
                            
                            
                            
                            
                            
                            ) 
                                    )       
                            # verbatimTextOutput("SummaryA1"),
                            # verbatimTextOutput("SummaryA2")         
                                  ),
                  
                
                  
                      
                      tabPanel("Step 5 - Check your results with - Silhouette Analysis",
                               
                              fluidPage(
                                
                                plotOutput("plot5"),
                                plotOutput("plot6"),
                                plotOutput("plot7")
                                
                                            )
                             #selectizeInput("VariablesA", label="Show variables:", choices=choicesA, multiple=TRUE, selected=choicesA),
                             #plotOutput("Mosaic")
                                            )
                      
                      
                      
                      
                      
                                           )
                                          ),
                  








 ###-------------------------------------------------------------------Tab 03----------------------------------------------------Mean shift Clustering        
                  # tabPanel("Spectral Clustering",
                  #                   
                  #      tabsetPanel(type ="tab",
                  # 
                  #                  tabPanel("Data Set"
                  #                           #selectizeInput("VariablesA", label="Show variables:", choices=choicesA, multiple=TRUE, selected=choicesA),
                  #                           #plotOutput("Mosaic")
                  #                           ),
                  # 
                  #                      tabPanel("Visualisation123"
                  #                               #selectizeInput("VariablesA", label="Show variables:", choices=choicesA, multiple=TRUE, selected=choicesA),
                  #                               #plotOutput("Mosaic")
                  #                      ),
                  #                      tabPanel("Visualisation456"
                  #                               #selectizeInput("VariablesA", label="Show variables:", choices=choicesA, multiple=TRUE, selected=choicesA),
                  #                               #plotOutput("Mosaic")
                  #                                      )
                  #                                   )
                  #                                 ),
                 
       
####-----------------------------------------------------------------Tab 04-------------------------------------------------------------EM Clustering 

              tabPanel("H- Clustering" ,
                 
                    tabsetPanel(type ="tab",
                                tabPanel("What's HClust",
                                   fluidPage(
                                      headerPanel("Introduction"),
                                      #sidebarPanel(
                                        #selectInput('xcol', 'X Variable', names(iris)),
                                        #selectInput('ycol', 'Y Variable', names(iris),
                                         #           selected=names(iris)[[2]]),
                                        #numericInput('clusters', 'Cluster count', 3,
                                                     #min = 1, max = 9)
                                      #),
                              mainPanel(
                                
                                tags$div(
                                  tags$p(
                                    " In data mining and statistics, hierarchical clustering (also called hierarchical cluster analysis or HCA) is a method of cluster analysis which seeks to build a hierarchy of clusters. Strategies for hierarchical clustering generally fall into two types:") ,
                                  
                                  tags$p(
                                    
                                    tags$li("Agglomerative: This is a bottom-up approach: each observation starts in its own cluster, and pairs of clusters are merged as one moves up the hierarchy."), 
                                    tags$li("Divisive: This is a top-down approach: all observations start in one cluster, and splits are performed recursively as one moves down the hierarchy."),
                                    tags$li	("In general, the merges and splits are determined in a greedy manner. The results of hierarchical clustering are usually presented in a dendrogram")
                                    
                                  ), 
                                  tags$p("Next tab demostrate how the H Clustering works, and however,"),
                                  tags$br(),
                                  tags$p("Hierarchical clustering starts by treating each observation as a separate cluster. Then, it repeatedly executes the following two steps: 
                                        (1) identify the two clusters that are closest together, and (2) merge the two most similar clusters. 
                                         This continues until all the clusters are merged together. This is illustrated in the diagrams given in the next tab"),
                                  tags$br(),
                                  tags$p("The main output of Hierarchical Clustering is a dendrogram, which shows the hierarchical relationship between the clusters:"),
                                  tags$br(),
                                  tags$h3("Linkage Criteria"),
                                  tags$p("After selecting a distance metric, it is necessary to determine from where distance is computed. For example, it can be computed between the two most similar parts of a cluster (single-linkage), the two least similar bits of a cluster (complete-linkage), 
                                         the center of the clusters (mean or average-linkage), or some other criterion. Many linkage criteria have been developed."),
                                  tags$br(),
                                  tags$h3("Agglomerative versus divisive algorithms"),
                                  tags$p("Hierarchical clustering typically works by sequentially merging similar clusters, as shown above. This is known as agglomerative hierarchical clustering. In theory, it can also be done by initially grouping all the observations into one cluster, and then successively splitting these clusters. 
                                         This is known as divisive hierarchical clustering. Divisive clustering is rarely done in practice.")
                                  
                                  
                                            )
                                      
                                          ) 
                                      
                                        )      
                                  #verbatimTextOutput("SummaryA1"),
                                  #verbatimTextOutput("SummaryA2")         
                                        ),
                                tabPanel("H Clust Demo",
                                         
                                         fluidRow(
                                           column(5,
                                                  selectInput("hclustMethod", label="method", choices=list(
                                                    "single"="single","complete"="complete","average"="average",
                                                    "mcquitty"="mcquitty","median"="median","centroid"="centroid",
                                                    "ward.D"="ward.D","ward.D2"="ward.D2"
                                                  ),selected="single"),
                                                  
                                                  selectInput("metric", label="distance", choices=list(
                                                    "euclidian"="euclidian","squared euclidian"="euclidian2","maximum"="maximum","manhattan"="manhattan",
                                                    "canberra"="canberra","binary"="binary","minkowski"="minkowski"
                                                  ),selected="single"),
                                                  
                                                  tags$div(style="margin:10px",
                                                           HTML(file_content("www/apply_heuristic_button.html")),
                                                           numericInput("minDistance","min. max. branching gap",1),
                                                           numericInput("splitTreeAt","split tree at",value="",min=0,max=100,step=1)
                                                  ),
                                                  
                                                  uiOutput("cssForPoints"),
                                                  div(id="hereComesTheCanvas"),
                                                  HTML(file_content("www/d3_canvas.html")),
                                                  tags$small("Left-Click to add or remove a point. Points can be dragged."),
                                                  
                                                  # this text input communicates the D3.js component state to the server
                                                  textInput("jsonPoints", "", "[]")
                                           ),
                                           
                                           # Show a plot of the generated distribution
                                           column(7,
                                                  plotOutput("treePlot"),
                                                  plotOutput("heights")
                                           )
                                           
                                         )   
                                         
                                         
                                    
                                              )
                           #        tabPanel("Explain Further on H-Clust"
                           #          #selectizeInput("VariablesA", label="Show variables:", choices=choicesA, multiple=TRUE, selected=choicesA),
                           #          #plotOutput("Mosaic")
                           # )
                          )#tabsetpanel
               )
                        
######------------------------------------------------------------------Tab 05------------------------------------------------------Desinty based   

# tabPanel("Density based clustering" ,
#          
#          tabsetPanel(type ="tab",
#                      tabPanel("den den den",
#                               fluidPage(
#                                 headerPanel('enter anything here'),
#                                 #sidebarPanel(
#                                 #selectInput('xcol', 'X Variable', names(iris)),
#                                 #selectInput('ycol', 'Y Variable', names(iris),
#                                 #           selected=names(iris)[[2]]),
#                                 #numericInput('clusters', 'Cluster count', 3,
#                                 #min = 1, max = 9)
#                                 #),
#                                 mainPanel(
#                                   #plotOutput('Boxplot2')
#                                 ) 
#                                 
#                               )      
#                               #verbatimTextOutput("SummaryA1"),
#                               #verbatimTextOutput("SummaryA2")         
#                      ),
#                      tabPanel("den base 1"
#                               #selectizeInput("VariablesA", label="Show variables:", choices=choicesA, multiple=TRUE, selected=choicesA),
#                               #plotOutput("Mosaic")
#                      ),
#                      tabPanel("den base 2"
#                               #selectizeInput("VariablesA", label="Show variables:", choices=choicesA, multiple=TRUE, selected=choicesA),
#                               #plotOutput("Mosaic")
#                      )
#          )#tabsetpanel
# ),

        
###-----------------------------------------------------------------Tab 06-----------------------------------------------Agglomerative h clustering    
        
# tabPanel("H- Clustering" ,
#          
#          tabsetPanel(type ="tab",
#                      tabPanel("Algo algo ",
#                               fluidPage(
#                                 headerPanel('enter anything anything anything here'),
#                                 #sidebarPanel(
#                                 #selectInput('xcol', 'X Variable', names(iris)),
#                                 #selectInput('ycol', 'Y Variable', names(iris),
#                                 #           selected=names(iris)[[2]]),
#                                 #numericInput('clusters', 'Cluster count', 3,
#                                 #min = 1, max = 9)
#                                 #),
#                                 mainPanel(
#                                   #plotOutput('Boxplot2')
#                                 ) 
#                                 
#                               )      
#                               #verbatimTextOutput("SummaryA1"),
#                               #verbatimTextOutput("SummaryA2")         
#                      ),
#                      tabPanel("den f base 1"
#                               #selectizeInput("VariablesA", label="Show variables:", choices=choicesA, multiple=TRUE, selected=choicesA),
#                               #plotOutput("Mosaic")
#                      ),
#                      tabPanel("den b base 2"
#                               #selectizeInput("VariablesA", label="Show variables:", choices=choicesA, multiple=TRUE, selected=choicesA),
#                               #plotOutput("Mosaic")
#                      )
#                         )#tabsetpanel
#             )#tabpanel 




# tabPanel("Console Panel" ,
#          
#          tabsetPanel(type ="tab",
#                      tabPanel("Console Console  ",
#                               fluidPage(
#                                 headerPanel('Cosole out put viewer'),
#                                 #sidebarPanel(
#                                 #selectInput('xcol', 'X Variable', names(iris)),
#                                 #selectInput('ycol', 'Y Variable', names(iris),
#                                 #           selected=names(iris)[[2]]),
#                                 #numericInput('clusters', 'Cluster count', 3,
#                                 #min = 1, max = 9)
#                                 #),
#                                 mainPanel(
#                                   pre(id = "console")
#                                 ) 
#                                 
#                               )      
#                               #verbatimTextOutput("SummaryA1"),
#                               #verbatimTextOutput("SummaryA2")         
#                      ),
#                      tabPanel("den f base 111"
#                               #selectizeInput("VariablesA", label="Show variables:", choices=choicesA, multiple=TRUE, selected=choicesA),
#                               #plotOutput("Mosaic")
#                      ),
#                      tabPanel("den b base 222"
#                               #selectizeInput("VariablesA", label="Show variables:", choices=choicesA, multiple=TRUE, selected=choicesA),
#                               #plotOutput("Mosaic")
#                      )
#          )#tabsetpanel
# )#tabpanel 







#------------------------------Tabs End---------------------------------------------------------------------------------------------------



                  
                          
                          )#tabsetpanel
              
              
    )#theme
    
    
    
    
  )#fluidpage
  
  
  
 

  
)#shinyUI

