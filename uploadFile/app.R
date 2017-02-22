##
## LIBRARY ---------------
##
library(shiny)
library(RMySQL)
library(pROC)

source('config.ini')

##
## UI ---------------
##
ui <- function(){
  fluidPage(
  titlePanel("Uploading Files"),
  sidebarLayout(
    sidebarPanel(
      textInput("token", "Token"),
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"')
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Performance",
          h4("Current model performance"),
          verbatimTextOutput("auc"),
      
          h4("User's best performance"),
          verbatimTextOutput("performanceBest"),      
      
          h4("Top 3 performances"),
          verbatimTextOutput("performanceTop3")),
      tabPanel("About", verbatimTextOutput("AUC calculation methodology"))
      )
    )
  )
)}

##
## SERVER ---------------
##
server = (function(input, output) {
  ## CONNECT TO MySQL ---------------
  connection <- dbConnect(MySQL(), 
                          dbname=options()$mysql$dbname, 
                          host=options()$mysql$host, 
                          user=options()$mysql$user, 
                          password=options()$mysql$password)
  
  ## READ DATA ---------------
  users <- dbReadTable(conn=connection, name='users')
  
  ## EXPOSE AUC ---------------
  output$auc <- renderPrint({
    ## READ USER INPUT ---------------
    inFile <- input$file1
    inToken <- input$token
    
    ## CHECK TO PROCEED ---------------
    if (is.null(inFile) | is.null(inToken))
      return(NULL)
    
    ## CALCULATE AUC ---------------
    estimData <- read.csv(inFile$datapath, header=input$header, 
                          sep=input$sep, quote=input$quote)
    names(estimData)[names(estimData) != 'id'] <- 'estim'
    
    #realData <- dbReadTable(conn=connection, name='businesses')
    realData <- data.frame(id=c(1,2,3,4),
                           default=c(0,1,0,0))
    names(realData)[names(realData) != 'id'] <- 'real'
    
    data <- merge(estimData, realData, by='id', all.x=TRUE)
    
    #roc_obj <- roc(realData$default, estimData$default)
    roc_obj <- roc(data$real, data$estim)
    auc <- auc(roc_obj)
    row <- data.frame(Score = unname(auc), 
                      UserId = 'test-test',
                      Date = Sys.time())
    #dbWriteTable(connection, value=row, name="results", append=TRUE, row.names=FALSE) 
    auc
  })

  ## READ DATA ---------------
  performance <- dbReadTable(conn=connection, name='results')
  
  ## EXPOSE BEST USER'S PERFORMANCE ---------------
  output$performanceBest <- renderPrint({
    ## READ USER INPUT ---------------
    inFile <- input$file1
    inToken <- input$token
    
    if (is.null(inFile) | is.null(inToken))
      return(NULL)
    
    performanceRelevant <- performance[performance$UserId == inToken,]
    performanceBest <- head(sort(performanceRelevant$Score, decreasing=TRUE), n=1)
    performanceBest
  })  
    
  ## EXPOSE TOP 3 USERS PERFORMANCE ---------------
  output$performanceTop3 <- renderPrint({
    ## READ USER INPUT ---------------
    inFile <- input$file1
    inToken <- input$token
    
    if (is.null(inFile) | is.null(inToken))
      return(NULL)
    
    performance <- dbReadTable(conn = mydb, name = 'results')
    performanceTop3 <- head(performance[order(-performance$Score),], n=3)
    rownames(performanceTop3) <- NULL
    performanceTop3[, c('UserId','Score')]
  })
  
  dbDisconnect(connection)
})

runApp(list(ui = ui, server = server))