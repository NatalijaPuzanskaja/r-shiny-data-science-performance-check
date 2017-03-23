##
## LIBRARY ---------------
##
library(shiny)
library(shinythemes)
library(RMySQL)
library(pROC)

source('config.ini')

options(shiny.sanitize.errors = TRUE)

##
## UI ---------------
##
ui <- function(){
  fluidPage(
  column(width = 12,
         h1("Danske Bank Data Science Challenge'17"),
         HTML(
           "<div class='alert alert-info'>",
           "<strong>Heads up!</strong> This is a <em>precheck</em> app using R/Shiny and connection to remote MySQL database. <br/>",
           "<a href='https://github.com/NatalijaPuzanskaja/r-shiny-data-science-performance-check' class='alert-link'>The code is free and open source on Github.</a>",
           "</div>"
         )
  ),
  titlePanel("Model Performance Check"),
  sidebarLayout(
    sidebarPanel(
      textInput("token", "Token"),
      tags$hr(),
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      helpText("Note: data files for upload must comply to required format"),
      helpText("The first column must be a client identification number"),
      helpText("The second column - probability of default"),
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
          plotOutput("auc"),
      
          h4("User's best performance"),
          verbatimTextOutput("performanceBest"),      
      
          h4("Top 3 performances"),
          verbatimTextOutput("performanceTop3")),
        tabPanel("About",
          h4("AUC calculation"),
          code("library(pROC)"),br(),
          code("roc(...)"),br(),
          code("plot.roc(...)"),br()
          )
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
  realData <- dbReadTable(conn=connection, name='businesses')
  
  ## FORMAT DATA ---------------
  users$FullName <- paste(users$Name, users$Surname, sep=' ')
  
  ## EXPOSE AUC ---------------
  output$auc <- renderPlot({
    ## READ USER INPUT ---------------
    inFile <- input$file1
    inToken <- input$token
    checkToken <- inToken %in% users$Token
    
    ## CHECK TO PROCEED ---------------
    if (is.null(inFile) | checkToken == FALSE)
      return(NULL)
    
    ## CALCULATE AUC ---------------
    estimData <- read.csv(inFile$datapath, header=input$header, 
                          sep=input$sep, quote=input$quote)
    names(estimData) <- c('Clientid','Estimate')
    
    data <- merge(estimData, realData, by='Clientid', all.x=TRUE)
    data <- data[is.na(data$Default) == FALSE,]
    
    roc_obj <- roc(data$Default, data$Estimate)
    auc <- auc(roc_obj)
    row <- data.frame(Score = unname(auc), 
                      UserId = inToken,
                      Date = Sys.Date())
    dbWriteTable(connection, value=row, name="results", append=TRUE, row.names=FALSE)
    dbDisconnect(connection)
    plot(roc_obj, print.auc=TRUE)
  })
  
  ## EXPOSE BEST USER'S PERFORMANCE ---------------
  output$performanceBest <- renderPrint({
    ## READ USER INPUT ---------------
    inFile <- input$file1
    inToken <- input$token
    checkToken <- inToken %in% users$Token
    
    if (is.null(inFile) | checkToken == FALSE)
      return(NULL)
    
    ## READ DATA ---------------
    connection <- dbConnect(MySQL(), 
                            dbname=options()$mysql$dbname, 
                            host=options()$mysql$host, 
                            user=options()$mysql$user, 
                            password=options()$mysql$password)
    performance <- dbReadTable(conn=connection, name='results')
    dbDisconnect(connection)
    
    performanceRelevant <- performance[performance$UserId == inToken,]
    performanceBest <- head(performanceRelevant[order(-performanceRelevant$Score),], n=1)
    performanceBest <- merge(performanceBest, users, by.x='UserId', by.y='Token', all.x=TRUE)
    rownames(performanceBest) <- NULL
    performanceBest[, c('FullName','Score','Date')]
  })  
    
  ## EXPOSE TOP 3 USERS PERFORMANCE ---------------
  output$performanceTop3 <- renderPrint({
    ## READ USER INPUT ---------------
    inFile <- input$file1
    inToken <- input$token
    checkToken <- inToken %in% users$Token
    
    if (is.null(inFile) | checkToken == FALSE)
      return(NULL)
    
    connection <- dbConnect(MySQL(), 
                            dbname=options()$mysql$dbname, 
                            host=options()$mysql$host, 
                            user=options()$mysql$user, 
                            password=options()$mysql$password)
    performance <- dbReadTable(conn=connection, name='results')
    dbDisconnect(connection)
    
    performance <- merge(performance, users, by.x='UserId', by.y='Token', all.x=TRUE)
    performanceMax <- performance[which(abs(performance$Score) == ave(performance$Score, performance$UserId, FUN=function(x) max(abs(x)))), ]
    performanceTop3 <- head(performanceMax[order(-performanceMax$Score),], n=3)
    rownames(performanceTop3) <- NULL
    performanceTop3[, c('FullName','Score','Date')]
  })
  
  
})

shinyApp(ui = ui, server = server)
#runApp(list(ui = ui, server = server))