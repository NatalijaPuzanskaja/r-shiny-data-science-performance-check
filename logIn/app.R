##
## LIBRARY ---------------
##
library(shiny)
library(RMySQL)

source('config.ini')

##
## GET DATA ---------------
##
mydb <- dbConnect(MySQL(), 
                  dbname=options()$mysql$dbname, 
                  host=options()$mysql$host, 
                  user=options()$mysql$user, 
                  password=options()$mysql$password)
users <- dbReadTable(conn = mydb, name = 'users')
results <- dbReadTable(conn = mydb, name = 'results')
businesses <- dbReadTable(conn = mydb, name = 'businesses')
dbDisconnect(mydb)

Logged = FALSE
Name = NA

##
## UI - LOGIN ---------------
##
uiLogin <- function(){
  tagList(
    div(id = "login",
        wellPanel(textInput("token", "Token"),
                  br(),actionButton("Login", "Log in"))),
    tags$style(type="text/css", "#login {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
  )}

##
## UI - UPLOAD FILE ---------------
##
uiUploadFile <- function(){
  tagList(
    tabPanel("Danske Data Science Challenge")
  )}

ui = (htmlOutput("page"))

##
## SERVER ---------------
##
server = (function(input, output,session) {
  
  USER <- reactiveValues(Logged = Logged)
  
  observe({ 
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Token <- isolate(input$token)
          Id.token <- which(users$Token == Token)
          if (length(Id.token) > 0) {
            userData <- users[users$Token == Token,]
            USER$userName <- paste(userData$Name, userData$Surname, sep=' ')
            USER$Logged <- TRUE
          }
        } 
      }
    }    
  })
  observe({
    if (USER$Logged == FALSE) {
      
      output$page <- renderUI({
        div(class="outer",do.call(bootstrapPage,c("",uiLogin())))
      })
    }
    if (USER$Logged == TRUE) 
    {
      output$page <- renderUI({
        div(class="outer",do.call(navbarPage,c(inverse=TRUE,title=USER$userName,uiUploadFile())))
      })
      print(ui)
    }
  })
})

runApp(list(ui = ui, server = server))