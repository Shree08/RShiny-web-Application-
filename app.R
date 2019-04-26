#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(dplyr)
library(RSQLite)
library(sqldf)
# Define UI for application that draws a histogram
#Logged = FALSE;
login = FALSE;

#ui1 <- shinyUI(
#  uiOutput('welcome')
#)
ui <- shinyUI( 
               navbarPage("", 
                         theme = shinytheme("darkly"),
                         tabPanel("Home",
                                  uiOutput('welcome')),
                         
                         tabPanel("Sample Submission",
                                   sidebarLayout(
                                    sidebarPanel(
                                      fileInput("file1",
                                                "Choose Test Sample file from directory",
                                                multiple = TRUE,
                                                accept=c('text/csv', 
                                                         'text/comma-separated-values,text/plain', 
                                                         '.csv'))
                                      
                                    ),
                                    mainPanel(
                                      
                                      tableOutput('contents')
                                    )
                                  )
                         ),
                         tabPanel("Analysis",
                                  sidebarLayout(
                                    sidebarPanel(
                                      downloadButton('downloadSample', 'Download Samples'),
                                      fileInput("file2",
                                                "Choose TestResults file from directory",
                                                multiple = TRUE,
                                                accept=c('text/csv', 
                                                         'text/comma-separated-values,text/plain', 
                                                         '.csv'))
                                    ),
                                    mainPanel(
                                       #tableOutput('results')
                                    )
                                  )  
                                  
                         ),
                         tabPanel("Test Results", 
                                  sidebarLayout(
                                    sidebarPanel(downloadButton('downloadResults', 'Download Results')),
                                    mainPanel(
                                      tableOutput('results')
                                    )
                                  )
                         )
)
)

#ui = (htmlOutput("page"))

# Define server logic required to draw a histogram
#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
#library(shinythemes)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  login <- reactiveValues(login = FALSE, user = NULL, role = NULL, email = NULL)
  
  # initially display the login modal
  observe({
        composeLoginModal()
    })
  
  observeEvent(input$logout_ok, {
    shiny::removeModal()
    
    # clear the values when logout is confirmed
    login$login <- FALSE
    login$user  <- NULL
    login$role  <- NULL
    login$email <- NULL
    
    composeLoginModal(
      div(
        id    = "modal-logout-message"
        , style = "margin-bottom: 10px"
        , span(class = "text-muted", "Successfully Logged Out")
      ) #/ modal-logout-message
    ) #/ composeLoginModal
  })
  
  # once a login is attempted, do some checks
  observeEvent(input$login_button, {
    
    # remove the modal while we check
    shiny::removeModal()
    
    # query the database for that user will return NAs if not populated
    stored <- sendUserGetQuery(input$login_user)
    
    # if any are NA then the record doesn't exist or the record is corrupted
    user_invalid <- stored %>% sapply(is.na) %>% any
    
    # try to login, will automatically handle NULL-y objects
    login$login <- validateLogin(stored$password, input$login_passwd)
    
    # if the login is not successful, toss up another login modal, 
    # this time with a message
    if (isTRUE(user_invalid) | login$login == FALSE) {
      composeLoginModal(
        div(
          id    = "modal-login-message"
          , style = "margin-bottom: 10px"
          , span(style = "color: red; font-weight:bold", "Incorrect Login/Password")
         ) #/ modal-login-message
      ) #/ composeLoginModal
    } else {
      # if the login is successful, populate the known values
      login$user  <- stored$user
      login$role  <- stored$role
      login$email <- stored$email
      
      rm(stored)
    } #/ fi
  }) #/ login_button Observer
  
  output$role <- reactive({
    login$role
  })
  #close database conncention on exit
  session$onSessionEnded(function() {
      dbDisconnect(db)
  })
  
  observeEvent(input$logout, {
    helpText("Are you sure you want to Logout? ") %>%
      div(style = "margin-bottom: 15px", .) %>%
      showConfirmModal("logout", .)
  })
  
  observeEvent(input$logout_cancel, {
    shiny::removeModal()
  })
  
  output$welcome <- renderUI({
    # wait to render until the login is truthy
    req(login$login)
    
    # a crude login card
    div(style = "width: 160px; margin-top: 15px"
        , wellPanel(
          img(src = "avatar.png")
          , span(class = "h3", login$user)
          , p("(", login$role, ")")
          , tags$small(class = "text-muted", tolower(login$email))
          , actionLink("logout", "Logout")
        )
    )
    
  })
  getData <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else {
      # browser()
      
      numfiles = nrow(inFile) 
      kata_csv1 = list()
      #DB <- dbConnect(RSQLite::SQLite(), dbname = "C:/Database/mydb2.db")
      
      for (i in 1:numfiles)
      {
        
        JSON_csv = read.csv(input$file1[[i, 'datapath']], header = TRUE)
        lastrow = nrow(JSON_csv)
        kata_csv1[[i]] = JSON_csv[-lastrow, ]
        
      }
      # browser()
       DB <- dbConnect(RSQLite::SQLite(), dbname = "C:/Database/mydb2.db")
      dbWriteTable(conn = DB, name = "Samples", value = do.call(rbind, kata_csv1),row.names = FALSE, header = TRUE, overwrite = TRUE)
      #dbDisconnect(DB) 
      do.call(rbind, kata_csv1)
      
    }
  })
  output$contents <- renderTable( 
    getData() 
  )
  
  
  output$downloadSample <- downloadHandler(
    filename = function() { 
      paste("Sample-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) { 
      write.csv(getData(), file, row.names=FALSE)   
    })
  updateData <- reactive({
    inFile <- input$file2
    if (is.null(inFile)){
      return(NULL)
    }else {
      # browser()
      numfiles = nrow(inFile) 
      kata_csv2 = list()
      #DB <- dbConnect(RSQLite::SQLite(), dbname = "C:/Database/mydb2.db")
      
      for (i in 1:numfiles)
      {
        
        JSON_csv = read.csv(input$file2[[i, 'datapath']], header = TRUE)
        lastrow = nrow(JSON_csv)
        kata_csv2[[i]] = JSON_csv[-lastrow, ]
        
      }
      # browser()
      DB <- dbConnect(RSQLite::SQLite(), dbname = "C:/Database/mydb2.db")
      dbWriteTable(conn = DB, name = "Results", value = do.call(rbind, kata_csv2),row.names = FALSE, header = TRUE, overwrite = TRUE)
      #dbDisconnect(DB)
      do.call(rbind, kata_csv2)
    }
  })
  output$results <- renderTable( 
    updateData() 
  )
  
  output$downloadResults <- downloadHandler(
    filename = function() { 
      paste("Results-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) { 
      write.csv(updateData(), file, row.names=FALSE)   
    })
  
}
# Run the application 
shinyApp(ui = ui, server = server)

