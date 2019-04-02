library(shiny)
library(shinyjs)
require(magrittr)
require(purrr)
require(dplyr)
require(datimvalidation)
require(datapackr)
require(scales)
require(openxlsx)
options(shiny.maxRequestSize = 100 * 1024 ^ 2)
options("baseurl" = "http://127.0.0.1:8080/")

DHISLogin <- function(baseurl, username, password) {
  httr::set_config(httr::config(http_version = 0))
  url <- URLencode(URL = paste0(getOption("baseurl"), "api/me"))
  #Logging in here will give us a cookie to reuse
  r <- httr::GET(url,
                 httr::authenticate(username, password),
                 httr::timeout(60))
  if (r$status != 200L) {
    return(FALSE)
  } else {
    me <- jsonlite::fromJSON(httr::content(r, as = "text"))
    options("organisationUnit" = me$organisationUnits$id)
    return(TRUE)
  }
}



shinyServer(function(input, output, session) {
  
  ready <- reactiveValues(ok = FALSE) 
  
  user_input <- reactiveValues(authenticated = FALSE, status = "")
  
  observeEvent(input$login_button, {
    user_input$authenticated <-DHISLogin(input$server,input$user_name,input$password)
  })   
  
  output$ui <- renderUI({
    
    if ( user_input$authenticated == FALSE ) {
      ##### UI code for login page
      fluidPage(
        fluidRow(
          column(width = 2, offset = 5,
                 br(), br(), br(), br(),
                 uiOutput("uiLogin"),
                 uiOutput("pass")
          )
        )
      )
    } else {
      fluidPage(
        tags$head(tags$style(".shiny-notification {
                             position: fixed;
                             top: 10%;
                             left: 33%;
                             right: 33%;}")),
        sidebarLayout(
          sidebarPanel(
            shinyjs::useShinyjs(),
            fileInput(
              "datapack",
              "Choose DataPack (Must be XLSX!):",
              accept = c(
                "application/xlsx",
                ".xlsx"
              )
            ),
            tags$hr(),
            fileInput(
              "sitetool",
              "Choose SiteTool (Must be XLSX!):",
              accept = c(
                "application/xlsx",
                ".xlsx"
              )
            ),
            tags$hr(),
            downloadButton("downloadFlatPack", "Download Comparison")
          ),
          mainPanel(tabsetPanel(
            type = "tabs",
            tabPanel("Messages",   tags$ul(uiOutput('messages'))),
            tabPanel("Indicator summary", dataTableOutput("indicator_summary"))
          ))
        ))
  }
})
  
  # password entry UI componenets:
  #   username and password text fields, login button
  output$uiLogin <- renderUI({
    
    wellPanel(fluidRow(
      img(src='pepfar.png', align = "center"),
      h4("Welcome to the DataPack-SiteTool Comparison App. Please login with your DATIM credentials:")
    ),
    fluidRow(
      textInput("user_name", "Username: ",width = "600px"),
      passwordInput("password", "Password:",width = "600px"),
      actionButton("login_button", "Log in!")
    ))
  })
  
  compare<-function() {
    
    if (!user_input$authenticated) {return(NULL)}
    
    dp_in <- input$datapack
    st_in <- input$sitetool
    messages<-""
    
    if ( is.null( dp_in ) ) return( NULL )
    
    if ( is.null( st_in ) ) return( NULL )
    
    messages<-list()
    
    withProgress(message = 'Parsing files...', value = 0,{
      
      incProgress(0.5, detail = ("Running comparison analysis"))
      d<-tryCatch({
        
        datapackr::comparePacks(datapack_path = dp_in$datapath,
                                sitetool_path =st_in$datapath )},
        error = function(e){
          return(e)
        })
      
      
    })
    
    return(d)
    
  }
  
  compare_targets <- reactive({ compare() })
  
  output$indicator_summary<-renderDataTable({
    
    compare_targets_out <-compare_targets()
    
    if (!inherits(compare_targets_out,"error") ){
      compare_targets_out  %>% 
        purrr::pluck(.,"summary.indicators")
    } else {
      NULL
    }
  })
  
  
  output$downloadFlatPack <- downloadHandler(
    
    filename = function() {
      
      datapack_name <- compare_targets() %>%  purrr::pluck("datapack_name")
      prefix <- "DataPack_SiteTool_Comparison_"
      date<-format(Sys.time(),"%Y%m%d_%H%M%S")
      paste0(paste(prefix,datapack_name,date,sep="_"),".xlsx")
    },
    content = function(file) {
      
      download_data <- compare_targets()
      
      wb <- openxlsx::createWorkbook()
      
      sheetNames <- c("Data Pack Data", "Site Tool Data", "Comparison", "Diffs",
                      "Category Summary", "Indicator Summary")
      
      invisible(sapply(sheetNames, function(x) openxlsx::addWorksheet(wb, sheetName = x)))
      
      openxlsx::writeData(wb, sheet = 1, x = download_data$datapack_data)
      openxlsx::writeData(wb, sheet = 2, x = download_data$sitetool_data)
      openxlsx::writeData(wb, sheet = 3, x = download_data$comparison)
      openxlsx::writeData(wb, sheet = 4, x = download_data$diffs)
      openxlsx::writeData(wb, sheet = 5, x = download_data$summary.categories)
      openxlsx::writeData(wb, sheet = 6, x = download_data$summary.indicators)
      
      openxlsx::write.xlsx(wb, file = file)
      
    })
  
  
  output$messages <- renderUI({
    
    vr<-compare_targets()
    messages<-NULL
    
    if ( is.null(vr) ) {
      return(NULL)
    }
    
    if ( inherits(vr,"error") ) {
      return( paste0("ERROR! ",vr$message) )
      
    } else {
      tags$li('Comparison analysis complete. Click "Download Comparison" for a detailed report' )
    }
    
  })
  })
