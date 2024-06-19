#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Libraries Needed:
library(shiny)
library(bslib)
library(googlesheets4)
library(tidyverse)
library(magrittr)
library(stringr)
library(lubridate)
library(shinyjqui)
library(ggplot2)

# global uses
eventTypes <- list("Social", "Career", "Workshop", "GBM", "Technical", "IM")
uni_col <- c("First Name", "Last Name",	"UFID (Ex: 12345678)",	"Year",	"Major",	"Total",	"Social",	"Career",	"Technical", "GBM",	"Workshop", "IM")
metricTypes <- list("Total", "Career", "Social", "Technical", "GBM", "Workshop", "IM")
metricTypesc <- c("Total", "Career", "Social", "Technical", "GBM", "Workshop", "IM")

# Define UI for application that draws a histogram
ui <- fluidPage(
  # HTML Styles
  tags$style(type = "text/css", "#mainPanel { margin-left: 20px; margin-right: 20px; }"),
  # image style
  tags$style(
    HTML("
         .logo { 
          margin-bottom: -200px; 
         }
    ")
  ),
  # title container style
  tags$head(
    tags$style(
      HTML("
      .title-container {
        display: flex;
        align-items: center;
      }
      ")
    )
  ),
  
  # Title Panel
  titlePanel(div(class='title-container', div(class = "logo", uiOutput("ourLogo")))),
  navset_card_underline(
    nav_panel("Instructions", "How To (Add Instructions)"),
    
    nav_panel("General Fields", 
              mainPanel(
                fluidRow(
                  layout_columns(
                    textInput('workbook',"Enter Attendnace Workbook URL:")
                  )
                ),
                fluidRow(
                  layout_columns(
                    textInput('masterName',"Enter Master Worksheet Name:"),
                    textInput('uniName',"Enter Unique Members Worksheet Name:"),
                    textInput('indiv',"Enter Individual Events Worksheet Name:"),
                    textInput('errors',"Enter Error Worksheet Name:"),
                  )
                ),
                fluidRow(
                  actionButton("submit", "Submit!", width=300),
                ),
              )
            ),
    
    nav_panel("Update Attendance", 
                mainPanel(
                  textInput('eventName',"Enter Event Worksheet Name:", width=500),
                  selectInput('eventType', "Select Event Type", choices=eventTypes, width=500),
                  actionButton("submit2", "Submit!", width=500),
                )
              ),
    
    nav_panel("Overall Metrics", 
                mainPanel(
                  fluidRow(
                    tags$h3("Event Demographic Charts"),
                    layout_columns(
                      selectInput('metrics', "Select Metric Category", choices=metricTypes),
                    ),
                    plotOutput("graph"),
                    plotOutput("graph2"),
                    plotOutput("graph3"),
                  ), 
                )
              ),
    
    nav_panel("Individual Event Metrics", 
                mainPanel(
                  layout_columns(
                    selectInput('individual', "Select an Event", choices=c("empty for now")),
                  ),
                )
              ),
    
    nav_panel("Top Member Results", 
                mainPanel(
                  tags$h3("Generate Top Members"),
                  numericInput("memAmt", "Number of Top Members", 5, min=0), br(),
                  tableOutput("table")
                )
              ),
  ),
)

# FUNCTIONS
# append data to master
writeMaster <- function(url, e, mName, eType, eName){
  sheet_append(url, data.frame(eType, eName, paste0("Attedance Count: ", nrow(e))), sheet=mName)
  sheet_append(url, e, sheet=mName)
  sheet_append(url, data.frame(""), sheet=mName)
  
}

# find errors with UFID
findErrors <- function(url, e, errName){
  hasErr <- FALSE
  for(r in 1:nrow(e)) {
    if(length(str_view(e$`UFID  (Ex: 12345678)`[r], "\\d{8}")) != 1) {
      incorrect <- e[r,]
      sheet_append(url, incorrect, sheet=errName)
      hasErr <- TRUE
    }
  }
  
  return(hasErr)
}

# create Uniques sheet
parseUniques <- function(url, uName, u, e){
  print("entered parser")
  test <- data.frame(NA, NA, NA, NA, NA, 0, 0, 0, 0, 0, 0, 0)
  print("test data set")
  colnames(test) <- uni_col
  print("test columns set")
  range_write(url, test, sheet=uName, range = "A1", col_names =TRUE)
  print("on to for loop")
  for(n in 1:nrow(e)){
    index <- which(u$`UFID (Ex: 12345678)`== e[[n, 'UFID  (Ex: 12345678)']])
    if(length(index) == 0){
      member <- data.frame(e[n, 'First Name'], 
                                   e[n, 'Last Name'], 
                                   e[n, 'UFID  (Ex: 12345678)'],
                                   e[n, 'Year'],
                                   e[n, 'Major'],
                                   0,
                                   0,
                                   0,
                                   0,
                                   0,
                                   0,
                                   0)
      colnames(member) <- uni_col
      member['Total'] = 1
      member[e$type[[n]]] = 1
      sheet_append(url, member, sheet=uName)
    } else {
      member <- data.frame(u[index[1], 'First Name'], 
                           u[index[1], 'Last Name'], 
                           u[index[1], 'UFID (Ex: 12345678)'],
                           u[index[1], 'Year'],
                           u[index[1], 'Major'],
                           u[index[1], 'Total'],
                           u[index[1], 'Social'],
                           u[index[1], 'Career'],
                           u[index[1], 'Technical'],
                           u[index[1], 'GBM'],
                           u[index[1], 'Workshop'],
                           u[index[1], 'IM'])
      colnames(member) <- uni_col
      member['Total'] = member['Total'] + 1
      member[e$type[[n]]] = member[e$type[[n]]] + 1
      range_write(url, member, sheet=uName, range=paste0("A",1+index[1]), col_names=FALSE)
    }
  }
  print("for loop completed")
}

# top member
topMem <- function(amount, u){
  top <- u %>% arrange(desc(Total))
  return (top %>% slice_head(n=amount) %>% select("First Name", "Last Name", "Total"))
}

# email listserv
emailList <- function(mast, url){
  colnames(mast) <- c("Event Type", "Timestamp", "First Name", "Last Name", "UFID", "Hear", "Return", "Email", "Pic", "Year", "Major")
  emailList <- mast %>% filter(!is.na(Email)) %>% distinct(Email) %>% inner_join(mast, by = "Email") %>%  select(Email, `First Name`, `Last Name`) %>% distinct(Email, `First Name`, `Last Name`)
  #colnames(emailList) <- c("email", "first name", "last name")
  sheet_write(emailList, url, sheet="ListServ")
}

# calculating axis data
calcAxis <- function(metric, u){
  eventType <- NA
  if(metric=="Total"){
    eventType <- colSums(u[, metricTypesc[2:length(metricTypesc)]])
  }
  year <- loopThrough(u, "Year", metric)

  major <- loopThrough(u, "Major", metric)

  return(list(eventType, year, major))
}

# more calculations for calculating member attendance counts
loopThrough <- function(u, crit, metric){
  unis <- unique(u[[crit]])
  mat <- data.frame(unis, 0)
  colnames(mat) <- c(crit, metric)
  for(i in 2:nrow(u)) {
    if (is.na(u[[crit]][i])) {
      mat[1, 2] = mat[1, 2] + u[[metric]][i]
    } else {
      for(j in 2:length(unis)) {
        if (mat[j, 1] == u[[crit]][i]) {
          mat[j, 2] = mat[j, 2] + u[[metric]][i]
        }
      }
    }
  }
  return(mat)
}

# Define server logic required to draw a histogram
server <- function(input, output) {
  # globals in server that do not need to be rewritten each time server loads
  initialSubmit <- reactiveVal(FALSE)
  url <- reactiveVal("")
  master <- reactiveVal(data.frame())
  mName <- reactiveVal("")
  unique <- reactiveVal(data.frame())
  uName <- reactiveVal("")
  errors <- reactiveVal("")
  indName <- reactiveVal("")
  indEvents <- reactiveVal(data.frame())
  
  # observing if general fields are good
  observeEvent(input$submit, {
    if (is.null(input$masterName) || input$masterName == "" || is.null(input$uniName) || input$uniName == "" || is.null(input$errors) || input$errors == "" || is.null(input$indiv) || input$indiv == "" || is.null(input$workbook) || input$workbook == ""){ 
      # any input is empty
      showNotification("One or more of the fields are empty. Please ensure that are fields are completed in order to submit.", duration=15)
      initialSubmit(FALSE)
    } else if (!(input$masterName %in% sheet_names(input$workbook)) | !(input$uniName %in% sheet_names(input$workbook)) | !(input$errors %in% sheet_names(input$workbook)) | !(input$indiv %in% sheet_names(input$workbook))) { 
      # ensures sheet name exists in the workbook
      print("throw error")
      showNotification("One or more of the sheet names you entered does not exist in the workbook. Please correct the enteries and retry.", duration=15)
      initialSubmit(FALSE)
    } else {
      # all input has been validated
      initialSubmit(TRUE)
      print("correct sheet names")
      # sets URL value
      url(input$workbook)
      # sets master sheet string name and data frame
      mName(input$masterName)
      master(read_sheet(url(), sheet=mName()))
      # sets unique sheet string name and data frame
      uName(input$uniName)
      unique(read_sheet(url(), sheet=uName()))
      # sets invalid UFID sheet string name
      errors(input$errors)
      # sets individual events sheet string name and dataframe
      indName(input$indivName)
      indEvents(read_sheet(url(), sheet=indName()))
      # updates list serv
      emailList(master(), url())
      showNotification("ListServ has been updated!")
    }
    
    print(initialSubmit)
  })
  
  # observing if update attendance is called
  observeEvent(input$submit2, {
    print(url())
    print("submit 2")
    if (nchar(input$eventName) == 0) {
      showNotification("Event Sheet Name is empty. Please ensure that the field are completed in order to submit.", duration=15)
    } else if (initialSubmit()){
      print(sheet_names(url()))
      if(!(input$eventName %in% sheet_names(url()))) {
        print("error 2")
        showNotification("The sheet name entered for Event Sheet does not exist in your workbook. Please correct the enteries and retry.", duration=15)
      } else {
        event <- read_sheet(url(), sheet=input$eventName)
        if(findErrors(url(), event, errors())){
          showNotification("There are errors with your UFIDs! Please correct them and retry. UFIDs should contain only numeric values and be 8 digits long.", duration=10)
        } else {
          event <- event %>% mutate(type=input$eventType) %>% relocate(type, .before=Timestamp) %>% mutate(`UFID  (Ex: 12345678)`=as.character(`UFID  (Ex: 12345678)`))
          writeMaster(url(), event, mName(), input$eventType, input$eventName)
          parseUniques(url(), uName(), unique(), event)
          master(read_sheet(url(), sheet=mName()))
          emailList(master(), url())
          showNotification("ListServ has been updated!")
        }
      }
    } else {
      print("error 1")
      showNotification("Error: General Fields have not be filled out saved.", duration=10)
    }
  })
  
  # graphs for overall metrics
  observe({
    if (initialSubmit()){
      if(input$metrics == "Total"){
        output$graph <- renderPlot({
          results <- calcAxis(input$metrics, unique())
          print("r")
          r <- as.data.frame(results[1], col.names = list("Total"))
          print("df")
          r <- rownames_to_column(r, var="event")
          print("rn")
          print(input$metrics)
          
          ggplot(r, aes(y=Total, x=event, fill=event)) + geom_bar(stat='identity')
        }, height=400, width=600)
        
      }
      output$graph2 <- renderPlot({
        results <- calcAxis(input$metrics, unique())
        print("r")
        r <- as.data.frame(results[2])
        colnames(r) <- c("Year", "Number of Attendees")
        print("df")
        print("rn")
        print(input$metrics)
        
        ggplot(r, aes(y=`Number of Attendees`, x=Year, fill=Year)) + geom_bar(stat='identity')  + ggtitle(paste0("Breakdown of Year by ", input$metrics, " Event"))
      }, height=400, width=600)
      
      output$graph3 <- renderPlot({
        results <- calcAxis(input$metrics, unique())
        r <- as.data.frame(results[3])
        colnames(r) <- c("Major", "Number of Attendees")
        
        
        ggplot(r, aes(y=`Number of Attendees`, x=Major, fill=Major)) + geom_bar(stat='identity') + ggtitle(paste0("Breakdown of Major by ", input$metrics, " Event"))
      }, height=400, width=600)
    }
  })
  
  # Top Member Stats
  observeEvent(input$memAmt, {
    if (initialSubmit()){
      unique(read_sheet(url(), sheet=uName()))
      tops <- topMem(input$memAmt, unique())
      output$table <- renderTable(tops)
    } else {
      showNotification("Error: General Fields has not be entered or saved to proceed", duration=15)
    }
  })
  
  # logo renderings
  output$logo <- renderImage({
    filename <- normalizePath(file.path('./', paste0('InDepthLogo', '.png', sep='')))
    list(src=filename, width=200, height=200, alt="Logo")}, 
    deleteFile = FALSE
  )
  output$ourLogo <- renderUI({
    imageOutput("logo", width = "200px")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
