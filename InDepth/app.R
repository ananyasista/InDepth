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
  tags$style(type = "text/css", "#mainPanel { margin-left: 20px; margin-right: 20px; }"),
  titlePanel("In Depth"),
  tags$h3("Upload Attendance"),
  # Upload Attendance Section
  mainPanel(
    
  fluidRow(
    layout_columns(
      textInput('workbook',"Enter Attendnace Workbook URL:")
    )
  ),
  fluidRow(
      layout_columns(
                     textInput('masterName',"Enter Master Worksheet Name:"),
                     textInput('eventName',"Enter Event Worksheet Name:"),
                     
      )
  ),
  fluidRow(
    layout_columns(
                   textInput('uniName',"Enter Unique Members Worksheet Name:"),
                   textInput('errors',"Enter Error Worksheet Name:"),
                   
    )
  ),
  fluidRow(
    layout_columns(
      selectInput('eventType', "Select Event Type", choices=eventTypes),
    )
  ),
  fluidRow(
    actionButton("submit", "Submit!", width=300),
  ),
), br(),
mainPanel(
  fluidRow(br(),tags$h3("ListServ Emails"),
           actionButton("emails", "Create ListServ Email List"), br(),
  ),
),
mainPanel(
  fluidRow(tags$h3("Generate Top Members"),
           numericInput("memAmt", "Number of Top Members", 5, min=0), br(),
           tableOutput("table")
  ),
),
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
  
  
  # Other Sections
  


)

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
  test <- data.frame(NA, NA, NA, NA, NA, 0, 0, 0, 0, 0, 0, 0)
  colnames(test) <- uni_col
  range_write(url, test, sheet=uName, range = "A1", col_names =TRUE)
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
  
}

topMem <- function(amount, u){
  top <- u %>% arrange(desc(Total))
  return (top %>% slice_head(n=amount) %>% select("First Name", "Last Name", "Total"))
}

emailList <- function(mast, url){
  colnames(mast) <- c("Event Type", "Timestamp", "First Name", "Last Name", "UFID", "Hear", "Return", "Email", "Pic", "Year", "Major")
  
  emailList <- mast %>% filter(!is.na(Email)) %>% distinct(Email) %>% inner_join(mast, by = "Email") %>%  select(Email, `First Name`, `Last Name`) %>% distinct(Email, `First Name`, `Last Name`)
  #colnames(emailList) <- c("email", "first name", "last name")
  sheet_write(emailList, url, sheet="ListServ")
}


calcAxis <- function(metric, u){
  eventType <- NA
  if(metric=="Total"){
    eventType <- colSums(u[, metricTypesc[2:length(metricTypesc)]])
  }
  year <- loopThrough(u, "Year", metric)

  major <- loopThrough(u, "Major", metric)

  return(list(eventType, year, major))
}


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


createGraph <- function(gdata, type){
    ggplot(gdata, aes(y=Total, x=event, fill=event)) + geom_bar(stat='identity')
    return(ggplot(gdata, aes(y=Total, x=event, fill=event)) + geom_bar(stat='identity'))

}


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$logo <- renderImage({
    outfile <- tempfile(fileext-'.png')
  })
    
  # on submission of intial URLs
    observeEvent(input$submit, {
      master <- read_sheet(input$workbook, sheet=input$masterName)
      event <- read_sheet(input$workbook, sheet=input$eventName)
      unique <- read_sheet(input$workbook, sheet=input$uniName)
      if(findErrors(input$workbook, event, input$errors)){
        showNotification("There are errors with your UFIDs! Please correct them and retry. UFIDs should contain only numeric values and be 8 digits long.", duration=10)
        
      } else {
        event <- event %>% mutate(type=input$eventType) %>% relocate(type, .before=Timestamp) %>% mutate(`UFID  (Ex: 12345678)`=as.character(`UFID  (Ex: 12345678)`))
        writeMaster(input$workbook, event, input$masterName, input$eventType, input$eventName)
        parseUniques(input$workbook, input$uniName, unique, event)
        master <- read_sheet(input$workbook, sheet=input$masterName)
      }
      
      if(input$metrics == "Total"){
        output$graph <- renderPlot({
          results <- calcAxis(input$metrics, unique)
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
        results <- calcAxis(input$metrics, unique)
        print("r")
        r <- as.data.frame(results[2])
        colnames(r) <- c("Year", "Number of Attendees")
        print("df")
        print("rn")
        print(input$metrics)
        
        ggplot(r, aes(y=`Number of Attendees`, x=Year, fill=Year)) + geom_bar(stat='identity')  + ggtitle(paste0("Breakdown of Year by ", input$metrics, " Event"))
      }, height=400, width=600)
      
      output$graph3 <- renderPlot({
        results <- calcAxis(input$metrics, unique)
        r <- as.data.frame(results[3])
        colnames(r) <- c("Major", "Number of Attendees")

        
        ggplot(r, aes(y=`Number of Attendees`, x=Major, fill=Major)) + geom_bar(stat='identity') + ggtitle(paste0("Breakdown of Major by ", input$metrics, " Event"))
      }, height=400, width=600)
      
      observeEvent(input$emails, {
        master <- read_sheet(input$workbook, sheet=input$masterName)
        emailList(master, input$workbook)
      })
      
      observeEvent(input$memAmt, {
        unique <- read_sheet(input$workbook, sheet=input$uniName)
        tops <- topMem(input$memAmt, unique)
        output$table <- renderTable(tops)
      })
      
    })
  
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
