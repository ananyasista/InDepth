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

# global uses
eventTypes <- list("Social", "Career", "Workshop", "GBM", "Technical")
uni_col <- c("First Name", "Last Name",	"UFID (Ex: 12345678)",	"Year",	"Major",	"Total",	"Social",	"Career",	"Technical", "GBM",	"Workshop")

# Define UI for application that draws a histogram
ui <- page_sidebar(
  title = "In Depth",
  sidebar = sidebar("sidebar"),
  # Upload Attendance Section
  "Upload Attendance",
  layout_columns(textInput('workbook',"Enter Attendnace Workbook URL:")
  ),
  layout_columns(col_widths=6,
    textInput('masterName',"Enter Master Worksheet Name:"),
    textInput('eventName',"Enter Event Worksheet Name:"),
    textInput('uniName',"Enter Unique Members Worksheet Name:"),
    textInput('errors',"Enter Error Worksheet Name:"),
  ),
  layout_columns(
    selectInput('eventType', "Select Event Type", choices=eventTypes),
  ),
  actionButton("submit", "Submit!"),
  # Other Sections
  card("Unique Members Data"), 
  card("Visuals"), 
  card("Emails to List Server")
)

# append data to master
writeMaster <- function(url, e, mName, eType, eName){
  sheet_append(url, data.frame(eType, eName), sheet=mName)
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
  test <- data.frame(NA, NA, NA, NA, NA, 0, 0, 0, 0, 0, 0)
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
                           u[index[1], 'Workshop'])
      colnames(member) <- uni_col
      member['Total'] = member['Total'] + 1
      member[e$type[[n]]] = member[e$type[[n]]] + 1
      range_write(url, member, sheet=uName, range=paste0("A",1+index[1]), col_names=FALSE)
    }
    
  }
  
}


# Define server logic required to draw a histogram
server <- function(input, output) {
    
  # on submission of intial URLs
    observeEvent(input$submit, {
      master <- read_sheet(input$workbook, sheet=input$masterName)
      event <- read_sheet(input$workbook, sheet=input$eventName)
      unique <- read_sheet(input$workbook, sheet=input$uniName)
      if(findErrors(input$workbook, event, input$errors)){
        showNotification("There are errors with your UFIDs! Please correct them and retry. UFIDs should contain only numeric values and be 8 digits long.", duration=10)
        
      } else {
        event <- event %>% mutate(type=input$eventType) %>% relocate(type, .before=Timestamp) %>% mutate(`UFID  (Ex: 12345678)`=as.character(`UFID  (Ex: 12345678)`))
        writeMaster(input$workbook, event, "TA", input$eventType, "ex")
        parseUniques(input$workbook, input$uniName, unique, event)
        master <- read_sheet(input$workbook, sheet=input$masterName)
        master
      }
      
      
    })
  
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
