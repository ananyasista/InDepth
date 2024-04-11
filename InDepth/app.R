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

# Define UI for application that draws a histogram
ui <- page_sidebar(
  title = "In Depth",
  sidebar = sidebar("sidebar"),
  # Upload Attendance Section
  "Upload Attendance",
  layout_columns(col_widths = 6, 
                 textInput('master',"Enter Master Attendance Workbook URL:"),
                 textInput('event',"Enter Event Attendance Sheet URL:"),
  ),
  layout_columns(
    textInput('eventName',"Enter Event Name:"),
    dateInput('eventDate', "Enter the Event Date:"),
    selectInput('eventType', "Select Event Type", choices=list("Social"=1, "Career"=2, "Workshop"=3, "GBM"=4, "Technical"=5)),
  ),
  submitButton("Submit Attendance"),
  # Other Sections
  card("Unique Members Data"),
  card("Visuals"), 
  card("Emails to List Server")
)

# Define server logic required to draw a histogram
server <- function(input, output) {

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
