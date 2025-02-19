# InDepth
<img src="InDepth/InDepthLogo.png" alt="InDepth Logo" style="display: block; margin-left: auto; margin-right: auto; width=300" />

💜 *Used By: UF Women in Computer Science & Engineering (WiCSE)* 💜

InDepth is an attendance sheet parser created specifically for the University of Florida organization Women in Computer Science and Engineering (WiCSE), but has features that allow it to be extended to other organizational use. Future extensions to the project include UI enhancements, further documentation, and options that allow the software’s functions to be more customizable to the context of the user.

## Table of Contents
- [About & History] (#about-amp-history)
- [User Guide & Installation] (#user-guide-amp-installation)
- [How to Use InDepth] (#how-to-use-indepth)
    - [Features & Functionalities] (#features-amp-functionalities)
    - [How to Use] (#how-to-use)
- [How to Contribute] (#how-to-contribute)
- [Tech Details] (#tech-details)

## About & History
InDepth is a project started by Ananya Sista and Elle Strauss to assist WiCSE secretaries (Ananya held the role back in the 2023-2024 academic year) with attendance data entries and parsing. This project was developed as Ananya's and Elle's STA3100 Final Project in the Spring 2024 semester. Not only did this showcase our proficiency in R but also assisted Ananya in calculating various statistics that WiCSE likes to have, such as event attendance, major and year distribution, etc.

InDepth interfaces directly with Google Sheets, and takes in the URL of an attendance Google Sheets workbook that an organization uses and parses through the data of a specified individual attendance sheet. That data is then appended to a total attendance sheet, a sheet containing unique members and their attendance counts, and an email list to upload to the organization ListServ. 

## User Guide & Installation
To use InDepth to parse your attendance sheets, please follow the guide linked to install R and get the application.

## How to Use InDepth
### Features & Functionalities
- Parses through attendance sheets
    - Adds it to the Total Attendance Sheet
    - Updates the Unique Member Sheet
- Detects Errors in Unique UFIDs
    - Prevents updates to sheets until fixed
- Creates bar graphs for event metrics by event type based on: 
    - Percentage of top majors 
    - Percentage of years (freshman, sophomores, juniors, seniors, graduate)
- Creates listServ file of members who input emails to be added to the newsletter
- Generates Table of Members with the Most Event Attendance
    - Users can request a specific amount (Top 5, Top 10, etc.) 

### How to Use
General Tab:
- Under General Field, insert the url to your google sheets workbook.
- Fill out the fields with the worksheet names of Master Worksheet, Unique Members Worksheet, Individual Events Worksheet, and Error Worksheet.
- This step will not change any of your data, it just stores the information to be used across the other tabs. 
- This MUST be done before using any of the other tabs. 
- All fields must be completed. If a worksheet name is entered incorrectly the program will alert you.

Update Attendance Tab:
- Under Update Attendance, enter the Event Worksheet name, which should be the google form responses for the event.
- Then select from the drop down menu the Event Type and click submit.
- Once submitted, it will check if the UFID is valid. If a UFID is invalid, that individual will be under your Error Worksheet and the attendance will not be added as you need to go back and fix those. Once fixed, then resubmit.
- This will update both the Master Worksheet and Unique Members Worksheets. Also the listServ sheet will be created, if it does not exist already, with the unique emails.

Overall Metric Tab:
- User inputs the Category based on the dropdown 
- Then graphs will be displayed with different metrics (year, major, etc.)

Individual Metric Tab: **[Under Construction]**
- Additonal feature to allow specific metrics per each event
- Assist with understanding the success of the event and the distribution of attendance

Top Members Result Tab:
- User inputs the number of members it would like to find
- Then it will generate a list of the x number of members with the highest event attendance


## How to Contribute
To contribute, please fork the repo and work on any present issues. Once an issue is fixed, please submit a PR and wait for approval.

Appreciate the help!

## Tech Details
This project uses R and RShiny to create the functionality and display of the application. Below, we have listed all of the R Libraries used within this project.

R Libraries: 
- RMarkdown
- googlesheets4
- Magrittr
- Shiny
- lubridate
- bslib
- tidyverse
- tinytex
- Shinyjqui
- knitr 
- stringr
- bookdown