# InDepth Installation and Setup Guide
### Table of contents:
1. [Installing R](#installing-r)
    1. [Windows](#for-windows)
    2. [MacOS](#for-macos)
2. [Cloning InDepth](#clone-the-indepth-repository)
3. [Installing Dependencies](#installing-dependencies)
    1. [Windows](#for-windows-1)
    2. [MacOS](#for-macos-1)
4. [Setting Up Google Sheets Credentials](#setting-up-google-sheets-credentials)
5. [Launching InDepth](#launching-indepth)

&nbsp;  

# Installing R
## For Windows
Go to this link: https://mirror.las.iastate.edu/CRAN/, and install R for Windows. Follow all default instructions and settings.

## For MacOS
Go to this link: https://mirror.las.iastate.edu/CRAN/, and install R for MacOS based on the kind of processor you have (M-series or Intel). Follow all default instructions and settings.

&nbsp;  

# Clone the InDepth Repository
In the location of your choosing, clone the InDepth repository.
```
git clone https://github.com/ananyasista/InDepth.git
```
>**Note:** 
>Make sure this directory is easily accessible from the terminal! We recommend taking note of the directory path to the where you cloned the repo.

&nbsp;  

# Installing Dependencies
## For Windows
Open the terminal application of your choice (for this guide, we used Windows Terminal) with administrator priveleges.

To open an R Terminal session, run the command:
```
R
```
or
```
R.exe
```
Both should achieve the same thing. If neither works, see if R needs to be added to your system PATH variable.

An R Terminal session should now be created. Run the following command to install needed dependencies:
```
install.packages(c(“googlesheets4”, “shiny”, “tidyverse”, “shinyjqui”, “gargle”))
```
After hitting enter, a dialog will appear asking you to select a CRAN mirror for use in the session. Select any (though we recommend one of ones that start with "USA").

Once the dependencies finish installing, continue to [Setting Up Google Sheets Credentials](#setting-up-google-sheets-credentials). Keep the R Terminal session open!

## For MacOS
Open the terminal, and install R with homebrew.
```
brew install r
```

Next, open an R Terminal session by entering:
```
R
```
An R Terminal session should now be created. Run the following command to install needed dependencies:
```
install.packages(c(“googlesheets4”, “shiny”, “tidyverse”, “shinyjqui”, “gargle”))
```
After hitting enter, a prompt will appear asking you to select a CRAN mirror for use in the session. Select any (though we recommend one of ones that start with "USA", i.e. "65").

Once the dependencies finish installing, continue to [Setting Up Google Sheets Credentials](#setting-up-google-sheets-credentials). Keep the R Terminal session open!

&nbsp;  

# Setting Up Google Sheets Credentials
Since InDepth interfaces with and directly edits Google Sheets, the first time you use the application you will have to set up your Google Credentials. Please make sure the credential is set up with whatever Google Account you will be using to access the attendance google forms and edit attendance sheets.

Navigate to the directory where InDepth is located, and into the directory InDepth that is inside of that. Inside this folder there should be two files: app.R and InDepthLogo.png. To change directories within the R Terminal, use the following command:
```
setwd(“\[your path]\InDepth\InDepth”)
```
To check that your working directory is correct use the following:
```
getwd()
```
Then, launch the InDepth application.
```
shiny::runApp('app.R')
```
Once the application is launched, fill out all fields in the "general" tab, and hit "submit".

Go back to the terminal. Enter "1".

Back in the browser, choose the Google account you want to authenticate for use with InDepth. Ensure all boxes are checked on the page that comes next.

You can now exit out of the R Terminal.
```
q()
n
```
(the n is to stop R from saving the workspace image).

&nbsp;  

# Launching InDepth
Now that everything is set up, use the following instructions to launch InDepth everytime you want to use it.

In your terminal, navigate to the directory where you cloned InDepth, and into the folder InDepth. Your directory path should now look like:
```
\[your path]\InDepth\InDepth
```
Within this folder, there should be two files: app.R and InDepthLogo.png.

In the terminal run:
```
Rscript app.R
```
>**Note:**
>If Rscript is not recognized as a command, you might have to add Rscript to your system PATH variable.

To open InDepth in your browser, click (or ctrl+click) on the link on the last line of the terminal (should come after the words "Listening on"), or enter the same URL in your web browser.

That should get InDepth up and running! To exit out of InDepth, close the tab in your browser, switch back to the terminal, and enter crtl+c.