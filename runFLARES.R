#This script will check wether the required packages are installed on your system. 
#If required packages are missing you will need an internet connection.
source("scripts/check_packages.R")
check.packages()

library(shiny)
runApp()
