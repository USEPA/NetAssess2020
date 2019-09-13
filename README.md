# NetAssess2020
Tools for the 2020 5-year Ambient Air Monitoring Network Assessments

Based on the NetAssess app from LADCO (https://github.com/LADCO/NetAssessApp/)

Author: Ben Wells, Office of Air Quality Planning and Standards, U.S. Environmental Protection Agency

Latest Update: September 13, 2019

# Instructions for users
0. You must have R installed on your local machine (if you don't have R, you can download it from cran.r-project.org)

1. Clone or download this repository to your local drive. Unzip the .zip file. Start an R session.

2. Make sure the following R packages are installed: shiny, data.table, deldir, fields, plyr, reshape2, rgdal, rgeos, RSQLite, sp

   To install these packages, copy the following into the command prompt:
   
   install.packages(c('shiny','data.table','deldir','fields','plyr','reshape2','rgdal','rgeos','RSQLite','sp'))
   
3. Change your working directory to the one where you downloaded the files: setwd("C:/Path_to_your/downloaded_files/")

4. Start the app: library(shiny); runApp();
   
# Contact info
Email: wells.benjamin@epa.gov

Phone: 919-541-7507
