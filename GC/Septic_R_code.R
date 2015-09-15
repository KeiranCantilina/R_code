# Septic_R_code.R
# Last edited 08/04/2015 by Allison Truhlar
#Modified by KKC 08/19/2015

# This code takes in raw data collected at the septic sites and outputs: 
# a. A graph of gas concentration vs time for each gas type and chamber
# b. The flux of CO2, CH4, and N2O, as calculated by both a quadratic and linear fit, at each chamber.

# Prior to running this code, please make sure you have done the following: 

# 1. Use the "Batch_descriptions" file in 2015_data/Raw data to identify the batch file you want to work with,
# and the homeowner/sampling date you will work with inside that batch.  Note that some sampling campaigns will
# span two batches.

# 2. Find or create a campaign folder called yyyymmdd in the appropriate homeowner(s) data folder(s) in Dropbox. 
# All the required data sheets and data outputs will go here.

# 3. Using the data from the chosen batch file, fill each of the needed templates and save them with appropriate 
# names in the campaign folder. There is one chamber heights template, one standard template per gas, and one 
# gas data template. 

      # Note that the gas data template needs to be filled out three times, one per type of gas. In the batch file
      # the first set of columns with non-zero numbers is N2O, the second set is CH4, and the third set is CO2.  
      # Within a two column set, the first column is the area under the peak (what we want) and the second column
      # is the time at the peak.  If you ever want to double check this, look in the "Headers" file in the Raw
      # Data folder.

      # If the sampling campaign spans two batches, input the standard curve from each batch in two different 
      # columns, called Area1 and Area2. Indicate this as 1 or 2, respectively, in the gas templates under the
      # "std_curve_num" column.

# 4. Once you have extracted all the raw data from a batch file and saved it to the appropriate campaign folders,
# mark that the batch file is "transcribed" in the "Batch_descriptions" file.

# 5. If it hasn't been done already, transcribe the chamber heights for the campaign (data sheets in the white 
# binder) into the chamber heights template and save in the campaign folder.

# 6. Once you have run this script to process the data from a campaign, mark that the data processing is complete 
# on the second tab of the "Batch_descriptions" file.

# 7. As you process the data for each campaign, take a look at the graphs that are generated for the concentration 
# of gas over time.  Pay special attention to the CO2 graph.  This graph, if none of the others, should show constant
# accumulation of CO2 over time.  If there are any notable decreases in concentration, double check the data has been
# transcribed correctly from the batch files.

#Clear workspace - always do this before processing data, just in case
rm(list=ls())

#Note: user has to copy in own working directory, ending in Data/.  Windows users change \ to \\
#Allison, Mac: "/Users/atruhlar/Dropbox/Cornell/Septic_2015/2015_data"
#Allison, PC:"C:\\Users\\Owner\\Dropbox\\Cornell\\Septic_2015\\2015_data\\"
dir <-"C://Users//Ross//Desktop//R_code//GC//"
setwd(dir)

library(gridExtra)

gas <- readline(prompt="Enter gas to be analyzed (all caps): ")
date <- readline(prompt="Enter sampling date in format mmdd: ")
name <- readline(prompt="Enter the name of the homeowner (lowercase): ")

# See the comments in individual functions for more information about each.

# Note: this function will ask you to indicate whether you are using a Mac (M) or Windows
# (W) operating system
source("gas.directory.fun.R")
directory <- gas.directory.fun(dir,gas,date,name)

#standard_data <- read.csv(directory[1])
source("standard.curve.fun.R")
#standard_coeffs <- standard.curve.fun(standard_data)
standard_coeffs <- read.csv(paste(dir,"2014_standard_coeffs.csv",sep=""))

chamber_heights <- read.csv(directory[2])
source("chamber.vol.fun.R")
chamber_volumes <- chamber.vol.fun(chamber_heights)	# Returns each chamber volume in liters

sample_data <- read.csv(directory[3])
source("conc.fun.R")
conc_data <- conc.fun(sample_data,standard_coeffs,chamber_volumes, directory, gas)
write.csv(directory[8],x=conc_data)

source("flux.fun.R")
flux_data <- flux.fun(conc_data,standard_coeffs,chamber_volumes,directory, gas)
write.csv(directory[6], x=flux_data)

source("flux.scale.fun.R")
scaled_flux_data <- flux.scale.fun(flux_data,chamber_volumes,dir,gas,date,name)
write.csv(directory[7], x=scaled_flux_data)


