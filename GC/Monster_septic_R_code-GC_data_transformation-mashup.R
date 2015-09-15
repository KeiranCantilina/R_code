#Script for automating data transformation from S&W Lab's GC
#Created by Keiran Cantilina
#Lasted edited by Keiran Cantilina on 8/19/2015 

#This script should magically take raw GC files and format them correctly for processiong
#by Allison's Septic_R_code.R script, provided you fill out the description file correctly.
#NOTE: Chamber measurements must be manually added

# Clear workspace
rm(list=ls())


#Load libraries if just starting
library(reshape2)
library(plyr)
library(ggplot2)
library(gridExtra)


#Set working R directory
#Keiran: "C://Users//Ross//Desktop//R_code//GC//"
dir <- "C://Users//Ross//Desktop//R_code//GC//"
setwd(dir)


#Set working batch directory
#Keiran: "C://Users//Ross//Desktop//Septic 2015//Septic 2015//2015_data//Raw data"
bdir <- "C://Users//Ross//Desktop//Septic 2015//Septic 2015//2015_data//Raw data"


#Set working template directory
#Keiran: "C://Users//Ross//Desktop//Septic 2015//Septic 2015//2015_data//Templates"
tdir <- "C://Users//Ross//Desktop//Septic 2015//Septic 2015//2015_data//Templates"


#Load description file
descript_file <- "\\Batch_descriptions.csv"
batch_descript <- (read.csv(paste(dir,descript_file,sep="")))


#Load Headers
headers <- read.csv(paste(bdir,"//Headers.csv", sep=""))


#Load templates
template <- read.csv(paste(tdir,"//Template-yyyymmdd_septic-homeowner_GAS.csv",sep=""))
template_co2 <- read.csv(paste(tdir,"//Template-yyyymmdd_septic-homeowner_CO2_standard.csv",sep=""))
template_n2o <- read.csv(paste(tdir,"//Template-yyyymmdd_septic-homeowner_N2O_standard.csv",sep=""))
template_ch4 <- read.csv(paste(tdir,"//Template-yyyymmdd_septic-homeowner_CH4_standard.csv",sep=""))

#Load Batch and add headers
gas <- 6 #sets first gas to n20
k <- 1 #Counts iterations of the big loop
i <- 1 #Indices
while (i<=nrow(batch_descript)&gas!=15){#Loop to evaluate and execute batch description instructions until the number of processed batches is equal to the number of rows in batch_descript
  h <- 1
  batch_ID <- batch_descript$Batch[i]#Read batch name
  batch_ID <- paste(batch_ID,".csv",sep="")
  batch_rawdata <- (read.csv(paste(bdir,batch_ID,sep="//"),header=FALSE))#Open batch file of given name
  colnames(batch_rawdata) <- headers$Value #Import headers
  #batch_rawdata[is.na(batch_rawdata)] <- 0 #Use if you need to turn NAs into 0s
  
  
  #Move area data into vector
  date <- batch_descript$Collection_date[i]
  owner <- batch_descript$House_owner[i]
  begin1 <- batch_descript$Range_begin[i]
  end1 <- batch_descript$Range_end[i]
  set_quant1 <- (end1-begin1+1) #keeps track of how many data points of the set are in the batch (the +1 is to mitigate fencepost error)
  n2o_area1 <- batch_rawdata[begin1:end1,gas] #Creates vector containing gas values
  std_curve_index <- rep(h,set_quant1) #Sets standard curve index and creates vector
  j <- i #sets placeholder index with i's value (because i can change and j keeps track of what i was before)
  n2o_area <- n2o_area1 #n2o_area is a common variable between loops
  set_quant <- set_quant1
  
  
  #Go to next item with same owner if data isn't complete (remember, this is still within the previous while loop)
  while(set_quant<24) {#there are 24 data points for each set
    h <- h+1 #advance standard curve index
    i <- i+1 #advance search index by one so we don't search the already read entry
    while (owner!=batch_descript$House_owner[i]) {#advances search index until a match for owner name is found
      i <- (i+1)
      if(date!=batch_descript$Collection_date[i]) {
        stop(paste("ERROR: Missing samples for", owner, "on date", date)) #Gives error if the next entry with the same owner is from a different sampling date
      }
    }
    batch_ID <- batch_descript$Batch[i]
    batch_ID <- paste(batch_ID,".csv",sep="")
    batch_rawdata <- (read.csv(paste(bdir,batch_ID,sep="//"),header=FALSE))
    colnames(batch_rawdata) <- headers$Value
    #batch_rawdata[is.na(batch_rawdata)] <- 0 #Use if you need to turn NAs into 0s
    begin2 <- batch_descript$Range_begin[i]
    end2 <- batch_descript$Range_end[i]
    set_quant2 <- (end2-begin2+1)
    std_curve_index2 <- rep(h,set_quant2)
    set_quant <- (set_quant2+set_quant)
    n2o_area2 <- batch_rawdata[begin2:end2,gas]
    std_curve_index <- c(std_curve_index,std_curve_index2) #Adds Std_Curve_Num together
    n2o_area <- c(n2o_area,n2o_area2) #adds next data onto incomplete previous data
    n2o_area3 <- n2o_area
  }
  
  
  #Move reconstituted data to file and increment/reset indices
  template$Area <- n2o_area #Put combined vectors into template
  template$Std_Curve_Num <- std_curve_index
  if (gas==6){gasname <- "N2O"}
  if (gas==9){gasname <- "CH4"}
  if (gas==12){gasname <- "CO2"}
  if (dir.exists(paste(dir, "Data-", owner,"//",date))==FALSE) {dir.create(paste(dir, "Data-", owner,"//",date,sep=""))} #Creates dated directory if one does not already exist
  write.csv(template, file=paste(dir, "Data-", owner,"//",date, "//" ,date,"_septic-",owner,"_",gasname,".csv",sep="")) #output file headed by K index number (in case of duplicates) (,k,"-")
  gascounter <- gas
  if(i==nrow(batch_descript)) {gas <- gas+3}+{g <- 1}
  if(j==(i-1)){g <- i+1} #If an experiment was split sequentially, continue on
  if(i>(j+1)){g <- j+1} #If an experiment was not split sequentially, go back to the gap
  if(i==j){g <- i+1} #If an experiment was not split, continue on
  if(gas>gascounter) {
    g <- 1
  }
  i <- g
  k <- k+1 #Counter for multiple sets for a given owner on a given date
}


#Standards data transformation
i <- 1 #Indices
while (i<=nrow(batch_descript)){#Loop to to find batch IDs until there are no more
  batch_ID <- batch_descript$Batch[i]#Read batch name
  batch_ID <- paste(batch_ID,".csv",sep="")
  batch_rawdata <- (read.csv(paste(bdir,batch_ID,sep="//"),header=FALSE))#Open batch file of given name
  colnames(batch_rawdata) <- headers$Value #Import headers
  #batch_rawdata[is.na(batch_rawdata)] <- 0 #Use if you need to turn NAs into 0s
  date <- batch_descript$Collection_date[i]
  owner <- batch_descript$House_owner[i]
  if (dir.exists(paste(dir, "Data-", owner,"//",date))==FALSE) {dir.create(paste(dir, "Data-", owner,"//",date,sep=""))} #Creates dated directory if one does not already exist
  template_n2o$Area1 <- c(batch_rawdata$Area1[1],batch_rawdata$Area1[2],batch_rawdata$Area1[3])
  template_ch4$Area1 <- c(batch_rawdata$Area2[1],batch_rawdata$Area2[2],batch_rawdata$Area2[3])
  template_co2$Area1 <- c(batch_rawdata$Area3[1],batch_rawdata$Area3[2],batch_rawdata$Area3[3])
  write.csv(template_n2o, file=paste(dir,"Data-", owner,"//",date, "//" ,date, "_septic-" ,owner,"_N2O_standard.csv",sep=""))
  write.csv(template_ch4, file=paste(dir,"Data-", owner,"//",date, "//" ,date,"_septic-",owner,"_CH4_standard.csv",sep=""))
  write.csv(template_co2, file=paste(dir,"Data-", owner,"//",date, "//" ,date,"_septic-",owner,"_CO2_standard.csv",sep=""))
  i <- i+1
}



print("DONE") #If you don't see this, something went wrong

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

standard_data <- read.csv(directory[1])
source("standard.curve.fun.R")
standard_coeffs <- standard.curve.fun(standard_data)

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



