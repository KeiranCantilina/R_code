## Created by Keiran Cantilina on 8/25/2015

rm(list=ls())
library(reshape2)
library(plyr)
library(ggplot2)
library(gridExtra)

dir <- "C://Users//Ross//Desktop//R_code//GC//"
setwd(dir)
old_dir <- paste(dir,"Standards_old//",sep="")
N2O_dir <- paste(dir,"temp_N2O//",sep="")
CH4_dir <- paste(dir,"temp_CH4//",sep="")

old_list <- data.frame(list.files(path=old_dir, pattern="*standard*"))
colnames(old_list) <- "value"
N2O_list <- data.frame(list.files(path=N2O_dir, pattern="*standard*"))
colnames(N2O_list) <- "value"
CH4_list <- data.frame(list.files(path=CH4_dir, pattern="*standard*"))
colnames(CH4_list) <- "value"

i<-1

old_results <- read.csv(paste(dir,"template_standards.csv",sep=""))
N2O_results <- read.csv(paste(dir,"template_standards.csv",sep=""))
CH4_results<-read.csv(paste(dir,"template_standards.csv",sep=""))

while (i<=(nrow(old_list))){
  standard_data <- read.csv(paste(old_dir,old_list$value[i],sep=""))
  source("standard.curve.fun.R")
  standard_coeffs <- standard.curve.fun(standard_data)
  old_results <- rbind(old_results,standard_coeffs)
  i<-i+1
}

i<-1

while (i<=(nrow(N2O_list))){
  standard_data <- read.csv(paste(N2O_dir,N2O_list$value[i],sep=""))
  source("standard.curve.fun.R")
  standard_coeffs <- standard.curve.fun(standard_data)
  N2O_results <- rbind(N2O_results,standard_coeffs)
  i<-i+1
}

i<-1

while (i<=(nrow(CH4_list))){
  standard_data <- read.csv(paste(CH4_dir,CH4_list$value[i],sep=""))
  source("standard.curve.fun.R")
  standard_coeffs <- standard.curve.fun(standard_data)
  CH4_results<- rbind(CH4_results,standard_coeffs)
  i<-i+1
}


write.csv(N2O_results,file=paste(dir,"N2O_results.csv", sep=""))
write.csv(CH4_results,file=paste(dir,"CH4_results.csv", sep=""))



histogram <- hist(old_results$Intercept, breaks=40,plot=TRUE)
# #T-test
# invisible(test <- wilcox.test(old_results$Intercept, mid_results$Intercept, paired= TRUE))
# p.value <- test$p.value
# V <- test$statistic
# if(p.value>0.05) answer <- ("POPULATIONS ARE NOT SIGNIFICANTLY DIFFERENT")
# if(p.value<=0.05) answer <-("POPULATIONS ARE SIGNIFICANTLY DIFFERENT")
# print(answer)

