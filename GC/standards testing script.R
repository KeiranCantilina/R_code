## Created by Keiran Cantilina on 8/25/2015

rm(list=ls())
library(reshape2)
library(plyr)
library(ggplot2)
library(gridExtra)

dir <- "C://Users//Ross//Desktop//R_code//GC//"
setwd(dir)
old_dir <- paste(dir,"Standards_co2_old//",sep="")
mid_dir <- paste(dir,"Standards_co2_mid//",sep="")
new_dir <- paste(dir,"Standards_co2_new//",sep="")

old_list <- data.frame(list.files(path=old_dir, pattern="*standard*"))
colnames(old_list) <- "value"
mid_list <- data.frame(list.files(path=mid_dir, pattern="*standard*"))
colnames(mid_list) <- "value"
new_list <- data.frame(list.files(path=new_dir, pattern="*standard*"))
colnames(new_list) <- "value"

i<-1

old_results <- read.csv(paste(dir,"template_standards.csv",sep=""))
mid_results <- read.csv(paste(dir,"template_standards.csv",sep=""))
new_results<-read.csv(paste(dir,"template_standards.csv",sep=""))

while (i<=(nrow(old_list))){
  standard_data <- read.csv(paste(old_dir,old_list$value[i],sep=""))
  source("standard.curve.fun.R")
  standard_coeffs <- standard.curve.fun(standard_data)
  old_results <- rbind(old_results,standard_coeffs)
  i<-i+1
}

i<-1

while (i<=(nrow(mid_list))){
  standard_data <- read.csv(paste(mid_dir,mid_list$value[i],sep=""))
  source("standard.curve.fun.R")
  standard_coeffs <- standard.curve.fun(standard_data)
  mid_results <- rbind(mid_results,standard_coeffs)
  i<-i+1
}

i<-1

while (i<=(nrow(new_list))){
  standard_data <- read.csv(paste(new_dir,new_list$value[i],sep=""))
  source("standard.curve.fun.R")
  standard_coeffs <- standard.curve.fun(standard_data)
  new_results<- rbind(new_results,standard_coeffs)
  i<-i+1
}

old_results_sum<- summarise(old_results, SD_intercept= sd(old_results$Intercept),avg_intercept=mean(old_results$Intercept),SD_slope = sd(old_results$Intercept),avg_slope = mean(old_results$Slope))
mid_results_sum<- summarise(mid_results, SD_intercept= sd(mid_results$Intercept),avg_intercept=mean(mid_results$Intercept),SD_slope = sd(mid_results$Intercept),avg_slope = mean(mid_results$Slope))
new_results_sum<- summarise(new_results, SD_intercept = sd(new_results$Intercept),avg_intercept = mean(new_results$Intercept),SD_slope =sd(new_results$Intercept),avg_slope = mean(new_results$Slope))

results<- rbind(old_results_sum,mid_results_sum,new_results_sum)
rows <-c("2014","2015A","2015B")
rownames(results) <- rows
write.csv(results,file=paste(dir,"standards_testing_output.csv", sep=""))



histogram <- hist(old_results$Intercept, breaks=40,plot=TRUE)
# #T-test
# invisible(test <- wilcox.test(old_results$Intercept, mid_results$Intercept, paired= TRUE))
# p.value <- test$p.value
# V <- test$statistic
# if(p.value>0.05) answer <- ("POPULATIONS ARE NOT SIGNIFICANTLY DIFFERENT")
# if(p.value<=0.05) answer <-("POPULATIONS ARE SIGNIFICANTLY DIFFERENT")
# print(answer)

