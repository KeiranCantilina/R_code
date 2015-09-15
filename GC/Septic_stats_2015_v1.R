# Script for septic stats - 2015!

# Last edited on 08/17/2015 by Allison Truhlar

# Clear workspace
rm(list=ls())

# Load libraries if just starting
library(reshape2)
library(plyr)
library(lme4)
library(lmerTest)
library(ggplot2)


# Set working directory
#Allison, Mac: "/Users/atruhlar/Dropbox/Cornell/Septic_2015/2015_data"
#Allison, PC:"C:\\Users\\Owner\\Dropbox\\Cornell\\Septic_2015\\2015_data\\"
dir <-"C://Users//Ross//Desktop//R_code//GC//"
setwd(dir)


########################

# 08/17/2015: for now, I just need to look at Todd's data quickly, so I'm going to cheat and use Excel
# Note: here, Mass_flux_linear = g/day

# Load flux data

todd_flux_file <- "todd_flux_data_updated0817.csv"
todd_flux_data <- read.csv(paste(dir,todd_flux_file,sep="",collapse=NULL))
todd_flux_data$Date <- as.factor(todd_flux_data$Date)

# Separate flux data by type of gas

todd_CH4 <- todd_flux_data[todd_flux_data$Gas == "CH4",]
todd_CO2 <- todd_flux_data[todd_flux_data$Gas == "CO2",]
todd_N2O <- todd_flux_data[todd_flux_data$Gas == "N2O",]

# CH4 plot

todd_CH4_boxpolot <- ggplot(todd_CH4, aes(x=Date, y=Mass_flux_linear, fill=Treatment))+
  geom_boxplot(width=0.8)+
  ylab(expression(bold("CH"[4]*" emissions, [g/day]")))+
  xlab("Date")+
  scale_fill_grey(start=0.3,end=1)+
  #coord_cartesian(ylim=c(-0.15,0.2))+
  #scale_y_continuous(breaks=seq(from = -0.1, to = 0.2, by = 0.1))+
  theme_bw()+
  theme(axis.text=element_text(size=18), axis.title.x=element_text(size=18,face="bold",vjust=-0.4), 
        axis.title.y=element_text(size=18,vjust=1.2), legend.text=element_text(size=18),
        legend.title=element_text(size=18,face="bold"), legend.position="top")
todd_CH4_boxpolot
todd_CH4_boxpolot_file <- paste(dir,"Data-todd\\todd_CH4_boxpolot.png",sep="")
ggsave(file=todd_CH4_boxpolot_file, plot=todd_CH4_boxpolot)

# CO2 plot

todd_CO2_boxpolot <- ggplot(todd_CO2, aes(x=Date, y=Mass_flux_linear, fill=Treatment))+
  geom_boxplot(width=0.8)+
  ylab(expression(bold("CO"[2]*" emissions, [g/day]")))+
  xlab("Date")+
  scale_fill_grey(start=0.3,end=1)+
  #coord_cartesian(ylim=c(-0.15,0.2))+
  #scale_y_continuous(breaks=seq(from = -0.1, to = 0.2, by = 0.1))+
  theme_bw()+
  theme(axis.text=element_text(size=18), axis.title.x=element_text(size=18,face="bold",vjust=-0.4), 
        axis.title.y=element_text(size=18,vjust=1.2), legend.text=element_text(size=18),
        legend.title=element_text(size=18,face="bold"), legend.position="top")
todd_CO2_boxpolot
todd_CO2_boxpolot_file <- paste(dir,"Data-todd\\todd_CO2_boxpolot.png",sep="")
ggsave(file=todd_CO2_boxpolot_file, plot=todd_CO2_boxpolot)

# N2O plot

todd_N2O_boxpolot <- ggplot(todd_N2O, aes(x=Date, y=Mass_flux_linear, fill=Treatment))+
  geom_boxplot(width=0.8)+
  ylab(expression(bold("N"[2]*"O emissions, [g/day]")))+
  xlab("Date")+
  scale_fill_grey(start=0.3,end=1)+
  #coord_cartesian(ylim=c(-0.15,0.2))+
  #scale_y_continuous(breaks=seq(from = -0.1, to = 0.2, by = 0.1))+
  theme_bw()+
  theme(axis.text=element_text(size=18), axis.title.x=element_text(size=18,face="bold",vjust=-0.4), 
        axis.title.y=element_text(size=18,vjust=1.2), legend.text=element_text(size=18),
        legend.title=element_text(size=18,face="bold"), legend.position="top",axis.text.x=element_text(size=12))
todd_N2O_boxpolot
todd_N2O_boxpolot_file <- paste(dir,"Data-todd\\todd_N2O_boxpolot.png",sep="")
ggsave(file=todd_N2O_boxpolot_file, plot=todd_N2O_boxpolot)

########################

# 08/18/2015: Todd's fluxes seemed pretty high compared to last year for CO2 and N2O.  This is somewhat expected for 
# CO2 due to the Licor, but not for N2O.  Now I'm taking a quick look at Laurie's data to try to determine if this is 
# a method issue, or if there is actually that much more N2O at Todd's this year.

# Note: here, Mass_flux_linear = g/day

# Load flux data

laurie_flux_file <- "\\laurie_flux_data_updated0818.csv"
laurie_flux_data <- read.csv(paste(dir,laurie_flux_file,sep="",collapse=NULL))
laurie_flux_data$Date <- as.factor(laurie_flux_data$Date)

# Separate flux data by type of gas

laurie_CH4 <- laurie_flux_data[laurie_flux_data$Gas == "CH4",]
laurie_CO2 <- laurie_flux_data[laurie_flux_data$Gas == "CO2",]
laurie_N2O <- laurie_flux_data[laurie_flux_data$Gas == "N2O",]

# CH4 plot

laurie_CH4_boxpolot <- ggplot(laurie_CH4, aes(x=Date, y=Mass_flux_linear, fill=Treatment))+
  geom_boxplot(width=0.8)+
  ylab(expression(bold("CH"[4]*" emissions, [g/day]")))+
  xlab("Date")+
  scale_fill_grey(start=0.3,end=1)+
  #coord_cartesian(ylim=c(-0.15,0.2))+
  #scale_y_continuous(breaks=seq(from = -0.1, to = 0.2, by = 0.1))+
  theme_bw()+
  theme(axis.text=element_text(size=18), axis.title.x=element_text(size=18,face="bold",vjust=-0.4), 
        axis.title.y=element_text(size=18,vjust=1.2), legend.text=element_text(size=18),
        legend.title=element_text(size=18,face="bold"), legend.position="top")
laurie_CH4_boxpolot
laurie_CH4_boxpolot_file <- paste(dir,"Data-laurie\\laurie_CH4_boxpolot.png",sep="")
ggsave(file=laurie_CH4_boxpolot_file, plot=laurie_CH4_boxpolot)

# CO2 plot

laurie_CO2_boxpolot <- ggplot(laurie_CO2, aes(x=Date, y=Mass_flux_linear, fill=Treatment))+
  geom_boxplot(width=0.8)+
  ylab(expression(bold("CO"[2]*" emissions, [g/day]")))+
  xlab("Date")+
  scale_fill_grey(start=0.3,end=1)+
  #coord_cartesian(ylim=c(-0.15,0.2))+
  #scale_y_continuous(breaks=seq(from = -0.1, to = 0.2, by = 0.1))+
  theme_bw()+
  theme(axis.text=element_text(size=18), axis.title.x=element_text(size=18,face="bold",vjust=-0.4), 
        axis.title.y=element_text(size=18,vjust=1.2), legend.text=element_text(size=18),
        legend.title=element_text(size=18,face="bold"), legend.position="top")
laurie_CO2_boxpolot
laurie_CO2_boxpolot_file <- paste(dir,"Data-laurie\\laurie_CO2_boxpolot.png",sep="")
ggsave(file=laurie_CO2_boxpolot_file, plot=laurie_CO2_boxpolot)

# N2O plot

laurie_N2O_boxpolot <- ggplot(laurie_N2O, aes(x=Date, y=Mass_flux_linear, fill=Treatment))+
  geom_boxplot(width=0.8)+
  ylab(expression(bold("N"[2]*"O emissions, [g/day]")))+
  xlab("Date")+
  scale_fill_grey(start=0.3,end=1)+
  #coord_cartesian(ylim=c(-0.15,0.2))+
  #scale_y_continuous(breaks=seq(from = -0.1, to = 0.2, by = 0.1))+
  theme_bw()+
  theme(axis.text=element_text(size=18), axis.title.x=element_text(size=18,face="bold",vjust=-0.4), 
        axis.title.y=element_text(size=18,vjust=1.2), legend.text=element_text(size=18),
        legend.title=element_text(size=18,face="bold"), legend.position="top")
laurie_N2O_boxpolot
laurie_N2O_boxpolot_file <- paste(dir,"Data-laurie\\laurie_N2O_boxpolot.png",sep="")
ggsave(file=laurie_N2O_boxpolot_file, plot=laurie_N2O_boxpolot)


########################

# 08/26/2015: Taking a quick look at the overnight campaign data - laurie first.  I will also generate graphs of the 
# environmental data from the overnight campaign

# Note: here, Mass_flux_linear = g/day

# Load flux data

laurie_overnight_flux_file <- "Data-laurie\\laurie_overnight_flux_data_updated0826.csv"
laurie_overnight_flux_data <- read.csv(paste(dir,laurie_overnight_flux_file,sep="",collapse=NULL))
laurie_overnight_flux_data$Date <- as.factor(laurie_overnight_flux_data$Date)
levels(laurie_overnight_flux_data$Date) <- c("12:30","15:30","18:30","22:30","1:30","5:00","7:30","10:30")

# Separate flux data by type of gas

laurie_overnight_CH4 <- laurie_overnight_flux_data[laurie_overnight_flux_data$Gas == "CH4",]
laurie_overnight_CO2 <- laurie_overnight_flux_data[laurie_overnight_flux_data$Gas == "CO2",]
laurie_overnight_N2O <- laurie_overnight_flux_data[laurie_overnight_flux_data$Gas == "N2O",]

# CH4 plot

laurie_overnight_CH4_boxpolot <- ggplot(laurie_overnight_CH4, aes(x=Date, y=Mass_flux_linear, fill=Treatment))+
  geom_boxplot(width=0.8)+
  ylab(expression(bold("CH"[4]*" emissions, [g/day]")))+
  xlab("Time")+
  scale_fill_grey(start=0.3,end=1)+
  #coord_cartesian(ylim=c(-0.15,0.2))+
  #scale_y_continuous(breaks=seq(from = -0.1, to = 0.2, by = 0.1))+
  theme_bw()+
  theme(axis.text=element_text(size=18), axis.title.x=element_text(size=18,face="bold",vjust=-0.4), 
        axis.title.y=element_text(size=18,vjust=1.2), legend.text=element_text(size=18),
        legend.title=element_text(size=18,face="bold"), legend.position="top")
laurie_overnight_CH4_boxpolot
laurie_overnight_CH4_boxpolot_file <- paste(dir,"Data-laurie\\laurie_overnight_CH4_boxpolot.png",sep="")
ggsave(file=laurie_overnight_CH4_boxpolot_file, plot=laurie_overnight_CH4_boxpolot)

# CO2 plot

laurie_overnight_CO2_boxpolot <- ggplot(laurie_overnight_CO2, aes(x=Date, y=Mass_flux_linear, fill=Treatment))+
  geom_boxplot(width=0.8)+
  ylab(expression(bold("CO"[2]*" emissions, [g/day]")))+
  xlab("Time")+
  scale_fill_grey(start=0.3,end=1)+
  #coord_cartesian(ylim=c(-0.15,0.2))+
  #scale_y_continuous(breaks=seq(from = -0.1, to = 0.2, by = 0.1))+
  theme_bw()+
  theme(axis.text=element_text(size=18), axis.title.x=element_text(size=18,face="bold",vjust=-0.4), 
        axis.title.y=element_text(size=18,vjust=1.2), legend.text=element_text(size=18),
        legend.title=element_text(size=18,face="bold"), legend.position="top")
laurie_overnight_CO2_boxpolot
laurie_overnight_CO2_boxpolot_file <- paste(dir,"Data-laurie\\laurie_overnight_CO2_boxpolot.png",sep="")
ggsave(file=laurie_overnight_CO2_boxpolot_file, plot=laurie_overnight_CO2_boxpolot)

# N2O plot

laurie_overnight_N2O_boxpolot <- ggplot(laurie_overnight_N2O, aes(x=Date, y=Mass_flux_linear, fill=Treatment))+
  geom_boxplot(width=0.8)+
  ylab(expression(bold("N"[2]*"O emissions, [g/day]")))+
  xlab("Time")+
  scale_fill_grey(start=0.3,end=1)+
  #coord_cartesian(ylim=c(-0.15,0.2))+
  #scale_y_continuous(breaks=seq(from = -0.1, to = 0.2, by = 0.1))+
  theme_bw()+
  theme(axis.text=element_text(size=18), axis.title.x=element_text(size=18,face="bold",vjust=-0.4), 
        axis.title.y=element_text(size=18,vjust=1.2), legend.text=element_text(size=18),
        legend.title=element_text(size=18,face="bold"), legend.position="top")
laurie_overnight_N2O_boxpolot
laurie_overnight_N2O_boxpolot_file <- paste(dir,"Data-laurie\\laurie_overnight_N2O_boxpolot.png",sep="")
ggsave(file=laurie_overnight_N2O_boxpolot_file, plot=laurie_overnight_N2O_boxpolot)

# Load environmental data

overnight_enviro_file <- "Overnight-environmental-data-2015.csv"
overnight_enviro_data <- read.csv(paste(dir,overnight_enviro_file,sep="",collapse=NULL))

# Summarize data by mean and standard deviation at each chamber

enviro_melted <- melt(overnight_enviro_data, id.vars=c("Homeowner","Date","Chamber","Treatment"), measure.vars=c("Temperature","Conductivity","VWC"))
enviro_means_sem <- ddply(enviro_melted, c("Homeowner","Date","Chamber","Treatment","variable"), summarise, mean=mean(value), sem=sd(value)/sqrt(length(value)))
enviro_means_sem <- transform(enviro.means.sem, lower=mean-sem, upper=mean+sem)

# Subset environmetal data for laurie

laurie_enviro_means_sem <- enviro_means_sem[enviro_means_sem$Homeowner=="laurie",]
laurie_enviro_means_sem$Date <- as.factor(laurie_enviro_means_sem$Date)
levels(laurie_enviro_means_sem$Date) <- c("12:30","15:30","18:30","22:30","1:30","5:00","7:30","10:30")

# Temperature plot

laurie_overnight_temp <- laurie_enviro_means_sem(laurie_enviro_means_sem$variable=="Temperature")
  
laurie_overnight_temp_plot <- ggplot(laurie_enviro_means_sem, aes(x=Date, y=Temperature))+
  geom_line()+
  ylab("Temperature, deg. C")+
  xlab("Time")+
  #coord_cartesian(ylim=c(-0.15,0.2))+
  #scale_y_continuous(breaks=seq(from = -0.1, to = 0.2, by = 0.1))+
  theme_bw()+
  theme(axis.text=element_text(size=18), axis.title.x=element_text(size=18,face="bold",vjust=-0.4), 
        axis.title.y=element_text(size=18,vjust=1.2), legend.text=element_text(size=18),
        legend.title=element_text(size=18,face="bold"), legend.position="top")
laurie_overnight_temp_plot
laurie_overnight_temp_plot_file <- paste(dir,"Data-laurie\\laurie_laurie_overnight_temp_plot.png",sep="")
ggsave(file=laurie_overnight_temp_plot_file, plot=laurie_overnight_temp_plot)


########################

# 08/26/2015: Taking a quick look at the overnight campaign data - matt second

# Note: here, Mass_flux_linear = g/day

# Load flux data

matt_overnight_flux_file <- "Data-matt\\matt_overnight_flux_data_updated0826.csv"
matt_overnight_flux_data <- read.csv(paste(dir,matt_overnight_flux_file,sep="",collapse=NULL))
matt_overnight_flux_data$Date <- as.factor(matt_overnight_flux_data$Date)
levels(matt_overnight_flux_data$Date) <- c("11:00","14:00","17:00","21:00","0:00","3:00","6:00","9:00")

# Separate flux data by type of gas

matt_overnight_CH4 <- matt_overnight_flux_data[matt_overnight_flux_data$Gas == "CH4",]
matt_overnight_CO2 <- matt_overnight_flux_data[matt_overnight_flux_data$Gas == "CO2",]
matt_overnight_N2O <- matt_overnight_flux_data[matt_overnight_flux_data$Gas == "N2O",]

# CH4 plot

matt_overnight_CH4_boxpolot <- ggplot(matt_overnight_CH4, aes(x=Date, y=Mass_flux_linear, fill=Treatment))+
  geom_boxplot(width=0.8)+
  ylab(expression(bold("CH"[4]*" emissions, [g/day]")))+
  xlab("Time")+
  scale_fill_grey(start=0.3,end=1)+
  #coord_cartesian(ylim=c(-0.15,0.2))+
  #scale_y_continuous(breaks=seq(from = -0.1, to = 0.2, by = 0.1))+
  theme_bw()+
  theme(axis.text=element_text(size=18), axis.title.x=element_text(size=18,face="bold",vjust=-0.4), 
        axis.title.y=element_text(size=18,vjust=1.2), legend.text=element_text(size=18),
        legend.title=element_text(size=18,face="bold"), legend.position="top")
matt_overnight_CH4_boxpolot
matt_overnight_CH4_boxpolot_file <- paste(dir,"Data-matt\\matt_overnight_CH4_boxpolot.png",sep="")
ggsave(file=matt_overnight_CH4_boxpolot_file, plot=matt_overnight_CH4_boxpolot)

# CO2 plot

matt_overnight_CO2_boxpolot <- ggplot(matt_overnight_CO2, aes(x=Date, y=Mass_flux_linear, fill=Treatment))+
  geom_boxplot(width=0.8)+
  ylab(expression(bold("CO"[2]*" emissions, [g/day]")))+
  xlab("Time")+
  scale_fill_grey(start=0.3,end=1)+
  #coord_cartesian(ylim=c(-0.15,0.2))+
  #scale_y_continuous(breaks=seq(from = -0.1, to = 0.2, by = 0.1))+
  theme_bw()+
  theme(axis.text=element_text(size=18), axis.title.x=element_text(size=18,face="bold",vjust=-0.4), 
        axis.title.y=element_text(size=18,vjust=1.2), legend.text=element_text(size=18),
        legend.title=element_text(size=18,face="bold"), legend.position="top")
matt_overnight_CO2_boxpolot
matt_overnight_CO2_boxpolot_file <- paste(dir,"Data-matt\\matt_overnight_CO2_boxpolot.png",sep="")
ggsave(file=matt_overnight_CO2_boxpolot_file, plot=matt_overnight_CO2_boxpolot)

# N2O plot

matt_overnight_N2O_boxpolot <- ggplot(matt_overnight_N2O, aes(x=Date, y=Mass_flux_linear, fill=Treatment))+
  geom_boxplot(width=0.8)+
  ylab(expression(bold("N"[2]*"O emissions, [g/day]")))+
  xlab("Time")+
  scale_fill_grey(start=0.3,end=1)+
  #coord_cartesian(ylim=c(-0.15,0.2))+
  #scale_y_continuous(breaks=seq(from = -0.1, to = 0.2, by = 0.1))+
  theme_bw()+
  theme(axis.text=element_text(size=18), axis.title.x=element_text(size=18,face="bold",vjust=-0.4), 
        axis.title.y=element_text(size=18,vjust=1.2), legend.text=element_text(size=18),
        legend.title=element_text(size=18,face="bold"), legend.position="top")
matt_overnight_N2O_boxpolot
matt_overnight_N2O_boxpolot_file <- paste(dir,"Data-matt\\matt_overnight_N2O_boxpolot.png",sep="")
ggsave(file=matt_overnight_N2O_boxpolot_file, plot=matt_overnight_N2O_boxpolot)








#######################
# Load all environmental data 
# **Make sure spreadsheet is updated first!!!
enviro_file <- "All-environmental-data.csv"
enviro_data <- read.csv(paste(dir,enviro_file,sep=""))
enviro_data$Round <- as.factor(enviro_data$Round)

# Summarise environmental data by chamber
enviro_melted <- melt(enviro_data, id.vars=c("Homeowner","Round","Chamber","Treatment"), measure.vars=c("Temperature","Conductivity","VWC"))
enviro.means.sem <- ddply(enviro_melted, c("Homeowner","Round","Chamber","Treatment","variable"), summarise, mean=mean(value), sem=sd(value)/sqrt(length(value)))
enviro.means.sem <- transform(enviro.means.sem, lower=mean-sem, upper=mean+sem)
# Separate by environmental variable
temp <- enviro.means.sem[enviro.means.sem$variable == "Temperature",]
cond <- enviro.means.sem[enviro.means.sem$variable == "Conductivity",]
vwc <- enviro.means.sem[enviro.means.sem$variable == "VWC",]


# Load flux data
flux_file_per_cap <- "All_flux_per_capita_data_revised.csv"
all_flux_data <- read.csv(paste(dir,flux_file_per_cap,sep="",collapse=NULL))

ground_flux_data <- all_flux_data[all_flux_data$Treatment != "Roof" & all_flux_data$Treatment != "Ground" & all_flux_data$Treatment != "Garage",]
ground_flux_data$Round <- as.factor(ground_flux_data$Round)


# Separate flux data by type of gas
ground_CH4 <- ground_flux_data[ground_flux_data$Gas == "CH4",]
ground_CO2 <- ground_flux_data[ground_flux_data$Gas == "CO2",]
ground_N2O <- ground_flux_data[ground_flux_data$Gas == "N2O",]


# Combine each type of flux data with enviro data
ground_CH4 <- transform(ground_CH4, Mean_temperature=temp$mean, Mean_conductivity=cond$mean, Mean_vwc=vwc$mean)
ground_CO2 <- transform(ground_CO2, Mean_temperature=temp$mean, Mean_conductivity=cond$mean, Mean_vwc=vwc$mean)
ground_N2O <- transform(ground_N2O, Mean_temperature=temp$mean, Mean_conductivity=cond$mean, Mean_vwc=vwc$mean)


##############################################################
# Remove all of dan and tom-house as two sites that are outliers
dan_tom_CH4 <- ground_CH4[c(1:18,154:171),]
dan_tom_ch4_flux <- dan_tom_CH4$Lin_flux_per_capita
ground_CH4 <- ground_CH4[c(-1:-18,-154:-171),]

# Does CH4 flux differ by treatment?
# ch4_flux <- ground_CH4$Lin_flux_per_capita
# ch4_treatment <- ground_CH4$Treatment
# ch4_temp <- ground_CH4$Mean_temperature
# ch4_cond <- ground_CH4$Mean_conductivity
# ch4_vwc <- ground_CH4$Mean_vwc
# ch4_time <- as.factor(ground_CH4$DOY)
# ch4_site <- ground_CH4$Homeowner

# ch4_model_1 <- lmer(ch4_flux ~ ch4_treatment + ch4_temp + ch4_cond + ch4_vwc + ch4_treatment:ch4_temp + ch4_treatment:ch4_cond + ch4_treatment:ch4_vwc + ch4_temp:ch4_cond + ch4_temp:ch4_vwc + ch4_cond:ch4_vwc + (1|ch4_site))
# summary(ch4_model_1) 
# residuals <- resid(ch4_model_1)
# fitted <- fitted(ch4_model_1)
# hist(residuals)
# plot(fitted, residuals)

# Remove line 67: matt, round 3, chamber 1, leach_field, per cap = -0.35
# Remove line 73: pat, round 1, chamber 1, sand_filter, per cap = -0.61 (current min)
ground_CH4 <- ground_CH4[c(-67,-73),]
ch4_flux <- ground_CH4$Lin_flux_per_capita
ch4_treatment <- ground_CH4$Treatment
ch4_temp <- ground_CH4$Mean_temperature
ch4_cond <- ground_CH4$Mean_conductivity
ch4_vwc <- ground_CH4$Mean_vwc
ch4_time <- as.factor(ground_CH4$DOY)
ch4_site <- ground_CH4$Homeowner

# ch4_model_2 <- lmer(ch4_flux ~ ch4_treatment + ch4_temp + ch4_cond + ch4_vwc + ch4_treatment:ch4_temp + ch4_treatment:ch4_cond + ch4_treatment:ch4_vwc + ch4_temp:ch4_cond + ch4_temp:ch4_vwc + ch4_cond:ch4_vwc + (1|ch4_site))
# summary(ch4_model_2) 
# residuals <- resid(ch4_model_2)
# fitted <- fitted(ch4_model_2)
# hist(residuals)
# plot(fitted, residuals)
# 
# ch4_model_3 <- lmer(ch4_flux ~ ch4_treatment + ch4_temp + ch4_cond + ch4_vwc + ch4_treatment:ch4_temp + ch4_treatment:ch4_cond + ch4_treatment:ch4_vwc + ch4_temp:ch4_vwc + ch4_cond:ch4_vwc + (1|ch4_site))
# summary(ch4_model_3) 
# residuals <- resid(ch4_model_3)
# fitted <- fitted(ch4_model_3)
# hist(residuals)
# plot(fitted, residuals)
# 
# ch4_model_4 <- lmer(ch4_flux ~ ch4_treatment + ch4_temp + ch4_cond + ch4_vwc + ch4_treatment:ch4_temp + ch4_treatment:ch4_cond + ch4_treatment:ch4_vwc + ch4_temp:ch4_vwc + (1|ch4_site))
# summary(ch4_model_4) 
# residuals <- resid(ch4_model_4)
# fitted <- fitted(ch4_model_4)
# hist(residuals)
# plot(fitted, residuals)
# 
# ch4_model_5 <- lmer(ch4_flux ~ ch4_treatment + ch4_temp + ch4_cond + ch4_vwc + ch4_treatment:ch4_temp + (1|ch4_site))
# summary(ch4_model_5) 
# residuals <- resid(ch4_model_5)
# fitted <- fitted(ch4_model_5)
# hist(residuals)
# plot(fitted, residuals)
# 
# ch4_model_6 <- lmer(ch4_flux ~ ch4_treatment + ch4_temp + ch4_cond + ch4_vwc + (1|ch4_site))
# summary(ch4_model_6) 
# residuals <- resid(ch4_model_6)
# fitted <- fitted(ch4_model_6)
# hist(residuals)
# plot(fitted, residuals)
# 
# ch4_model_7 <- lmer(ch4_flux ~ ch4_temp + ch4_cond + ch4_vwc + (1|ch4_site))
# summary(ch4_model_7) 
# residuals <- resid(ch4_model_7)
# fitted <- fitted(ch4_model_7)
# hist(residuals)
# plot(fitted, residuals)
# 
# ch4_model_8 <- lmer(ch4_flux ~ ch4_temp + ch4_vwc + (1|ch4_site))
# summary(ch4_model_8) 
# residuals <- resid(ch4_model_8)
# fitted <- fitted(ch4_model_8)
# hist(residuals)
# plot(fitted, residuals)

ch4_model_9 <- lmer(ch4_flux ~ ch4_vwc + (1|ch4_site))
summary(ch4_model_9) 
residuals <- resid(ch4_model_9)
fitted <- fitted(ch4_model_9)
hist(residuals)
plot(fitted, residuals)

# Calculate means of leach field and sand filter fluxes
ch4_flux_lf <- ground_CH4$Lin_flux_per_capita[ground_CH4$Treatment=="Leach_field"]
mean_ch4_lf <- mean(ch4_flux_lf)
sd_ch4_lf <- sd(ch4_flux_lf)
ch4_flux_sf <- ground_CH4$Lin_flux_per_capita[ground_CH4$Treatment=="Sand_filter"]
mean_ch4_sf <- mean(ch4_flux_sf)
sd_ch4_sf <- sd(ch4_flux_sf)
ch4_flux_c <- ground_CH4$Lin_flux_per_capita[ground_CH4$Treatment=="Control"]
mean_ch4_c <- mean(ch4_flux_c)
sd_ch4_c <- sd(ch4_flux_c)

# All sites but dan and tom graph

melted_homeowners <- melt(ground_CH4, id.vars=c("Homeowner","Treatment"), measure.vars="Lin_flux_per_capita")
melted_homeowners$Treatment <- factor(melted_homeowners$Treatment)
levels(melted_homeowners$Treatment) <- c("Control","Leach field","Sand filter")

# Create dummy data to fill in dan and tom-house spots
melted_homeowners <- rbind(melted_homeowners, data.frame(Homeowner="dan",Treatment="Sand filter",variable="Lin_flux_per_capita", value=1))
melted_homeowners <- rbind(melted_homeowners, data.frame(Homeowner="dan",Treatment="Control",variable="Lin_flux_per_capita", value=1))
melted_homeowners <- rbind(melted_homeowners, data.frame(Homeowner="dan",Treatment="Leach field",variable="Lin_flux_per_capita", value=1))

melted_homeowners <- rbind(melted_homeowners, data.frame(Homeowner="tom-house",Treatment="Sand filter",variable="Lin_flux_per_capita", value=1))
melted_homeowners <- rbind(melted_homeowners, data.frame(Homeowner="tom-house",Treatment="Control",variable="Lin_flux_per_capita", value=1))
melted_homeowners <- rbind(melted_homeowners, data.frame(Homeowner="tom-house",Treatment="Leach field",variable="Lin_flux_per_capita", value=1))

melted_homeowners$Homeowner <- factor(melted_homeowners$Homeowner)
levels(melted_homeowners$Homeowner) <- c("1","2","3","4","5","6","7","8","9")

#Create dummy data to fill in spots with no data for leach field or sand filter (for asthetic purposes)
melted_homeowners <- rbind(melted_homeowners, data.frame(Homeowner="2",Treatment="Leach field",variable="Lin_flux_per_capita", value=1))
melted_homeowners <- rbind(melted_homeowners, data.frame(Homeowner="3",Treatment="Sand filter",variable="Lin_flux_per_capita", value=1))
melted_homeowners <- rbind(melted_homeowners, data.frame(Homeowner="4",Treatment="Sand filter",variable="Lin_flux_per_capita", value=1))
melted_homeowners <- rbind(melted_homeowners, data.frame(Homeowner="5",Treatment="Sand filter",variable="Lin_flux_per_capita", value=1))
melted_homeowners <- rbind(melted_homeowners, data.frame(Homeowner="7",Treatment="Sand filter",variable="Lin_flux_per_capita", value=1))
melted_homeowners <- rbind(melted_homeowners, data.frame(Homeowner="8",Treatment="Leach field",variable="Lin_flux_per_capita", value=1))

pd <- position_dodge(0.1)

boxplot_by_homeowner <- ggplot(melted_homeowners, aes(x=Homeowner,y=value, fill=Treatment))+
  geom_boxplot(width=0.8)+
  ylab(expression(bold("Mean "*"CH"[4]*" emissions, [g/capita/day]")))+
  xlab("Site")+
  scale_fill_grey(start=0.3,end=1)+
  coord_cartesian(ylim=c(-0.15,0.2))+
  scale_y_continuous(breaks=seq(from = -0.1, to = 0.2, by = 0.1))+
  theme_bw()+
  theme(axis.text=element_text(size=18), axis.title.x=element_text(size=18,face="bold",vjust=-0.4), 
        axis.title.y=element_text(size=18,vjust=1.2), legend.text=element_text(size=18),
        legend.title=element_text(size=18,face="bold"), legend.position="top")
boxplot_by_homeowner
graph_file <- paste(dir,"CH4_Boxplot_ByHomeowner.png",sep="")
ggsave(file=graph_file, plot=boxplot_by_homeowner)

# Graphs for Dan and Tom

melted_dan_tom <- melt(dan_tom_CH4, id.vars=c("Homeowner","Treatment"), measure.vars="Lin_flux_per_capita")

# Add dummy data to fix spacing
melted_dan_tom <- rbind(melted_dan_tom, data.frame(Homeowner="dan",Treatment="Sand filter",variable="Lin_flux_per_capita", value=10))
melted_dan_tom <- rbind(melted_dan_tom, data.frame(Homeowner="tom-house",Treatment="Sand filter",variable="Lin_flux_per_capita", value=10))

melted_dan_tom$Treatment <- factor(melted_dan_tom$Treatment)
levels(melted_dan_tom$Treatment) <- c("Control","Leach field", "Sand filter")
melted_dan_tom$Homeowner <- factor(melted_dan_tom$Homeowner)
levels(melted_dan_tom$Homeowner) <- c("1","9")

boxplot_by_dan_tom <- ggplot(melted_dan_tom, aes(x=Homeowner,y=value, fill=Treatment))+
  geom_boxplot(width=0.8)+
  coord_cartesian(ylim=c(-0.5,5.5))+
  scale_y_continuous(breaks = seq(from = 0, to = 5, by = 1))+
  geom_segment(aes(x=0.5,xend=2.5,y=0.16,yend=0.16),size=0.5,linetype=2)+
  geom_segment(aes(x=0.5,xend=2.5,y=-0.11,yend=-0.11),size=0.5,linetype=2)+
  geom_boxplot(width=0.8)+
  ylab(expression(bold("Mean "*"CH"[4]*" flux, [g/capita/day]")))+
  xlab("Site")+
  scale_fill_grey(start=0.3,end=1)+
  theme_bw()+
  theme(axis.text=element_text(size=18), axis.title.x=element_text(size=22,face="bold",vjust=-0.4), 
        axis.title.y=element_text(size=22,vjust=1.2), legend.text=element_text(size=18),
        legend.title=element_text(size=18,face="bold"), legend.position="top")
boxplot_by_dan_tom
graph_file <- paste(dir,"CH4_Boxplot_dan_tom.png",sep="")
ggsave(file=graph_file, plot=boxplot_by_dan_tom)


CH4_enviro_data <- data.frame(ch4_flux=ch4_flux, ch4_vwc=ch4_vwc)

CH4_vwc_graph <- ggplot(CH4_enviro_data,aes(x=ch4_vwc,y=ch4_flux))+
  geom_point(shape=18)+
  geom_abline(intercept=-0.0248007,slope=0.0011119)+
  ylab(expression(bold("CH"[4]*" flux, [g/capita/day]")))+
  xlab("Mean volumetric water content")+
  coord_cartesian(ylim=c(-0.2,0.2))+
  theme_bw()+
  theme(axis.text=element_text(size=18), axis.title.x=element_text(size=22,face="bold",vjust=-0.4), 
        axis.title.y=element_text(size=22,vjust=1.2))
CH4_vwc_graph
graph_file <- paste(dir,"CH4_vwc.png",sep="")
ggsave(file=graph_file, plot=CH4_vwc_graph)

# Attempt to make deviation from control graph
melted <- melt(ground_CH4, id.vars=c("Homeowner","Treatment","Round"), measure.vars="Lin_flux_per_capita")
means.sem <- ddply(melted, c("Homeowner","Treatment","Round","variable"), summarise,
                             mean=mean(value), sem=sd(value)/sqrt(length(value)))
#I ended up copying and pasting the means.sem data into Excel and then manually calculating the deviations
#between the leach field and control and sand filter and control.  Below I then import the resulting file
#back into R

devs_ch4_file <- "Deviations_ch4.csv"
devs_ch4_data <- read.csv(paste(dir,devs_ch4_file,sep=""))

levels(devs_ch4_data$Treatment) <- c("Leach field","Sand filter")

#colors <- c("#1b9e77","#d95f02")
boxplot_devs <- ggplot(devs_ch4_data, aes(x=Treatment,y=Dev, fill=Treatment))+
  geom_abline(intercept=0,slope=0,linetype="dashed")+
  geom_boxplot(width=0.8)+
  coord_cartesian(ylim=c(-0.08,0.08))+
  ylab(expression(bold("Deviation from control, [g/capita/day]")))+
  xlab("Treatment")+
  scale_fill_grey(start=0.6,end=1)+
  #scale_fill_manual(values=colors)+
  theme_bw()+
  theme(axis.text=element_text(size=18), axis.title.x=element_text(size=18,face="bold",vjust=-0.4), 
        axis.title.y=element_text(size=18,vjust=1.2), legend.position="none")
boxplot_devs
graph_file <- paste(dir,"CH4_Boxplot_Deviation-from-control.png",sep="")
ggsave(file=graph_file, plot=boxplot_devs)


########################################################################

# Does N2O flux differ by treatment?
# n2o_flux <- ground_N2O$Lin_flux_per_capita
# n2o_treatment <- ground_N2O$Treatment
# n2o_temp <- ground_N2O$Mean_temperature
# n2o_cond <- ground_N2O$Mean_conductivity
# n2o_vwc <- ground_N2O$Mean_vwc
# n2o_time <- as.factor(ground_N2O$DOY)
# n2o_site <- ground_N2O$Homeowner
# 
# min_n2o <- min(n2o_flux)
# transf_n2o <- log(n2o_flux+abs(min_n2o)+0.01)
# n2o_model_1 <- lmer(transf_n2o ~ n2o_treatment + n2o_temp + n2o_cond + n2o_vwc + n2o_treatment:n2o_temp + n2o_treatment:n2o_cond + n2o_treatment:n2o_vwc + n2o_temp:n2o_cond + n2o_temp:n2o_vwc + n2o_cond:n2o_vwc + (1|n2o_site))
# summary(n2o_model_1)
# residuals <- resid(n2o_model_1)
# fitted <- fitted(n2o_model_1)
# hist(residuals)
# plot(fitted,residuals)

# Remove line 159 in ground_N2O: tom-house, rnd 1, chmb 6, per cap = 1.0 (the current, and always, max)

ground_N2O <- ground_N2O[-159,]

# n2o_flux <- ground_N2O$Lin_flux_per_capita
# n2o_treatment <- ground_N2O$Treatment
# n2o_temp <- ground_N2O$Mean_temperature
# n2o_cond <- ground_N2O$Mean_conductivity
# n2o_vwc <- ground_N2O$Mean_vwc
# n2o_time <- as.factor(ground_N2O$DOY)
# n2o_site <- ground_N2O$Homeowner
# 
# min_n2o <- min(n2o_flux)
# transf_n2o <- log(n2o_flux+abs(min_n2o)+0.01)
# n2o_model_2 <- lmer(transf_n2o ~ n2o_treatment + n2o_temp + n2o_cond + n2o_vwc + n2o_treatment:n2o_temp + n2o_treatment:n2o_cond + n2o_treatment:n2o_vwc + n2o_temp:n2o_cond + n2o_temp:n2o_vwc + n2o_cond:n2o_vwc + (1|n2o_site))
# summary(n2o_model_2)
# residuals <- resid(n2o_model_2)
# fitted <- fitted(n2o_model_2)
# hist(residuals)
# plot(fitted,residuals)

# Remove line 18: dan, round 3, chamber 6, control, lin_flux_per_capita = -0.024
# Remove line 103: pat, round 2, chamber 4, control, lin_flux_per_capita = -0.024

ground_N2O <- ground_N2O[c(-18,-103),]

n2o_flux <- ground_N2O$Lin_flux_per_capita
n2o_treatment <- ground_N2O$Treatment
n2o_temp <- ground_N2O$Mean_temperature
n2o_cond <- ground_N2O$Mean_conductivity
n2o_vwc <- ground_N2O$Mean_vwc
n2o_time <- as.factor(ground_N2O$DOY)
n2o_site <- ground_N2O$Homeowner

min_n2o <- min(n2o_flux)
transf_n2o <- log(n2o_flux+abs(min_n2o)+0.01)
# n2o_model_4 <- lmer(transf_n2o ~ n2o_treatment + n2o_temp + n2o_cond + n2o_vwc + n2o_treatment:n2o_temp + n2o_treatment:n2o_cond + n2o_treatment:n2o_vwc + n2o_temp:n2o_cond + n2o_temp:n2o_vwc + n2o_cond:n2o_vwc + (1|n2o_site))
# summary(n2o_model_4)
# residuals <- resid(n2o_model_4)
# fitted <- fitted(n2o_model_4)
# hist(residuals)
# plot(fitted,residuals)
# 
# n2o_model_5 <- lmer(transf_n2o ~ n2o_treatment + n2o_temp + n2o_cond + n2o_vwc + n2o_treatment:n2o_temp + n2o_treatment:n2o_cond + n2o_temp:n2o_cond + n2o_temp:n2o_vwc + n2o_cond:n2o_vwc + (1|n2o_site))
# summary(n2o_model_5)
# residuals <- resid(n2o_model_5)
# fitted <- fitted(n2o_model_5)
# hist(residuals)
# plot(fitted,residuals)
# 
# n2o_model_6 <- lmer(transf_n2o ~ n2o_treatment + n2o_temp + n2o_cond + n2o_vwc + n2o_treatment:n2o_temp + n2o_treatment:n2o_cond + n2o_temp:n2o_vwc + n2o_cond:n2o_vwc + (1|n2o_site))
# summary(n2o_model_6)
# residuals <- resid(n2o_model_6)
# fitted <- fitted(n2o_model_6)
# hist(residuals)
# plot(fitted,residuals)
# 
# n2o_model_7 <- lmer(transf_n2o ~ n2o_treatment + n2o_temp + n2o_cond + n2o_vwc + n2o_treatment:n2o_temp + n2o_treatment:n2o_cond + n2o_cond:n2o_vwc + (1|n2o_site))
# summary(n2o_model_7)
# residuals <- resid(n2o_model_7)
# fitted <- fitted(n2o_model_7)
# hist(residuals)
# plot(fitted,residuals)
# 
# n2o_model_8 <- lmer(transf_n2o ~ n2o_treatment + n2o_temp + n2o_cond + n2o_vwc + n2o_treatment:n2o_cond + n2o_cond:n2o_vwc + (1|n2o_site))
# summary(n2o_model_8)
# residuals <- resid(n2o_model_8)
# fitted <- fitted(n2o_model_8)
# hist(residuals)
# plot(fitted,residuals)

n2o_model_9 <- lmer(transf_n2o ~ n2o_treatment + n2o_temp + n2o_cond + n2o_vwc + n2o_cond:n2o_vwc + (1|n2o_site))
summary(n2o_model_9)
residuals <- resid(n2o_model_9)
fitted <- fitted(n2o_model_9)
hist(residuals)
plot(fitted,residuals)

# Calculate means of leach field and sand filter fluxes
n2o_flux_lf <- ground_N2O$Lin_flux_per_capita[ground_N2O$Treatment=="Leach_field"]
transf_n2o_flux_lf <- log(n2o_flux_lf+abs(min_n2o)+0.01)
mean_transf_n2o_flux_lf <- mean(transf_n2o_flux_lf)
sd_transf_n2o_flux_lf <- sd(transf_n2o_flux_lf)
mean_n2o_flux_lf <- exp(mean_transf_n2o_flux_lf) - (abs(min_n2o)+0.01)
sd_n2o_flux_lf <- exp(sd_transf_n2o_flux_lf) - (abs(min_n2o)+0.01)

n2o_flux_sf <- ground_N2O$Lin_flux_per_capita[ground_N2O$Treatment=="Sand_filter"]
transf_n2o_flux_sf <- log(n2o_flux_sf+abs(min_n2o)+0.01)
mean_transf_n2o_flux_sf <- mean(transf_n2o_flux_sf)
sd_transf_n2o_flux_sf <- sd(transf_n2o_flux_sf)
mean_n2o_flux_sf <- exp(mean_transf_n2o_flux_sf) - (abs(min_n2o)+0.01)
sd_n2o_flux_sf <- exp(sd_transf_n2o_flux_sf) - (abs(min_n2o)+0.01)

n2o_flux_c <- ground_N2O$Lin_flux_per_capita[ground_N2O$Treatment=="Control"]
transf_n2o_flux_c <- log(n2o_flux_c+abs(min_n2o)+0.01)
mean_transf_n2o_flux_c <- mean(transf_n2o_flux_c)
mean_n2o_flux_c <- exp(mean_transf_n2o_flux_c) - (abs(min_n2o)+0.01)

# Create plot
melted_homeowners <- melt(ground_N2O, id.vars=c("Homeowner","Treatment"), measure.vars="Lin_flux_per_capita")

melted_homeowners$Treatment <- factor(melted_homeowners$Treatment)
levels(melted_homeowners$Treatment) <- c("Control","Leach field","Sand filter")
melted_homeowners$Homeowner <- factor(melted_homeowners$Homeowner)
levels(melted_homeowners$Homeowner) <- c("1","2","3","4","5","6","7","8","9")

#Create dummy data to fill in spots with no data for leach field or sand filter (for asthetic purposes)
melted_homeowners <- rbind(melted_homeowners, data.frame(Homeowner="1",Treatment="Sand filter",variable="Lin_flux_per_capita", value=1))
melted_homeowners <- rbind(melted_homeowners, data.frame(Homeowner="2",Treatment="Leach field",variable="Lin_flux_per_capita", value=1))
melted_homeowners <- rbind(melted_homeowners, data.frame(Homeowner="3",Treatment="Sand filter",variable="Lin_flux_per_capita", value=1))
melted_homeowners <- rbind(melted_homeowners, data.frame(Homeowner="4",Treatment="Sand filter",variable="Lin_flux_per_capita", value=1))
melted_homeowners <- rbind(melted_homeowners, data.frame(Homeowner="5",Treatment="Sand filter",variable="Lin_flux_per_capita", value=1))
melted_homeowners <- rbind(melted_homeowners, data.frame(Homeowner="7",Treatment="Sand filter",variable="Lin_flux_per_capita", value=1))
melted_homeowners <- rbind(melted_homeowners, data.frame(Homeowner="8",Treatment="Leach field",variable="Lin_flux_per_capita", value=1))
melted_homeowners <- rbind(melted_homeowners, data.frame(Homeowner="9",Treatment="Sand filter",variable="Lin_flux_per_capita", value=1))

boxplot_by_homeowner <- ggplot(melted_homeowners, aes(x=Homeowner,y=value, fill=Treatment))+
  geom_boxplot(width=0.8)+
  ylab(expression(bold("Mean N"[2]*"O emissions, [g/capita/day]")))+
  xlab("Site")+
  coord_cartesian(ylim=c(-0.02,0.18))+
  scale_fill_grey(start=0.3,end=1)+
  theme_bw()+
  theme(axis.text=element_text(size=18), axis.title.x=element_text(size=18,face="bold",vjust=-0.4), 
        axis.title.y=element_text(size=18,vjust=1.2), legend.text=element_text(size=18),
        legend.title=element_text(size=18,face="bold"), legend.position="top")
boxplot_by_homeowner
graph_file <- paste(dir,"N2O_Boxplot_ByHomeowner.png",sep="")
ggsave(file=graph_file, plot=boxplot_by_homeowner)



N2O_enviro_data <- data.frame(n2o_flux=n2o_flux, n2o_temp=n2o_temp, n2o_cond=n2o_cond, n2o_vwc=n2o_vwc)

N2O_enviro_data$fixed_effects <- predict(n2o_model_9)
N2O.rand <- ldply(list(unique(ground_N2O$Homeowner)),function(x) data.frame(
  Treatment=ground_N2O$Treatment,
  Homeowner=x,
  Temperature=N2O_enviro_data$n2o_temp,
  Conductivity=N2O_enviro_data$n2o_cond,
  VWC=N2O_enviro_data$n2o_vwc,
  rand=N2O_enviro_data$fixed_effects + ranef(n2o_model_9)$n2o_site[as.character(x),]))

N2O_temp_graph <- ggplot(N2O_enviro_data,aes(x=n2o_temp,y=n2o_flux))+
  geom_point(shape=18)+
  geom_abline(intercept=-0.051099,slope=0.003189)+
  ylab(expression(bold("N"[2]*"O flux, [g/capita/day]")))+
  xlab("Mean temperature [C]")+
  theme_bw()+
  theme(axis.text=element_text(size=18), axis.title.x=element_text(size=22,face="bold",vjust=-0.4), 
        axis.title.y=element_text(size=22,vjust=1.2))
N2O_temp_graph
graph_file <- paste(dir,"N2O_temp.png",sep="")
ggsave(file=graph_file, plot=N2O_temp_graph)


# Attempt to make deviation from control graph
melted <- melt(ground_N2O, id.vars=c("Homeowner","Treatment","Round"), measure.vars="Lin_flux_per_capita")
means.sem <- ddply(melted, c("Homeowner","Treatment","Round","variable"), summarise,
                   mean=mean(value), sem=sd(value)/sqrt(length(value)))
#I ended up copying and pasting the means.sem data into Excel and then manually calculating the deviations
#between the leach field and control and sand filter and control.  Below I then import the resulting file
#back into R

devs_n2o_file <- "Deviations_n2o.csv"
devs_n2o_data <- read.csv(paste(dir,devs_n2o_file,sep=""))

levels(devs_n2o_data$Treatment) <- c("Leach field","Sand filter")

#colors <- c("#1b9e77","#d95f02")
boxplot_devs <- ggplot(devs_n2o_data, aes(x=Treatment,y=Dev, fill=Treatment))+
  geom_abline(intercept=0,slope=0,linetype="dashed")+
  geom_boxplot(width=0.8)+
  coord_cartesian(ylim=c(-0.05,0.1))+
  scale_y_continuous(breaks=seq(from = -0.05, to = 0.1, by = 0.025))+
  ylab(expression(bold("Deviation from control, [g/capita/day]")))+
  xlab("Treatment")+
  scale_fill_grey(start=0.6,end=1)+
  #scale_fill_manual(values=colors)+
  #annotate("text",x=1, y=0.085,label="*",size=16)+
  theme_bw()+
  theme(axis.text=element_text(size=18), axis.title.x=element_text(size=18,face="bold",vjust=-0.4), 
        axis.title.y=element_text(size=18,vjust=1.2), legend.position="none")
boxplot_devs
graph_file <- paste(dir,"N2O_Boxplot_Deviation-from-control.png",sep="")
ggsave(file=graph_file, plot=boxplot_devs)

########################################################################

# Remove Dan's house (very high fluxes)
dan_CO2 <- ground_CO2[c(1:18),]
dan_co2_flux <- dan_CO2$Lin_flux_per_capita
ground_CO2 <- ground_CO2[c(-1:-18),]

co2_flux <- ground_CO2$Lin_flux_per_capita
co2_treatment <- ground_CO2$Treatment
co2_temp <- ground_CO2$Mean_temperature
co2_cond <- ground_CO2$Mean_conductivity
co2_vwc <- ground_CO2$Mean_vwc
co2_time <- as.factor(ground_CO2$DOY)
co2_site <- ground_CO2$Homeowner

# co2_model_1 <- lmer(co2_flux ~ co2_treatment + co2_temp + co2_cond + co2_vwc + co2_treatment:co2_temp + co2_treatment:co2_cond + co2_treatment:co2_vwc + co2_temp:co2_cond + co2_temp:co2_vwc + co2_cond:co2_vwc + (1|co2_site))
# summary(co2_model_1)
# residuals <- resid(co2_model_1)
# fitted <- fitted(co2_model_1)
# hist(residuals)
# plot(fitted,residuals)
# 
# co2_model_2 <- lmer(co2_flux ~ co2_treatment + co2_temp + co2_cond + co2_vwc + co2_treatment:co2_temp + co2_treatment:co2_cond + co2_treatment:co2_vwc + co2_temp:co2_vwc + co2_cond:co2_vwc + (1|co2_site))
# summary(co2_model_2)
# residuals <- resid(co2_model_2)
# fitted <- fitted(co2_model_2)
# hist(residuals)
# plot(fitted,residuals)
# 
# co2_model_3 <- lmer(co2_flux ~ co2_treatment + co2_temp + co2_cond + co2_vwc + co2_treatment:co2_temp + co2_treatment:co2_vwc + co2_temp:co2_vwc + co2_cond:co2_vwc + (1|co2_site))
# summary(co2_model_3)
# residuals <- resid(co2_model_3)
# fitted <- fitted(co2_model_3)
# hist(residuals)
# plot(fitted,residuals)
# 
# co2_model_4 <- lmer(co2_flux ~ co2_treatment + co2_temp + co2_cond + co2_vwc + co2_treatment:co2_temp + co2_temp:co2_vwc + co2_cond:co2_vwc + (1|co2_site))
# summary(co2_model_4)
# residuals <- resid(co2_model_4)
# fitted <- fitted(co2_model_4)
# hist(residuals)
# plot(fitted,residuals)
# 
# co2_model_5 <- lmer(co2_flux ~ co2_treatment + co2_temp + co2_cond + co2_vwc + co2_temp:co2_vwc + co2_cond:co2_vwc + (1|co2_site))
# summary(co2_model_5)
# residuals <- resid(co2_model_5)
# fitted <- fitted(co2_model_5)
# hist(residuals)
# plot(fitted,residuals)

co2_model_6 <- lmer(co2_flux ~ co2_treatment + co2_temp + co2_cond + co2_vwc + co2_cond:co2_vwc + (1|co2_site))
summary(co2_model_6)
residuals <- resid(co2_model_6)
fitted <- fitted(co2_model_6)
hist(residuals)
plot(fitted,residuals)

# Calculate means of leach field and sand filter fluxes
co2_flux_lf <- ground_CO2$Lin_flux_per_capita[ground_CO2$Treatment=="Leach_field"]
mean_co2_lf <- mean(co2_flux_lf)
sd_co2_lf <- sd(co2_flux_lf)
co2_flux_sf <- ground_CO2$Lin_flux_per_capita[ground_CO2$Treatment=="Sand_filter"]
mean_co2_sf <- mean(co2_flux_sf)
sd_co2_sf <- sd(co2_flux_sf)
co2_flux_c <- ground_CO2$Lin_flux_per_capita[ground_CO2$Treatment=="Control"]
mean_co2_c <- mean(co2_flux_c)

# Enviro plots - none

# Create plot
ground_CO2$Treatment <- factor(ground_CO2$Treatment)
melted_homeowners <- melt(ground_CO2, id.vars=c("Homeowner","Treatment"), measure.vars="Lin_flux_per_capita")
melted_homeowners$Treatment <- factor(melted_homeowners$Treatment)
levels(melted_homeowners$Treatment) <- c("Control","Leach field","Sand filter")

# Create dummy data to fill in dan and tom-house spots
melted_homeowners <- rbind(melted_homeowners, data.frame(Homeowner="dan",Treatment="Sand filter",variable="Lin_flux_per_capita", value=1000))
melted_homeowners <- rbind(melted_homeowners, data.frame(Homeowner="dan",Treatment="Control",variable="Lin_flux_per_capita", value=1000))
melted_homeowners <- rbind(melted_homeowners, data.frame(Homeowner="dan",Treatment="Leach field",variable="Lin_flux_per_capita", value=1000))

melted_homeowners <- rbind(melted_homeowners, data.frame(Homeowner="tom-house",Treatment="Sand filter",variable="Lin_flux_per_capita", value=1000))
melted_homeowners <- rbind(melted_homeowners, data.frame(Homeowner="tom-house",Treatment="Control",variable="Lin_flux_per_capita", value=1000))
melted_homeowners <- rbind(melted_homeowners, data.frame(Homeowner="tom-house",Treatment="Leach field",variable="Lin_flux_per_capita", value=1000))

melted_homeowners$Homeowner <- factor(melted_homeowners$Homeowner)
levels(melted_homeowners$Homeowner) <- c("1","2","3","4","5","6","7","8","9")

#Create dummy data to fill in spots with no data for leach field or sand filter (for asthetic purposes)
melted_homeowners <- rbind(melted_homeowners, data.frame(Homeowner="2",Treatment="Leach field",variable="Lin_flux_per_capita", value=1000))
melted_homeowners <- rbind(melted_homeowners, data.frame(Homeowner="3",Treatment="Sand filter",variable="Lin_flux_per_capita", value=1000))
melted_homeowners <- rbind(melted_homeowners, data.frame(Homeowner="4",Treatment="Sand filter",variable="Lin_flux_per_capita", value=1000))
melted_homeowners <- rbind(melted_homeowners, data.frame(Homeowner="5",Treatment="Sand filter",variable="Lin_flux_per_capita", value=1000))
melted_homeowners <- rbind(melted_homeowners, data.frame(Homeowner="7",Treatment="Sand filter",variable="Lin_flux_per_capita", value=1000))
melted_homeowners <- rbind(melted_homeowners, data.frame(Homeowner="8",Treatment="Leach field",variable="Lin_flux_per_capita", value=1000))

pd <- position_dodge(0.1)

boxplot_by_homeowner <- ggplot(melted_homeowners, aes(x=Homeowner,y=value, fill=Treatment))+
  geom_boxplot(width=0.8)+
  ylab(expression(bold("Mean CO"[2]*" emissions, [g/capita/day]")))+
  xlab("Site")+
  coord_cartesian(ylim=c(-175,475))+
  scale_y_continuous(breaks = seq(from = -150, to = 450, by = 150))+
  scale_fill_grey(start=0.3,end=1)+
  theme_bw()+
  theme(axis.text=element_text(size=18), axis.title.x=element_text(size=18,face="bold",vjust=-0.4), 
        axis.title.y=element_text(size=18,vjust=1.2), legend.text=element_text(size=18),
        legend.title=element_text(size=18,face="bold"), legend.position="top")
boxplot_by_homeowner
graph_file <- paste(dir,"CO2_Boxplot_ByHomeowner.png",sep="")
ggsave(file=graph_file, plot=boxplot_by_homeowner)

# Graph for Dan

melted_dan <- melt(dan_CO2, id.vars=c("Homeowner","Treatment"), measure.vars="Lin_flux_per_capita")

# Add dummy data to fix spacing
melted_dan <- rbind(melted_dan, data.frame(Homeowner="dan",Treatment="Sand filter",variable="Lin_flux_per_capita", value=2000))

melted_dan$Treatment <- factor(melted_dan$Treatment)
levels(melted_dan$Treatment) <- c("Control","Leach field", "Sand filter")
melted_dan$Homeowner <- factor(melted_dan$Homeowner)
levels(melted_dan$Homeowner) <- c("1")

boxplot_dan <- ggplot(melted_dan, aes(x=Homeowner,y=value, fill=Treatment))+
  geom_boxplot(width=0.8)+
  coord_cartesian(ylim=c(-250,1350))+
  scale_y_continuous(breaks = seq(from = -200, to = 1200, by = 200))+
  geom_segment(aes(x=0.6,xend=1.4,y=450,yend=450),size=0.5,linetype=2)+
  geom_segment(aes(x=0.6,xend=1.4,y=-150,yend=-150),size=0.5,linetype=2)+
  geom_boxplot(width=0.8)+
  ylab(expression(bold("Mean CO"[2]*" flux, [g/capita/day]")))+
  xlab("Site")+
  scale_fill_grey(start=0.3,end=1)+
  theme_bw()+
  theme(axis.text=element_text(size=18), axis.title.x=element_text(size=22,face="bold",vjust=-0.4), 
        axis.title.y=element_text(size=22,vjust=1.2), legend.text=element_text(size=18),
        legend.title=element_text(size=18,face="bold"), legend.position="top")
boxplot_dan
graph_file <- paste(dir,"CO2_Boxplot_dan.png",sep="")
ggsave(file=graph_file, plot=boxplot_dan)


# Attempt to make deviation from control graph
melted <- melt(ground_CO2, id.vars=c("Homeowner","Treatment","Round"), measure.vars="Lin_flux_per_capita")
means.sem <- ddply(melted, c("Homeowner","Treatment","Round","variable"), summarise,
                   mean=mean(value), sem=sd(value)/sqrt(length(value)))
#I ended up copying and pasting the means.sem data into Excel and then manually calculating the deviations
#between the leach field and control and sand filter and control.  Below I then import the resulting file
#back into R

devs_co2_file <- "Deviations_co2.csv"
devs_co2_data <- read.csv(paste(dir,devs_co2_file,sep=""))

levels(devs_co2_data$Treatment) <- c("Leach field","Sand filter")

boxplot_devs <- ggplot(devs_co2_data, aes(x=Treatment,y=Dev, fill=Treatment))+
  geom_abline(intercept=0,slope=0,linetype="dashed")+
  geom_boxplot(width=0.8)+
  coord_cartesian(ylim=c(-350,350))+
  scale_y_continuous(breaks=seq(from = -350, to = 350, by = 100))+
  ylab(expression(bold("Deviation from control, [g/capita/day]")))+
  xlab("Treatment")+
  scale_fill_grey(start=0.6,end=1)+
  #annotate("text",x=2, y=50,label="*",size=16)+
  theme_bw()+
  theme(axis.text=element_text(size=18), axis.title.x=element_text(size=18,face="bold",vjust=-0.4), 
        axis.title.y=element_text(size=18,vjust=1.2), legend.position="none")
boxplot_devs
graph_file <- paste(dir,"CO2_Boxplot_Deviation-from-control.png",sep="")
ggsave(file=graph_file, plot=boxplot_devs)

