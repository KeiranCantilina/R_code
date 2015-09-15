flux.fun <-function(conc_data, standard_coeffs, chamber_volumes, directory, gas, print=FALSE){
  
  # This is the septic system flux function
  # Generally, it subsets by chamber to plot and do the flux calculations at each
  # Last edited 08/03/2015 by Allison Truhlar
  
  library(ggplot2)
  library(gridExtra)
  
  collar_surface_area <- 0.064 #meters
  conc_data$Treatment <- as.character(conc_data$Treatment)
  
  #This for loop plots all the concentration values from each chamber over time
  
  max_conc <- max(conc_data$Concentration)
  min_conc <- min(conc_data$Concentration)
  num_chamber <- max(unique(conc_data$Chamber))
  
  p <- list()
  
#   for (i in 1:num_chamber){
#     data <- conc_data[conc_data$Chamber==i,]
#     p[[i]] <- qplot(Time_min,Concentration,
#                     data = data,
#                     geom = c("point","line"),
#                     xlab = "Time (min)", ylab="Concentration (ppm)",
#                     ylim = c(min_conc,max_conc),
#                     xlim = c(0,30),
#                     main = paste("Chamber ",i))
#   }
  #png(directory[5])
  #do.call(grid.arrange, c(p, ncol=3, main=paste(gas," concentration over time", sep="", collapse=NULL)))
  #dev.off()
  
  #Creating empty vectors for linear and quadratic fit coefficients
  
  treatment <- vector(mode="character")
  r_squared_lin <-vector()
  intercept <-vector()
  slope <-vector()
  r_squared_quad <-vector()
  b_coeff_quad <- vector() 
  flux_lin <-vector()
  flux_quad <-vector()
  
  na.omit.data <- data.frame(Concentration=conc_data$Concentration, Time=conc_data$Time)
  na.omit.data <- na.omit(na.omit.data)
  
  #This for loop calculates the linear and quadratic fits for the concentration values from each chamber
  #over time, resulting in a flux in units of uL/m^2/s
  
  for (i in 1:num_chamber){
    data <- conc_data[conc_data$Chamber==i,]
    treatment[i] <- data$Treatment[1]
    data$Time2 <- (data$Time)^2
    
    lin_model <- lm(Concentration ~ Time, data = data)
    r_squared_lin[i] <- summary(lin_model)$r.squared
    intercept[i] <- summary(lin_model)$coefficients[1]
    slope[i] <- summary(lin_model)$coefficients[2]
    
    quad_model <- lm(Concentration ~ Time + Time2, data = data)
    r_squared_quad[i] <- summary(quad_model)$r.squared
    b_coeff_quad[i] <- summary(quad_model)$coefficients[2]
    
    flux_lin[i] <-  slope[i]*chamber_volumes[i]/collar_surface_area #uL/m^2/s
    flux_quad[i] <- b_coeff_quad[i]*chamber_volumes[i]/collar_surface_area #uL/m^2/s
    
  }
  
  Chamber_nums <- unique(conc_data$Chamber)
  
  flux_data <- data.frame(Chamber=Chamber_nums, Treatment=treatment, Linear_flux=flux_lin,Slope=slope,Intercept=intercept,Linear_r_squared=r_squared_lin,Quadratic_flux=flux_quad,b_coeff=b_coeff_quad,Quadratic_r_squared=r_squared_quad)
  
  
}