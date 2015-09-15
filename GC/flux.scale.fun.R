flux.scale.fun <- function(flux_data,chamber_volumes,dir,gas,date,name){
  
  # Define constants
  
  p_a <- 99261 # air pressure in Pa, as calculated using p_a = 1013000*exp(-A/8200).  
  # See eqn on pg 41 of Environmental Biophysics book.  
  # Assumed A = 547 ft (166.726 m), the elevation asl of the Ithaca weather stn
  
  m3_uL <- 1E-9 # converts uL to m^3
  
  d_s <- 86400 # coverts s to days
  
  R <- 8.314 # universal gas constant, units of Pa*m^3/mol*K
  
  collar_area <- 0.064 #m^2
  
  vent_area <- 0.0081 #m^2
  
  M_N2O <- 44.013 # g/mol
  M_CH4 <- 16.04  # g/mol
  M_CO2 <- 44.01  # g/mol
  
  # Load additional data for air temps and septic areas
  
  air_temps <- read.csv(paste(dir,"Air_temps.csv",sep="",collapse=NULL))
  air_temps$Date <- as.character(air_temps$Date)
  
  septic_areas <- read.csv(paste(dir,"Septic_area.csv",sep="",collapse=NULL))
  
  
  # Start calculations
  
  # Set molecular mass
  
  if (gas=="N2O"){
    MM <- M_N2O
  } else if (gas=="CH4"){
    MM <- M_CH4
  } else if (gas=="CO2"){
    MM <- M_CO2
  }
  
  # pull out air temperature
  date = paste("2015",date,sep="",collapse=NULL)
  t_a <- (air_temps[air_temps[,1]==name & air_temps[,2]==date,3]) + 273.15 #air temp in units K
  
  # Define empty vectors
  
  lin_mass_flux <- vector()
  lin_area_mass_flux <- vector()
  quad_mass_flux <- vector()
  quad_area_mass_flux <- vector()
  
  # convert volume flux to mass flux
  
  for (i in 1:nrow(flux_data)){
    
    #Force the levels of the treatment factors to be the same from the sample data and the septic areas data
    treatment <- factor(flux_data$Treatment[i], levels=unique(c(levels(septic_areas[,2]), levels(flux_data$Treatment))))
    septic_areas[,2] <- factor(septic_areas[,2], levels=unique(c(levels(septic_areas[,2]), levels(flux_data$Treatment))))
    
    lin_mass_flux[i] <- flux_data$Slope[i] * (chamber_volumes[i]/collar_area) * (p_a / t_a) * (MM / R) * m3_uL * d_s
    quad_mass_flux[i] <- flux_data$b_coeff[i] * (chamber_volumes[i]/collar_area) * (p_a / t_a) * (MM / R) * m3_uL * d_s
    
    # Find septic area and convert to meters squared
    if (treatment == "Control"){
      septic_area <- septic_areas[septic_areas[,1]==name,3]*0.0929
    } else {
      septic_area <- septic_areas[septic_areas[,1]==name & septic_areas[,2]==treatment,3]*0.0929
    }
    
    lin_area_mass_flux[i] <- lin_mass_flux[i]*septic_area
    quad_area_mass_flux[i] <- quad_mass_flux[i]*septic_area
    
  } 
  
  
  scaled_flux_data <- data.frame(Chamber=flux_data$Chamber,Treatment=flux_data$Treatment,Mass_flux_linear=lin_area_mass_flux,Mass_flux_quadratic=quad_area_mass_flux)
  
}
# Notes: units of lin_mass_flux are g per day per m^2
# Units of lin_area_mass_flux are g per day