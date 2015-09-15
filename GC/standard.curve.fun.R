standard.curve.fun <-function(standard_data, print=FALSE){
# This is the septic standard curve function
# Last edited 07/14/2014 by Allison Truhlar
	
# Create empty variable vectors
	R_squared <- vector()
	standard_coeffs <- vector()
	intercept <- vector()
	slope <- vector()
	std_curve_num <- vector()
  
  # Counts the columns in standard_data (varies based on the # of runs and related standard curves)
	ncol <- ncol(standard_data)
	
	for (i in 1:(ncol-2)){
		
		std_curve_num[i] <- i
		
    # Extract the area for each of the standards for the standard curve of interest
		area <- standard_data[,(i+2)]
    
    # Work-around for manually removed standard points
    standard <- data.frame(Known_ppm = standard_data$Known_ppm, Area = area)
    standard <- na.omit(standard)
		
    # Linear fit of known ppm of each of the standards versus the area
		standard_curve <- lm(standard$Known_ppm ~ standard$Area)
		
    # R-squared value from the linear fit
		R_squared[i] <- summary(standard_curve)$r.squared
	
    # Quick check
	if (R_squared[i] < 0.95){
		
		continue <- readline(prompt = paste("Warning: R-squared for standard curve is less than 0.95. R-squared = ", 		R_squared[i], sep="", collapse=NULL))
		
	}
	
  # Extract the intercept and slope from the linear fit
	intercept[i] <- summary(standard_curve)$coefficients[1]
	slope[i] <- summary(standard_curve)$coefficients[2]
	
	}	
	
  # Output the intercept, slope, and r-squared for each standard curve
	standard_coeffs <- data.frame(Std_Curve_Num=std_curve_num,Intercept=intercept,Slope=slope,r_squared=R_squared)
		
}