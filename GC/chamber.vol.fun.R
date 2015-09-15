chamber.vol.fun <-function(chamber_heights, print=FALSE){
  # This function estimates the volume of the chambers based on the average of measured
  # chamber heights
	
	unit <- chamber_heights$unit[1]
	
	if(unit == 'in'){
		chamber_heights$Height=chamber_heights$Height*2.54
	}
	
	library(plyr)
	
	avg_height <- ddply(chamber_heights, c("Chamber"), summarise, mean=mean(Height))
	
	diameter <- 27.4 #cm
	
	volume <- sapply(avg_height[,2], function(x) x*pi*(diameter/2)^2*0.001)
	
}