gas.directory.fun <-function(dir, gas, date, name, print=FALSE){
  # This function creates an array of file names that will be used to open appropriate csv
  # data files and to save your generated graphs
	
	OS <- readline(prompt="Enter W for Windows or M for Mac: ")
	
	if (OS == 'W'){
		dir <- gsub("\\\\","/",dir, fixed=TRUE)
	}
	
	sample_directory <- rep(NA_character_, 2)
	
	sample_directory <- c(paste(dir,"Data-",name,"/2015",date,"/2015",date,"_septic-",name,"_",gas,"_standard.csv", sep="", collapse=NULL), 
	paste(dir,"Data-",name,"/2015",date,"/2015",date,"_septic-",name,"_chamber-heights.csv", sep="", collapse=NULL),
	paste(dir,"Data-",name,"/2015",date,"/2015",date,"_septic-",name,"_",gas,".csv", sep="", collapse=NULL),
	paste(dir,"Data-",name,"/2015",date,"/2015",date,"_septic-",name,"_cap-velocity.csv", sep="", collapse=NULL),
	paste(dir,"Data-",name,"/2015",date,"/2015",date,"_septic-",name,"_",gas,"_graph.png", sep="", collapse=NULL),
	paste(dir,"Data-",name,"/2015",date,"/2015",date,"_septic-",name,"_",gas,"_processed.csv", sep="", collapse=NULL),
  paste(dir,"Data-",name,"/2015",date,"/2015",date,"_septic-",name,"_",gas,"_scaled_flux.csv",sep="",collapse=NULL),
	paste(dir,"Data-",name,"/2015",date,"/2015",date,"_septic-",name,"_",gas,"_conc.csv",sep="",collapse=NULL))
	
}
