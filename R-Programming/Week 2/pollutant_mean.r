pollutantmean <- function(directory, pollutant, id = 1:332){
	## 'directory' is a character vector of length 1 indicating the location 
	## of the CSV files.

	## 'pollutant' is a character vector of length 1 indicating the name of 
	## the pollutant for which we will calculate the mean; either "sulfate" 
	## or "nitrate".

	## 'id' is an integer vector indicating the monitor ID numbers to be used.

	##Return the mean of the pollutant accross all monitors listed in the 	
	## 'id' vector (ignoring NA values). NOTE: Do not round the result!

	#change working directory
	working.dir <- getwd()
	new.dir <- paste(working.dir, directory, sep = "/")
	setwd(new.dir)
	
	
	full_data = data_frame()
	#read in tables
	for(i in id){
		if(i < 10){ ## add in '00' to match file names
			j <- paste("00",i, sep = "")
			full_data<- rbind(full_data, read.csv(paste(j,".csv", sep = ""),na.strings=c("NA","NaN", " ")))
		}
		else if(i >= 10 && i <100){ # add in '00' to match file names
			j <- paste("0",i, sep = "")
			full_data<- rbind(full_data,read.csv(paste(j,".csv", sep = ""),na.strings=c("NA","NaN", " ")))
		}
		else{
			full_data<- rbind(full_data, read.csv(paste(i,".csv", sep = ""),na.strings=c("NA","NaN", " ")))
		}
	
	}

        ## reset working directory
        setwd(working.dir)

        ## remove NA values
        full_data = na.omit(full_data)
	  n = nrow(full_data)

        ## extract appropriate column
        p_data <- select(full_data, one_of(pollutant))

        ##calculate mean
        p_sum <- sum(p_data)

        ## return value
        return(p_sum/n)
}
