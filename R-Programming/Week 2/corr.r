corr <- function(directory, threshold = 0) {
	## 'directory' is a character of vector length 1
	## indicating the location of the csv files

	## 'threshold' is a numeric vector of length 1
	## indicating the number of completely observed observations
	## (on all drives) required to compute the correlation between 
	## nitrate and sulfate; the default is 0
	
	## return a numeric vector of correlations
	## NOTE: Do not round the result!

	#change working directory
	working.dir <- getwd()
	new.dir <- paste(working.dir, directory, sep = "/")
	setwd(new.dir)
	
	#create empty variables
	full_data = NULL
	corr_vec = NULL

	#read in tables
	for(i in 1:length(list.files())){
		if(i < 10){ ## add in '00' to match file names
			j <- paste("00",i, sep = "")
			full_data <- read.csv(paste(j,".csv", sep = ""),na.strings=c("NA","NaN", " "))
		}
		else if(i >= 10 && i <100){ # add in '00' to match file names
			j <- paste("0",i, sep = "")
			full_data <- read.csv(paste(j,".csv", sep = ""),na.strings=c("NA","NaN", " "))
		}
		else{
			full_data <- read.csv(paste(i,".csv", sep = ""),na.strings=c("NA","NaN", " "))
		}

		## remove NA values
		full_data = na.omit(full_data)

		##check that it meets the threshold
		if (nrow(full_data) > threshold) {
                                  corr_vec = c(corr_vec, cor(full_data[,2], full_data[,3]))
                               }
    
	}
	
	## reset working directory
	setwd(working.dir)
	
	return(corr_vec)

}
	
