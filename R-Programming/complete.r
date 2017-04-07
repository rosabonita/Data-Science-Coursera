complete <- function(directory, id = 1:332) {
	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files

	## 'id' is an integer vector indicating the monitor ID numbers
	## to be used

	## Return a data frame of the form:
	## id nobs
	## 1 117
	## 2 1041
	## ...
	## where 'id' is the monitor ID number and 'nobs' is the
	## number of complete cases

	#change working directory
	working.dir <- getwd()
	new.dir <- paste(working.dir, directory, sep = "/")
	setwd(new.dir)

	# initialize empty data frame
	com_data <- data.frame()

	#read in tables
	for(i in id){
		if(i < 10){ ## add in '00' to match file names
			j <- paste("00",i, sep = "")
			my_data <- read.csv(paste(j,".csv", sep = ""), header = T, na.strings=c("NA","NaN", " "))

		}
		else if(i >= 10 && i <100){ # add in '00' to match file names
			j <- paste("0",i, sep = "")
			my_data <- read.csv(paste(j,".csv", sep = ""), header = T, na.strings=c("NA","NaN", " "))
		}
		else{
			my_data <- read.csv(paste(i,".csv", sep = ""), header = T, na.strings=c("NA","NaN", " "))

		}

	com_cases <- sum(complete.cases(my_data))
	com_data <- rbind(com_data,c(i,com_cases))
	
	}

	## reset working directory
	setwd(working.dir)

	## fix column names
	names(com_data) = c('id', 'nobs')

	## return com_data
	return(com_data)
}
