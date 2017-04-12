best <- function(state, outcome) {
	## Read outcome data
	outcomes <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available",stringsAsFactors=FALSE )

	## load state name data
	data(state)
	abb <- state.abb

	## Check that state is valid
	if (!(state %in% abb)){
		stop("invalid state")
	}
	
	## Check that outcome is valid
	valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
	if (!(outcome %in% valid_outcomes)){
		stop("invalid outcome")
	}
	
	## Translate outcome into column number
	## These column numbers are from the documentation
	if (outcome == "heart attack"){
		col_num = 11
	} else if (outcome == "heart failure"){
		col_num = 17
	} else if (outcome == "pneumonia"){
		col_num = 23
	}

	hospital_name_col = 2
	state_col = 7

	## Return hospital name in that state with lowest 30-day death rate
	## First, create a data set which has only the desired state
	x <- outcomes[(outcomes[,state_col] == state), ]

	## Remove NA values from the outcome column
	remove_na_col <- na.omit(x[,col_num])
	
	## Find the minimum mortality rate for the given outcome
	min_mort <- min(remove_na_col)

	## Find which rows have the minimum mortality rate for the given outcome
	w_min_mort <- suppressWarnings(which(x[,col_num] == min_mort))

	## Find the names of the hostpitals with the best mortality rates
	## Sort this list alphabetically according to tie rules
	best_list <- sort(x[w_min_mort, 2])

	## Return the first best hospital
	return(best_list[1])
}
