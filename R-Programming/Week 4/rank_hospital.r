rankhospital <- function(state, outcome, num = "best") {
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
	
	## Create Simplified Data Frame using only needed columns
	simple <- outcomes[, c(2,7,col_num)]

	## Name the new columns
	names(simple) <- c("hospital", "state", "outcome")

	## Remove NA values
	simple <- na.omit(simple)

	## Select the requested State
	state_simple <- simple[simple$state == state,]
	
	## Sort the state first by outcome, then by hospital
	sorted_state_simple <- state_simple[with(state_simple, order(outcome, hospital)),]
	
	## Return hospital name in that state with the given rank 
	if (num == "best"){
		return(sorted_state_simple$hospital[1])
	} else if (num == "worst"){
		new_num = nrow(sorted_state_simple)
		return(sorted_state_simple$hospital[new_num])
	} else if (num > nrow(sorted_state_simple)){
		return(NA)
	} else{
		return(sorted_state_simple$hospital[num])
	}
}

