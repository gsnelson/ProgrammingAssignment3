best <- function(st_abrev, outcome) {
	library(dplyr)
	library(stringr)
	
	## Read outcome data
	vals <-
		read.csv("datasets/outcome-of-care-measures.csv", na.strings = "Not Available")

	
	## Check that state and outcome are valid
	try(if (length(grep(st_abrev, state.abb)) == 0)
		stop("invalid state"))
	outcome_types <- c("heart attack", "heart failure", "pneumonia")
	try(if (length(grep(outcome, outcome_types)) == 0)
		stop("invalid outcome"))

		
	## Return hospital name in that state with lowest 30-day death rate
	outcome.t <- sub(" ", ".", str_to_title(outcome, locale = "en"))
	best.c <-
		arrange(filter(
			select(
				vals,
				hosp.h = Hospital.Name,
				state.h = State,
				outcome.h = starts_with("Hospital.30.Day.Death") &
					ends_with(outcome.t)
			),
			state.h == st_abrev,
			outcome.h != "NA"
		),
		outcome.h,
		hosp.h)
	return(best.c[1,])
}

out <- best("SC", "heart attack")
print(out)