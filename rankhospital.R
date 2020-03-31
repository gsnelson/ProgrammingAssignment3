rankhospital <- function(st_abrev, outcome, num = "best") {
	library(dplyr)
	library(stringr)
	
	## Read outcome data
	ranks <-
		read.csv("datasets/outcome-of-care-measures.csv", na.strings = "Not Available")
	print(head(ranks))
	
	
	## Check that state and outcome are valid
	try(if (length(grep(st_abrev, state.abb)) == 0)
		stop("invalid state"))
	outcome_types <- c("heart attack", "heart failure", "pneumonia")
	try(if (length(grep(outcome, outcome_types)) == 0)
		stop("invalid outcome"))
	
	
	## Return hospital name in that state with the given rank
	outcome.t <- sub(" ", ".", str_to_title(outcome, locale = "en"))
	rank.c <-
		arrange(filter(
			select(
				ranks,
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
	
	hosp_count <- nrow(rank.c)

	if (num == "best" | num == 1) {
		return(rank.c[1, ])
	}
	else if (num == "worst" | num == hosp_count) {
		return(rank.c[hosp_count, ])
	}
	else if (num < hosp_count & num > 1) {
		return(rank.c[num, ])
	}
	else {
		return(NA)
	}
}

out <- rankhospital("WV", "heart failure", 25)
print(out)
