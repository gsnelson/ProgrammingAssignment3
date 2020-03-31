rankall <- function(outcome, num = "best") {
	library(dplyr)
	library(stringr)
	library(tidyverse)
	library(readr)
	
	state.rank <- c()
	
	## Read outcome data
	rank.all <- read_csv("datasets/outcome-of-care-measures.csv",
						 col_names = TRUE,
						 na = "Not Available")
	
	st.list <- sort(rank.all$State[!duplicated(rank.all$State)])
	
	## Check that outcome is valid
	outcome_types <- c("heart attack", "heart failure", "pneumonia")
	try(if (length(grep(outcome, outcome_types)) == 0)
		stop("invalid outcome"))
	
	
	## For each state, find the hospital of the given rank
	for (s in st.list) {
		rank.st <- arrange(filter(
			select(
				rank.all,
				hospital = "Hospital Name",
				state = "State",
				outcome.h =
					starts_with("Hospital 30-Day Death") &
					ends_with(outcome)
			),
			state == s,
			outcome.h != "NA"
		),
		outcome.h,
		hospital)
		
		hosp_count <- nrow(rank.st)
		
		if (num > hosp_count & num != "best" & num != "worst") {
			state.rank <- append(state.rank, c(NA, s))
		}	else {
			if (num == "best" | num == 1) {
				state.rank <- append(state.rank, rank.st[1, 1:2],
									 after = length(state.rank))
			} else if (num == "worst" | num == hosp_count) {
				state.rank <- append(state.rank, rank.st[hosp_count, 1:2],
									 after = length(state.rank))
			} else {
				state.rank <- append(state.rank, rank.st[num, 1:2],
									 after = length(state.rank))
			}
		}
		
	}
	## Return a data frame with the hospital names and the
	## (abbreviated) state name
	st.matrix <-
		matrix(
			state.rank,
			nrow = length(st.list),
			ncol = 2,
			byrow = TRUE,
			dimnames = list(st.list, c("hospital", "state"))
		)
	
	state.rankings <- as.data.frame(st.matrix)
	return(state.rankings)
	
}

out <- rankall("pneumonia", "worst")
print(out)
