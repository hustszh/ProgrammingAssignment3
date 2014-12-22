best <- function(state, outcome) {
    ## Read outcome data
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    head(outcome)
    ## Check that state and outcome are valid
    state_name <- as.character(state)
    if (is.na(state_name) | state_name == "") {
        stop("invalid state")
    }
    print(state_name)
    head(outcome[,"State"])
    if (!any(outcome[,"State"] == state_name)) {
        stop("invalid state 2")
    }
    
    ## Return hospital name in that state with lowest 30-day death
    
    ## rate
}