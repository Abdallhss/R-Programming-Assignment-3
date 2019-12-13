outcome <- read.csv("outcome-of-care-measures.csv",
                    colClasses = "character")

disease = c("heart attack","heart failure","pneumonia")
dis_cols = c(11,17,23)
names(dis_cols) = disease
unique_states = unique(outcome$State)

best <- function(state,dis){
    if (!is.element(dis,disease)){
        stop("invalid outcome")
    }
    if (!is.element(state,unique_states)){
        stop("invalid state")
    }
    X = outcome[outcome$State == state,c(2,dis_cols[dis])]
    X[,2] = as.numeric(X[,2])
    ordered_X = X[order(X[2],X[1]),]
    ordered_X[1,1]
}

best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("NY", "hert attack")

best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")
