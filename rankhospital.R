outcome <- read.csv("outcome-of-care-measures.csv",
                    colClasses = "character")

disease = c("heart attack","heart failure","pneumonia")
dis_cols = c(11,17,23)
names(dis_cols) = disease
unique_states = unique(outcome$State)

rankhospital <- function(state,dis,rank){
    if (!is.element(dis,disease)){
        stop("invalid outcome")
    }
    if (!is.element(state,unique_states)){
        stop("invalid state")
    }
    X = outcome[outcome$State == state,c(2,dis_cols[dis])]
    X[,2] = as.numeric(X[,2])
    ordered_X = X[order(X[2],X[1],na.last = NA),]
    if(rank == 'best'){return(head(ordered_X,1)[1,1])}
    if(rank == 'worst'){return(tail(ordered_X,1)[1,1])}
    if (rank > dim(ordered_X)){
        return(NA)
    }
    ordered_X[rank,1]
}

rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)

rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)
