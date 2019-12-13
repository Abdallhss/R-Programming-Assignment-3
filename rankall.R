outcome <- read.csv("outcome-of-care-measures.csv",
                    colClasses = "character")

disease = c("heart attack","heart failure","pneumonia")
dis_cols = c(11,17,23)
names(dis_cols) = disease
unique_states = unique(outcome$State,)

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

rankall <- function(dis,rank="best"){
    rank_hospital = vector(length = length(unique_states),
                           mode ="numeric")
    for (i in seq_along(unique_states)){
        rank_hospital[i] = rankhospital(unique_states[i],
                                        dis,rank)
    }
    Y = data.frame(rank_hospital,unique_states)
    Y[order(Y[2]),]
}
head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)

r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)

r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
