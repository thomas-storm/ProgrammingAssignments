## Programming Assignment 3 for R Programming
## Function to find best hospital in a specific state for a specific outcome
## Possible outcomes: death rate within 30 days with original diagnosis of heart attack, heart failure, or pneumonia
## Test

best <- function(state, outcome){
        ## Read outcome data
        outcome.df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        state.list <- levels(as.factor(outcome.df[, 7]))
        if(!(state %in% state.list)){
                stop("invalid state")
        }
        outcome.list <- c("heart attack", "heart failure", "pneumonia")
        if(!(outcome %in% outcome.list)){
                stop("invalid outcome")
        }
        outcome.df <- subset(outcome.df, State == state)
        if(outcome == outcome.list[1]){
                outcome.df <- outcome.df[order(suppressWarnings(as.numeric(outcome.df[,11])), outcome.df[,2]),]
                rownames(outcome.df) <- NULL
                best.hospital <- outcome.df[1,2]
        }
        if(outcome == outcome.list[2]){
                outcome.df <- outcome.df[order(suppressWarnings(as.numeric(outcome.df[,17])), outcome.df[,2]),]
                rownames(outcome.df) <- NULL
                best.hospital <- outcome.df[1,2]
        }
        if(outcome == outcome.list[3]){
                outcome.df <- outcome.df[order(suppressWarnings(as.numeric(outcome.df[,23])), outcome.df[,2]),]
                rownames(outcome.df) <- NULL
                best.hospital <- outcome.df[1,2]
        }
        best.hospital
} 