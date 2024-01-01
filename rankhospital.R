## Programming Assignment 3 for R Programming
## Function to display hospital in a specific state for a specific outcome for a specific rank ("best", "rank", "worst")
## Possible outcomes: death rate within 30 days with original diagnosis of heart attack, heart failure, or pneumonia

rankhospital <- function(state, outcome, num = "best") {
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
                outcome.col <- 11
        }
        if(outcome == outcome.list[2]){
                outcome.col <- 17
        }
        if(outcome == outcome.list[3]){
                outcome.col <- 23
        }
        
        outcome.row <- list("best", "worst", 1:nrow(outcome.df))
        
        if(sum(sapply(outcome.row, FUN=function(X) num %in% X)) == 1){
        
                if(num == outcome.row[[1]]){
                        outcome.rank <- 1
                }
                if(num == outcome.row[[2]]){
                        outcome.rank <- nrow(outcome.df)
                }
                if(num %in% outcome.row[[3]]){
                        outcome.rank <- num
                }
        
        outcome.df <- outcome.df[order(suppressWarnings(as.numeric(outcome.df[,outcome.col])), outcome.df[,2]),]
        rownames(outcome.df) <- NULL
        rank.hospital <- outcome.df[outcome.rank,2]
        }
        
        if(sum(sapply(outcome.row, FUN=function(X) num %in% X)) == 0){
                rank.hospital <- NA
        }
        rank.hospital
}