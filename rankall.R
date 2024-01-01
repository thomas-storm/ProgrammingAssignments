## Programming Assignment 3 for R Programming
## Function to display hospitals of a given rank ("best", "rank", "worst") for all states for a specific outcome 
## Possible outcomes: death rate within 30 days with original diagnosis of heart attack, heart failure, or pneumonia

rankall <- function(outcome, num = "best") {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        
        outcome.df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        state.list <- levels(as.factor(outcome.df[, 7]))
        
        i <- 1
        
        rank.hospital.state <- numeric(length = length(state.list))
        
        outcome.list <- c("heart attack", "heart failure", "pneumonia")
        
        if(!(outcome %in% outcome.list)){
                stop("invalid outcome")
        }
        if(outcome == outcome.list[1]){
                outcome.col <- 11
        }
        if(outcome == outcome.list[2]){
                outcome.col <- 17
        }
        if(outcome == outcome.list[3]){
                outcome.col <- 23
        }
        
        for (state in state.list){
              
                outcome.loop <- subset(outcome.df, State == state)
                
                outcome.loop <- outcome.loop[order(suppressWarnings(as.numeric(outcome.loop[,outcome.col])), outcome.loop[,2]),]
                
                outcome.loop[,outcome.col] <- suppressWarnings(as.numeric(outcome.loop[, outcome.col]))
                
                outcome.loop <- na.omit(outcome.loop)
                
                outcome.row <- list("best", "worst", 1:nrow(outcome.df))
                
                if(sum(sapply(outcome.row, FUN=function(X) num %in% X)) == 1){
                        
                        if(num == outcome.row[[1]]){
                                outcome.rank <- 1
                        }
                        if(num == outcome.row[[2]]){
                                outcome.rank <- nrow(outcome.loop)
                        }
                        if(num %in% outcome.row[[3]]){
                                outcome.rank <- num
                        }
                        
                        rownames(outcome.loop) <- NULL
                        rank.hospital <- outcome.loop[outcome.rank,2]
                }
                if(sum(sapply(outcome.row, FUN=function(X) num %in% X)) == 0){
                        rank.hospital <- NA
                }
                rank.hospital.state[i] <- rank.hospital
                i <- i + 1
        }
        rank.df <- data.frame(rank.hospital.state, state.list)
        row.names(rank.df) <- state.list
        rank.df
}