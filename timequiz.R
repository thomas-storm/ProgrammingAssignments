system.time({
        n <- 5000
        for(i in 1:n){
                sapply(split(DT$pwgtp15,DT$SEX),mean)
        }
})

system.time({
        n <- 5000
        for(i in 1:n){
                tapply(DT$pwgtp15,DT$SEX,mean)
        }
})

system.time({
        n <- 5000
        for(i in 1:n){
                mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15)
        }
})

system.time({
        n <- 5000
        for(i in 1:n){
                DT[,mean(pwgtp15),by=SEX]
        }
})

