library(rbenchmark)
library(microbenchmark)

benchmark(split = {
                sapply(split(DT$pwgtp15,DT$SEX),mean)
        },
        tapply = {
                tapply(DT$pwgtp15,DT$SEX,mean)
        },
        bySEX = {
                DT[,mean(pwgtp15),by=SEX]
        },
        replications = 1500,
        columns = c("test", "replications", "elapsed", 
                    "relative", "user.self", "sys.self")
        )

benchmark(split = {
        sapply(split(DT1$pwgtp15,DT1$SEX),mean)
},
tapply = {
        tapply(DT1$pwgtp15,DT1$SEX,mean)
},
bySEX = {
        DT1[,mean(pwgtp15),by=SEX]
},
replications = 300,
columns = c("test", "replications", "elapsed", 
            "relative", "user.self", "sys.self")
)