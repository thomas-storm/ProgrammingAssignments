## For Quiz

num.char.in.page <- function(x1 = 10, x2 = 20, x3 = 30, x4 = 100){
        counter <- c(x1,x2,x3,x4)
        num.char <- numeric(4)
        for(i in 1:4){
                num.char[i] <- nchar(spl.1[spl.1$xx == counter[i], 2])
        }
        num.char
}1