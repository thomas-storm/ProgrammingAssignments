library(httr)

url <- "http://biostat.jhsph.edu/~jleek/contact.html"
html = GET(url)
content = content(html, as="text")

content.split <- strsplit(content, "\n")
xx <- 1:180
spl.1 <- data.frame(xx, content.split)

source("./numcharinrow.R")
num.char.in.page()

## OR (could also be used in a function)

chars.row <- sapply(content.split, nchar)
counter <- c(10,20,30,100)
number <- numeric(4)
for (i in 1:4){ 
        number[i] <- chars.row[counter[i],1]
}
number