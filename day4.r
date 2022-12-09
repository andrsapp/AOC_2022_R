# Day 4 code

library(dplyr)

df <- as.data.frame((read.delim("day4.txt", header = FALSE, sep = ",")))
df

df$V1
x <- strsplit(df$V1, "-")
x
y <- matrix(unlist(x), ncol = 2, byrow = TRUE)
y
a1 <- as.data.frame(y)
a1

df$V2
x <- strsplit(df$V2, "-")
x
y <- matrix(unlist(x), ncol = 2, byrow = TRUE)
y
a2 <- as.data.frame(y)
a2

new <- data.frame(as.numeric(a1$V1)
    , as.numeric(a1$V2)
    , as.numeric(a2$V1)
    , as.numeric(a2$V2))
colnames(new) <- (c("a", "b", "c", "d"))
new

# -------------------------------------

new$test <- case_when(
    (new$a <= new$c & new$b >= new$d) ~ 1
    , (new$c <= new$a & new$d >= new$b) ~ 1
    , TRUE ~ 0

)
new
sum(new$test)

# ---------------------------------------

new$test <- case_when(
    new$b < new$c ~ 0
    , new$d < new$a ~ 0
    , TRUE ~ 1

)
new
sum(new$test)
