# Day 3 puzzle



sample <- cbind("vJrwpWtwJgWrhcsFMMfFFhFp"
, "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
, "PmmdzqPrVvPwwTWBwg"
, "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
, "ttgJtRGJQctTZtZT"
, "CrZsJsPPZsGzwwsLwLmpwMDw")

sample <- as.data.frame((read.csv("day3.txt")))
sample

half1 <- data.frame()
half2 <- data.frame()


for (i in seq_len(nrow(sample))) {
    half1[i, 1] <- substr(sample[i, 1], 1, nchar(sample[i, 1]) / 2)
    half2[i, 1] <- substr(sample[i, 1], nchar(sample[i, 1]) / 2 + 1, nchar(sample[i, 1]))
}
half1
half2

i <- 2
substr(sample[i,1], 1, nchar(sample[i,1]) / 2)

paste0(half1, half2) == sample

df <- cbind(half1, half2)
df

v <- c()
v

for (a in seq_len(nrow(df))) {
    for (i in 1:(nchar(df[a, 1]))) {
        for (j in 1:(nchar(df[a, 2]))) {
            k <- ifelse(
                substr(df[a, 1], i, i) == substr(df[a, 2], j, j)
                , substr(df[a, 1], i, i)
                , "N"
                )
            if (k != "N") break
        }
        if (k != "N") break

    }
    v[a] <- k
}
v <- as.data.frame(v)
colnames(v) <- "a"
v
pts <- cbind(c(letters, toupper(letters)), seq_len(26*2))
pts <- data.frame(c(letters, toupper(letters)), seq_len(26*2))
colnames(pts) <- c("a", "b")
pts

fin <- merge(
    v
    , pts
)
sum(fin$b)

#==============================================================

sample <- c("vJrwpWtwJgWrhcsFMMfFFhFp"
, "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
, "PmmdzqPrVvPwwTWBwg"
, "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
, "ttgJtRGJQctTZtZT"
, "CrZsJsPPZsGzwwsLwLmpwMDw")

sample <- as.data.frame((read.csv("day3.txt")))
df <- data.frame(sample)
df

new <- data.frame()

b <- 1
for (a in seq(1, (nrow(df)), by = 3)) {
    new[b, 1] <- df[a, 1]
    new[b, 2] <- df[a + 1, 1]
    new[b, 3] <- df[a + 2, 1]
    b <- b + 1
}
new
df <- new
df

v <- c()
for (a in seq_len(nrow(df))) {
    for (x in 1:(nchar(df[a, 3]))) {
        for (i in 1:(nchar(df[a, 1]))) {
            for (j in 1:(nchar(df[a, 2]))) {
                k <- ifelse(
                    substr(df[a, 1], i, i) == substr(df[a, 2], j, j)
                    & substr(df[a, 1], i, i) == substr(df[a, 3], x, x)
                    & substr(df[a, 2], j, j) == substr(df[a, 3], x, x)
                    , substr(df[a, 1], i, i)
                    , "N"
                    )
                if (k != "N") break
            }
            if (k != "N") break
        }
        if (k != "N") break
    }
    v[a] <- k
}
v <- as.data.frame(v)
colnames(v) <- "a"
v
pts <- cbind(c(letters, toupper(letters)), seq_len(26*2))
pts <- data.frame(c(letters, toupper(letters)), seq_len(26*2))
colnames(pts) <- c("a", "b")
pts

fin <- merge(
    v
    , pts
)
sum(fin$b)
fin