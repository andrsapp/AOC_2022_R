# day 1

input <- readLines("inputs/day01_sample.txt")
input <- readLines("inputs/day01.txt")

input <- as.integer(input)

sum <- 0
most <- 0

for (i in seq_along(input)) {
    if (is.na(input[i])) {
        sum <- 0
    } else {
        sum <- sum + input[i]
        if (sum > most) most <- sum
    }
}
most

# p2 ====================================

input <- readLines("inputs/day01.txt")
input <- readLines("inputs/day01_sample.txt")

input <- as.integer(input)

sum <- 0
total <- c()

for (i in seq_along(input)) {
    if (is.na(input[i])) {
        total <- append(total, sum)
        sum <- 0
    } else {
        sum <- sum + input[i]
    }
    if (i == length(input)) total <- append(total, sum)
}

v <- sort(total, decreasing = TRUE)
sum(v[1:3])
