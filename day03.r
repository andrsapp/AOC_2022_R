# day 3

library(stringr)

input <- readLines("inputs/day03.txt")
input <- readLines("inputs/day03_sample.txt")
input


first_half <- substr(input, 1, nchar(input) / 2)
second_half <- substr(input, nchar(input) / 2 + 1, nchar(input))

first_list <- str_split(first_half, "")
second_list <- str_split(second_half, "")


v <- c()
for (i in seq_along(first_list)) {
    v[i] <- intersect(first_list[[i]], second_list[[i]])
}

v <- as.data.frame(v)
colnames(v) <- "letters"
pts <- data.frame(
        letters = c(letters, toupper(letters))
        , points =  seq_len(26 * 2))


fin <- merge(
    v
    , pts
)
sum(fin$points)

# p2 =================================================

input <- readLines("inputs/day03.txt")
input <- readLines("inputs/day03_sample.txt")
input

snack_list <- str_split(input, "")

v <- c()
for (i in seq(1, length(snack_list) - 2, by = 3)) {
    v[length(v) + 1] <- Reduce(intersect, list(
        snack_list[[i]]
        , snack_list[[i + 1]]
        , snack_list[[i + 2]]))
}

v <- as.data.frame(v)
colnames(v) <- "letters"
pts <- data.frame(
        letters = c(letters, toupper(letters))
        , points =  seq_len(26 * 2))


fin <- merge(
    v
    , pts
)
sum(fin$points)
