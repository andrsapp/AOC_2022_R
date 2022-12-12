input <- readLines("day8.txt")
input

sum <- 0
for (j in 2:(length(input) - 1)) {
    for (i in 2:(nchar(input[j]) - 1)) {
        max_up <- max(substr(input[1:(j - 1)], i, i))
        max_down <- max(substr(input[(j + 1):length(input)], i, i))
        max_left <- max(strsplit(substr(input[j], 1, i - 1), "")[[1]])
        max_right <-
            max(strsplit(substr(input[j], i + 1, nchar(input[j])), "")[[1]])
        min_tree <- min(max_up, max_down, max_left, max_right)
        if (substr(input[j], i, i) > min_tree) sum <- sum + 1
    }
}
sum
edge <- nchar(input[1]) + nchar(input[length(input)]) + length(input) * 2 - 4
edge

sum + edge

# p2 ========================================================

f_up <- function(k) {
    return(ifelse(identical(k, integer(0)), 1, max(k)))
}

f_down <- function(k, n) {
    return(ifelse(identical(k, integer(0)), n, min(k)))
}

f_left <- function(k) {
    return(ifelse(identical(k, integer(0)), 1, max(k)))
}

f_right <- function(k, n) {
    return(ifelse(identical(k, integer(0)), n, min(k)))
}

input <- readLines("day8.txt")

wide <- nchar(input[1])
long <- length(input)

max_score <- 1
for (j in 2:(length(input) - 1)) {
    for (i in 2:(nchar(input[j]) - 1)) {
        tree <- substr(input[j], i, i)
        score <- 1

        up_pts <- j - f_up(which(substr(
            input[1:(j - 1)], i, i) >= tree))
        down_pts <- f_down(which(substr(
            input[(j + 1):length(input)], i, i) >= tree), long - j)
        left_pts <- i - f_left(which(strsplit(substr(
            input[j], 1, i - 1), "")[[1]] >= tree ))
        right_pts <- f_right(which(strsplit(substr(
            input[j], i + 1, nchar(input[j])), "")[[1]] >= tree), wide - i)
        score <- prod(up_pts, down_pts, left_pts, right_pts)

        if (score > max_score) max_score <- score
    }
}
max_score


prod(up_pts, down_pts, left_pts, right_pts)

which(substr(input[1:(j - 1)], i, i) >= tree)
which(substr(input[(j + 1):length(input)], i, i) >= tree)
min(which(substr(input[(j + 1):length(input)], i, i) >= tree))
