# day 15

library(tidyverse)

input <- readLines("inputs/day15.txt")
input <- readLines("inputs/day15_sample.txt")

input <- str_remove_all(input, "[a-zA-Z]|=")
input <- str_split(input, ",|:")
input <- map(input, as.integer)
#input


v_manhattan <- c()
for (i in seq_along(input)) {
    v_manhattan[length(v_manhattan) + 1] <- abs(input[[i]][1] - input[[i]][3]) +
                abs(input[[i]][2] - input[[i]][4])
}
v_manhattan


y <- 10
y <- 2000000

v_width <- c()
x_start <- c()
for (i in seq_along(input)) {
    y_dist <- abs(input[[i]][2] - y)

    v_width[i] <- max((v_manhattan[i] * 2 + 1) - 2 * y_dist, 0)

    x_start[i] <- input[[i]][1] - (v_width[i] - 1) / 2

}

x_end <- x_start + v_width - 1
x_start <- x_start[which(v_width != 0)]
x_end <- x_end[which(v_width != 0)]


v_union <- x_start[1]:x_end[1]
for (i in 2:length(x_start)) {
    v_union <- union(x_start[i]:x_end[i], v_union)
}

length(v_union) - 1

# p2 ===================================================
# doesn't work if point is on edge

input <- readLines("inputs/day15.txt")
input <- readLines("inputs/day15_sample.txt")

input <- str_remove_all(input, "[a-zA-Z]|=")
input <- str_split(input, ",|:")
input <- map(input, as.integer)
#input

v_manhattan <- c()
for (i in seq_along(input)) {
    v_manhattan[length(v_manhattan) + 1] <- abs(input[[i]][1] - input[[i]][3]) +
                abs(input[[i]][2] - input[[i]][4])
}
v_manhattan
input


i <- 1
yint_list <- list()
for (x in input) {
    yint_left_top <- x[2] - -1 * (x[1] - v_manhattan[i])
    yint_right_top <- x[2] - 1 * (x[1] + v_manhattan[i])
    yint_left_bot <- x[2] - 1 * (x[1] - v_manhattan[i])
    yint_right_bot <- x[2] - -1 * (x[1] + v_manhattan[i])
    yint_combine <- list(
        c(yint_left_top, yint_right_top, yint_left_bot, yint_right_bot))

    yint_list <- append(yint_list, yint_combine)
    i <- i + 1
}
yint_list

v <- list()
w <- list()
for (i in seq_along(yint_list)) {
    for (j in seq_along(yint_list)) {
        if (i != j) {
            if (yint_list[[i]][1] == yint_list[[j]][4] + 2) {
                v <- append(v, yint_list[[j]][4] + 1)
            }
            if (yint_list[[i]][4] == yint_list[[j]][1] + 2) {
                v <- append(v, yint_list[[j]][1] + 1)
            }
            if (yint_list[[i]][2] == yint_list[[j]][3] + 2) {
                w <- append(w, yint_list[[j]][3] + 1)
            }
            if (yint_list[[i]][3] == yint_list[[j]][2] + 2) {
                w <- append(w, yint_list[[j]][2] + 1)
            }

        }
    }
}
v <- unique(unlist(v))
w <- unique(unlist(w))


coord_list <- list()
for (i in seq_along(v)) {
    for (j in seq_along(w)) {
        x_coord <- (v[i] - w[j]) / 2
        y_coord <- x_coord + w[j]
        coord_list <- append(coord_list, list(c(x_coord, y_coord)))
    }
}
coord_list

coord_list[[1]][1] * 4000000 + coord_list[[1]][2]
