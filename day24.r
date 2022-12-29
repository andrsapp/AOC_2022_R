# day 24 ==========================================================

library(tidyverse)

input <- readLines("inputs/day24.txt")
input <- readLines("inputs/day24_sample.txt")

valley <- matrix(unlist(strsplit(input, ""))
    , nrow = length(input), byrow = TRUE)

start_col <- which(valley[1, ] ==".")
end_col <- which(valley[nrow(valley), ] == ".")


s_valley_right <- matrix(unlist(strsplit(str_replace_all(
    input, "<|v|\\^", "."), "")), nrow = length(input), byrow = TRUE)

s_valley_left <- matrix(unlist(strsplit(str_replace_all(
    input, ">|v|\\^", "."), "")), nrow = length(input), byrow = TRUE)

s_valley_up <- matrix(unlist(strsplit(str_replace_all(
    input, ">|v|<", "."), "")), nrow = length(input), byrow = TRUE)

s_valley_down <- matrix(unlist(strsplit(str_replace_all(
    input, ">|\\^|<", "."), "")), nrow = length(input), byrow = TRUE)


f_right <- function(old) {
    new <- old
    new[2:(nrow(old) - 1), 2] <- old[2:(nrow(old) - 1), ncol(old) - 1]
    for (i in 3:(ncol(old) - 1)) {
        new[2:(nrow(old) - 1), i] <- old[2:(nrow(old) - 1), i - 1]
    }
    return(new)
}

f_left <- function(old) {
    new <- old
    new[2:(nrow(old) - 1), ncol(old) - 1] <- old[2:(nrow(old) - 1), 2]
    for (i in 2:(ncol(old) - 2)) {
        new[2:(nrow(old) - 1), i] <- old[2:(nrow(old) - 1), i + 1]
    }
    return(new)
}

f_up <- function(old) {
    new <- old
    new[nrow(old) - 1, ] <- old[2, ]
    for (i in 2:(nrow(old) - 2)) {
        new[i, ] <- old[i + 1, ]
    }
    return(new)
}

f_down <- function(old) {
    new <- old
    new[2, ] <- old[nrow(old) - 1, ]
    for (i in 3:(nrow(old) - 1)) {
        new[i, ] <- old[i - 1, ]
    }
    return(new)
}


# north, south, east, west, wait
dr <- c(-1, 1, 0, 0, 0)
dc <- c(0, 0, 1, -1, 0)


# reset
rq <- 1
cq <- start_col

valley_right <- f_right(s_valley_right)
valley_left <- f_left(s_valley_left)
valley_up <- f_up(s_valley_up)
valley_down <- f_down(s_valley_down)

moves <- 0
nodes_left <- 1
nodes_next <- 0

row_end <- nrow(valley)
col_end <- end_col

#f_expedition <- function(row_end, col_end) {
while (TRUE) {
    r_idx <- rq[1]
    c_idx <- cq[1]

    rq <- rq[-1]
    cq <- cq[-1]

    if (r_idx == row_end && c_idx == col_end) break

    for (i in 1:5) {
        rr <- r_idx + dr[i]
        cc <- c_idx + dc[i]

        if (rr < 1 || rr > nrow(valley)) next
        if (cc < 1 || cc > ncol(valley)) next

        if (valley_right[rr, cc] != ".") next
        if (valley_left[rr, cc] != ".") next
        if (valley_up[rr, cc] != ".") next
        if (valley_down[rr, cc] != ".") next

        rq <- c(rq, rr)
        cq <- c(cq, cc)

        nodes_next <- nodes_next + 1

    }

    nodes_left <- nodes_left - 1
    if (nodes_left == 0) {

        unique_coords <- unique(complex(real = rq, imaginary = cq))
        rq <- Re(unique_coords)
        cq <- Im(unique_coords)

        nodes_left <- length(rq)
        nodes_next <- 0
        moves <- moves + 1



        # blizzard step
        valley_right <- f_right(valley_right)
        valley_left <- f_left(valley_left)
        valley_up <- f_up(valley_up)
        valley_down <- f_down(valley_down)
    }

}
#return(moves)
#}

moves


# p2 =============================================

answer <- moves

# reset
rq <- nrow(valley)
cq <- end_col

moves <- 0
nodes_left <- 1
nodes_next <- 0

row_end <- 1
col_end <- start_col

while (TRUE) {
    r_idx <- rq[1]
    c_idx <- cq[1]

    rq <- rq[-1]
    cq <- cq[-1]

    if (r_idx == row_end && c_idx == col_end) break

    for (i in 1:5) {
        rr <- r_idx + dr[i]
        cc <- c_idx + dc[i]

        if (rr < 1 || rr > nrow(valley)) next
        if (cc < 1 || cc > ncol(valley)) next

        if (valley_right[rr, cc] != ".") next
        if (valley_left[rr, cc] != ".") next
        if (valley_up[rr, cc] != ".") next
        if (valley_down[rr, cc] != ".") next

        rq <- c(rq, rr)
        cq <- c(cq, cc)

        nodes_next <- nodes_next + 1

    }

    nodes_left <- nodes_left - 1
    if (nodes_left == 0) {

        unique_coords <- unique(complex(real = rq, imaginary = cq))
        rq <- Re(unique_coords)
        cq <- Im(unique_coords)

        nodes_left <- length(rq)
        nodes_next <- 0
        moves <- moves + 1



        # blizzard step
        valley_right <- f_right(valley_right)
        valley_left <- f_left(valley_left)
        valley_up <- f_up(valley_up)
        valley_down <- f_down(valley_down)
    }

}

moves
answer <- answer + moves

# reset
rq <- 1
cq <- start_col

moves <- 0
nodes_left <- 1
nodes_next <- 0

row_end <- nrow(valley)
col_end <- end_col

while (TRUE) {
    r_idx <- rq[1]
    c_idx <- cq[1]

    rq <- rq[-1]
    cq <- cq[-1]

    if (r_idx == row_end && c_idx == col_end) break

    for (i in 1:5) {
        rr <- r_idx + dr[i]
        cc <- c_idx + dc[i]

        if (rr < 1 || rr > nrow(valley)) next
        if (cc < 1 || cc > ncol(valley)) next

        if (valley_right[rr, cc] != ".") next
        if (valley_left[rr, cc] != ".") next
        if (valley_up[rr, cc] != ".") next
        if (valley_down[rr, cc] != ".") next

        rq <- c(rq, rr)
        cq <- c(cq, cc)

        nodes_next <- nodes_next + 1

    }

    nodes_left <- nodes_left - 1
    if (nodes_left == 0) {

        unique_coords <- unique(complex(real = rq, imaginary = cq))
        rq <- Re(unique_coords)
        cq <- Im(unique_coords)

        nodes_left <- length(rq)
        nodes_next <- 0
        moves <- moves + 1



        # blizzard step
        valley_right <- f_right(valley_right)
        valley_left <- f_left(valley_left)
        valley_up <- f_up(valley_up)
        valley_down <- f_down(valley_down)
    }

}
moves

answer <- answer + moves

answer
