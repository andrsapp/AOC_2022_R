# day 23

library(tidyverse)

input <- readLines("inputs/day23.txt")
input <- readLines("inputs/day23_sample.txt")

input <- strsplit(input, "")

grove <- matrix(unlist(input), nrow = length(input), byrow = TRUE)


start_coord <- c()
for (i in seq_len(nrow(grove))) {
    for (j in seq_len(ncol(grove))) {
        if (grove[i, j] == "#") {
            coord <- complex(real = i, imaginary = j)
            start_coord[length(start_coord) + 1] <- coord
        }
    }
}


check_north <- function(m, coord_move) {
    if (!((coord_move[m] - 1 + 0i) %in% coord_move ||
        (coord_move[m] - 1 - 1i) %in% coord_move ||
        (coord_move[m] - 1 + 1i) %in% coord_move)
        ) return(1)
    else return(0)
}

check_south <- function(m, coord_move) {
    if (!((coord_move[m] + 1 + 0i) %in% coord_move ||
        (coord_move[m] + 1 - 1i) %in% coord_move ||
        (coord_move[m] + 1 + 1i) %in% coord_move)
        ) return(1)
    else return (0)
}

check_west <- function(m, coord_move) {
    if (!((coord_move[m] + 0 - 1i) %in% coord_move ||
        (coord_move[m] + 1 - 1i) %in% coord_move ||
        (coord_move[m] - 1 - 1i) %in% coord_move)
        ) return(1)
    else return(0)
}

check_east <- function(m, coord_move) {
    if (!((coord_move[m] + 0 + 1i) %in% coord_move ||
        (coord_move[m] + 1 + 1i) %in% coord_move ||
        (coord_move[m] - 1 + 1i) %in% coord_move)
        ) return(1)
    else return(0)
}

check_direction <- function(k, m, coord_move, proposal) {
    if (k %% 4 == 1) {
        if (check_north(m, coord_move) == 1)
            proposal[length(proposal) + 1] <- coord_move[m] - 1 + 0i
        else if (check_south(m, coord_move) == 1)
            proposal[length(proposal) + 1] <- coord_move[m] + 1 + 0i
        else if (check_west(m, coord_move) == 1)
            proposal[length(proposal) + 1] <- coord_move[m] + 0 - 1i
        else if (check_east(m, coord_move) == 1)
            proposal[length(proposal) + 1] <- coord_move[m] + 0 + 1i
        else
            proposal[length(proposal) + 1] <- coord_move[m] + 0 + 0i

    } else if (k %% 4 == 2) {
        if (check_south(m, coord_move) == 1)
            proposal[length(proposal) + 1] <- coord_move[m] + 1 + 0i
        else if (check_west(m, coord_move) == 1)
            proposal[length(proposal) + 1] <- coord_move[m] + 0 - 1i
        else if (check_east(m, coord_move) == 1)
            proposal[length(proposal) + 1] <- coord_move[m] + 0 + 1i
        else if (check_north(m, coord_move) == 1)
            proposal[length(proposal) + 1] <- coord_move[m] - 1 + 0i
        else
            proposal[length(proposal) + 1] <- coord_move[m] + 0 + 0i
    } else if (k %% 4 == 3) {
        if (check_west(m, coord_move) == 1)
            proposal[length(proposal) + 1] <- coord_move[m] + 0 - 1i
        else if (check_east(m, coord_move) == 1)
            proposal[length(proposal) + 1] <- coord_move[m] + 0 + 1i
        else if (check_north(m, coord_move) == 1)
            proposal[length(proposal) + 1] <- coord_move[m] - 1 + 0i
        else if (check_south(m, coord_move) == 1)
            proposal[length(proposal) + 1] <- coord_move[m] + 1 + 0i
        else
            proposal[length(proposal) + 1] <- coord_move[m] + 0 + 0i
    } else if (k %% 4 == 0) {
        if (check_east(m, coord_move) == 1)
            proposal[length(proposal) + 1] <- coord_move[m] + 0 + 1i
        else if (check_north(m, coord_move) == 1)
            proposal[length(proposal) + 1] <- coord_move[m] - 1 + 0i
        else if (check_south(m, coord_move) == 1)
            proposal[length(proposal) + 1] <- coord_move[m] + 1 + 0i
        else if (check_west(m, coord_move) == 1)
            proposal[length(proposal) + 1] <- coord_move[m] + 0 - 1i
        else
            proposal[length(proposal) + 1] <- coord_move[m] + 0 + 0i
    }
    return(proposal)
}


final <- start_coord
for (k in 1:10) {
# for (k in seq_along(coord_move)) {
    coord_move <- final
    proposal <- c()
    for (m in seq_along(coord_move)) {
        if (sum(
            check_north(m, coord_move)
            , check_south(m, coord_move)
            , check_west(m, coord_move)
            , check_east(m, coord_move)) == 4) {
                proposal[m] <- coord_move[m]
        } else {
            proposal <- check_direction(k, m, coord_move, proposal)
        }
    }
    final <- c()
    for (m in seq_along(proposal)) {
        if (proposal[m] %in% proposal[-m]) final[length(final) + 1] <-
            coord_move[m]
        else final[length(final) + 1] <- proposal[m]
    }
}

end <- final

(max(Re(end)) - min(Re(end)) + 1) * (max(Im(end)) - min(Im(end)) + 1) -
length(end)


# p2 ======================================================

final <- start_coord
k <- 1
move <- 1

while (move != 0) {

    coord_move <- final
    move <- 0
    proposal <- c()

    for (m in seq_along(coord_move)) {
        if (sum(
            check_north(m, coord_move)
            , check_south(m, coord_move)
            , check_west(m, coord_move)
            , check_east(m, coord_move)) == 4) {
                proposal[m] <- coord_move[m]
        } else {
            proposal <- check_direction(k, m, coord_move, proposal)
            move <- move + 1
        }
    }

    final <- c()
    for (m in seq_along(proposal)) {
        if (proposal[m] %in% proposal[-m]) final[length(final) + 1] <-
            coord_move[m]
        else final[length(final) + 1] <- proposal[m]
    }
    k <- k + 1
}

k - 1
