# day 14

library(tidyverse)

input <- readLines("inputs/day14.txt")
input <- readLines("inputs/day14_sample.txt")

input <- str_split(input, " -> |,") %>%
    map(as.integer)

input

# get edges
left_edge <- 500
right_edge <- 500
bottom_edge <- 0
for (i in seq_along(input)) {
    for (j in seq_along(input[[i]])) {
        if (j %% 2 != 0 && input[[i]][j] < left_edge)
            left_edge <- input[[i]][j]
        else if (j %% 2 != 0 && input[[i]][j] > right_edge)
            right_edge <- input[[i]][j]
        else if (j %% 2 == 0 && input[[i]][j] > bottom_edge)
            bottom_edge <- input[[i]][j]
    }
}
left_edge
right_edge
bottom_edge

# rescale list
for (i in seq_along(input)) {
    for (j in seq_along(input[[i]])) {
        if (j %% 2 != 0)
            input[[i]][j] <- input[[i]][j] - left_edge + 1
        else
            input[[i]][j] <- input[[i]][j] + 1
    }
}
input

# create empty waterfall cave
waterfall <- matrix(rep(".")
    , nrow = bottom_edge + 1
    , ncol = right_edge - left_edge + 1
    )
waterfall


# fill in rocks
f_fillrocks <- function(line, waterfall) {
    for (k in seq(1, length(input[[line]]) - 3, by = 2)) {
        for (i in input[[line]][k + 1]:input[[line]][k + 3]) {
            for (j in input[[line]][k]:input[[line]][k + 2]) {
                waterfall[i, j] <- "#"
            }
        }
    }
    return(waterfall)
}


for (line in seq_along(input)) {
    waterfall <- f_fillrocks(line, waterfall)
}
start_waterfall <- waterfall

# sand fill
sand_entry <- 500 - left_edge + 1


# reset
waterfall <- start_waterfall
for (i in 1:1000) {
    waterfall <- f_fillsand(waterfall)
}
#waterfall

length(which(waterfall == "o"))

# p2 =====================================================================

waterfall <- start_waterfall

col_add <- 1000
empty_matrix <- matrix(rep("."), nrow = nrow(waterfall), ncol = col_add)
waterfall <- cbind(empty_matrix, waterfall, empty_matrix)

waterfall[nrow(waterfall), ] <- "#"

start_waterfall <- waterfall

# sand fill
sand_entry <- 500 - left_edge + 1 + col_add
sand_entry

# reset
waterfall <- start_waterfall
for (i in 1:100000) {
    waterfall <- f_fillsand(waterfall)
}

length(which(waterfall == "o"))
