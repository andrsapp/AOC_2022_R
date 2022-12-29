# day 20

library(rlist)
library(tidyverse)

input <- readLines("inputs/day20.txt")
input <- readLines("inputs/day20_sample.txt")

input <- as.integer(input)

input_list <- list()
for (i in seq_along(input)) {
    input_list <- append(input_list, list(c(number = input[i], position = i)))
}
head(input_list, 10)

len <- length(input) - 1

for (i in seq_along(input_list)) {
    k <- list.which(input_list, position == i)

    new_pos <- ((input_list[[k]][1]) %% len + k) %% (len + 1)

    if (new_pos == 0) new_pos <- len + 1

    input_list <- append(input_list, list(input_list[[k]])
        , after = new_pos)

    if (new_pos > k) {
        input_list <- input_list[-k]
    } else {
        input_list <- input_list[-(k + 1)]
    }
}

length(input_list)

zero_loc <- list.which(input_list, number == 0)

input_list[[((1000 %% (len + 1)) + zero_loc) %% (len + 1)]][1] +

input_list[[((2000 %% (len + 1)) + zero_loc) %% (len + 1)]][1] +

input_list[[((3000 %% (len + 1)) + zero_loc) %% (len + 1)]][1]

# p2 ==============================================================

key <- 811589153

input <- readLines("inputs/day20.txt")
input <- readLines("inputs/day20_sample.txt")

input <- as.integer(input)

len <- length(input) - 1

input <- input * (key %% len)
input
input_list <- list()
for (i in seq_along(input)) {
    input_list <- append(input_list, list(c(number = input[i], position = i)))
}
head(input_list, 10)



for (j in 1:10) {
for (i in seq_along(input_list)) {
    # convert to vector first?
    k <- list.which(input_list, position == i)

    new_pos <- ((input_list[[k]][1]) %% len + k) %% (len + 1)

    if (new_pos == 0) new_pos <- len + 1

    input_list <- append(input_list, list(input_list[[k]])
        , after = new_pos)

    if (new_pos > k) {
        input_list <- input_list[-k]
    } else {
        input_list <- input_list[-(k + 1)]
    }
}
}

head(input_list, 10)

length(input_list)

zero_loc <- list.which(input_list, number == 0)

input_list[[((1000 %% (len + 1)) + zero_loc) %% (len + 1)]][1] / (key %% len)

input_list[[((2000 %% (len + 1)) + zero_loc) %% (len + 1)]][1] / (key %% len)

input_list[[((3000 %% (len + 1)) + zero_loc) %% (len + 1)]][1] / (key %% len)



7973051839072