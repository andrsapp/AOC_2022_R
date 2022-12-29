# day 21

library(tidyverse)
library(rlist)
options(scipen = 999)

input <- readLines("inputs/day21.txt")
input <- readLines("inputs/day21_sample.txt")


# Pattern of numbers
ptn <- "[0-9]"
ndx <- grep(ptn, input)

# Expressions
input_list <- input[-ndx] %>%
    str_remove_all(":") %>%
    str_split(" ")
head(input_list, 10)

# Numbers
num_list <- input[ndx] %>%
    str_remove_all(":") %>%
    str_split(" ")
head(num_list, 10)


ordered_list <- list()
v <- c()
for (x in num_list) {
    ordered_list <- append(ordered_list, list(x))
    v[length(v) + 1] <- x[1]
}

ordered_list
v


while (length(input_list) > 0) {
    del_vec <- c()
    for (i in seq_along(input_list)) {
        if (input_list[[i]][2] %in% v && input_list[[i]][4]  %in% v) {
            ordered_list <- append(ordered_list, list(input_list[[i]]))
            v[length(v) + 1] <- input_list[[i]][1]
            del_vec[length(del_vec) + 1] <- i
        }
    }
    input_list <- input_list[-del_vec]
}

for (x in ordered_list) {
    if (str_detect(x[2], "[0-9]")) {
        eval(parse(text = paste0(x[1], "<-", x[2])))
    } else {
        eval(parse(text = paste0(x[1], "<-", x[2], x[3], x[4])))
    }
}

root

# p2 ==============================================================

input <- readLines("inputs/day21.txt")
input <- readLines("inputs/day21_sample.txt")


# Pattern of numbers
ptn <- "[0-9]"
ndx <- grep(ptn, input)

# Expressions
input_list <- input[-ndx] %>%
    str_remove_all(":") %>%
    str_split(" ")
head(input_list, 10)

# Numbers
ndx_human <- grep("^humn*", input, invert = TRUE)
ndx_num <- intersect(ndx, ndx_human)
num_list <- input[ndx_num] %>%
    str_remove_all(":") %>%
    str_split(" ")
head(num_list, 10)

start_input_list <- input_list


yell <- 5
input_list <- start_input_list
ordered_list <- list(c("humn", yell))
v <- c("humn")
for (x in num_list) {
    ordered_list <- append(ordered_list, list(x))
    v[length(v) + 1] <- x[1]
}

while (length(input_list) > 0) {
    del_vec <- c()
    for (i in seq_along(input_list)) {
        if (input_list[[i]][2] %in% v && input_list[[i]][4]  %in% v) {
            ordered_list <- append(ordered_list, list(input_list[[i]]))
            v[length(v) + 1] <- input_list[[i]][1]
            del_vec[length(del_vec) + 1] <- i
        }
    }
    input_list <- input_list[-del_vec]
}

ordered_list <- ordered_list[-1]



#reset
humn <- 3453748220116
inc <- 1

for (i in 1:100) {

    for (x in ordered_list) {
        if (str_detect(x[2], "[0-9]")) {
            eval(parse(text = paste0(x[1], "<-", x[2])))
        } else {
            eval(parse(text = paste0(x[1], "<-", x[2], x[3], x[4])))
        }
    }

    root_command <- ordered_list[[length(ordered_list)]]

    if (eval(parse(text = root_command[2])) > 5697586809113) {
        humn <- humn + inc
    } else {
        humn <- humn - inc
        break
    }
}
humn

eval(parse(text = root_command[2]))
eval(parse(text = root_command[4]))

15423341515360
5697586809113