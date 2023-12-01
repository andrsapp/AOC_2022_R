# day 25

library(tidyverse)
options(scipen = 999)

input <- readLines("inputs/day25.txt")
input <- readLines("inputs/day25_sample.txt")

input <- strsplit(input, "") %>%
    map(function(x) str_replace_all(x, "-", "-1")) %>%
    map(function(x) str_replace_all(x, "=", "-2")) %>%
    map(as.integer)


answer <- c()

for (x in input) {
    num <- 0
    for (i in seq_along(x)) {
        num <- num + x[i] * 5^(length(x) - i)
    }
    answer <- c(answer, num)
}
answer

snafu <- sum(answer)

snafu

# reverse snafu
v <- c(-2, -1, 0, 1, 2)


# get first place
breaks <- cumsum(2 * rep(5, 20) ^ seq(0, 19))

places <- min(which(breaks > snafu)) - 1

if (abs(2 * 5^places - snafu) > abs(1 * 5^places - snafu)) {
    first_place <- 1
} else {
    first_place <- 2
}

start <- first_place * 5^places

digit_list <- first_place


new_num <- start
# rest of number
for (i in (places - 1):0) {
    distance <- abs(v * 5^i - (snafu - new_num))
    digit <- v[which(distance == min(distance))]

    digit_list <- c(digit_list, digit)
    new_num <- new_num + digit * 5^i
}
new_num
digit_list

a <- as.character(digit_list) %>%
    str_replace_all("-2", "=") %>%
    str_replace_all("-1", "-")

paste0(a, collapse = "")