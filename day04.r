# day 4

library(tidyverse)

input <- readLines("inputs/day04.txt")
input <- readLines("inputs/day04_sample.txt")


split <- str_split(input, "[,-]") %>%
    map(as.integer)


compare <- function(m) {
    if (m[1] <= m[3] && m[2] >= m[4]) a <- 1
    else if (m[3] <= m[1] && m[4] >= m[2]) a <- 1
    else a <- 0
    return(a)
}

sum(map_dbl(split, compare))

# p2 ========================================


compare <- function(m) {
    if (m[2] < m[3]) a <- 0
    else if (m[4] < m[1]) a <- 0
    else a <- 1
    return(a)
}

sum(map_dbl(split, compare))
