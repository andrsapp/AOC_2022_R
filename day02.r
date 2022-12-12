# day 2

library(tidyverse)

input <- read.delim("inputs/day02.txt", header = FALSE)

input <- read.delim("inputs/day02_sample.txt", header = FALSE)

colnames(input) <- "strategy"

key <- c(
    "A X" = 1 + 3
    , "A Y" = 2 + 6
    , "A Z" = 3 + 0

    , "B X" = 1 + 0
    , "B Y" = 2 + 3
    , "B Z" = 3 + 6

    , "C X" = 1 + 6
    , "C Y" = 2 + 0
    , "C Z" = 3 + 3
)

total <- map_dbl(input$strategy, function(x) key[x])
sum(total)



# p2 ================================

input <- read.delim("inputs/day02.txt", header = FALSE)

input <- read.delim("inputs/day02_sample.txt", header = FALSE)

colnames(input) <- "strategy"

key <- c(
    "A X" = 3 + 0
    , "A Y" = 1 + 3
    , "A Z" = 2 + 6

    , "B X" = 1 + 0
    , "B Y" = 2 + 3
    , "B Z" = 3 + 6

    , "C X" = 2 + 0
    , "C Y" = 3 + 3
    , "C Z" = 1 + 6
)

total <- map_dbl(input$strategy, function(x) key[x])
sum(total)
