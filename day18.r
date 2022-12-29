# day 18

library(tidyverse)

input <- read.csv("inputs/day18.txt", header = FALSE)
input <- read.csv("inputs/day18_sample.txt", header = FALSE)
colnames(input) <- c("x", "y", "z")
input$x_sides <- 0
input$y_sides <- 0
input$z_sides <- 0


ordered <- input[with(input, order(x, y, z)), ]

for (i in seq_len(nrow(input))) {
    if (i != 1) {
        if (ordered$z[i - 1] == ordered$z[i] - 1 &&
            ordered$x[i - 1] == ordered$x[i] &&
            ordered$y[i - 1] == ordered$y[i]) {
            ordered$z_sides[i] <- ordered$z_sides[i] + 1
        }
    }
    if (i != nrow(input)) {
        if (ordered$z[i + 1] == ordered$z[i] + 1 &&
            ordered$x[i + 1] == ordered$x[i] &&
            ordered$y[i + 1] == ordered$y[i]) {
            ordered$z_sides[i] <- ordered$z_sides[i] + 1
        }
    }
}

ordered <- ordered[with(ordered, order(x, z, y)), ]

for (i in seq_len(nrow(input))) {
    if (i != 1) {
        if (ordered$y[i - 1] == ordered$y[i] - 1 &&
            ordered$x[i - 1] == ordered$x[i] &&
            ordered$z[i - 1] == ordered$z[i]) {
            ordered$y_sides[i] <- ordered$y_sides[i] + 1
        }
    }
    if (i != nrow(input)) {
        if (ordered$y[i + 1] == ordered$y[i] + 1 &&
            ordered$x[i + 1] == ordered$x[i] &&
            ordered$z[i + 1] == ordered$z[i]) {
            ordered$y_sides[i] <- ordered$y_sides[i] + 1
        }
    }
}

ordered <- ordered[with(ordered, order(y, z, x)), ]

for (i in seq_len(nrow(input))) {
    if (i != 1) {
        if (ordered$x[i - 1] == ordered$x[i] - 1 &&
            ordered$z[i - 1] == ordered$z[i] &&
            ordered$y[i - 1] == ordered$y[i]) {
            ordered$x_sides[i] <- ordered$x_sides[i] + 1
        }
    }
    if (i != nrow(input)) {
        if (ordered$x[i + 1] == ordered$x[i] + 1 &&
            ordered$z[i + 1] == ordered$z[i] &&
            ordered$y[i + 1] == ordered$y[i]) {
            ordered$x_sides[i] <- ordered$x_sides[i] + 1
        }
    }
}

nrow(input) * 6 - sum(ordered[, 4:6])

# 4418

# p2 ========================================================

f_air_z <- function(i, ordered, co_list) {
    for (j in (ordered$z[i] + 1):(ordered$z[i + 1] - 1)) {
        co_list <- append(co_list, paste0(
            ordered$x[i], ",", ordered$y[i], ",", j))
    }
    return(co_list)
}

f_air_y <- function(i, ordered, co_list) {
    for (j in (ordered$y[i] + 1):(ordered$y[i + 1] - 1)) {
        co_list <- append(co_list, paste0(
            ordered$x[i], ",", j, ",", ordered$z[i]))
    }
    return(co_list)
}

f_air_x <- function(i, ordered, co_list) {
    for (j in (ordered$x[i] + 1):(ordered$x[i + 1] - 1)) {
        co_list <- append(co_list, paste0(
            j, ",", ordered$y[i], ",", ordered$z[i]))
    }
    return(co_list)
}

# 2nd iteration ======================================================

f_air_z2 <- function(i, ordered, co_list) {
    new_list <- co_list
    for (j in (ordered$z[i] + 1):(ordered$z[i + 1] - 1)) {
        coord <- paste0(ordered$x[i], ",", ordered$y[i], ",", j)
        if (!(coord %in% x_list && coord %in% y_list)) return(co_list)
        else new_list <- append(new_list, list(c(
            ordered$x[i], ordered$y[i], j)))
    }
    return(new_list)
}

f_air_y2 <- function(i, ordered, co_list) {
    new_list <- co_list
    for (j in (ordered$y[i] + 1):(ordered$y[i + 1] - 1)) {
        coord <- paste0(ordered$x[i], ",", j, ",", ordered$z[i])
        if (!(coord %in% x_list && coord %in% z_list)) return(co_list)
        else new_list <- append(new_list, list(c(
            ordered$x[i], j, ordered$z[i])))
    }
    return(new_list)
}

f_air_x2 <- function(i, ordered, co_list) {
    new_list <- co_list
    for (j in (ordered$x[i] + 1):(ordered$x[i + 1] - 1)) {
        coord <- paste0(j, ",", ordered$y[i], ",", ordered$z[i])
        if (!(coord %in% y_list && coord %in% z_list)) return(co_list)
        else new_list <- append(new_list, list(c(
            j, ordered$y[i], ordered$z[i])))
    }
    return(new_list)
}


input <- read.csv("inputs/day18.txt", header = FALSE)
input <- read.csv("inputs/day18_sample.txt", header = FALSE)
colnames(input) <- c("x", "y", "z")
input$x_sides <- 0
input$y_sides <- 0
input$z_sides <- 0

x_list <- list()
y_list <- list()
z_list <- list()


ordered <- input[with(input, order(x, y, z)), ]

for (i in seq_len(nrow(input))) {

    if (i != nrow(input)) {
        if (ordered$z[i + 1] > ordered$z[i] + 1 &&
            ordered$x[i + 1] == ordered$x[i] &&
            ordered$y[i + 1] == ordered$y[i]) {
            z_list <- f_air_z(i, ordered, z_list)
        }
    }
}

ordered <- ordered[with(ordered, order(x, z, y)), ]

for (i in seq_len(nrow(input))) {

    if (i != nrow(input)) {
        if (ordered$y[i + 1] > ordered$y[i] + 1 &&
            ordered$x[i + 1] == ordered$x[i] &&
            ordered$z[i + 1] == ordered$z[i]) {
            y_list <- f_air_y(i, ordered, y_list)
        }
    }
}

ordered <- ordered[with(ordered, order(y, z, x)), ]

for (i in seq_len(nrow(input))) {

    if (i != nrow(input)) {
        if (ordered$x[i + 1] > ordered$x[i] + 1 &&
            ordered$z[i + 1] == ordered$z[i] &&
            ordered$y[i + 1] == ordered$y[i]) {
            x_list <- f_air_x(i, ordered, x_list)
        }
    }
}

# iteration 2 ==========================================================

x_list2 <- list()
y_list2 <- list()
z_list2 <- list()


ordered <- input[with(input, order(x, y, z)), ]

for (i in seq_len(nrow(input))) {

    if (i != nrow(input)) {
        if (ordered$z[i + 1] > ordered$z[i] + 1 &&
            ordered$x[i + 1] == ordered$x[i] &&
            ordered$y[i + 1] == ordered$y[i]) {
            z_list2 <- f_air_z2(i, ordered, z_list2)
        }
    }
}

ordered <- ordered[with(ordered, order(x, z, y)), ]

for (i in seq_len(nrow(input))) {

    if (i != nrow(input)) {
        if (ordered$y[i + 1] > ordered$y[i] + 1 &&
            ordered$x[i + 1] == ordered$x[i] &&
            ordered$z[i + 1] == ordered$z[i]) {
            y_list2 <- f_air_y2(i, ordered, y_list2)
        }
    }
}

ordered <- ordered[with(ordered, order(y, z, x)), ]

for (i in seq_len(nrow(input))) {

    if (i != nrow(input)) {
        if (ordered$x[i + 1] > ordered$x[i] + 1 &&
            ordered$z[i + 1] == ordered$z[i] &&
            ordered$y[i + 1] == ordered$y[i]) {
            x_list2 <- f_air_x2(i, ordered, x_list2)
        }
    }
}

length(x_list2)
length(y_list2)
length(z_list2)

head(x_list2)
head(y_list2)
head(z_list2)

length(x_list)
length(y_list)
length(z_list)

air <- intersect(intersect(x_list2, y_list2), z_list2)
air <- map(air, as.integer)
length(air)
head(air)


ordered$xyz <- paste0(ordered$x, ",", ordered$y, ",", ordered$z)

air_sum <- 0
for (m in air) {
    if (paste0(m[1] - 1, ",", m[2], ",", m[3]) %in% ordered$xyz) {
        air_sum <- air_sum + 1
    }
    if (paste0(m[1] + 1, ",", m[2], ",", m[3]) %in% ordered$xyz) {
        air_sum <- air_sum + 1
    }
    if (paste0(m[1], ",", m[2] - 1, ",", m[3]) %in% ordered$xyz) {
        air_sum <- air_sum + 1
    }
    if (paste0(m[1], ",", m[2] + 1, ",", m[3]) %in% ordered$xyz) {
        air_sum <- air_sum + 1
    }
    if (paste0(m[1], ",", m[2], ",", m[3] - 1) %in% ordered$xyz) {
        air_sum <- air_sum + 1
    }
    if (paste0(m[1], ",", m[2], ",", m[3] + 1) %in% ordered$xyz) {
        air_sum <- air_sum + 1
    }
}
air_sum

4418 - air_sum