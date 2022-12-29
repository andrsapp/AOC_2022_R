# day 17

getx <- function(j) {
    if (j %% 5 == 1) x <- c(2, 3, 4, 5)
    else if (j %% 5 == 2) x <- c(4, 3, 4, 5, 4)
    else if (j %% 5 == 3) x <- c(3, 3, 3, 4, 5)
    else if (j %% 5 == 4) x <- rep(5, 4)
    else x <- c(5, 4, 5, 4)

    return(x)
}

gety <- function(j) {
    if (j %% 5 == 1) y <- rep(4, 4) #wide
    else if (j %% 5 == 2) y <- c(6, 5, 5, 5, 4) #plus
    else if (j %% 5 == 3) y <- c(6, 5, 4, 4, 4) #el
    else if (j %% 5 == 4) y <- c(4, 5, 6, 7) #tall
    else y <- c(5, 5, 4, 4) #cube

    return(y)
}



input <- readLines("inputs/day17.txt")
input <- readLines("inputs/day17_sample.txt")

input <- unlist(strsplit(input, ""))
length(input)
input <- rep(input, 2022 * 5)

# INTIALIZERS
# create empty grid, other initializers
grid <- matrix(rep(".")
    , nrow = 4000
    , ncol = 7
    )
grid[1, ] <- "#"
max_grid <- 1
n <- 1
j <- 0
fallen_rocks <- 0


#change for loop
#j <- 3

while (fallen_rocks < 2022) {
j <- j + 1
a <- 0
x <- getx(j)
y <- max_grid + gety(j)
# good for all shapes ====================================
while (a != 1) {
    # gas - move shape
    b <- 0
    if (input[n] == ">" && min(x - 1) == 0) {
        b <- 1
    } else if (input[n] == "<" && max(x + 1) == 8) {
        b <- 1
    } else if (input[n] == ">") {
        for (i in seq_along(x)) {
            if (grid[y[i], x[i] - 1] == "#") {
                b <- 1
                break
            }
        }
    } else {
        for (i in seq_along(x)) {
            if (grid[y[i], x[i] + 1] == "#") {
                b <- 1
                break
            }
        }
    }

    if (b != 1 && input[n] == ">") {
        x <- x - 1
    } else if (b != 1 && input[n] == "<") {
        x <- x + 1
    }
    n <- n + 1

    # check for collision of shape
    for (i in seq_along(x)) {
        if (grid[y[i] - 1, x[i]] == "#") {
            a <- 1
            break
        }
    }

    # rock falls or stays
    if (a == 0) {
        y <- y - 1
    } else {
        for (i in seq_along(x)) {
            grid[y[i], x[i]] <- "#"
        }
        max_grid <- max(y, max_grid)
        fallen_rocks <- fallen_rocks + 1
    }
}
}

max_grid - 1

grid[1:200, ]

# p2 ====================================================

input <- readLines("inputs/day17.txt")
input <- readLines("inputs/day17_sample.txt")

input <- unlist(strsplit(input, ""))
len_input <- length(input)
input <- rep(input, 2022 * 5)

# INTIALIZERS
# create empty grid, other initializers
grid <- matrix(rep(".")
    , nrow = 100000
    , ncol = 7
    )
grid[1, ] <- "#"
max_grid <- 1
n <- 1
j <- 0
fallen_rocks <- 0

try <- len_input * 5
while (fallen_rocks < 15104) {
j <- j + 1
a <- 0
x <- getx(j)
y <- max_grid + gety(j)
# good for all shapes ====================================
while (a != 1) {
    # gas - move shape
    b <- 0
    if (input[n] == ">" && min(x - 1) == 0) {
        b <- 1
    } else if (input[n] == "<" && max(x + 1) == 8) {
        b <- 1
    } else if (input[n] == ">") {
        for (i in seq_along(x)) {
            if (grid[y[i], x[i] - 1] == "#") {
                b <- 1
                break
            }
        }
    } else {
        for (i in seq_along(x)) {
            if (grid[y[i], x[i] + 1] == "#") {
                b <- 1
                break
            }
        }
    }

    if (b != 1 && input[n] == ">") {
        x <- x - 1
    } else if (b != 1 && input[n] == "<") {
        x <- x + 1
    }
    n <- n + 1

    # check for collision of shape
    for (i in seq_along(x)) {
        if (grid[y[i] - 1, x[i]] == "#") {
            a <- 1
            break
        }
    }

    # rock falls or stays
    if (a == 0) {
        y <- y - 1
    } else {
        for (i in seq_along(x)) {
            grid[y[i], x[i]] <- "#"
        }
        max_grid <- max(y, max_grid)
        fallen_rocks <- fallen_rocks + 1
    }
}
}
fallen_rocks
max_grid
n

rocks <- 8700
height <- 13620
ratio <- height / rocks

height_at_8700rocks <- 13614

height_at_15104rocks <- 23649

height_at_15104rocks - height_at_8700rocks


total_rocks <- 1000000000000
sample_height <- 1514285714288

1565517241382