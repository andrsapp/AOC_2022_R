# day 12

library(igraph)

input <- readLines("inputs/day12.txt")
input <- readLines("inputs/day12_sample.txt")

input <- strsplit(input, "")
input <- map(input, function(x) match(x, letters))
input <- matrix(unlist(input), ncol = length(input), nrow = length(input[[1]]))
input <- t(input)
input

start <- which(is.na(input))[1]
end <- which(is.na(input))[2]

input[start] <- 1
input[end] <- 26

# igraph example
edges_test <- matrix(c(1, 2, 2, 3, 3, 6, 6, 5), nrow = 4, ncol = 2)
edges_test
g <- graph_from_edgelist(edges_test)
plot(g)

news.path <- shortest_paths(g, from = 1, to = 5)
news.path$vpath[[1]]
length(news.path$vpath[[1]])

# =====================================================

f_above <- function(node, edges, j, i) {
    # above
    if (input[j - 1, i] <= input[j, i] + 1) {
        node_above <- (j - 1) + height * (i - 1)
        edges <- append(edges, list(c(node, node_above)))
    }
    return(edges)
}

f_below <- function(node, edges, j, i) {
    # below
    if (input[j + 1, i] <= input[j, i] + 1) {
        node_below <- (j + 1) + height * (i - 1)
        edges <- append(edges, list(c(node, node_below)))
    }
    return(edges)
}
f_left <- function(node, edges, j, i) {
    # left
    if (input[j, i - 1] <= input[j, i] + 1) {
        node_left <- j + height * (i - 1 - 1)
        edges <- append(edges, list(c(node, node_left)))
    }
    return(edges)
}
f_right <- function(node, edges, j, i) {
    # right
    if (input[j, i + 1] <= input[j, i] + 1) {
        node_right <- j + height * (i + 1 - 1)
        edges <- append(edges, list(c(node, node_right)))
    }
    return(edges)
}

# reset =============================================
edges <- list()
edges

width <- ncol(input)
height <- nrow(input)
for (i in 1:width) {
    for (j in 1:height) {
        node <- j + height * (i - 1)

        if (j != 1) edges <- f_above(node, edges, j, i)
        if (j != height) edges <- f_below(node, edges, j, i)
        if (i != 1) edges <- f_left(node, edges, j, i)
        if (i != width) edges <- f_right(node, edges, j, i)

    }
}
head(edges)

edge_matrix <- t(matrix(unlist(edges), nrow = 2))
g <- graph_from_edgelist(edge_matrix)
g

short_path <- shortest_paths(g, from = start, to = end)
short_path$vpath[[1]]
length(short_path$vpath[[1]]) - 1

# p2 ===================================================================

low_elev <- which(input == 1)

v_short_path <- c()

for (i in seq_along(low_elev)) {
    short_path <- shortest_paths(g, from = low_elev[i], to = end)
    v_short_path[length(v_short_path) + 1] <- length(short_path$vpath[[1]]) - 1
}
v_short_path
min(v_short_path[(which(v_short_path != -1))])