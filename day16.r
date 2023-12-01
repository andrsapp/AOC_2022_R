# day 16

library(tidyverse)
library(igraph)

input <- readLines("inputs/day16.txt")
input <- readLines("inputs/day16_sample.txt")

input <- str_remove_all(input, "Valve|[a-z]| ")
input <- str_split(input, ";|,|=")

node <- map_chr(input, function(x) x[1])
flow <- map_chr(input, function(x) x[2])
start_valves <- as.data.frame(cbind(node, flow))
start_valves$flow <- as.integer(start_valves$flow)
start_valves <- start_valves[order(start_valves$node), ]


edges <- list()
for (x in input) {
    node_left <- x[1]
    for (j in 3:length(x)) {
        node_right <- x[j]
        edges <- append(edges, list(c(node_left, node_right)))
    }
}

edge_matrix <- t(matrix(unlist(edges), nrow = 2))
g <- graph_from_edgelist(edge_matrix)
plot(g)


# dfs

f_dfs <- function(minutes, current_node, valves, total_pressure, num_valves) {

    dist <- distances(g, v = current_node, to = V(g), weights = NA)
    dist <- as.data.frame(dist)
    dist <- dist[order(names(dist))]

    pressure <- (minutes - dist - 1) * valves$flow

    # pruning
    # if (

    #     total_pressure + sum(pressure[which(pressure >= 0)]) <
    #         1300
    # ) return()

    if (length(which(pressure > 0)) == 0 || num_valves > 5) {
        local_pressure <- append(max_pressure, total_pressure)
        assign("max_pressure", local_pressure, envir = .GlobalEnv)

        local_nodes <- append(nodes_list
            , list(valves$node[which(valves$flow > 0)]))
        assign("nodes_list", local_nodes, envir = .GlobalEnv)

        return()
    }

    for (index in seq_along(pressure)) {
        if (pressure[index] <= 0) {
            next
        } else {
            valves_local <- valves
            valves_local$flow[index] <- 0
            f_dfs(minutes - dist[, index] - 1
                , valves$node[index]
                , valves_local
                , total_pressure + pressure[, index]
                , num_valves + 1)
        }
    }

}

max_pressure <- list()

f_dfs(30, "AA", start_valves, 0, 0)

length(max_pressure)
max(unlist(max_pressure))

head(nodes_list)

# p2 ===================================================


max_pressure <- list()
nodes_list <- list()

f_dfs(26, "AA", start_valves, 0, 0)

length(max_pressure)
max(unlist(max_pressure))
head(nodes_list)

pressure_v <- unlist(max_pressure)
top_pressure <- pressure_v
top_nodes <- nodes_list
top_pressure <- pressure_v[which(pressure_v > 1300)]
length(top_pressure)

top_nodes <- nodes_list[which(pressure_v > 1300)]
length(top_nodes)

flow_nodes <- start_valves$node[which(start_valves$flow > 0)]
diff_nodes <- list()
for (i in seq_along(top_nodes)) {
    diff_nodes[[i]] <- setdiff(flow_nodes, top_nodes[[i]])
}
head(diff_nodes)
head(top_nodes)

int_list <- list()

for (i in seq_along(diff_nodes)) {
    for (j in i:length(diff_nodes)) {
        if (length(intersect(diff_nodes[[i]], diff_nodes[[j]])) == 0) {
            int_list <- append(int_list, list(c(i, j)))
        }
    }
}

v <- c()
for (x in int_list) {
    v <- c(v, top_pressure[x[1]] + top_pressure[x[2]])
}

max(v)