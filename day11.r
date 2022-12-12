# day 11

library(tidyverse)

input <- readLines("inputs/day11.txt")
input <- readLines("inputs/day11_sample.txt")

# Starting Items
ptn <- "Start*"
ndx <- grep(ptn, input)
start_items <- input[ndx] %>%
    str_remove_all("[a-zA-Z: ]") %>%
    str_split(",") %>%
    map(as.integer) %>%
    map(as.list)
start_items

# Operation
ptn <- "Opera*"
ndx <- grep(ptn, input)
operation <- input[ndx] %>%
    str_remove_all("  Operation: |= |new ") %>%
    str_split(" ")
operation

# Test
ptn <- "Test*"
ndx <- grep(ptn, input)
test <- input[ndx] %>%
    str_remove_all("[a-zA-Z: ]") %>%
    str_split(",") %>%
    map(as.integer)
test

# test if true
ptn <- "true*"
ndx <- grep(ptn, input)
if_true <- input[ndx] %>%
    str_remove_all("[a-zA-Z: ]") %>%
    str_split(",") %>%
    map(as.integer)
if_true

# test if false
ptn <- "false*"
ndx <- grep(ptn, input)
if_false <- input[ndx] %>%
    str_remove_all("[a-zA-Z: ]") %>%
    str_split(",") %>%
    as.integer
if_false



f_operation <- function(op, item) {
    if (op[3] == "old") a <- item
    else a <- as.integer(op[3])

    if (op[2] == "+") return(as.integer((item + a) / 3))
    else return(as.integer((item * a) / 3))
}


f_test <- function(test, if_true, if_false, item) {
    if (item %% test == 0) return(if_true + 1)
    else return(if_false + 1)
}

# reset
items <- start_items
inspect <- rep(0, length(items))

for (k in 1:20) {
    for (j in seq_along(items)) {
        while (length(items[[j]] > 0)) {
            inspect[j] <- inspect[j] + 1
            worry <- f_operation(operation[[j]], items[[j]][[1]])
            passto <- f_test(test[[j]], if_true[[j]], if_false[[j]], worry)
            items[[passto]] <- append(items[[passto]], worry)
            items[[j]] <- items[[j]][-1]
        }
    }
}
items
v <- sort(inspect, decreasing = TRUE)
v[1] * v[2]

# p2 ===============================================

# (ab modm)=((a modm)(b modm)) modm
# (a + b) mod m = (a modm + b modm) modm

f_operation <- function(op, item, j) {
    if (op[3] == "old") {
        for (i in seq_along(item)) {
            a <- item[i]
            if (op[2] == "+") item[i] <- (item[i] + a %% test[[i]]) %% test[[i]]
            else item[i] <- (item[i] * a %% test[[i]]) %% test[[i]]
        }
    } else {
        a <- as.integer(op[3])
        for (i in seq_along(item)) {
            if (op[2] == "+") item[i] <- (item[i] + a %% test[[i]]) %% test[[i]]
            else item[i] <- (item[i] * a %% test[[i]]) %% test[[i]]
        }
    }
    return(item)
}

f_test <- function(test, if_true, if_false, item, j) {
    if (item[j] %% test == 0) return(if_true + 1)
    else return(if_false + 1)
}

f_modlist <- function(x, test) {
    for (i in seq_along(test)) {
        v_mod[i] <- x %% test[[i]]
    }
    return(v_mod)
}


v_mod <- c()
mod_list <- list()
for (i in seq_along(start_items)) {
    mod_list[[i]] <- map(start_items[[i]], function(x) f_modlist(x, test))
}
start_modlist <- mod_list

# reset
mod_list <- start_modlist
inspect <- rep(0, length(mod_list))

for (k in 1:10000) {
    for (j in seq_along(mod_list)) {
        while (length(mod_list[[j]]) > 0) {
            inspect[j] <- inspect[j] + 1
            worry <- f_operation(operation[[j]], mod_list[[j]][[1]])
            passto <- f_test(test[[j]], if_true[[j]], if_false[[j]], worry, j)
            mod_list[[passto]] <- append(mod_list[[passto]], list(worry))
            mod_list[[j]] <- mod_list[[j]][-1]
        }
    }
}

inspect
v <- sort(inspect, decreasing = TRUE)
v[1] * v[2]
