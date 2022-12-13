# day 13

library(tidyverse)

input <- readLines("inputs/day13.txt")
input <- readLines("inputs/day13_sample.txt")

input <- gsub("10", "A", input)
input <- input[which(input != "")]
#input

f_packet_to_list <- function(packet) {
    packet <- gsub("^.|.$", "", packet)
    set_list <- list()
    bracket <- 0
    for (i in 1:nchar(packet)) {
        if (substr(packet, i, i) == "[") {
            bracket <- bracket + 1
            if (bracket == 1) start <- i
        }

        if (bracket == 0 && grepl("[0-9|A]", substr(packet, i, i))) {
            b <- substr(packet, i, i)
            set_list <- append(set_list, b)
        }

        if (substr(packet, i, i) == "]") {
            bracket <- bracket - 1
            if (bracket == 0) {
                end <- i
                set_list <- append(set_list, substr(packet, start, end))
            }
        }
        #if (bracket == 0) #compare

    }
    return(set_list)
}


f_compare <- function(packet_left, packet_right) {
    if (nchar(packet_left) == 1 && nchar(packet_right) == 1) {
        if (packet_left < packet_right) return("correct")
        else if (packet_left > packet_right) return("wrong")
        else return("continue")
    } else if (nchar(packet_left) != 1 && nchar(packet_right) != 1) {
        set_list_left <- f_packet_to_list(packet_left)
        set_list_right <- f_packet_to_list(packet_right)
        return(f_mainloop(set_list_left, set_list_right))
    } else if (nchar(packet_left) != 1 && nchar(packet_right) == 1) {
        packet_right <- paste0("[", packet_right, "]")
        set_list_left <- f_packet_to_list(packet_left)
        set_list_right <- f_packet_to_list(packet_right)
        return(f_mainloop(set_list_left, set_list_right))
    } else {
        packet_left <- paste0("[", packet_left, "]")
        set_list_left <- f_packet_to_list(packet_left)
        set_list_right <- f_packet_to_list(packet_right)
        return(f_mainloop(set_list_left, set_list_right))
    }

}


f_mainloop <- function(set_list_left, set_list_right) {
    for (i in seq_len(max(length(set_list_left), length(set_list_right)))) {
        if (length(set_list_left) < i) return("correct")
        else if (length(set_list_right) < i) return("wrong")
        else command <- f_compare(set_list_left[[i]], set_list_right[[i]])

        if (command == "correct") return("correct")
        else if (command == "wrong") return("wrong")
        else "continue"
    }
    return("continue")
}


v <- c()
for (i in seq(1, length(input) - 1, by = 2)) {
    sample1 <- list(input[i])
    sample2 <- list(input[i + 1])
    v[length(v) + 1] <- f_mainloop(sample1, sample2)
}
v
sum(which(v == "correct"))

# p2 ==============================================================

# bubble sort
# bubble_sort = function(vec){
#   n = length(vec)
#   for(i in 1:(n-1)) {
#     for(j in (i+1):n) {
#       if(vec[i] >= vec[j]){
#         temp = vec[i]
#          vec[i] = vec[j]
#           vec[j] = temp
#       }
#     }
#   }
#   return(vec)
# }
# bubble_sort(vec)

input <- readLines("inputs/day13.txt")
input <- readLines("inputs/day13_sample.txt")

input <- gsub("10", "A", input)
input <- input[which(input != "")]
#input

input[length(input) + 1] <- "[[2]]"
input[length(input) + 1] <- "[[6]]"

start_input <- input
input <- start_input

n <- length(input)
n
for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
        sample1 <- list(input[i])
        sample2 <- list(input[j])
        test <- f_mainloop(sample1, sample2)
        if (test == "wrong") {
            temp <- input[i]
            input[i] <- input[j]
            input[j] <- temp
        }
    }
}
#input
which(input == "[[2]]") * which(input == "[[6]]")