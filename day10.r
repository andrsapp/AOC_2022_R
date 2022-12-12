# day 10

input <- read.delim("inputs/day10.txt", sep = " ", header = FALSE)
input <- read.delim("inputs/day10_sample.txt", sep = " ", header = FALSE)


colnames(input) <- c("instr", "strength")

input$strength <- as.integer(input$strength)

v <- c()

check_signal <- function(cyc, sig, vec) {
    if (cyc == 0 || (cyc - 20) %% 40 == 0) vec[length(vec) + 1] <- sig * cyc
    return(vec)
}

cycle <- 0
signal <- 1
for (i in seq_len(nrow(input))) {
    cycle <- cycle + 1
    v <- check_signal(cycle, signal, v)

    if (input$instr[i] == "addx") {
        cycle <- cycle + 1
        v <- check_signal(cycle, signal, v)
        signal <- signal + input$strength[i]
    }

}
v
sum(v)

# p2 =================================

input <- read.delim("inputs/day10.txt", sep = " ", header = FALSE)
input <- read.delim("inputs/day10_sample.txt", sep = " ", header = FALSE)


colnames(input) <- c("instr", "strength")

input$strength <- as.integer(input$strength)

pos <- 0
register <- 1
for (i in seq_len(nrow(input))) {
    if (pos %% 40 >= register - 1 && pos %% 40 <= register + 1) v[pos + 1] <- "#"
    else v[pos + 1] <- "."

    if (input$instr[i] == "addx") {
        pos <- pos + 1
        if (pos %% 40 >= register - 1 && pos %% 40 <= register + 1) v[pos + 1] <- "#"
        else v[pos + 1] <- "."
        register <- register + input$strength[i]
    }
    pos <- pos + 1
}
v

print(v, width = 165)
