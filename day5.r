# Day 5 Code

library(tidyverse)
library(readr)


df <- as.data.frame((read.delim("day5.txt", header = FALSE, sep = "")))
df <- as.data.frame(apply(df, 2, function(y) gsub("\\[|\\]", "", y)))
df

ptn <- "move*"
ndx <- grep(ptn, df$V1)
crates <- df[-ndx, ]
crates <- apply(crates, 2, rev)
crates <- as.data.frame(crates)
crates <- crates[-1, ]
first <- crates
first
crates <- first
crates

procedure <- df[ndx, c("V2", "V4", "V6")]
procedure <- apply(procedure, 2, as.integer)
procedure

for (j in seq_len(nrow(procedure))) {
    for (i in 1:procedure[j, 1]) {

        from <- procedure[j, 2]
        to <- procedure[j, 3]
        len_from <- length(
            which(crates[, from] != ("") & crates[, from] != ("0")))
        len_to <- length(
            which(crates[, to] != ("") & crates[, to] != ("0")))

        crates[len_to + 1, to] <- crates[len_from, from]
        crates[len_from, from] <- ""
        crates

    }
}
crates

answer <- ""
for (k in seq_len(ncol(crates))) {
    len <- length(which(crates[, k] != ("") & crates[, k] != ("0")))
    letter <- crates[len, k]
    answer <- paste0(answer, letter)
}
answer

# part 2 ==========================================

crates <- first
crates
for (j in seq_len(nrow(procedure))) {

    from <- procedure[j, 2]
    to <- procedure[j, 3]
    len_from <- length(
        which(crates[, from] != ("") & crates[, from] != ("0")))
    len_to <- length(
        which(crates[, to] != ("") & crates[, to] != ("0")))

    len_box <- procedure[j, 1]

    for (i in 1:procedure[j, 1]) {

        crates[len_to + len_box - i + 1, to] <- crates[len_from - i + 1, from]
        crates[len_from - i + 1, from] <- ""
        crates

    }
}

crates

answer <- ""
for (k in seq_len(ncol(crates))) {
    len <- length(which(crates[, k] != ("") & crates[, k] != ("0")))
    letter <- crates[len, k]
    answer <- paste0(answer, letter)
}
answer
