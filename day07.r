# day 7 code

input <- as.data.frame(read.delim("day7.txt", header = FALSE, sep=""))


df <- input

cd_vector <- as.data.frame(cbind(1:50, "a"))
cd_vector

level <- 0
current <- "a"
for (i in seq_len(nrow(df) - 1)) {
    #i <- 34
    if (df[i, 2] == "cd" && df[i, 3] != "..") {
        level <- level + 1
        cd_vector[level, 2] <- paste0(cd_vector[level - 1, 2], "_", df[i, 3])
    }
    if (df[i, 2] == "cd" && df[i, 3] == "..") {
        level <- level - 1
        cd_vector[level + 1, 2] <- cd_vector[level, 2]
    }
    #head(cd_vector, 10)
    df[i + 1, 4] <- level
    df[i + 1, 5] <- cd_vector[level, 2]

}
head(df, 100)
cd_vector

df$V2 <- ifelse(
    df$V2 == "cd" | df$V2 == "ls", df$V2, paste0(df$V2, df$V4, "_", df$V5))
df$V3 <- ifelse(
    df$V3 == ".." | df$V2 != "cd", df$V3, paste0(df$V3, df$V4, "_", df$V5))
head(df, 100)

dir_vector <- c()
j <- 1
for (i in seq_len(nrow(df))) {
    if (df[i, 1] == "dir") {
        dir_vector[j] <- df[i, 2]
        j <- j + 1
    }
}
dir_vector <- as.data.frame(dir_vector)
colnames(dir_vector) <- "letter"
dir_vector

for (j in seq_len(nrow(dir_vector))) {
    for (i in seq_len(nrow(df))) {
        if (df[i, 2] == "cd" && df[i + 1, 2] == "ls"
        && df[i, 3] == dir_vector[j, 1]) {
            dir_vector[j, 2] <- i
        }
        dir_vector[j, 4] <- df[i, 4]
    }
}
dir_vector

f_dirfind <- function(l) {
    idx <- which(df$V2 == "cd" & df$V3 == l)
    return(ifelse(idx == "", 0, idx))
}


f_sum <- function(k) {
    a <- k + 2
    sum <- 0
    while (df[a, 1] != "$" && a <= nrow(df)) {
        sum <- sum +
            ifelse(df[a, 1] == "dir"
                , f_sum(f_dirfind(df[a, 2]))
                , as.integer(df[a, 1]))
        a <- a + 1
    }
    return(sum)
}


dir_vector$sum <- c()
for (i in seq_len(nrow(dir_vector))) {
    dir_vector[i, 3] <- f_sum(dir_vector[i, 2])
}

dir_vector

# total --------------------------------------
total <- 0
for (i in seq_len(nrow(dir_vector))) {
    if (dir_vector[i, 3] < 100000) total <- total + dir_vector[i, 3]
}
total

disk <- 70000000
update <- 30000000


for (j in seq_len(nrow(dir_vector))) {
    for (i in seq_len(nrow(df))) {
        if (df[i, 2] == "cd" && df[i + 1, 2] == "ls"
        && df[i, 3] == dir_vector[j, 1]) {
            dir_vector[j, 4] <- df[i, 4]
        }
    }
}
dir_vector

used_space <- sum(dir_vector$V3[which(dir_vector$V4 == 1)])

unused <- disk - used_space

to_delete <- update - unused
to_delete

dir_vector$letter[which(dir_vector$V3 >= to_delete)]
min(dir_vector$V3[which(dir_vector$V3 >= to_delete)])

dir_vector
34772236 > to_delete
which(dir_vector$V3 >= to_delete)
2195372 >= to_delete
to_delete
