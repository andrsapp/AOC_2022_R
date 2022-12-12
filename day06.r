# Day 6 code

buffer <- readLines("day6_sample.txt")
buffer <- readLines("day6.txt")

buffer

k <- ""
for (i in 4:nchar(buffer)) {
    if (!(grepl(substr(buffer, i, i), substr(buffer, i - 3, i - 1)))
        && !(grepl(substr(buffer, i - 1, i - 1), substr(buffer, i - 3, i - 2)))
        && !(grepl(substr(buffer, i - 2, i - 2), substr(buffer, i - 3, i - 3)))
        ) {
        k <- i
        break
    }
}
k

# part 2 ============================================

k <- ""
for (i in 4:nchar(buffer)) {
    for (j in 1:13) {
        if (grepl(substr(buffer, i - (j - 1), i - (j - 1))
            , substr(buffer, i - 13, i - j))
            ) break
        if (j == 13) k <- i
    }
    if (k != "") break
}
k
