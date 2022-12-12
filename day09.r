#day 9 ""
input <- read.delim("day9_sample.txt", sep = "", header = FALSE)
input <- read.delim("day9.txt", sep = "", header = FALSE)


colnames(input) <- c("direction", "scalar")

input$complex <- case_when(
    input$direction == "U" ~ 0 + 1i
    , input$direction == "D" ~ 0 - 1i
    , input$direction == "L" ~ -1 + 0i
    , input$direction == "R" ~ 1 + 0i
)
input$move <- input$complex * input$scalar
input$complex[3]

start <- 0 + 0i
pt_list <- list(0 + 0i)
h <- 0 + 0i
t <- 0 + 0i

for (i in seq_along(input$complex)) {
    #i <- 1
    for (j  in 1:input$scalar[i]) {
        h <- h + input$complex[i]
        if (abs(h - t) >= 2) {
            t <- h - input$complex[i]
            pt_list <- append(pt_list, t)
        }
    }

}

length(unique(pt_list))

#part 2 ==================================

f_im <- function(v) {
    if (abs(Re(v)) == 2) {
        real <- Re(v) / 2
    } else {
        real <- Re(v)
    }

    if (abs(Im(v)) == 2) {
        imag <- Im(v) / 2
    } else {
        imag <- Im(v)
    }
    return(complex(1, real, imag))
}


pt_list <- list(0 + 0i)

rope <- rep(0 + 0i, 10)
rope

for (k in seq_along(input$complex)) {
    #k <- 3
    for (j  in 1:input$scalar[k]) {
        #j <- 2
        rope[1] <- rope[1] + input$complex[k]
        for (i in 1:(length(rope) - 1)) {
            #i <- 1
            if (abs(rope[i] - rope[i + 1]) >= 2) {
                v <- rope[i] - rope[i + 1]
                v <- f_im(v)
                rope[i + 1] <- rope[i + 1] + v

                if (i + 1 == 10) pt_list <- append(pt_list, rope[10])
            }
        }
    }
}

#rope
#pt_list
length(unique(pt_list))
