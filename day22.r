# day 22 ==========================================================

library(tidyverse)

input <- readLines("inputs/day22.txt")
input <- readLines("inputs/day22_sample.txt")

input <- str_replace_all(input, " ", "x")

path <- tail(input, 1)

directions <- unlist(str_extract_all(path, "[A-Z]"))

moves <- as.integer(unlist(strsplit(path, "[A-Z]")))


board <- head(input, - 2)
len <- max(nchar(board))
board <- strsplit(board, "")

for (i in seq_along(board)) {
    board[[i]] <- c("x", board[[i]],
        rep("x", len + 1 - length(board[[i]])))
}

board <- matrix(c(rep("x", len + 2), unlist(board), rep("x", len + 2)),
    nrow = length(board) + 2, byrow = TRUE)
#print(board, width = 120)


start_pos_col <- min(which(board[2, ] != "x"))
start_pos_row <- 2
start_face <- "right"


# reset ========================
face <- start_face
pos_row <- start_pos_row
pos_col <- start_pos_col


for (i in seq_along(moves)) {

    if (i != 1) {
        face <- f_changeface(directions[i - 1])
    }

    if (face == "right") {
        pos_col <- f_moveright(moves[i])
    } else if (face == "left") {
        pos_col <- f_moveleft(moves[i])
    } else if (face == "down") {
        pos_row <- f_movedown(moves[i])
    } else {
        pos_row <- f_moveup(moves[i])
    }

}
pos_row
pos_col
face

if (face == "right") {
    face_pts <- 0
} else if (face == "down") {
    face_pts <- 1
} else if (face == "left") {
    face_pts <- 2
} else {
    face_pts <- 3
}

sum(1000 * (pos_row - 1), 4 * (pos_col - 1), face_pts)



f_changeface <- function(dir) {
    if (dir == "R") {
        if (face == "right") face <- "down"
        else if (face == "down") face <- "left"
        else if (face == "left") face <- "up"
        else face <- "right"
    } else {
        if (face == "right") face <- "up"
        else if (face == "down") face <- "right"
        else if (face == "left") face <- "down"
        else face <- "left"
    }
    return(face)
}


f_moveright <- function(scalar) {
    open <- min(which(board[pos_row, ] == "."))
    wall <- min(which(board[pos_row, ] == "#"))

    for (j in seq_len(scalar)) {
        if (board[pos_row, pos_col + 1] == "#") {
            break
        } else if (board[pos_row, pos_col + 1] == ".") {
            pos_col <- pos_col + 1
        } else if (board[pos_row, pos_col + 1] == "x") {
            if (wall < open) break
            else pos_col <- open
        } else {
            print("WRONG")
        }
    }
    return(pos_col)
}

f_moveleft <- function(scalar) {
    open <- max(which(board[pos_row, ] == "."))
    wall <- max(which(board[pos_row, ] == "#"))

    for (j in seq_len(scalar)) {
        if (board[pos_row, pos_col - 1] == "#") {
            break
        } else if (board[pos_row, pos_col - 1] == ".") {
            pos_col <- pos_col - 1
        } else if (board[pos_row, pos_col - 1] == "x") {
            if (wall > open) break
            else pos_col <- open
        } else {
            print("WRONG")
        }
    }
    return(pos_col)
}

f_movedown <- function(scalar) {
    open <- min(which(board[, pos_col] == "."))
    wall <- min(which(board[, pos_col] == "#"))

    for (j in seq_len(scalar)) {
        if (board[pos_row + 1, pos_col] == "#") {
            break
        } else if (board[pos_row + 1, pos_col] == ".") {
            pos_row <- pos_row + 1
        } else if (board[pos_row + 1, pos_col] == "x") {
            if (wall < open) break
            else pos_row <- open
        } else {
            print("WRONG")
        }
    }
    return(pos_row)
}

f_moveup <- function(scalar) {
    open <- max(which(board[, pos_col] == "."))
    wall <- max(which(board[, pos_col] == "#"))

    for (j in seq_len(scalar)) {
        if (board[pos_row - 1, pos_col] == "#") {
            break
        } else if (board[pos_row - 1, pos_col] == ".") {
            pos_row <- pos_row - 1
        } else if (board[pos_row - 1, pos_col] == "x") {
            if (wall > open) break
            else pos_row <- open
        } else {
            print("WRONG")
        }
    }
    return(pos_row)
}


# p2 ==================================================


input <- readLines("inputs/day22.txt")
input <- readLines("inputs/day22_sample.txt")


path <- tail(input, 1)

directions <- unlist(str_extract_all(path, "[A-Z]"))

moves <- as.integer(unlist(strsplit(path, "[A-Z]")))

# board
input <- str_replace_all(input, " ", "x")

board <- head(input, - 2)
len <- max(nchar(board))
board <- strsplit(board, "")

for (i in seq_along(board)) {
    board[[i]] <- c(board[[i]],
        rep("x", len - length(board[[i]])))
}

board <- matrix(unlist(board), nrow = length(board), byrow = TRUE)
print(board, width = 120)

start_pos_col <- 1
start_pos_row <- 1
start_face <- "right"
start_side <- "bottom"

# CHANGE FOR INPUT

right <- board[1:50, 101:150]
bottom <- board[1:50, 51:100]
back <- board[51:100, 51:100]
top <- board[101:150, 51:100]
left <- board[101:150, 1:50]
front <- board[151:200, 1:50]

d <- 50 # change to 50

# reset ========================
side <- start_side
face <- start_face
pos_row <- start_pos_row
pos_col <- start_pos_col
pos_list <- list(side, pos_row, pos_col)

for (i in seq_along(moves)) {
    if (i != 1) {
        face <- f_changeface(directions[i - 1])
    }

    if (face == "right") {
        pos_list <- f_moveright(
            1, moves[i], pos_list[[1]], pos_list[[2]], pos_list[[3]])
    } else if (face == "left") {
        pos_list <- f_moveleft(
            1, moves[i], pos_list[[1]], pos_list[[2]], pos_list[[3]])
    } else if (face == "down") {
        pos_list <- f_movedown(
            1, moves[i], pos_list[[1]], pos_list[[2]], pos_list[[3]])
    } else {
        pos_list <- f_moveup(
            1, moves[i], pos_list[[1]], pos_list[[2]], pos_list[[3]])
    }

}
pos_list
face

1000 * (38 + 100) + 4 * (32) + 3

if (face == "right") {
    face_pts <- 0
} else if (face == "down") {
    face_pts <- 1
} else if (face == "left") {
    face_pts <- 2
} else {
    face_pts <- 3
}


f_cuberight <- function(m, scalar, side, pos_row, pos_col) {

    if (side == "bottom") {
        if (right[pos_row, 1] == "#") {
            return(list(side, pos_row, pos_col))
        } else {
            pos_list <- f_moveright(m + 1, scalar
                , "right", pos_row, 1)
        }
    } else if (side == "right") {
        if (top[d - pos_row + 1, d] == "#") {
            return(list(side, pos_row, pos_col))
        } else {
            pos_list <- f_moveleft(m + 1, scalar
                , "top", d - pos_row + 1, d)
        }
    } else if (side == "back") {
        if (right[d, pos_row] == "#") {
            return(list(side, pos_row, pos_col))
        } else {
            pos_list <- f_moveup(m + 1, scalar
                , "right", d, pos_row)
        }
    } else if (side == "left") {
        if (top[pos_row, 1] == "#") {
            return(list(side, pos_row, pos_col))
        } else {
            pos_list <- f_moveright(m + 1, scalar
                , "top", pos_row, 1)
        }
    } else if (side == "top") {
        if (right[d - pos_row + 1, d] == "#") {
            return(list(side, pos_row, pos_col))
        } else {
            pos_list <- f_moveleft(m + 1, scalar
                , "right", d - pos_row + 1, d)
        }
    } else if (side == "front") {
        if (top[d, pos_row] == "#") {
            return(list(side, pos_row, pos_col))
        } else {
            pos_list <- f_moveup(m + 1, scalar
                , "top", d, pos_row)
        }
    }


    return(pos_list)
}


f_cubeleft <- function(m, scalar, side, pos_row, pos_col) {

    if (side == "bottom") {
        if (left[d - pos_row + 1, 1] == "#") {
            return(list(side, pos_row, pos_col))
        } else {
            pos_list <- f_moveright(m + 1, scalar
                , "left", d - pos_row + 1, 1)
        }
    } else if (side == "right") {
        if (bottom[pos_row, d] == "#") {
            return(list(side, pos_row, pos_col))
        } else {
            pos_list <- f_moveleft(m + 1, scalar
                , "bottom", pos_row, d)
        }
    } else if (side == "back") {
        if (left[1, pos_row] == "#") {
            return(list(side, pos_row, pos_col))
        } else {
            pos_list <- f_movedown(m + 1, scalar
                , "left", 1, pos_row)
        }
    } else if (side == "left") {
        if (bottom[d - pos_row + 1, 1] == "#") {
            return(list(side, pos_row, pos_col))
        } else {
            pos_list <- f_moveright(m + 1, scalar
                , "bottom", d - pos_row + 1, 1)
        }
    } else if (side == "top") {
        if (left[pos_row, d] == "#") {
            return(list(side, pos_row, pos_col))
        } else {
            pos_list <- f_moveleft(m + 1, scalar
                , "left", pos_row, d)
        }
    } else if (side == "front") {
        if (bottom[1, pos_row] == "#") {
            return(list(side, pos_row, pos_col))
        } else {
            pos_list <- f_movedown(m + 1, scalar
                , "bottom", 1, pos_row)
        }
    }


    return(pos_list)
}


f_cubeup <- function(m, scalar, side, pos_row, pos_col) {

    if (side == "bottom") {
        if (front[pos_col, 1] == "#") {
            return(list(side, pos_row, pos_col))
        } else {
            pos_list <- f_moveright(m + 1, scalar
                , "front", pos_col, 1)
        }
    } else if (side == "right") {
        if (front[d, pos_col] == "#") {
            return(list(side, pos_row, pos_col))
        } else {
            pos_list <- f_moveup(m + 1, scalar
                , "front", d, pos_col)
        }
    } else if (side == "back") {
        if (bottom[d, pos_col] == "#") {
            return(list(side, pos_row, pos_col))
        } else {
            pos_list <- f_moveup(m + 1, scalar
                , "bottom", d, pos_col)
        }
    } else if (side == "left") {
        if (back[pos_col, 1] == "#") {
            return(list(side, pos_row, pos_col))
        } else {
            pos_list <- f_moveright(m + 1, scalar
                , "back", pos_col, 1)
        }
    } else if (side == "top") {
        if (back[d, pos_col] == "#") {
            return(list(side, pos_row, pos_col))
        } else {
            pos_list <- f_moveup(m + 1, scalar
                , "back", d, pos_col)
        }
    } else if (side == "front") {
        if (left[d, pos_col] == "#") {
            return(list(side, pos_row, pos_col))
        } else {
            pos_list <- f_moveup(m + 1, scalar
                , "left", d, pos_col)
        }
    }


    return(pos_list)
}


f_cubedown <- function(m, scalar, side, pos_row, pos_col) {

    if (side == "bottom") {
        if (back[1, pos_col] == "#") {
            return(list(side, pos_row, pos_col))
        } else {
            pos_list <- f_movedown(m + 1, scalar
                , "back", 1, pos_col)
        }
    } else if (side == "right") {
        if (back[pos_col, d] == "#") {
            return(list(side, pos_row, pos_col))
        } else {
            pos_list <- f_moveleft(m + 1, scalar
                , "back", pos_col, d)
        }
    } else if (side == "back") {
        if (top[1, pos_col] == "#") {
            return(list(side, pos_row, pos_col))
        } else {
            pos_list <- f_movedown(m + 1, scalar
                , "top", 1, pos_col)
        }
    } else if (side == "left") {
        if (front[1, pos_col] == "#") {
            return(list(side, pos_row, pos_col))
        } else {
            pos_list <- f_movedown(m + 1, scalar
                , "front", 1, pos_col)
        }
    } else if (side == "top") {
        if (front[pos_col, d] == "#") {
            return(list(side, pos_row, pos_col))
        } else {
            pos_list <- f_moveleft(m + 1, scalar
                , "front", pos_col, d)
        }
    } else if (side == "front") {
        if (right[1, pos_col] == "#") {
            return(list(side, pos_row, pos_col))
        } else {
            pos_list <- f_movedown(m + 1, scalar
                , "right", 1, pos_col)
        }
    }


    return(pos_list)
}

get_board <- function(side) {
    if (side == "bottom") return(bottom)
    else if (side == "right") return(right)
    else if (side == "back") return(back)
    else if (side == "top") return(top)
    else if (side == "left") return(left)
    else if (side == "front") return(front)
    else print("boardWRONG")

}

f_moveright <- function(m, scalar, side, pos_row, pos_col) {

    assign("face", "right", envir = .GlobalEnv)

    if (m > scalar) return(list(side, pos_row, pos_col))

    board <- get_board(side)

    for (j in m:scalar) {
        if (pos_col + 1 > d) {
            return(f_cuberight(j, scalar, side, pos_row, pos_col))
        } else if (board[pos_row, pos_col + 1] == "#") {
            break
        } else if (board[pos_row, pos_col + 1] == ".") {
            pos_col <- pos_col + 1
        } else {
            print("WRONG")
        }
    }
    return(list(side, pos_row, pos_col))
}

f_moveleft <- function(m, scalar, side, pos_row, pos_col) {

    assign("face", "left", envir = .GlobalEnv)

    if (m > scalar) return(list(side, pos_row, pos_col))

    board <- get_board(side)

    for (j in m:scalar) {
        if (pos_col - 1 < 1) {
            return(f_cubeleft(j, scalar, side, pos_row, pos_col))
        } else if (board[pos_row, pos_col - 1] == "#") {
            break
        } else if (board[pos_row, pos_col - 1] == ".") {
            pos_col <- pos_col - 1
        } else {
            print("WRONG")
        }
    }
    return(list(side, pos_row, pos_col))
}

f_movedown <- function(m, scalar, side, pos_row, pos_col) {

    assign("face", "down", envir = .GlobalEnv)

    if (m > scalar) return(list(side, pos_row, pos_col))

    board <- get_board(side)

    for (j in m:scalar) {
        if (pos_row + 1 > d) {
            return(f_cubedown(j, scalar, side, pos_row, pos_col))
        } else if (board[pos_row + 1, pos_col] == "#") {
            break
        } else if (board[pos_row + 1, pos_col] == ".") {
            pos_row <- pos_row + 1
        } else {
            print("WRONG")
        }
    }
    return(list(side, pos_row, pos_col))
}

f_moveup <- function(m, scalar, side, pos_row, pos_col) {

    assign("face", "up", envir = .GlobalEnv)

    if (m > scalar) return(list(side, pos_row, pos_col))

    board <- get_board(side)

    for (j in m:scalar) {
        if (pos_row - 1 < 1) {
            return(f_cubeup(j, scalar, side, pos_row, pos_col))
        } else if (board[pos_row - 1, pos_col] == "#") {
            break
        } else if (board[pos_row - 1, pos_col] == ".") {
            pos_row <- pos_row - 1
        } else {
            print("WRONG")
        }
    }
    return(list(side, pos_row, pos_col))
}
