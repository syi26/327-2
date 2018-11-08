# Name: Sirui Yi
# Email: syi26@wisc.edu

rm(list = ls())

# Implement Connect Four in the same manner that we
# implemented Tic-tac-toe in class. Start by implementing
# the helper functions, below, and testing them by running
#   source("hw3test.R")
# Then write code for the game itself.
#
# We'll test your code by running
#   source("hw3.R")
# We might also play Connect Four, read your code, and do other tests.

# Returns TRUE if vector v (of character strings) contains
# (at least) four in a row of player (character string). e.g.
#   four.in.a.row("X", c("O","X","X","X","X","O"))
# is TRUE, while
#   four.in.a.row("O", c("O","X","X","X","X","O"))
# is FALSE.
four.in.a.row = function(player, v, debug=FALSE) {
  if (debug) {
    cat(sep="", "four.in.a.row(player=", player, ", v=", v, ")\n")
  }
  for (i in 1:length(v)) {
    num <- 0
    for (j in i:length(v)) {
      if (v[j] == player) {
        num <- num + 1
        if (num >= 4) {
          return(TRUE)
        } 
      } 
      else {
        break
      }
    }
  }
  return(FALSE)
}

# Returns TRUE if (matrix) board (of character strings)
# contains at least four in a row of (string) player, who
# just played in position (r, c). (Here "r" means "row" and
# "c" means "column").
#
# Hint: this function should call four.in.a.row() four times, once
# each for the current row, column, diagonal, and reverse diagonal.
won = function(player, board, r, c, debug=FALSE) {
  if (debug) {
    cat(sep="", "won(player=", player, ", board=\n")
    print(board)
    cat(sep="", ", r=", r, ", c=", c, ")\n")
  }
  
  if (four.in.a.row(player, board[row(board) == r]) == TRUE) {
    return(TRUE)
  } else if (four.in.a.row(player, board[col(board) == c]) == TRUE) {
    return(TRUE)
  } else if (four.in.a.row(player, board[row(board) - col(board) == r - c]) == TRUE) {
    return(TRUE)
  } else if (four.in.a.row(player, board[row(board) + col(board) == r + c]) == TRUE) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Returns largest index of an empty position in column col
# of (matrix) board. If there is no such empty position in
# board, return value is NULL.
largest.empty.row = function(board, col, debug=FALSE) {
  if (debug) {
    cat(sep="", "largest.empty.row(board=\n")
    print(board)
    cat(sep="", ", col=", col, ")\n")
  }
  
  counter <- 0
  for (i in 1:nrow(board)) {
    if (board[i, col] == "E")
      counter = counter + 1 
  }
  
  if (counter == 0) {
    return(NULL)
  }
  return(counter)
}

source("hw3test.R") # Run tests on the functions above.

x <- rep(1:7, each = 6)
y <- rep(1:6, times = 7)

plot(x, y, type = "n", xlim = c(0, 8), ylim = c(7, 0))

segments(x0 <- rep(0.5, 7), y0 <- seq(7.5, 0.5, -1), x1 <- rep(7.5, 7), y1 <- seq(7.5, 0.5, -1))
segments(x0 <- seq(0.5, 7.5, 1), y1 <- rep(0.5, 7), x1 <- seq(0.5, 7.5, 1), rep(6.5, 7))

board <- matrix(data = rep("E", times = 7 * 6), nrow = 6, ncol = 7)

player <- "X"
for (i in 1:42) {
  if (player == "X") {
    repeat { 
      index = identify(x, y, n = 1, plot = FALSE) 
      col = x[index]
      row = largest.empty.row(board, col)
      if (is.null(row) | is.null(col)){
        cat(sep = "", "Choose another column since this one is full.\n")
        next
      } else if (board[row, col] == "E") {
        break
      }
    }
  }
  else {
    index <- sample(x = which(c(board) == "E"), size = 1)
    col = x[index]
    row=largest.empty.row(board, col)
  }
  board[row, col] = player
  text(x = col, y = row, labels = player)
  print(board)
  if (won(player, board, row, col)) {
    text(x = 4, y = 1/3, labels = paste(player, " won!"), col = "red")
    break
  }
  player = ifelse(test = (player == "X"), yes = "O", no = "X")
}

# Hint: this program is modeled on the tic-tac-toe program we did in
# class, so studying the latter program is worthwhile.

# Note that a user click in a column indicates that a checker should
# go to that column's lowest empty row (unless the column is full).

# Note that you should implement a computer player. At the least, it
# should choose randomly from among the non-full columns. (Feel free
# to do much more! If your computer player beats me on its first try,
# you will earn a package of M&Ms. This is a hard task. Feel free to
# ask for tips.)

