# TRUE OR FALSE
stopifnot(isTRUE(all.equal(1.414214, sqrt(2))))
stopifnot(isTRUE(all.equal(49 * (1 / 49), 1)))
stopifnot(isTRUE(49 * (1 / 49) == 1))
isTRUE(49 * (1/ 49) != 1)
all.equal(0, 0.1, tolerance = 0.2)
stopifnot(0 < 1, 1:3 <= 3)
stopifnot(49 * (1 / 49) == 1)
all.equal(49 * (1 / 49), 1)

# FUNCTION
is.heads = function() {
  return(rnorm(1) < 0)
}
x <- is.heads()
N = 10000
coins <- replicate(N, is.heads())

baby.t.test <- function(x, mu = 0, conf.level = 0.95) {
  stopifnot(length(x) >= 2)
  stopifnot((conf.level > 0) & (conf.level < 1))
  test <- list()
  n <- length(x)
  test$parameter <- n - 1
  xbar <- mean(x)
  xsd <- sd(x)
  t <- (xbar - mu) / (xsd / sqrt(n))
  test$statistic <- t
  test$p.value <- 2 * pt(q = -abs(t), df = test$parameter)
  alpha <- 1 - conf.level
  t_conf_level <- -qt(p = alpha / 2, df = test$parameter)
  error_margin <- t_conf_level * xsd / sqrt(n)
  test$conf.int <- c(xbar - error_margin, xbar + error_margin)
  test$estimate <- xbar
  test$null.value <- mu
  return(test)
}
baby.t <- baby.t.test(1:10, 5, 0.90)
t <- t.test(x <- 1:10, mu = 5, conf.level = 0.90)
stopifnot(isTRUE(all.equal(baby.t$parameter, as.numeric(t$parameter))))
stopifnot(isTRUE(all.equal(baby.t$statistic, as.numeric(t$statistic))))
stopifnot(isTRUE(all.equal(baby.t$p.value, as.numeric(t$p.value))))
stopifnot(isTRUE(all.equal(baby.t$conf.int, as.numeric(t$conf.int))))
stopifnot(isTRUE(all.equal(baby.t$estimate, as.numeric(t$estimate))))
stopifnot(isTRUE(all.equal(baby.t$null.value, as.numeric(t$null.value))))

#LOOP
n = 10
for (i in seq_len(n)) {
  even_or_odd <- ifelse(i %% 2 == 0, "even", "odd")
  cat(i, "is", even_or_odd, "\n")
}

x <- c(2, 3, 5)
total <- 0
product <- 1
for (x.i in x) {
  total <- total + x.i
}
for (x.i in x) {
  product <- product * x.i
}
product2 <- 1
for (i in seq_len(length(x))) {
  product2 <- product2 * x[i]
}

x <- 1
while (x < 10) {
  cat("x = ", x, "\n")
  x <- 2 * x
}

repeat {
  cat("Please answer 'yes' or 'no': ")
  decision <- scan(what = character(), n = 1, quiet = TRUE)
  if ((decision == "yes") | (decision == "no")) {
    break
  }
}

repeat {
  cat("Pete and Repeat were sitting on fece. Peter fall off. Who's sitting?\n")
  if (scan(what = character(), n = 1, quiet = TRUE) != "Repeat") {
    break
  }
}

n = 3
M <- matrix(rep(x = 0, times = n^2), nrow = n, ncol = n)
m <- M
for (col in 1:n) {
  for (row in 1:n) {
    m[row, col] <- 1
  }
}

#APPLY FUNCTION
red.density <- function(x, ...) {
  plot(density(x), col = "red", ...)
}
red.density(x = rnorm(100), main = "Two extra arguments", lty = "dashed")
average <- lapply(mtcars, mean)
sapply(mtcars, mean)
sapply(mtcars, quantile)
x <- 1:4
y <- 5:8
z <- 9:12
mapply(sum, x, y, z)

m <- matrix(data = 1:6, nrow = 2, ncol = 3)
apply(X = m, MARGIN = 1, FUN = sum)
apply(X = m, MARGIN = 2, FUN = sum)

tapply(mtcars$mpg, mtcars$cyl, mean)
tapply(mtcars$mpg, list(mtcars$cyl, mtcars$gear), mean)
mean(mtcars$mpg[(mtcars$cyl == 4) & (mtcars$gear == 5)])
tapply(mtcars$mpg, mtcars$cyl, quantile)

#ARRAY & MATRIX
a <- array(data = -(1:24), dim = c(3, 4, 2))
dimnames(a) <- list(c("slow", "medium", "fast"), c("cold", "tepid", "warm", "hot"), c("Monday", "Tuesday"))
dim(a) <- c(4, 6)
dim(a) <- NULL

m <- matrix(1:12, nrow = 3, ncol = 4, byrow = TRUE)
kids <- matrix(c(c(1, 2, 6, 7, 9, 11), c(1, 5, 100, 100, 100, 100)), nrow = 2, ncol = 6, byrow = TRUE, 
               dimnames = list(c("Age", "#Toys"), c("Teresa", "Margaret", "Monica", "Andrew", "Mary", "Philip")))
kids
m <- cbind(m, 101:103)
m <- rbind(m, 105:(105 + ncol(m) - 1))
m[row(m) == col(m)]
m[(row(m) + col(m)) == (ncol(m) + 1)]
r = 2
c = 3
m[row(m) - col(m) == r - c]

A <- matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE)
B <- matrix(18:26, nrow = 3, ncol = 3)
A * B
A %*% B
A <- matrix(data = 1:4, nrow = 2, ncol = 2)
b <- c(7, 10)
x <- solve(a = A, b = b)
A %*% x

#PATTERN
a <- c("Brown,Joe    123456789  jbrown@wisc.edu     1000",
       "Sirui,Yi     387475948  syi26@wisc.edu      5000",
       "Chen,Jean    374528572  chen@wisc.edu      24000",
       "Juniper,Jack 474637278  jjuniper@wisc.edu 300000")
sub(pattern = "e", replacement = "E", x = a)
gsub(pattern = "e", replacement = "_E_", x = a)
gsub(pattern = "[aeiou]", replacement = "", x = a)
gsub(pattern = "[^aeiou]", replacement = "", x = a)
grep(pattern = "^b", x = a)
grep(pattern = "^s", ignore.case = TRUE, x = a)
grep(pattern = "0$", x = a)
grep(pattern = "r\\>", x = a)
grep(pattern = "\\<s", x = a)
grep(pattern = "\\d{4}$", x = a)
grep(pattern = " \\d{4}$", x = a)
grep(pattern = " \\d{4,5}$", x = a)
sub(pattern = "\\d{1, }", replacement = "X", x = a)
gsub(pattern = "\\d{1, } ", replacement = "X", x = a)

link <- "blah blah ... <a href=http://www.google.com>Google</a> blah ..."
sub(pattern = ".*<a href=(.*)>.*", replacement = "\\1", x = link)
sub(pattern = ".*<a href=(.*?)e.*", replacement = "\\1", x = link)
sub(pattern = ".*<a href=([^>]*)>.*", replacement = "\\1", x = link)

b <- sub(pattern = "(\\w+),(\\w+).*", replacement = "\\2,\\1", x = a)
b

grep(pattern = "Joe|Jack", x = a)
grep(pattern = "(w|e)n", x = a)

strsplit(x = a, split = ",")
strsplit(x = a, split = " +")
strsplit(x = a, split = ",|( +)")

grep("[a-z]", letters)
txt <- c("arm", "foot", "lefroo", "bafoobar")
if (length(i <- grep("foo", txt))) {
  cat("'foo' appears in ", txt[i], "\n")
}
txt[i]
gsub("([ab])", "\\1_\\1", "abc and ABC")

sub(pattern="\\d{1, }" , replacement="X", x=a)
sub(pattern="\\d{1, }?" , replacement="X", x=a)

a = c("Brown,Joe    123456789 jbrown@wisc.edu      1000",
      "Sirui,Yi     456789123 sroukos@wisc.edu     5000",
      "Chen,Jean    789123456 chen@wisc.edu       24000",
      "Juniper,Jack 345678912 jjuniper@wisc.edu  300000"
)
b = sub(pattern = "(\\w+),(\\w+) +(\\w+) +(\\w+).*", replacement = "\\2,\\1,\\4,\\3", x=a)
b
d = read.csv(file=textConnection(b), header=FALSE, col.names=c("first","last","user","ID"))
d
(lists.of.words = strsplit(x=a, split=",|( +)"))
lists.of.words
vector.of.words <- unlist(lists.of.words)
vector.of.words
(m = matrix(vector.of.words, 4, 5, byrow=TRUE))
d <- as.data.frame(m)
d
colnames(d) <- c("","","","","")
d

baby.factorial <- function(n) {
  stopifnot(n >= 0)
  if (n == 0) {
    return(1)
  } else {
    product <- 1
    for (i in 1:n) {
      product <- product * i
    }
    return(product)
  }
}
stopifnot(baby.factorial(0) == 1)
stopifnot(baby.factorial(1) == 1)
stopifnot(baby.factorial(2) == 2)
stopifnot(baby.factorial(3) == 6)
stopifnot(baby.factorial(5) == 120)
baby.factorial(2)
baby.factorial(5)

diagnal <- function(m) {
  return(m[row(m) == col(m)])
}
diagnal(matrix(data=1:12, nrow=3, ncol=4))

get.NetIds <- function(file) {
  a <- scan(file, what = character(), quiet = TRUE)
  b <- a[grep(pattern = "@", x = a)]
  return(sub(pattern = "(.*)@.*", replacement = "\\1", x = b))
}
get.NetIds(file = "1.txt")


















