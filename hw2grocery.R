### Name: Sirui Yi

### Email: syi26@wisc.edu

groceries <- read.table("groceries.csv", stringsAsFactors = FALSE, sep = ",")

how.many <- function(item, n.max) {
  cat(sep = "", "How many ", item, "?")
  n <- as.numeric(scan(what = integer(), nmax = 1, quiet = TRUE))
  while (n > n.max) {
    cat(sep = "", "  ERROR: too many for the budget", "\n")
    cat(sep = "", "How many ", item, "?")
    n <- as.numeric(scan(what = integer(), nmax = 1, quiet = TRUE))
  }
  return(n)
}

grocery.list <- function(file, budget) {
  groceries <- read.table(file, stringsAsFactors = FALSE, sep = ",")
  colnames(groceries) <- c("item", "price")
  groceries
  temp = 0
  groceryList <- data.frame(item = character(), price = character(), quantity = character())
  for (i in 1:length(groceries$item)) {
    if (groceries$price[i] < (budget - temp)) {
      num <- how.many(groceries$item[i], n.max = floor((budget - temp) / groceries$price[i]))
      temp = temp + num * groceries$price[i]
      purchase <- data.frame(groceries$item[i], groceries$price[i], num)
      groceryList <- rbind(groceryList, purchase)
    }
  }
  colnames(groceryList) = c("item", "price", "quantity")
  out <- groceryList[!apply(groceryList, 1, function(x) any(x == 0)), ]
  #print(out)
  #cat(sep = "", "Your bill is $", sum(out$price * out$quantity))
  print(out); cat(sep = "", "Your bill is $", sum(out$price * out$quantity))
  return(out)
}

outcome <- grocery.list("groceries.csv", 10)

