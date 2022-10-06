# RNGversion(min(as.character(getRversion()),"3.5.3"))
#
# set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
# n <- 2000
# knapsack_objects <-
#   data.frame(
#     w=sample(1:4000, size = n, replace = TRUE),
#     v=runif(n = n, 0, 10000)
#   )

greedy_knapsack <- function(x,W){
  stopifnot(class(x) == "data.frame") #check that is a data frame
  stopifnot(colnames(x) == c("w","v"),is.numeric(x$w),is.numeric(x$v),x$w>0, x$v>0)
  stopifnot(class(W) == "numeric", W>0) #check that is a numeric value

  i <- 1
  rows <- nrow(x)
  ratio <- matrix(nrow = rows, ncol = 1)
  ratio <- data.frame(ratio)
  weight_sum <- 0
  while(i<= rows){
    ratio[i,1] <- (x[i,2]/x[i,1])
    i <- i + 1
  }
  x1 <- cbind(x,ratio)
  #print(dim(x1))
  #return(x1)
  order<- x1[order(x1[,3],decreasing = TRUE),c(1,2,3)]
  #print(dim(order))
  #return(order)

  index <- c()
  total_value <- 0
  y<-1
  while( weight_sum <= W){

      total_value <- total_value + order[y,2]
      index <- append(index,as.numeric(rownames(order)[order$v == order[y,2]]))
      weight_sum <- weight_sum + order[y,1]
      y<- y +1
  }

  y1<- y-1
    total_value <- total_value - order[y1,2]
    weight_sum <- weight_sum - order[y1,1]
    index <- index[-length(index)]


  list <- list("value"= round(total_value), "elements"= index)
  #index<- as.list(index)
  #print(index)
  #print(order$v)
  #print(weight_sum)

  #print(total_value)

  return(list)
}
