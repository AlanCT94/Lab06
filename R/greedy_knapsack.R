# RNGversion(min(as.character(getRversion()),"3.5.3"))
#
# set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
# n <- 100000
# knapsack_objects <-
#   data.frame(
#     w=sample(1:4000, size = n, replace = TRUE),
#     v=runif(n = n, 0, 10000)
#   )

greedy_knapsack <- function(x,W, type= "normal"){
  stopifnot(class(x) == "data.frame") #check that is a data frame
  stopifnot(colnames(x) == c("w","v"),is.numeric(x$w),is.numeric(x$v),x$w>0, x$v>0)
  stopifnot(class(W) == "numeric", W>0) #check that is a numeric value

  i <- 1

  ratio <- matrix(nrow = nrow(x), ncol = 1)
  ratio <- data.frame(ratio)
  weight_sum <- 0
  while(i<= nrow(x)){
    ratio[i,1] <- (x[i,2]/x[i,1])
    i <- i + 1
    }
  x1 <- cbind(x,ratio)

  order<- x1[order(x1[,3],decreasing = TRUE),c(1,2,3)]

  index <- c()
  total_value <- 0
  y<-1

  while( weight_sum <= W){

      total_value <- total_value + order[y,2]
      index <- append(index,as.numeric(rownames(order)[order$v == order[y,2]]))
      weight_sum <- weight_sum + order[y,1]
      y<- y +1
  }

  if(type == "normal"){

    y1<- y-1
    total_value <- total_value - order[y1,2]
    weight_sum <- weight_sum - order[y1,1]
    index <- index[-length(index)]

    list <- list("value"= round(total_value), "elements"= index)
    # print(W-weight_sum)
    return(list)

  }

  if(type == "plus" && weight_sum > W){

  # print(weight_sum)
  # print(W)
  # print(y)
  total_value <- total_value - order[y-1,2]
  weight_sum <- weight_sum - order[y-1,1]
  index <- index[-length(index)]
  # print(weight_sum)
    while(isTRUE(weight_sum <= W) == TRUE ){
      total_value <- total_value + order[y,2]
      weight_sum <-weight_sum + order[y,1]
      index <- append(index,as.numeric(rownames(order)[order$v == order[y,2]]))
      # print(weight_sum)
      # print(total_value)
      # print(index)

      if(isTRUE(weight_sum > W) == TRUE ){
        total_value <- total_value - order[y,2]
        weight_sum <- weight_sum - order[y,1]
        index <- index[-length(index)]
        # print(weight_sum)
        # print(total_value)
      }
      else if(isTRUE(weight_sum <= W) == TRUE){
        list <- list("value"= round(total_value), "elements"= index)
        return(list)
      }
      y <- y +1
    }

  list <- list("value"= round(total_value), "elements"= index)
  # print(y)
  # print(weight_sum)
  return(list)

  }
}


