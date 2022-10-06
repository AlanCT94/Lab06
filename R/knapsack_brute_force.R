RNGversion(min(as.character(getRversion()),"3.5.3"))

set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

# sleep_func <- function(){Sys.sleep(5)}
# startTime<-Sys.time()
brute_force_knapsack <- function(x, W){
  sleep_func <- function(){Sys.sleep(5)}
  startTime<-Sys.time()
 # startTime<-Sys.time()
  stopifnot(class(x) == "data.frame") #check that is a data frame
  stopifnot(class(W) == "numeric", W >0) #check that is a numeric value

  n <- nrow(x)      #we obtain the number of rows
  df<- c() #create an empty vector
  value <- c()
  weight<- c()
  i <- 0            #start our counter
  j <- 1
  n1 <- 2^n
  combination <- data.frame()

  while(i < n1){
    r <- intToBits(i)
    r1 <- r[1:8]
    df <- rbind(df, r1)
    suma <- 0
    sumaw <- 0
    for(e in r)
    {
      if(e == 1){
        suma = suma + x[j,2]
        sumaw = sumaw + x[j,1]
      }
      j <- j + 1
      value[i] <- suma
      weight[i]<- sumaw
    }
    i <- i + 1
    j <- 1
    z<- data.frame(which(r==01))
    combination<- append(combination,z)
  }
  #names(combination) <- c("Elements")
  val1 <- matrix(value, nrow = (n1-1))
  weight1 <- matrix(weight, nrow = (n1-1))
  sum_matrix<- cbind(weight1, val1)
  colnames(sum_matrix) <- c("Weight", "Value")
  sum_matrix<- as.data.frame(sum_matrix)
  #combination<- as.list.data.frame(combination)

  #sum_matrix<- append(sum_matrix,combination)
  #print(sum_matrix)
  #order<- sum_matrix[order(sum_matrix[,1]),c(1,2)]
  #es<-sum_matrix$Weight
 # es1<- which(sum_matrix$Weight <= W)
  es2 <- sum_matrix[sum_matrix$Weight <= W,]
  es3 <-es2[es2$Value == max(es2$Value),]
  max_value<- max(es2$Value)
  index<- which(es2==max_value)
 # mvalue<- list("value"= round(max_value))
 # elements<-list("elements"= which(intToBits(as.numeric(rownames(es3)))==01))
  result <- list("value"= round(max_value), "elements"= which(intToBits(as.numeric(rownames(es3)))==01))

  #print(mvalue)
  #print(elements)
  return(result)
  endTime <- Sys.time()
  print(endTime- startTime)
}
# endTime <- Sys.time()
# print(endTime- startTime)

