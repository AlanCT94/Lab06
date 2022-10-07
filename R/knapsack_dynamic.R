#' knapsack_dynamic
#'
#' Implementation of a dynamic programming algorithm for the knapsack problem,
#' taken from this page: \url{https://en.wikipedia.org/wiki/Knapsack_problem}
#'
#' @param x data frame with two variables v and w
#' @param W knapsack size
#'
#' @return list with value and elements
#' @export
#'
#' @examples
#' RNGversion(min(as.character(getRversion()),"3.5.3"))
#' set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
#' n <- 2000
#' knapsack_objects <-
#' data.frame(
#'    w=sample(1:4000, size = n, replace = TRUE),
#'    v=runif(n = n, 0, 10000)
#'  )
#' x <- knapsack_objects[1:8,]
#' knapsack_dynamic(x = x, W = 3500)

knapsack_dynamic <- function(x,W) {

  #- Check of input
  stopifnot(is.data.frame(x),names(x)==c("w","v"),is.numeric(x$w),
            is.numeric(x$v),x$w>0,x$v>0,W>=0)

  #- Some data management
  v <- x$v
  w <- x$w

  n <- nrow(x)
  m <- matrix(0,(n+1),(W+1))

  i_v<- 0:n
  j_v <- 0:W

  #- Find m[i,w], where m[i,w] is the maximum value that can be attained with
  #  weight less than or equal to w using items up to first i items
  for (i in 2:(n+1)){
    for (j in 1:(W+1)) {
      if (w[i_v[i]] > j_v[j]) {
        m[i,j] <- m[i-1,j]
      }
      else {
        m[i,j] <-
          max(m[i-1,j],
              m[i-1,j-w[i_v[i]]]+v[i_v[i]])
      }
    }
  }

  #- Find maximum value and elements
  ind <- find_indeces(m, n+1, W+1, w, W) - 1
  value <- sum(v[ind])
  elements <- sort(ind)

  structure(
    list(
      value = value,
      elements = elements
    )
  )

}

#' find_indeces
#'
#' Help function to function knapsack_dynamic, finds the indeces of the elements
#' that gives the solution with highest value given the maximum weight W
#' restriction
#'
#' @param m matrix that for each index pair i and j specifies the maximum value
#'          that can be attained with weight less than or equal to j using items
#'          up to first i items. It is calulated in function knapsack_dynamic
#' @param i number of elements plus 1
#' @param j maximum weight plus 1
#' @param w vector with weight values
#' @param W maximum weight
#'
#' @return vector with indeces
#'
#' examples see function knapsack_dynamic
find_indeces <- function (m, i, j, w, W) {

  #- Checks of input
  stopifnot(is.matrix(m),is.numeric(m),
           is.numeric(i), is.numeric(j),i>0,j>0,
           is.numeric(w),w > 0, is.numeric(W), W >0)

  #- Some data management
  n <- length(w)
  ind <- c()
  i_v<- 0:n
  j_v <- 0:W

  #- Find indeces
  if (i == 1) {
    return(ind)
  }
  if (m[i, j] > m[i-1, j]){
    ind <- union(i,find_indeces(m,i-1, j-w[i_v[i]],w,W))
    return(ind)
  }
  else{
    ind <- find_indeces(m,i-1, j,w,W)
    return(ind)
  }
}

