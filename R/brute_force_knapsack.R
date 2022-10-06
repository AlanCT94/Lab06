
brute_force_knapsack <- function(x, W, type="forloop", filter_items=FALSE){

  #- Checks of input
  stopifnot(is.data.frame(x),names(x)==c("w","v"),is.numeric(x$w),
            is.numeric(x$v),x$w>0,x$v>0,W>=0)

  n1 <- nrow(x)

  #- If remove items for which w > W
  if (filter_items==TRUE) {
    ind_w_le_W <- which(x$w<=W)
    x <- x[ind_w_le_W,]
  }
  else {
    ind_w_le_W <- 1:n1
  }
  #- Some data management
  v <- x$v
  w <- x$w
  n <- nrow(x)
  n_comb <- 2^n

  if (type=="forloop") {
    #- Initialise matrices
    comb_bin <- matrix(intToBits(0),nrow=n_comb,ncol=n)
    comb_num <- matrix(0,n_comb,n)
    comb_v <- matrix(0,nrow=n_comb,ncol=1)
    comb_w <- matrix(0,nrow=n_comb,ncol=1)

    #- Calculate v and w for each combination
    for (i in 1:n_comb)  {
      comb_bin[i,] <- intToBits(i)[1:n]
      ind <- which(comb_bin[i,]==01)
      comb_num[i,ind] <- 1
      comb_v[i] <- sum(comb_num[i,]*v)
      comb_w[i] <- sum(comb_num[i,]*w)
    }

  }
  if (type=="matrix") {
    v_mat <- matrix(v,nrow=n)
    w_mat <- matrix(w,nrow=n)

    comb_bin <- intToBits(1:n_comb)
    comb_num <- matrix(as.numeric(comb_bin),nrow=n_comb,byrow = TRUE)
    comb_num <- comb_num[,1:n]

    comb_v <- comb_num %*% v_mat
    comb_w <- comb_num %*% w_mat
  }
  if (type=="apply1a") {
    v_mat <- matrix(v,nrow=n)
    w_mat <- matrix(w,nrow=n)
    #comb_bin <- sapply(vect,FUN=intToBits)
    comb_bin <- sapply(1:n_comb,FUN=intToBits)
    comb_num <-apply(comb_bin,FUN=as.numeric,MARGIN=1)[,1:n]
    comb_v <- comb_num %*% v_mat
    comb_w <- comb_num %*% w_mat
  }
  if (type=="apply1b") {
    comb_bin <- sapply(1:n_comb,FUN=intToBits)
    comb_num <-apply(comb_bin,FUN=as.numeric,MARGIN=1)[,1:n]
    comb_v <-apply(comb_num,FUN=crossprod,MARGIN=1,y=v)
    comb_w <-apply(comb_num,FUN=crossprod,MARGIN=1,y=w)
  }

  if (type=="apply2") {
    comb_res <- sapply(1:n_comb,FUN=calc_comb,n,v,w)
    #comb_res <- vapply(1:n_comb,FUN=calc_comb,n,v,w,FUN.VALUE = c(v,w,ind)
    #comb_res <- vapply(1:n_comb,FUN=calc_comb,n,v,w,FUN.VALUE = numeric(24))
    comb_res_df <- as.data.frame(comb_res)
    comb_v <- as.vector(unlist(comb_res_df[1,]))
    comb_w <- as.vector(unlist(comb_res_df[2,]))
    comb_ind <- comb_res_df[3,]
  }

  if (type=="parallel1a") {
    #vect <- 1:n_comb
    v_mat <- matrix(v,nrow=n)
    w_mat <- matrix(w,nrow=n)

    cores <- parallel::detectCores()
    # Using parLapply()
    # Set up the ’cluster’
    cl <- parallel::makeCluster(cores, type = "PSOCK")
    # Parallel calculation (parLapply):
    system.time({
      comb_bin <- parallel::parSapply(cl, 1:n_comb, intToBits)
      comb_num <-parallel::parApply(cl,comb_bin,FUN=as.numeric,MARGIN=1)[,1:n]
      comb_v <- comb_num%*%v_mat
      comb_w <- comb_num%*%w_mat
    })
    # Shut down cluster
    parallel::stopCluster(cl)
  }

  if (type=="parallel1b") {
    #vect <- 1:n_comb
    cores <- parallel::detectCores()
    # Using parLapply()
    # Set up the ’cluster’
    cl <- parallel::makeCluster(cores, type = "PSOCK")
    # Parallel calculation (parLapply):
    system.time({
      comb_bin <- parallel::parSapply(cl, 1:n_comb, intToBits)
      comb_num <-parallel::parApply(cl,comb_bin,FUN=as.numeric,MARGIN=1)[,1:n]
      comb_v <-parallel::parApply(cl,comb_num,FUN=crossprod,MARGIN=1,y=v)
      comb_w <-parallel::parApply(cl,comb_num,FUN=crossprod,MARGIN=1,y=w)
    })
    # Shut down cluster
    parallel::stopCluster(cl)
  }

  if (type=="parallel2") {

    cores <- parallel::detectCores()
    # Using parLapply()
    # Set up the ’cluster’
    cl <- parallel::makeCluster(cores, type = "PSOCK")
    # Parallel calculation (parLapply):
    #vect <- 1:n_comb
    comb_res <- parallel::parSapply(cl,1:n_comb,FUN=calc_comb,n,v,w)
    #comb_res <- parallel::parVapply(1:n_comb,FUN=calc_comb,n,v,w,FUN.VALUE = numeric(24))
    #comb_res <- vapply(1:n_comb,FUN=calc_comb,n,v,w,FUN.VALUE = c(v,w,ind))
    # Shut down cluster
    parallel::stopCluster(cl)

    comb_res_df <- as.data.frame(comb_res)
    comb_v <- as.vector(unlist(comb_res_df[1,]))
    comb_w <- as.vector(unlist(comb_res_df[2,]))
    comb_ind <- comb_res_df[3,]
  }

  #- Find max v where w <= W, and corresponding elements
  ind1 <- which(comb_w <= W)
  ind <- which.max(comb_v[ind1])
  ind_final <- ind1[ind]
  if (type %in% c("apply2","parallel2")) {
    elements = ind_w_le_W[comb_ind[[ind_final]]$ind]
  }
  else {
    elements <- ind_w_le_W[which(comb_num[ind_final,]==1)]
  }
  #elements <- which(comb_num[ind_final,]==1)
  #value <- comb_v[ind_final,1]
  value <- comb_v[ind_final]

  #- Compile results
  structure(
    list(
      value = value,
      elements = elements
    )
  )

}

#' calc_comb
#'
#' Calculates value and weight of the combinations
#'
#' @param vect vector to apply the function for
#' @param n number of knapsack items
#' @param v values
#' @param w weights
#'
#' @return list with sum v, sum w and indeces for all combinations
#'
calc_comb <- function(vect,n,v,w) {
  comb_bin <- intToBits(vect)[1:n]
  comb_num <- as.numeric(comb_bin)
  comb_v <- sum(comb_num*v)
  comb_w <- sum(comb_num*w)
  ind <- which(comb_num==1)
  #- Compile results
  res <-
    list(
      comb_v = comb_v,
      comb_w = comb_w,
      ind = ind
    )
  #str(comb_v)
  #str(comb_w)
  #str(comb_num)
  #res <- cbind(comb_v,comb_w,comb_num)
}
