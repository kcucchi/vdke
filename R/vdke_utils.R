#' change vector to array
#'
#' change vector to array
#'
#' Transforms a dataset in the form of a vector in a dataset in the form of an
#' array
#'
#' @param vect vector
#' @return an array with dimensions (1,length(vect))
#' @examples
#' vectToArray(1:10)
#' @export
vectToArray <- function(vect){
  if(is.vector(vect)){vect <- array(vect,dim = c(1,length(vect)))}
  return(vect)
}


# -------- Calculate bw for rule of thumb -----------------

#' Rule-of-thumb bandwidth
#'
#' \code{ruleOfThumb} calculates the rule-of-thumb bandwidth according to the
#' standard formula (eg. p23 of np package documentation).
#'
#' These are the details of the function.
#'
#' @param tdat array containing the training dataset.
#' @return a number corresponding to the standard bandwidth
#' @examples
#' ruleOfThumb(runif(100,0,1))
#' @export
ruleOfThumb <- function(tdat){

  # if tdat is vector change to array with first dimension 1
  tdat <- vectToArray(tdat)

  p <- dim(tdat)[1] # space dimensionality
  n <- dim(tdat)[2] # number of points in training dataset

  spread <- numeric(length = p)

  for (iDim in 1:p){
    spread[iDim] <- min(sd(tdat[iDim,]),
                        mean(abs(tdat[iDim,]-mean(tdat[iDim,])))/1.4826,
                        IQR(tdat[iDim,])/1.349)
  }

  return(1.06 * spread * n^(-1/(2*p+1)))

}

# -------- Calculate distances to nearest neighbors ---------------------------------

#' Distance to kth nearest neighbor
#'
#' \code{dist_to_knn} calculates the distance between one point and its kth
#' nearest neighbor in a vector
#'
#' These are the details of the function.
#'
#' @param x number (univariate) or vector (multivariate) corresponding to the
#'   reference point.
#' @param nghDat vector (univariate) or array (multivariate) of numbers
#'   corresponding to the neighbors.
#' @param k integer specifying the order of the neighbor.
#' @return The distance between \code{x} and its \code{k}th nearest neighbor in
#'   \code{nghDat}.
#' @examples
#' dist_to_knn(x = 1.4, nghDat = 0:10, k = 4)
#' dist_to_knn(x=c(1,2),nghDat=array(1:10,dim=c(2,5)),k=1)
#' @export
dist_to_knn = function(x,nghDat,k){

  # if nghDat is vector change it to array
  nghDat <- vectToArray(nghDat)

  # sanity check
  if(length(x)!=(dim(nghDat)[1])){
    stop(paste0('The length of x should be equal to the first dimension of nghDat.',
                '\nHere length(x)=',length(x),' and dim(nghDat)[1]=',dim(nghDat)[1],'.'))
  }

  # compute distances between reference point and neighbor vector
  diff <- sweep(nghDat,1,x) # substracts x from each column of nghDat
  dist <- sqrt( colSums( (diff)^2 )) # sum over the dimensions
  # sort distances in increasing order
  distOrder <- sort(dist)
  # look for distance between reference point and kth nearest neighbor
  return(distOrder[k])

}

#' calculates vector of nearest of nearest neighbors
#'
#' \code{sample_dist_to_knn} calculates kth-nearest-neighbor distances from a
#' sample.
#'
#' These are the details of the function.
#'
#' @param sampleDat vector containing sample datapoints.
#' @param k integer specifying the order of the neighbor.
#' @return a vector of distances, where the \code{i}th element contains the
#'   distance between the \code{i}th element of \code{sampleDat} and its
#'   \code{k}th neighbor.
#' @examples
#' sample_dist_to_knn(sampleDat = 1:4,k = 3)
#' sample_dist_to_knn(sampleDat = array(1:12,dim = c(3,4)),k = 1)
#' @export
sample_dist_to_knn <- function(sampleDat,k){

  sampleDat <- vectToArray(sampleDat)

  # check that k is less than number of points in sample dataset
  if(k > dim(sampleDat)[2]){
    stop(paste0('k should be lower than the number of sample datapoints.',
                '\nHere k=',k,' and dim(sampleDat)[2]=',dim(sampleDat)[2],'.'))
  }

  ans <- numeric(length = dim(sampleDat)[2])

  for(i in 1:length(ans)){
    ans[i] <- dist_to_knn(x = sampleDat[,i],
                          nghDat = sampleDat[,-i],
                          k = k)
  }

  return(ans)

}
