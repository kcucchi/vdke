

#' Distance to kth nearest neighbor
#'
#' \code{dist_to_knn} calculates the distance between one point and its kth
#' nearest neighbor in a vector
#'
#' These are the details of the function.
#'
#' @param p number corresponding to the reference point.
#' @param nbrVect vector of numbers corresponding to the neighbors.
#' @param k integer specifying the order of the neighbor.
#' @return The distance between \code{p} and its \code{k}th nearest neighbor
#'   from vector \code{nbrVect}.
#' @examples
#' dist_to_knn(p = 1.4, nbrVect = 0:10, k = 4)
dist_to_knn = function(p,nbrVect,k){

  # compute distances between reference point and neighbor vector
  dist <- sqrt( (nbrVect - p)^2 )
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
#' @param sampleVect vector containing sample datapoints.
#' @param k integer specifying the order of the neighbor.
#' @return a vector of distances, where the \code{i}th element contains the
#'   distance between the \code{i}th element of \code{sampleVect} and its
#'   \code{k}th neighbor.
#' @examples
#' sample_dist_to_knn(1:4,3)
sample_dist_to_knn = function(sampleVect,k){

  ans <- numeric(length = length(sampleVect))

  for(i in 1:length(ans)){
    ans[i] <- dist_to_knn(p = sampleVect[i],
                          nbrVect = sampleVect[-i],
                          k = k)
  }

  return(ans)

}
