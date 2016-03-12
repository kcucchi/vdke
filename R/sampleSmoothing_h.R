#' Loftsgaarden-Quesenberry kth nearest neighbor bandwidths
#'
#' \code{bw_balloon_LQ} looks for the bandwidth following the
#' Loftsgaarden-Quesenberry algorithm. The bandwidth depends on the point of
#' estimation.
#'
#' These are the details of the function.
#'
#' @param tdat a vector containing the training dataset.
#' @param k specifying the order of the neighbor.
#' @return a vector of bandwidths  (one bandwidth per evaluation data point)
#' @examples
#' bw_sampleSmoothing(tdat = 1:10,k = 3)
#' bw_sampleSmoothing(tdat = (1:10)^2,k = 3)
bw_sampleSmoothing = function(tdat,k){

  # sanity checks
  # check that k is smaller than the number of training data points
  if(k >= length(tdat)){
    stop(paste0('k should be strictly lower than the length of the training dataset.',
                '\nHere k=',k,' and length(tdat)=',length(tdat),'.'))
  }

  h = sample_dist_to_knn(sampleVect = tdat,k)

  return(h)

}
