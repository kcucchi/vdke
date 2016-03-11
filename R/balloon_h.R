# In this file I store all functions for calculating bandwidths based on the balloon approach.


#' Loftsgaarden-Quesenberry kth nearest neighbor bandwidths
#'
#' code{bw_ballon_LQ} looks for the bandwidth following the
#' Loftsgaarden-Quesenberry algorithm The bandwidth depends on the point of
#' estimation.
#'
#' These are the details of the function.
#'
#' @param tdat a vector containing the training dataset.
#' @param edat a vector containing the evaluation data points.
#' @param k specifying the order of the neighbor.
#' @return a vector of bandwidths  (one bandwidth per evaluation data point)
#' @examples
#' bw_balloon_LQ(tdat=runif(n=10,min=0,max=2),edat=c(0.1,0.5,1),k=3)
#' @section Warning These can be some warnings.
bw_balloon_LQ = function(tdat,edat,k){

  # sanit checks
  # check that k is smaller than the number of training data points
  if(k > length(tdat)){
    stop(paste0('k should be larger than the length of the training dataset.',
                '\nHere k=',k,'and length(tdat)=',length(tdat)))
  }

  h = rep(NA, length.out=length(edat))# the vector of bandwidths
  for (i in 1:length(edat)){

    # compute distances between evalution point and training dataset
    dist <- sqrt( (tdat - edat[i])^2 )
    distOrder <- order(dist)
    # look for distance between evaluation point and kth nearest neighbor training point
    h[i] <-distOrder[k]
  }

  return(h)

}
