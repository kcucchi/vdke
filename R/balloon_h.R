#' Loftsgaarden-Quesenberry kth nearest neighbor bandwidths
#'
#' \code{bw_balloon_LQ} looks for the bandwidth following the
#' Loftsgaarden-Quesenberry algorithm. The bandwidth depends on the point of
#' estimation.
#'
#' These are the details of the function.
#'
#' @param tdat vector containing the training dataset.
#' @param edat vector containing the evaluation data points.
#' @param k integer specifying the order of the neighbor.
#' @return a vector of bandwidths  (one bandwidth per evaluation data point)
#' @examples
#' bw_balloon_LQ(tdat = 1:10,edat = 5.1,k = 3)
#' bw_balloon_LQ(tdat = 1:10,edat = c(2.8,5.1),k = 3)
bw_balloon_LQ = function(tdat,edat,k){

  # sanity checks
  # check that k is smaller than the number of training data points
  if(k > length(tdat)){
    stop(paste0('k should be lower than the length of the training dataset.',
                '\nHere k=',k,' and length(tdat)=',length(tdat),'.'))
  }

  h = rep(NA, length.out=length(edat))# the vector of bandwidths
  for (i in 1:length(edat)){
    h[i] <- dist_to_knn(p = edat[i],nbrVect = tdat,k = k)

  }

  return(h)

}
