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
#' @export
bw_balloon_LQ = function(tdat,edat,k){

  # if tdat and edat are vectors change to arrays
  tdat <- vectToArray(tdat)
  edat <- vectToArray(edat)

  # sanity checks
  # check that the dimensionalities of tdat and edat
  if(dim(tdat)[1]!=dim(edat)[1]){
    stop(paste0('tdat and edat should have the same dimension.',
                '\nHere dim(tdat)[1]=',dim(tdat)[1],
                ' and dim(edat)[1]=',dim(edat)[1],'.'))
  }
  # check that k is smaller than the number of training data points
  if(k > dim(tdat)[2]){
    stop(paste0('k should be lower than the length of the training dataset.',
                '\nHere k=',k,' and dim(tdat)[2]=',dim(tdat)[2],'.'))
  }

  h = rep(NA, length.out=dim(edat)[2]) # the vector of bandwidths
  for (i in 1:length(edat)){
    h[i] <- dist_to_knn(x = edat[i],nghDat = tdat,k = k)

  }

  return(h)

}
