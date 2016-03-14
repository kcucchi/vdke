#'Loftsgaarden-Quesenberry kth nearest neighbor bandwidths
#'
#'\code{bw_balloon_LQ} looks for the bandwidth following the
#'Loftsgaarden-Quesenberry algorithm. The bandwidth depends on the point of
#'estimation.
#'
#'These are the details of the function.
#'
#'@param tdat vector or array containing the training dataset.
#'@param edat vector or array containing the evaluation data points.
#'@param k integer specifying the order of the neighbor.
#'@return a vector of bandwidths  (one bandwidth per evaluation data point)
#' @examples
#' # univariate
#' bw_balloon_LQ(tdat = 1:10,edat = 5.1,k = 3)
#' bw_balloon_LQ(tdat = 1:10,edat = c(2.8,5.1),k = 3)
#' # multivariate
#' bw_balloon_LQ(tdat = array(1:20,dim=c(4,5)),edat = 1:4,k = 1)
#'@export
bw_balloon_LQ = function(tdat,edat,k){

  # if tdat array and edat vector change edat to array
  if(is.array(tdat) && is.vector(edat)){
    eArray <- array(edat,dim=c(length(edat),1))
  }else{ # if tdat and edat are vectors change to arrays
    eArray <- vectToArray(edat)
  }
  tArray <- vectToArray(tdat)

  # sanity checks
  # check matching of dimensionalities of tdat and eArray
  if(dim(tArray)[1]!=dim(eArray)[1]){
    stop(paste0('tdat and edat should have the same dimension.',
                '\nHere dim(tdat)[1]=',dim(tArray)[1],
                ' and dim(edat)[1]=',dim(eArray)[1],'.'))
  }
  # check that k is smaller than the number of training data points
  if(k > dim(tArray)[2]){
    stop(paste0('k should be lower than the length of the training dataset.',
                '\nHere k=',k,' and dim(tdat)[2]=',dim(tArray)[2],'.'))
  }

  h = rep(NA, length.out=dim(eArray)[2]) # the vector of bandwidths
  for (i in 1:dim(eArray)[2]){ # loop over evaluation points
    h[i] <- dist_to_knn(x = eArray[,i],nghDat = tArray,k = k)
  }

  # replace zeros by tiny bandwidths
  h[which(h==0)] <- 1e-9

  # check that no bandwidth is NA
  if(any(is.na(h))){
    stop(paste0('There was a problem when computing bandwidths in bw_balloon_LQ.',
                '\nNA were computed.'))
  }

  return(h)

}
