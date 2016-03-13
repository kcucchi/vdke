# Description of the package vdke
# according to the paper of Terrell and Scott, 1992

#'Single Gaussian Kernel
#'
#'\code{sgk} evaluates a kernel with a given center and bandwidth.
#'
#'These are the details of the function.
#'
#'@param t a number or vector corresponding to the training data point.
#'@param e a vector, or array corresponding to the evaluation data points.
#'@param h the width of the kernel.
#'@return The evaluation at location \code{e} of the Gaussian kernel centered at
#'  \code{t} and with width \code{h}.
#'@examples
#'singleKde1D <- sgk(0,seq(from=-5,to=5,by=0.1),1)
#'plot(singleKde1D$x_pdf,singleKde1D$y_pdf,
#'    type='l',
#'    xlab = 'x',ylab='kernel density estimate')
#'    axis(1,singleKde$t,labels = NA,lwd.ticks = 2,col.ticks = 'red',tck=0.1)
#'singleKde2D <- sgk(t=c(0,1),e=array(1:10,dim=c(2,5)),h=1)
#'@export
sgk <- function(t,e,h){

  # format e to array
  eArray <- vectToArray(e)
  # format t to array
  if(is.vector(t) & length(t)==dim(eArray)[1]){
    # t is a vector containing coordinates of training datapoint
    tArray <- array(t,dim=c(length(t),1))
  }else{
    tArray <- t
  }

  # if vectors change to arrays
  tArray <- vectToArray(tArray)

  # sanity check
  # check dimensionality of t and e
  if(dim(tArray)[1] != dim(eArray)[1]){
    stop(paste0('The dimensionalities of t and e do not match.',
                '\nHere dim(t)[1]=',dim(tArray)[1],' and dim(e)[1]=',dim(eArray)[1]))
  }
  # check that t is only one point
  if(dim(tArray)[2] != 1){
    stop(paste0('t should represent one point.',
                '\nHere dim(t)[2]=',dim(tArray)[2],'.'))
  }

  # dimensionality of the problem
  n <- dim(tArray)[1]

  # check dimensionality of h
  if(length(h)!=1 && length(h)!=n ){
    stop(paste0('The dimensionality of h and the other arguments do not match.',
                '\nHere length(h)=',length(h),' and dim(e)[1]=',dim(eArray)[1]))
  }
  if(n > 1 && length(h)==1){
    h <- rep(h,n)
  }

  # calculate multivariate sgk
  evalPerDim <- array(0,dim=c(n,dim(eArray)[2]))
  # evalPerDim[iDim,iEval] is the contribution of the ith dimension point iEval
  for (iDim in 1:n){
    evalPerDim[iDim,] <- 1 / (h[iDim] * sqrt(2*pi)) * exp(-(eArray[iDim,]-tArray[iDim,])^2 / h[iDim])
  }
  # evaluation is product of all dimensions
  ans <- apply(evalPerDim,2,prod)

  return(list(x_pdf=e,
              y_pdf=ans,
              t=t,
              h=h))
}


#'Variable Kernel Density Estimation
#'
#'This is the description of the function \code{vkde}.
#'
#'These are the details of the function.
#'
#'@param tdat a vector containing the training dataset.
#'@param edat a vector containing the evaluation data points.
#'@param bwtype a string corresponding to the method for calculating bandwidths.
#'  Either \code{'fixed'}, \code{'balloon'} or \code{'sampleSmoothing'}.
#'@param h the bandwidth(s) of the kernels. It should be a scalar when
#'  \code{'bwtype=fixed'}, a vector of length \code{length(edat)} for
#'  \code{'bwtype=balloon'}, a vector of length \code{length(tdat)} for
#'  \code{'bwtype=sampleSmoothing'}. It can be calculated from other functions
#'  in this package.
#'@return The density at locations \code{edat} based on the training dataset
#'  \code{tdat} following the method specified by \code{bwtype}. The bandwidths
#'  \code{h} can be calculated from other functions in this package.
#' @examples
#' evalVect <- seq(from=-5,to=5,by=0.1)
#' trainingVect <- runif(5,min = -1,max = 2)
#' kernelVect <- vkde(tdat=trainingVect,edat=evalVect,'fixed',1)
#' plot(evalVect,kernelVect$kde,
#'      type='l',
#'      xlab = 'x',ylab='kernel density estimate')
#' axis(1,trainingVect,labels = NA,lwd.ticks = 2,col.ticks = 'red',tck=0.1)
#' for(i in 1:length(trainingVect)){
#'   lines(evalVect,kernelVect$individualKernels[i,],lty=2)
#' }
#'@export
vkde <- function(tdat,edat,bwtype,h){

  # ----- sanity checks -------
  if(bwtype=='fixed'){
    print(paste0('Starting kde with fixed bandwidth (h=',h,') ...'))

  }else if(bwtype=='balloon'){
    print('Starting kde with balloon estimator...')
    # check that there are as many bandwidths as evaluation data points
    if(length(h)!=length(edat)){
      stop(paste0('In vkde, when bwtype="balloon", you must have length(h)==length(edat).\n',
                  'You have length(h)=',length(h),' and length(edat)=',length(edat),'.'))
    }

  }else if(bwtype=='sampleSmoothing'){
    print('Starting kde with sample smoothing estimator...')
    # check that there are as many bandwidths as training data points
    if(length(h)!=length(tdat)){
      stop(paste0('In vkde, when bwtype="sampleSmoothing", you must have length(h)==length(tdat).\n',
                  'You have length(h)=',length(h),' and length(tdat)=',length(tdat),'.'))
    }

  }else{stop('In vkde, bwtype should be set to "fixed", "balloon" or "sampleSmoothing".')}

  # ----- start compute kernels -----

  n <- length(tdat) # number of individual kernels
  individualContrib <- array(0,dim=c(n,length(edat)))
  # vectIndividuals[iTrain,iEval] is the contribution of edat[iTrain] to the evaluation at edat[iEval]

  # individual contributions depend on bwtype
  if(bwtype=='fixed'){
    for (iTrain in 1:n){
      cat('.')
      individualContrib[iTrain,] <- sgk(tdat[iTrain],edat,h)$kpdf$y
    }

  }else if(bwtype=='balloon'){
    for (iEval in 1:length(edat)){ # loop over evaluation dataset
      cat('.')
      # h_iEval <- dist_to_knn(p = edat[iEval],nbrVect = tdat, k = k) # h for the (iEval)th evaluation point
      for (iTrain in 1:n){ # loop over training dataset
        individualContrib[iTrain,iEval] <- sgk(tdat[iTrain],edat[iEval],h[iEval])$kpdf$y
      }
    }

  }else if(bwtype=='sampleSmoothing'){
    # h <- bw_sampleSmoothing(tdat,k)
    for (iTrain in 1:n){ # loop over training dataset
      cat('.')
      individualContrib[iTrain,] <- sgk(tdat[iTrain],edat,h[iTrain])$kpdf$y
    }
  }

  # the result at location edat[iEval] is the average of individual kernels evaluations
  individualKernels <- 1/n * individualContrib
  ans <- colSums(individualKernels)

  return(list(kde = ans,individualKernels = individualKernels))

}

