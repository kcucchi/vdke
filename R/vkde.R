# Description of the package vdke
# according to the paper of Terell and Scott, 1992



#' Single Gaussian Kernel
#'
#' \code{sgk} evaluates a kernel with a given center and bandwidth.
#'
#' These are the details of the function.
#'
#' @param t a number corresponding to the training data point.
#' @param e a number or numeric vector corresponding to the evaluation data point.
#' @param h the width of the kernel.
#' @return The evaluation at location \code{e} of the Gaussian kernel centered
#'   at \code{t} and with width \code{h}.
#' @examples
#' sgk(0,seq(from=-5,to=5,by=0.1),1)
#' @section Warning These can be some warnings.
sgk = function(t,e,h){
  normVal = 1 / (h * sqrt(2 * pi))
  expVal = exp( - (e - t)^2 / (2*h^2))
  return(normVal * expVal)
}

# # ---------------- testing functions -----------------------------------------
#
# evalVect = seq(from=-5,to=5,by=0.1)
# kernelVect = singleGaussianKernel(0,evalVect,1)
# plot(evalVect,kernelVect,
#      type='l',
#      xlab = 'x',ylab='kernel density estimate')
# axis(1,0,labels = NA,lwd.ticks = 2,col.ticks = 'red',tck=0.1)
#
# # ---------------- end testing functions -------------------------------------
#


#' title.
#'
#' This is the description of the function \code{kde_h}.
#'
#' These are the details of the function.
#'
#' @param tdat a vector containing the training dataset.
#' @param edat a vector containing the evaluation data points.
#' @param bwtype a string corresponding to the method for calculating
#'   bandwidths. Either \code{'fixed'}, \code{'balloon'} or
#'   \code{'sampleSmoothing'}.
#' @param h the bandwdth(s) of the kernels. It should be a scalar when
#'   \code{'bwtype=fixed'}, a vector of length \code{length(edat)} for
#'   \code{'bwtype='balloon'}, a vector of length \code{length(tdat)} for
#'   \code{'bwtype='sampleSmoothing'}. It can be calculated from other functions
#'   in this package.
#' @return The density at locations \code{edat} based on the training dataset
#'   \code{tdat} following the method specified by \code{bwtype}. The bandwidths
#'   \code{h} can be calculated from other functions in this package.
#' @examples
#'
#' @section Warning These can be some warnings.
kde_h = function(tdat,edat,bwtype,h){

  # sanity checks
  if(bwtype=='fixed'){
    print(paste0('Starting kde with fixed bandwidth (h=',h,') ...'))

  }else if(bwtype=='balloon'){
    print('Starting kde with balloon estimator...')
    # check that there are as many bandwidths as evaluation data points
    if(length(h)!=length(edat)){
      stop(paste0('In kde_h, when bwtype="balloon", you must have length(h)==length(edat).\n',
                  'You have length(h)=',length(h),' and length(edat)=',length(edat),'.'))
    }

  }else if(bwtype=='sampleSmoothing'){
    print('Starting kde with sample smoothing estimator...')
    # check that there are as many bandwidths as training data points
    if(length(h)!=length(tdat)){
      stop(paste0('In kde_h, when bwtype="sampleSmoothing", you must have length(h)==length(tdat).\n',
                  'You have length(h)=',length(h),' and length(tdat)=',length(tdat),'.'))
    }

  }else{stop('In kde_h, bwtype should be set to "fixed", "balloon" or "sampleSmoothing".')}

  n = length(tdat) # this is the number of individual kernels
  vectIndividuals = array(0,dim=c(n,length(edat))) # each row is an individual kernel

  # loop over each training data point
  if(bwtype=='fixed'){
    for (i in 1:n){
      vectIndividuals[i,] = singleGaussianKernel(tdat[i],edat,h)
    }
    # the result is the average of individual kernels
    individualKernels = 1/n * vectIndividuals
    ans = colSums(individualKernels)

  }else if(bwtype=='balloon'){
    # TO DO
    # loop over estimation data points

  }else if(bwtype=='sampleSmoothing'){
    for (i in 1:n){ # loop over training dataset
      vectIndividuals[i,] = singleGaussianKernel(tdat[i],edat,h[i])
    }
    # the result is the average of individual kernels
    individualKernels = 1/n * vectIndividuals
    ans = colSums(individualKernels)
  }

  return(list(kde = ans,individualKernels = individualKernels))

}
#
#
# # ---------------- testing functions -----------------------------------------
#
# evalVect = seq(from=-5,to=5,by=0.1)
# trainingVect = tdat = runif(5,min = -1,max = 2)
# kernelVect = kde_h(trainingVect,evalVect,1)
# plot(evalVect,kernelVect$kde,
#      type='l',
#      xlab = 'x',ylab='kernel density estimate')
# axis(1,trainingVect,labels = NA,lwd.ticks = 2,col.ticks = 'red',tck=0.1)
# for(i in 1:length(trainingVect)){
#   lines(evalVect,kernelVect$individualKernels[i,],lty=2)
# }
#
# # ---------------- end testing functions -------------------------------------
#

#
#
#
# tdat = runif(10,min = 0,max = 1)
