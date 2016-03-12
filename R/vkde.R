# Description of the package vdke
# according to the paper of Terell and Scott, 1992



#' Single Gaussian Kernel
#'
#' \code{sgk} evaluates a kernel with a given center and bandwidth.
#'
#' These are the details of the function.
#'
#' @param t a number corresponding to the training data point.
#' @param e a number or numeric vector corresponding to the evaluation data
#'   point.
#' @param h the width of the kernel.
#' @return The evaluation at location \code{e} of the Gaussian kernel centered
#'   at \code{t} and with width \code{h}.
#' @examples
#' singleKde <- sgk(0,seq(from=-5,to=5,by=0.1),1)
#' plot(singleKde$kpdf$x,singleKde$kpdf$y,
#'    type='l',
#'    xlab = 'x',ylab='kernel density estimate')
#'    axis(1,singleKde$t,labels = NA,lwd.ticks = 2,col.ticks = 'red',tck=0.1)
sgk <- function(t,e,h){
  normVal <- 1 / (h * sqrt(2 * pi))
  expVal <- exp( - (e - t)^2 / (2*h^2))
  return(list(kpdf=data.frame(x=e,y=normVal*expVal),
              t=t,
              h=h))
}


#'title.
#'
#'This is the description of the function \code{kde_h}.
#'
#'These are the details of the function.
#'
#'@param tdat a vector containing the training dataset.
#'@param edat a vector containing the evaluation data points.
#'@param bwtype a string corresponding to the method for calculating bandwidths.
#'  Either \code{'fixed'}, \code{'balloon'} or \code{'sampleSmoothing'}.
#'@param h the bandwdth(s) of the kernels. It should be a scalar when
#'  \code{'bwtype=fixed'}, a vector of length \code{length(edat)} for
#'  \code{'bwtype='balloon'}, a vector of length \code{length(tdat)} for
#'  \code{'bwtype='sampleSmoothing'}. It can be calculated from other functions
#'  in this package.
#'@return The density at locations \code{edat} based on the training dataset
#'  \code{tdat} following the method specified by \code{bwtype}. The bandwidths
#'  \code{h} can be calculated from other functions in this package.
#' @examples
#' evalVect <- seq(from=-5,to=5,by=0.1)
#' trainingVect <- runif(5,min = -1,max = 2)
#' kernelVect <- kde_h(tdat=trainingVect,edat=evalVect,'fixed',1)
#' plot(evalVect,kernelVect$kde,
#'      type='l',
#'      xlab = 'x',ylab='kernel density estimate')
#' axis(1,trainingVect,labels = NA,lwd.ticks = 2,col.ticks = 'red',tck=0.1)
#' for(i in 1:length(trainingVect)){
#'   lines(evalVect,kernelVect$individualKernels[i,],lty=2)
#' }
kde_h <- function(tdat,edat,bwtype,h){

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

  n <- length(tdat) # number of individual kernels

  # loop over each training data point
  if(bwtype=='fixed'){
    individualContrib <- array(0,dim=c(n,length(edat))) # each row is an individual kernel
    for (iTrain in 1:n){
      individualContrib[iTrain,] <- sgk(tdat[iTrain],edat,h)$kpdf$y
    }
    # the result is the average of individual kernels
    individualKernels <- 1/n * individualContrib
    ans <- colSums(individualKernels)

  }else if(bwtype=='balloon'){
    individualContrib <- array(0,dim=c(n,length(edat)))
    # vectIndividuals[iTrain,iEval] is the contribution of edat[iTrain] to the evaluation at edat[iEval]
    for (iEval in 1:length(edat)){ # loop over evaluation dataset
      # h_iEval <- dist_to_knn(p = edat[iEval],nbrVect = tdat, k = k) # h for the (iEval)th evaluation point
      for (iTrain in 1:n){ # loop over training dataset
        individualContrib[iTrain,iEval] <- sgk(tdat[iTrain],edat[iEval],h[iEval])$kpdf$y
      }
    }
    # the result at location edat[iEval] is the average of individual kernels evaluations
    individualKernels <- 1/n * individualContrib
    ans <- colSums(individualKernels)

  }else if(bwtype=='sampleSmoothing'){
    individualContrib <- array(0,dim=c(n,length(edat)))
    # vectIndividuals[iTrain,iEval] is the contribution of edat[iTrain] to the evaluation at edat[iEval]
    # h <- bw_sampleSmoothing(tdat,k)
    for (iTrain in 1:n){ # loop over training dataset
      individualContrib[iTrain,] <- sgk(tdat[iTrain],edat,h[iTrain])$kpdf$y
    }
    # the result is the average of individual kernels
    individualKernels <- 1/n * individualContrib
    ans <- colSums(individualKernels)
  }

  return(list(kde = ans,individualKernels = individualKernels))

}

