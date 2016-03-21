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
#'@param p the dimensionality of the space. Default is univariate (p=1).
#'@return y_pdf The evaluation at location \code{e} of the Gaussian kernel centered at
#'  \code{t} and with width \code{h}.
#'@examples
#'singleKde1D <- sgk(0,seq(from=-5,to=5,by=0.1),1)
#'plot(singleKde1D$x_pdf,singleKde1D$y_pdf,
#'    type='l',
#'    xlab = 'x',ylab='kernel density estimate')
#'    axis(1,singleKde$t,labels = NA,lwd.ticks = 2,col.ticks = 'red',tck=0.1)
#'singleKde2D <- sgk(t=c(0,1),e=array(1:10,dim=c(2,5)),h=1)
#'@export
sgk <- function(t,e,h,p=1){

  # format e to array
  if(is.vector(e)){
    eArray <- array(e,dim=c(p,length(e)/p))
  }else{eArray <- e}
  # format t to array
  if(is.vector(t) & length(t)==p){
    # t is a vector containing coordinates of training datapoint
    tArray <- array(t,dim=c(p,1))
  }else{
    tArray <- t
  }

  # if vectors change to arrays
  tArray <- vectToArray(tArray)

  # sanity check
  # check dimensionality of t and e
  if(dim(tArray)[1] != p || dim(eArray)[1] != p){
    stop(paste0('Dimension problem in sgk.',
                '\nHere dim(tArray)[1]=',dim(tArray)[1],
                ', dim(eArray)[1]=',dim(eArray)[1],
                ' and p=',p,'.'))
  }
  # check that t is only one point
  if(dim(tArray)[2] != 1){
    stop(paste0('t should represent one point.',
                '\nHere dim(t)[2]=',dim(tArray)[2],'.'))
  }

  # check dimensionality of h
  if(length(h)!=1 && length(h)!=p ){
    stop(paste0('The dimensionality of h and the other arguments do not match.',
                '\nHere length(h)=',length(h),' and p=',p,'.'))
  }
  if(p > 1 && length(h)==1){
    h <- rep(h,p)
  }

  # calculate multivariate sgk
  evalPerDim <- array(0,dim=c(p,dim(eArray)[2]))
  # evalPerDim[iDim,iEval] is the contribution of the ith dimension point iEval
  for (iDim in 1:p){
    evalPerDim[iDim,] <- 1 / (h[iDim] * sqrt(2*pi)) *
      exp(-(eArray[iDim,]-tArray[iDim,])^2 / (2 * h[iDim]^2))
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
vkde <- function(tdat,edat,bwtype,h,p=1,verbose=F){

  # ----- modify argument types -------
  # if tdat array and edat vector change edat to array
  if(is.array(tdat) && length(edat)==p){
    eArray <- array(edat,dim=c(length(edat),1))
  }else{ # if tdat and edat are vectors change to arrays
    eArray <- vectToArray(edat)
  }
  tArray <- vectToArray(tdat)

  n <- dim(tArray)[2] # number of individual kernels
  m <- dim(eArray)[2] # number of evaluation datapoints

  # ----- sanity checks -------
  # check compatibility of tdat and edat first dimensions
  if(dim(eArray)[1]!=dim(tArray)[1]){
    stop(paste0('In vkde, dimensions of edat and tdat do not match.\n',
                'Here dim(eArray)[1]=',dim(eArray)[1],
                ' and dim(tArray)[1]=',dim(tArray)[1],'.'))
  }

  # p <- dim(eArray)[1] # dimension of space

  # check compatibility if h dimensions
  if(bwtype=='fixed'){
    if(is.vector(h)){
      if(length(h)!=1 && length(h)!=p){
        stop(paste0('In vkde, when bwtype=',bwtype,
                    ' h should be a number or a vector of size p.\n',
                    'Here length(h)=',length(h),
                    ' and p=',p,'.'))
      }
      hArray <- array(h,dim = c(p,1))
    }else if(is.array(h)){
      if(dim(h)[2]!=1){
        stop(paste0('h should refer to one bandwidth only.\n',
                    'Here dim(h)[2]=',dim(h)[2],'.'))
      }
      if(dim(h)[1]==1){
        hArray <- array(rep(h,p),dim=c(p,1))
      }else if(dim(h)[1]==p){
        hArray <- h
      }else{
        stop(paste0('h should have dimension 1 or p.\n',
                    'Here dim(h)[1]=',dim(h)[1],' and p=',p,'.'))
      }
    }else{stop(' h should be an array or a vector.')}

  }else if(bwtype=='balloon'){

    # check that there are as many bandwidths as evaluation data points
    if(is.vector(h)){
      if(length(h)==m){ # one bw per evaluation point
        hArray <- array(rep(h,p,each=p),dim=c(p,m))
      }else{
        stop(paste0('In vkde, when bwtype=',bwtype,
                    ' h should have the same size as the number of evaluation data points.\n',
                    'You have length(h)=',length(h),' and m=',m,'.'))
      }

    }else if(is.array(h)){
      if(dim(h)[2]!=m){
        stop(paste0('In vkde, when bwtype=',bwtype,
                    ' h should have the same size as the number of evaluation data points.\n',
                    'You have dim(h)[2]=',dim(h)[2],' and m=',m,'.'))
      }
      if(dim(h)[1]==1){ # repeat the bandwidths over all directions
        hArray <- array(rep(hArray,p,each=p),dim=c(p,m))
      }else if(dim(h)[1]==p){
        hArray <- h
      }else{
        stop(paste0('In vkde, dimension error\n',
                    'dim(h)[1]=',dim(h)[1],' and p=',p,'.'))
      }

    }else{stop('h should be an array or a vector.')}

  }else if(bwtype=='sampleSmoothing'){

    # check that there are as many bandwidths as evaluation data points
    if(is.vector(h)){
      if(length(h)==n){ # one bw per training datapoint
        hArray <- array(rep(h,p,each=p),dim=c(p,n))
      }else{
        stop(paste0('In vkde, when bwtype=',bwtype,
                    ' h should have the same size as the number of training data points.\n',
                    'You have length(h)=',length(h),' and n=',n,'.'))
      }
    }else if(is.array(h)){
      if(dim(h)[2]!=n){
        stop(paste0('In vkde, when bwtype=',bwtype,
                    ' h should have the same size as the number of training data points.\n',
                    'You have dim(h)[2]=',dim(h)[2],' and n=',n,'.'))
      }
      if(dim(h)[1]==1){
        hArray <- array(rep(hArray,p,each=p),dim=c(p,n))
      }else if(dim(h)[1]==p){
        hArray <- h
      }else{
        stop(paste0('In vkde, dimension error\n',
                    'dim(h)[1]=',dim(h)[1],' and p=',p,'.'))
      }
    }else{stop('h should be an array or a vector.')}

  }else{stop('In vkde, bwtype should be set to "fixed", "balloon" or "sampleSmoothing".')}

  # ----- start compute kernels -----

  individualContrib <- array(0,dim=c(n,m))
  # vectIndividuals[iTrain,iEval] is the contribution of tdat[iTrain] to the evaluation at edat[iEval]

  # individual contributions depend on bwtype
  if(bwtype=='fixed'){
    if(verbose) print(paste0('Starting kde with fixed bandwidth.'))
    for (iTrain in 1:n){
      cat('.')
      # contribution of (iTrain)th training point at evaluation locations
      individualContrib[iTrain,] <-
        sgk(tArray[,iTrain],eArray,hArray,p=p)$y_pdf
    }

  }else if(bwtype=='balloon'){
    if(verbose) print('Starting kde with balloon estimator')
    for (iEval in 1:m){ # loop over evaluation dataset
      cat('.')
      for (iTrain in 1:n){ # loop over training dataset
        # contribution of (iTrain)th training point at (iEval)th evaluation location
        individualContrib[iTrain,iEval] <-
          sgk(tArray[,iTrain],eArray[,iEval],hArray[,iEval],p=p)$y_pdf
      }
    }

  }else if(bwtype=='sampleSmoothing'){
    if (verbose) print('Starting kde with sample smoothing estimator...')
    for (iTrain in 1:n){ # loop over training dataset
      cat('.')
      # contribution of (iTrain)th training point at evaluation locations
      individualContrib[iTrain,] <-
        sgk(tArray[,iTrain],eArray,hArray[,iTrain],p=p)$y_pdf
    }
  }

  # the result at location edat[iEval] is the average of individual kernels evaluations
  individualKernels <- 1/n * individualContrib
  ans <- colSums(individualKernels)

  return(list(x_pdf=edat,
              y_pdf=ans,
              individualKernels = individualKernels,
              n=n,
              m=m,
              p=p))

}

