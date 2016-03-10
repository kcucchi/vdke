wd = 'C:/Users/Karina/Dropbox/01_Dossiers/01a_NumExp/variableKernelDensityEstimation'
setwd(wd)

singleGaussianKernel = function(t,e,h){
  # t is for training data
  # e is for evaluation data
  # h is the bandwidth of the single kernel
  normVal = 1 / (h * sqrt(2 * pi))
  expVal = exp( - (e - t)^2 / (2*h^2))
  return(normVal * expVal)
}

# ---------------- testing functions -----------------------------------------

evalVect = seq(from=-5,to=5,by=0.1)
kernelVect = singleGaussianKernel(0,evalVect,1)
plot(evalVect,kernelVect,
     type='l',
     xlab = 'x',ylab='kernel density estimate')
axis(1,0,labels = NA,lwd.ticks = 2,col.ticks = 'red',tck=0.1)

# ---------------- end testing functions -------------------------------------

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
  
  n = length(tdat) # this is the number of kernels
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
    
  }else if(bwtype=='sampleSmoothing'){
    # TO DO
  }
  
  return(list(kde = ans,individualKernels = individualKernels))
  
}


# ---------------- testing functions -----------------------------------------

evalVect = seq(from=-5,to=5,by=0.1)
trainingVect = tdat = runif(5,min = -1,max = 2)
kernelVect = kde(trainingVect,evalVect,1)
plot(evalVect,kernelVect$kde,
     type='l',
     xlab = 'x',ylab='kernel density estimate')
axis(1,trainingVect,labels = NA,lwd.ticks = 2,col.ticks = 'red',tck=0.1)
for(i in 1:length(trainingVect)){
  lines(evalVect,kernelVect$individualKernels[i,],lty=2)
}

# ---------------- end testing functions -------------------------------------

bw_balloon_LQ = function(tdat,edat,k){
  # This function looks for the bandwidth 
  # following the Loftsgaarden-Quesenberry algorithm
  # The bandwidth depends on the point of estimation.
  
  
  
  # look for distance between evaluation point and kth nearest neighbor training point
  
  for (i in 1:length(edat)){
    
    # compute distances between evalution point and training dataset  
    dist = sqrt( (tdat - edat[i])^2 )
    
  }
  
  # here the length of the bandwidth vector corresponds to the length of the evaluation dataset.
  
}



tdat = runif(10,min = 0,max = 1)
