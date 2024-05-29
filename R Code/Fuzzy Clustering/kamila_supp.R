library("devtools")
library("KernSmooth")
# install_github("cran/KernSmooth")

jointProbs <- function(catVec, weights, numClust) {
  levels <- unique(catVec)
  catProbs <- matrix(NA, numClust, length(levels))
  for (l in 1:length(levels)) {
    ind <- which(catVec == levels[l])
    if (length(ind) == 1) {
      catProbs[, l] <- weights[ind, ]
    } else {
      catProbs[, l] <- colSums(weights[ind, ])
    }
  }
  return(catProbs / colSums(weights))
}

initMeans <- function(conVar,method,numClust) {
  if (method=='sample') {
    return(
      sapply(conVar,function(xx) sample(xx,size=numClust,replace=TRUE))
    )
  } else if (method=='runif') {
    ranges <- sapply(conVar,range)
    return(
      apply(ranges,2,function(xx) runif(numClust,min=xx[1],max=xx[2]))
    )
  } else {
    stop('Unrecognized mean initialization method')
  }
}

wnorm <- function(x) {
  return(x/sqrt(2*pi)*exp(-1/2*x**2))
}

kam_bkde <- function(x, kernel = "normal", canonical = FALSE, bandwidth,
                 gridsize = 401L, range.x, truncate = TRUE)
{
  ## Install safeguard against non-positive bandwidths:
  if (!missing(bandwidth) && bandwidth <= 0)
    stop("'bandwidth' must be strictly positive")
  
  kernel <- match.arg(kernel,
                      c("normal", "box", "epanech", "biweight", "triweight", "wnormal"))
  
  ## Rename common variables
  
  n <- length(x)
  M <- gridsize
  
  ## Set canonical scaling factors
  
  del0 <- switch(kernel,
                 "normal" = (1/(4*pi))^(1/10),
                 "box" = (9/2)^(1/5),
                 "epanech" = 15^(1/5),
                 "biweight" = 35^(1/5),
                 "triweight" = (9450/143)^(1/5),
                 "wnormal" = NA)
  
  if (length(canonical) != 1L || !is.logical(canonical))
    stop("'canonical' must be a length-1 logical vector")
  
  ## Set default bandwidth
  
  h <- if (missing(bandwidth)) del0 * (243/(35*n))^(1/5)*sqrt(var(x))
  else if(canonical) del0 * bandwidth else bandwidth
  
  ## Set kernel support values
  
  tau <-  if (kernel == "normal" | kernel == "wnormal") 4 else 1
  
  if (missing(range.x)) range.x <- c(min(x)-tau*h, max(x)+tau*h)
  a <- range.x[1L]
  b <- range.x[2L]
  
  ## Set up grid points and bin the data
  
  gpoints <- seq(a, b, length.out = M)
  gcounts <- linbin(x, gpoints, truncate)
  
  ## Compute kernel weights
  
  delta  <- (b - a)/(h * (M-1L))
  L <- min(floor(tau/delta), M)
  if (L == 0)
    warning("Binning grid too coarse for current (small) bandwidth: consider increasing 'gridsize'")
  
  lvec <- 0L:L
  kappa <- if (kernel == "normal")
    dnorm(lvec*delta)/(n*h)
  else if (kernel == "wnormal")
    wnorm(lvec*delta)/(n*h)
  else if (kernel == "box")
    0.5*dbeta(0.5*(lvec*delta+1), 1, 1)/(n*h)
  else if (kernel == "epanech")
    0.5*dbeta(0.5*(lvec*delta+1), 2, 2)/(n*h)
  else if (kernel == "biweight")
    0.5*dbeta(0.5*(lvec*delta+1), 3, 3)/(n*h)
  else if (kernel == "triweight")
    0.5*dbeta(0.5*(lvec*delta+1), 4, 4)/(n*h)
  
  ## Now combine weight and counts to obtain estimate
  ## we need P >= 2L+1L, M: L <= M.
  P <- 2^(ceiling(log(M+L+1L)/log(2)))
  kappa <- c(kappa, rep(0, P-2L*L-1L), rev(kappa[-1L]))
  tot <- sum(kappa) * (b-a)/(M-1L) * n # should have total weight one
  gcounts <- c(gcounts, rep(0L, P-M))
  kappa <- fft(kappa/tot)
  gcounts <- fft(gcounts)
  list(x = gpoints, y = (Re(fft(kappa*gcounts, TRUE))/P)[1L:M])
}

environment(kam_bkde) <- asNamespace('KernSmooth')
assignInNamespace("bkde", kam_bkde, ns = "KernSmooth")

radialKDE <- function(radii,evalPoints,pdim,returnFun=FALSE, kernel="normal") {
  MAXDENS <- 1
  # Note using a chosen constant for bw reduces time by about 7%
  radialBW <- bw.nrd0(radii)
  radKDE <- kam_bkde(
    x = radii
    ,kernel = kernel
    ,bandwidth = radialBW
    #   ,range.x = c(0, max(radii))
    ,range.x = c(0,max(evalPoints))
  )
  
  # remove any zero and negative density estimates
  newY <- radKDE$y
  nonnegTF <- newY > 0
  if (any(!nonnegTF)) {
    minPos <- min(newY[nonnegTF])
    newY[!nonnegTF] <- minPos/100
  }
  
  # at bottom 5th percentile, replace with line through (0,0) and (q05,f(q05)).
  # This removes substantial variability in output near zero
  quant05 <- quantile(x = radKDE$x, probs = 0.05)
  #quant05 <- psort(xx = radKDE$x, pp = 0.05)
  coordsLtQ05 <- radKDE$x < quant05
  maxPt <- max(which(coordsLtQ05))
  newY[coordsLtQ05] <- radKDE$x[coordsLtQ05] * (newY[maxPt]/radKDE$x[maxPt]) # y = x * sl
  
  # radial Jacobian transformation; up to proportionality constant
  radY <- c(0,newY[-1] / radKDE$x[-1]^(pdim-1))
  
  # replace densities over MAXDENS with MAXDENS
  overMax <- radY > MAXDENS
  radY[overMax] <- MAXDENS
  
  # remove bumps (i.e. make sure nondecreasing)
  # not used currently
  #  radY <- rmBump1(radY)
  
  # normalize to area 1
  binwidthX <- diff(radKDE$x[1:2])
  densR <- radY/(binwidthX * sum(radY))
  
  # now create resampling function
  resampler <- approxfun(x=radKDE$x, y=densR,rule=1:2, method='linear')
  kdes <- resampler(evalPoints)
  if (!returnFun) {
    resampler <- NULL
  }
  
  #return(list(kdes=resampler(evalPoints),resampler=resampler))
  return(list(kdes=kdes,resampler=resampler))
}

dptm <- function(pts, myMeans, wgts, ppDim, kkMean, nn) {
  .Call('_kamila_dptm', PACKAGE = 'kamila', pts, myMeans, wgts, ppDim, kkMean, nn)
}

rowMax <- function(inMat) {
  .Call('_kamila_rowMax', PACKAGE = 'kamila', inMat)
}

rowMin <- function(inMat) {
  .Call('_kamila_rowMin', PACKAGE = 'kamila', inMat)
}

rowMaxInds <- function(inMat) {
  .Call('_kamila_rowMaxInds', PACKAGE = 'kamila', inMat)
}

sumMatList <- function(x) {
  .Call('_kamila_sumMatList', PACKAGE = 'kamila', x)
}

getIndividualLogProbs <- function(catFactorNum, catWeights, logProbsCond_i) {
  .Call('_kamila_getIndividualLogProbs', PACKAGE = 'kamila', catFactorNum, catWeights, logProbsCond_i)
}

aggregateMeans <- function(conVar, membNew, kk) {
  .Call('_kamila_aggregateMeans', PACKAGE = 'kamila', conVar, membNew, kk)
}

jointTabSmoothedList <- function(catFactorNum, membNew, numLev, catBw, kk) {
  .Call('_kamila_jointTabSmoothedList', PACKAGE = 'kamila', catFactorNum, membNew, numLev, catBw, kk)
}

