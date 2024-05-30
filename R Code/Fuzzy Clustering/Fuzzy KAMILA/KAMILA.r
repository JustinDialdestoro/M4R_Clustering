source("M4R_Clustering/R Code/Fuzzy Clustering/Fuzzy KAMILA/dependencies.r")

new_kamila <- function(conVar, catFactor, numClust, numInit,
  conWeights = rep(1,ncol(conVar)),
  catWeights = rep(1,ncol(catFactor)),
  maxIter = 25,
  conInitMethod = 'runif',
  catBw = 0.025,
  calcNumClust = 'none',
  numPredStrCvRun = 10,
  predStrThresh = 0.8,
  e = 1e-2,
  m = 2) {

  if (length(numClust) != 1) {
    stop("Input parameter numClust must be length 1 if calcNumClust == 'none'")
  }
  
  # main function

  # deprecated option
  returnResampler <- FALSE
  
  # check data is in correct form
  if (!is.data.frame(conVar) | !is.data.frame(catFactor)) {
    stop("Input datasets must be dataframes")
  }
  
  # check variable weights are valid
  if (max(c(conWeights,catWeights)) > 1 | min(c(conWeights,catWeights)) < 0) {
    stop("Weights must be in [0,1]")
  }

  # check number of variables match
  numObs <- nrow(conVar)
  numConVar <- ncol(conVar)
  if (nrow(catFactor) != numObs) {
    stop("Number of observations in con and cat vars don't match")
  }
  numCatVar <- ncol(catFactor)
  
  # count number of levels in each categorical variable
  numLev <- sapply(catFactor, function(xx) length(levels(xx)))
  
  # convert categorical variables into numeric factors
  catFactorNumeric <- sapply(catFactor, as.numeric, simplify = TRUE)
  
  # initiaise variables
  totalLogLikVect <- rep(NaN, numInit)
  totalDist <- sum(dptmCpp(pts=conVar, myMeans = matrix(colMeans(conVar), nrow=1), wgts = conWeights))
  
  # loop over each initialization
  for (init in 1:numInit) {
    # initialize means: matrix size numClust x numConVar
    means_i <- initMeans(conVar=conVar,method=conInitMethod,numClust=numClust)
    
    # initialize new weights: matrix size numObs x numClust
    weights_i <- matrix(runif(numObs * numClust), numObs, numClust)
    weights_new <- weights_i / rowSums(weights_i)
    
    # initialize iteration variables
    numIter <- 0
    totLogLiks_new <- 0
    totLogLiks_old <- 0
    
    # Loop until convergence
    while((abs(totLogLiks_new - totLogLiks_old) > e) | numIter < 2) { 
      numIter <- numIter + 1
      
      # update cluster probabilities
      pis_i <- colSums(weights_new**m) / numObs
      
      # compute distances from each point to each mean
      dist_i <- dptmCpp(pts = conVar, myMeans = means_i, wgts = conWeights)
      
      # extract minimum distances to each mean
      minDist_i <- rowMin(dist_i)
      
      # compute log RKDE of minimum distances
      logDistRadDens_vec <- log(radialKDE(radii = minDist_i ,evalPoints = c(dist_i),
                                          pdim = numConVar, returnFun = returnResampler, 
                                          kernel = "normal")$kdes)
      # convert into a matrix
      logDistRadDens_i <- matrix(logDistRadDens_vec, nrow = numObs, ncol = numClust)

      # compute weighted RKDE for weight mean updates
      WDistRadDens_vec <- radialKDE(radii = minDist_i, evalPoints = c(dist_i), pdim = numConVar, 
                                    returnFun = returnResampler, kernel = "wnormal")$kdes
      # convert into a matrix
      WDistRadDens_i <- matrix(WDistRadDens_vec, nrow = numObs, ncol = numClust)
      
      # compute omega weights for mean computation
      omega_i <- WDistRadDens_i / (exp(logDistRadDens_i) * bw.nrd0(minDist_i) * dist_i**2) +
        (numConVar - 1) / (dist_i**2)
      
      # update means: matrix size numClust X p_con
      means_i <- t((weights_new**m) * omega_i) %*% as.matrix(conVar) / colSums((weights_new**m) * omega_i)
      
      # compute empirical probabilities of each categorical level
      jointProbsList <- replicate(numCatVar, c())
      for (i in 1:numCatVar) {
        jointProbsList[[i]] <- jointProbs(catFactorNumeric[, i], weights_new**m, numClust)
      }
      # update categorical parameters
      logProbsCond_i <- lapply(jointProbsList, FUN=function(xx) log(xx))
      
      # get individual categorical log probabilities
      individualLogProbs <- getIndividualLogProbs(catFactorNum = catFactorNumeric, 
                                                  catWeights = catWeights,
                                                  logProbsCond_i = logProbsCond_i)

      # compute categorical log likelihood
      catLogLiks <- Reduce(f = "+", x = individualLogProbs)
      
      # total log likelihoods: matrix size numObs x numClust
      allLogLiks <- logDistRadDens_i + catLogLiks
      
      # update cluster membership weights
      clustLogLiks <- t(t(allLogLiks) + log(pis_i))
      # weights_new <- exp(t(clustLogLiks) - log(colSums(exp(clustLogLiks))))
      
      if (length(which(is.infinite(clustLogLiks))) > 0) {
        break
      }
      
      weights_new <- 1 / (clustLogLiks**(1 / (m - 1)) * rowSums(1 / clustLogLiks**(1 / (m - 1))))
      weights_new[is.na(weights_new)] <- 1
      
      totLogLiks_old <- totLogLiks_new
      totLogLiks_new <- sum(weights_new**m * clustLogLiks)
    }
    
    # store log likelihood for each initialization
    totalLogLikVect[init] <- totLogLiks_new
    
    # store current solution if objective beats all others
    if ((init == 1) | (init > 1 & totalLogLikVect[init] > max(totalLogLikVect[1:(init-1)]))) {
      finalObj <- totalLogLikVect[init]
      finalWeights <- weights_new
      finalCenters <- matrix(data=as.matrix(means_i), nrow=nrow(means_i), ncol=numConVar, 
                              dimnames = list(cluster=paste("Clust", 1:nrow(means_i)), 
                                              variableMean=paste("Mean",1:numConVar)))
      # rescale by weights
      finalProbs <- lapply(logProbsCond_i, exp)
      names(finalProbs) <- paste("Categorical Variable", 1:numCatVar)
    }
  }
  
  return(
    list(
      finalWeights = finalWeights
      ,finalObj = finalObj
      ,finalCenters = finalCenters
      ,finalProbs = finalProbs
    )
  )
}