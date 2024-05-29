source("M4R_Clustering/R Code/Fuzzy Clustering/kamila_supp.r")

kamila_test <- function(
  conVar
  ,catFactor
  ,numClust
  ,numInit
  ,conWeights = rep(1,ncol(conVar))
  ,catWeights = rep(1,ncol(catFactor))
  ,maxIter = 25
  ,conInitMethod = 'runif'
  ,catBw = 0.025
  ,verbose = FALSE
  ,calcNumClust = 'none'
  ,numPredStrCvRun = 10
  ,predStrThresh = 0.8
) {

  if (calcNumClust == 'none') {
    if (length(numClust) != 1) {
      stop('Input parameter numClust must be length 1 if calcNumClust == "none"')
    }
    # main function
  
    # Deprecated option
    returnResampler = FALSE
    
    # (0) extract data characteristics, checks
    if (!is.data.frame(conVar) | !is.data.frame(catFactor)) {
      stop("Input datasets must be dataframes")
    }
    if (max(c(conWeights,catWeights)) > 1 | min(c(conWeights,catWeights)) < 0) stop('Weights must be in [0,1]')
    
    numObs <- nrow(conVar)
    numConVar <- ncol(conVar)
    if (nrow(catFactor) != numObs) {
      stop("Number of observations in con and cat vars don't match")
    }
    numCatVar <- ncol(catFactor)
    
    numLev <- sapply(catFactor,function(xx) length(levels(xx)))
    
    # for later use, convert to numeric matrix by level codes
    catFactorNumeric <- sapply(catFactor,as.numeric,simplify=TRUE)
    
    numIterVect <- rep(NaN,numInit)
    totalLogLikVect <- rep(NaN,numInit)
    catLogLikVect <- rep(NaN,numInit)
    winDistVect <- rep(NaN,numInit)
    totalDist <- sum(dptmCpp(
      pts=conVar
      ,myMeans=matrix(colMeans(conVar),nrow=1)
      ,wgts=conWeights #rep(1,numConVar)
    ))
    objectiveVect <- rep(NaN,numInit)
    
    # for verbose output, list of memberships for each init, iter
    if (verbose) membLongList = rep(list(list()),numInit)
    
    
    # Apply continuous weighting vector directly to variables
    #for (colInd in 1:ncol(conVar)) conVar[,colInd] <- conVar[,colInd] * conWeights[colInd]
    
    # (1) loop over each initialization
    for (init in 1:numInit) {
      # initialize means; matrix size numClust X numConVar
      means_i <- initMeans(conVar=conVar,method=conInitMethod,numClust=numClust)
      #print(means_i)
      
      # initialize probabilities; list length numCatVar of numClust X numLev matrices
      # generate each level prob from dirichlet(alpha=rep(1,nlev))
      # These are P( level | clust )
      logProbsCond_i <- lapply(
        numLev
        ,function(xx) {
          matrix(
            data=log(gtools::rdirichlet(n=numClust,alpha=rep(1,xx)))
            ,nrow=numClust
            ,dimnames=list(clust=1:numClust,level=1:xx)
          )
        }
      )
      
      # NEW CODE STARTS HERE
      
      # initialize cluster probabilities; length numClust
      pis_i <- runif(numClust)
      pis_i <- pis_i / sum(pis_i)
      
      # initialize posterior probabilities; matrix size numObs x numClust
      # weights_i <- matrix(runif(numObs * numClust), numObs, numClust)
      # weights_i <- weights_i / rowSums(weights_i)
      
      # END OF NEW CODE
      
      #print('logProbsCond_i')
      #print(logProbsCond_i)
      
      # initialize structures for iterative procedure
      # membOld <- membNew <- rep(0,numObs)
      numIter <- 0
      degenerateSoln <- FALSE
      
      # Loop until convergence
      # loop for minimum two iterations, while all memberships are NOT UNchanged, while still under max # iter
      while(
        ( (numIter < 3) | !all(membOld == membNew))
        & (numIter < maxIter)
      ) { 
        
        
        numIter <- numIter + 1
        #print(paste('Iteration',numIter))
        
        #print('conVar')
        #str(conVar)
        #print('means_i')
        #str(means_i)
        #print('conWeights')
        #str(conWeights)
        #print('initMeans Structure')
        #str(initMeans(conVar,conInitMethod,2))
        #str(initMeans(cbind(conVar,conVar),conInitMethod,2))
        
        #2 calc weighted euclidean distances to means
        # result is numObs X numClust matrix
        #str(means_i)
        #str(conVar)
        
        # by default no uniform weights
        dist_i <- dptmCpp(pts=conVar,myMeans=means_i,wgts=conWeights)
        #dist_i <- dptmCpp(pts=conVar,myMeans=means_i,wgts=rep(1,numConVar)) # no weights
        #print('Distance X cluster matrix')
        #print(head(dist_i,n=15))
        
        #3 extract min distances
        minDist_i <- rowMin(dist_i)
        
        #4 RKDE of min distances
        #5 Evaluate all distances with kde
        logDistRadDens_vec <- log(
          radialKDE(
            radii=minDist_i
            ,evalPoints=c(dist_i)
            ,pdim=numConVar
            ,returnFun = returnResampler
            ,kernel = "normal"
          )$kdes
        )
        # numObs x numClust
        logDistRadDens_i <- matrix(
          logDistRadDens_vec
          ,nrow=numObs
          ,ncol=numClust
        )
        #print('log Density X cluster')
        #print(head(logDistRadDens_i))
        if (FALSE) { #(verbose & numIter == 1) {
          hist(minDist_i)
          plot(c(dist_i),c(exp(logDistRadDens_i)))
          myXs <- seq(0,10,by=0.01)
          plot(myXs,log(radialKDE(radii=minDist_i,evalPoints=myXs,pdim=numConVar)$kdes),main='Radial Density')
        }
        
        # NEW CODE STARTS HERE
        
        WDistRadDens_vec <- radialKDE(
          radii=minDist_i
          ,evalPoints=c(dist_i)
          ,pdim=numConVar
          ,returnFun = returnResampler
          ,kernel = "wnormal"
        )$kdes
        
        # numObs x numClust
        WDistRadDens_i <- matrix(
          WDistRadDens_vec
          ,nrow=numObs
          ,ncol=numClust
        )
        
        omega_i <- WDistRadDens_i / (exp(logDistRadDens_i) * bw.nrd0(minDist_i) * dist_i**2) +
          (numConVar - 1) / (dist_i**2)
        
        #9 calculate new means: numClust X p_con matrix
        means_i <- t(weights_i * omega_i) %*% as.matrix(conVar) / colSums(weights_i * omega_i)
        
        # END OF NEW CODE
        
        #6 categorical likelihoods
        # this step takes the log probs for each level of each
        # categorical variable (list length Q, elements k X l_q)
        # and creates a list of length Q, each element is n X k
        # giving the log prob for nth observed level for variable q, cluster k
        
        # New Rcpp method
        individualLogProbs <- getIndividualLogProbs(
          catFactorNum = catFactorNumeric
          ,catWeights = catWeights
          ,logProbsCond_i = logProbsCond_i
        )
        
        #7 log likelihood eval for each point, for each of k clusters (WEIGHTED)
        # output is n X k matrix of log likelihoods
        catLogLiks <- Reduce(f='+',x=individualLogProbs)
        
        # sumMatList is cpp function that replaces the above; not clear if faster than Reduce
        #catLogLiks <- sumMatList(individualLogProbs)
        
        # scaling relative con to cat likelihood by weights
        #allLogLiks <- mean(conWeights)*logDistRadDens_i + catLogLiks
        
        # NOT scaling relative to weights
        allLogLiks <- logDistRadDens_i + catLogLiks
        
        # NEW CODE STARTS HERE
        
        # calculate new cluster probabilities
        pis_i <- colSums(weights_i) / numObs
        
        #10.1 new joint probability table, possibly with kernel estimator
        #10.2 Also calculate conditional probabilities: P(lev | clust)
        
        # print(apply(catFactorNumeric, 2, table))
        
        jointProbsList <- replicate(numCatVar, c())
        for (i in 1:numCatVar) {
          jointProbsList[[i]] <- jointProbs(catFactorNumeric[, i], weights_i, numClust)
        }
        logProbsCond_i <- lapply(jointProbsList,FUN=function(xx) log(xx))
        
        weights_old <- weights_new
        
        clustLogLiks <- t(allLogLiks) + log(pis_i)
        weights_new <- t(clustLogLiks) - log(colSums(exp(clustLogLiks)))
        
        # END OF NEW CODE
        
        #8 partition data into clusters
        membOld <- membNew
        
        #membNew <- apply(allLogLiks,1,which.max)
        membNew <- rowMaxInds(allLogLiks)
        
        #9 calculate new means: numClust X p_con matrix
        #means_i <- as.matrix(aggregate(x=conVar,by=list(membNew),FUN=mean)[,-1])
        means_i <- aggregateMeans(
          conVar = as.matrix(conVar)
          ,membNew = membNew
          ,kk = numClust
        )
        
        #10.1 new joint probability table, possibly with kernel estimator
        #10.2 Also calculate conditional probabilities: P(lev | clust)
        
        # New Rcpp implementation: list length numCatVar of numClust X numLev matrices
        jointProbsList <- jointTabSmoothedList(catFactorNumeric,membNew,numLev,catBw,kk=numClust)
        logProbsCond_i <- lapply(jointProbsList,FUN=function(xx) log(xx/rowSums(xx)))
        
        ### Old approach:
        #logProbsCond_i <- lapply(
        #  X = as.list(catFactor)
        # ,FUN = function(xx) {
        #    jointTab <- myCatKern(
        #      kdat=data.frame(clust=membNew,level=xx)
        #     ,bw=catBw
        #     ,tabOnly=TRUE
        #    )
        #    #jointTab <- table(membNew,xx) # without kernel
        #    return( log(jointTab / rowSums(jointTab)) )
        #  }
        #)
        
        # print current plot and metrics, if requested
        if (FALSE) { #(verbose) {
          catLikTmp <- sum(rowMax(catLogLiks))
          winDistTmp <- sum(dist_i[cbind(1:numObs,membNew)])
          winToBetRatTmp <- winDistTmp / (totalDist - winDistTmp)
          if (winToBetRatTmp < 0) winToBetRatTmp <- 100
          objectiveTmp <- winToBetRatTmp * catLikTmp 
          
          plot(conVar[,1],conVar[,2],col=membNew,main=paste('Init',init,'; Iter',numIter) )
          points(means_i,pch=18,col='blue',cex=2)
          legend('topleft',legend=c(
            paste('catLik =',round(catLikTmp ,2))
            ,paste('w/b dist =',round(winDistTmp / (totalDist - winDistTmp),2))
            ,paste('objective =',round(objectiveTmp,2))
          ))
        }
        
        # store every solution if verbose=true
        if (verbose) {
          membLongList[[init]][[numIter]] <- membOld
        }
        
        #11 test for degenerate solution, calculate total log-likelihood (WEIGHTED)
        if(length(unique(membNew)) < numClust) {
          degenerateSoln <- TRUE 
          break
        }
      }
      
      # WHILE ENDS HERE
      
      
      # Store log likelihood for each initialization
      if (degenerateSoln) {
        totalLogLikVect[init] <- -Inf
      } else {
        totalLogLikVect[init] <- sum(rowMax(allLogLiks))
      }
      
      # store num init
      numIterVect[init] <- numIter
      
      # other useful internal measures of cluster quality
      catLogLikVect[init] <- sum(rowMax(catLogLiks))
      winDistVect[init] <- sum(dist_i[cbind(1:numObs, membNew)])
      winToBetRat <- winDistVect[init] / (totalDist - winDistVect[init])
      if (winToBetRat < 0) winToBetRat <- 100
      # Note catLogLik is negative, larger is better
      # Note WSS/BSS is positive, with smaller better
      # Thus, we maximize their product.
      objectiveVect[init] <- winToBetRat * catLogLikVect[init]
      
      # Store current solution if objective beats all others
      if (
        (init == 1)
        | (init > 1 & objectiveVect[init] > max(objectiveVect[1:(init-1)]))
      ) {
        finalLogLik <- totalLogLikVect[init]
        finalObj <- objectiveVect[init]
        finalMemb <- membNew
        finalCenters <- matrix(
          data=as.matrix(means_i)
          ,nrow=nrow(means_i)
          ,ncol=numConVar
          ,dimnames=list(
            cluster=paste('Clust',1:nrow(means_i))
            ,variableMean=paste('Mean',1:numConVar))
        )
        # rescale by weights
        #    finalCenters <- finalCenters * matrix(rep(1/conWeights,times=numClust),byrow=TRUE,nrow=numClust)
        finalProbs <- lapply(logProbsCond_i,exp)
        names(finalProbs) <- paste('Categorical Variable',1:numCatVar)
        finalClustSize <- table(membNew)
      }
      
      # store last membership also
      if (verbose) membLongList[[init]][[numIter+1]] <- membNew
      
    }
    
    
    #12 Prepare output data structure
    if (verbose) {
      optionalOutput <- list(
        totalLogLikVect =totalLogLikVect
        ,catLogLikVect = catLogLiks
        ,winDistVect = winDistVect
        ,totalDist = totalDist
        ,objectiveVect = objectiveVect
        ,membLongList = membLongList
      )
    } else {
      optionalOutput <- list()
    }
    
    inputList <- list(
      conVar = conVar
      ,catFactor = catFactor
      ,numClust = numClust
      ,numInit = numInit
      ,conWeights = conWeights
      ,catWeights = catWeights
      ,maxIter = maxIter
      ,conInitMethod = conInitMethod
      ,catBw = catBw
      ,verbose = verbose
    )
    
    
    return(
      list(
        finalMemb=as.numeric(finalMemb)
        ,numIter=numIterVect
        ,finalLogLik=finalLogLik
        ,finalObj=finalObj
        ,finalCenters=finalCenters
        ,finalProbs=finalProbs
        ,input=inputList
        ,verbose=optionalOutput
        ,nClust=list()
      )
    )
  } else if (calcNumClust == 'ps') {
    # Recursive call to kamila implementing prediction strength method.

    # Test that numClust is an integer vector.
    allEqInteger <- all(numClust == as.integer(numClust))
    if ( is.na(allEqInteger) ||
         !allEqInteger ||
         !all(sapply(numClust, is.numeric)) ||
         length(numClust) != length(unique(numClust)) ) {
      stop('Input parameter numClust must be a vector of unique integers
         if input parameter calcNumClust == "ps"')
    }
    if (length(numClust) == 1) {
      warning('Input parameter numClust is a scalar; the prediction strength 
        method is probably not appropriate or desired')
    }
    if (any(numClust > floor(nrow(conVar)/2))) {
      stop('The number of clusters in input parameter numClust cannot exceed
        one-half of the sample size.')
    }

    # Test that predStrThresh is within (0,1).
    if ( length(predStrThresh) != 1 ||
         is.na(predStrThresh) ||
         predStrThresh <= 0 ||
         predStrThresh >= 1 ) {
      stop('Input parameter predStrThresh must be scalar in (0,1)')
    }

    # Test that numPredStrCvRun is a valid number of cv runs.
    if ( length(numPredStrCvRun) != 1 ||
         is.na(numPredStrCvRun) || 
         numPredStrCvRun != as.integer(numPredStrCvRun) ||
         numPredStrCvRun < 1 ) {
      stop('Input parameter numPredStrCvRun must be a positive integer.')
    }

    psCvRes <- matrix(
      NaN,
      nrow = length(numClust),
      ncol = numPredStrCvRun,
      dimnames = list(
        nClust = numClust,
        CVRun = paste('Run', 1:numPredStrCvRun)
      )
    )
    
    # Implement CV procedure
    numObs <- nrow(conVar)
    numInTest <- floor(numObs/2)
    for (cvRun in 1:numPredStrCvRun) {
      for (ithNcInd in 1:length(numClust)) {
        # generate cv indices
        testInd <- sample(numObs, size=numInTest, replace=FALSE)

        # cluster test data
        testClust <- kamila(
           conVar = conVar[testInd,,drop=FALSE],
           catFactor = catFactor[testInd,,drop=FALSE],
           numClust = numClust[ithNcInd],
           numInit = numInit,
           conWeights = conWeights,
           catWeights = catWeights,
           maxIter = maxIter,
           conInitMethod = conInitMethod,
           catBw = catBw,
           verbose = FALSE
        )
        
        # cluster training data
        trainClust <- kamila(
          conVar = conVar[-testInd,,drop=FALSE],
          catFactor = catFactor[-testInd,,drop=FALSE],
          numClust = numClust[ithNcInd],
          numInit = numInit,
          conWeights = conWeights,
          catWeights = catWeights,
          maxIter = maxIter,
          conInitMethod = conInitMethod,
          catBw = catBw,
          verbose = FALSE
        )
        
        # Generate a list of indices for each cluster within test data.
        # Note there are two index systems floating around:
	# Indices of the full data set slicing out test set (testInd)
        # and indices of cluster membership within test data (testIndList).
	# If the length of the outputs are equal, mapply defaults to returning
	# a matrix unless default of SIMPLIFY parameter is changed to FALSE.
        testIndList <- mapply(
          x = 1:numClust[ithNcInd],
          function(x) which(testClust$finalMemb == x),
	  SIMPLIFY = FALSE
        )
        
        # Allocate test data based on training clusters.
        teIntoTr <- classifyKamila(
          trainClust,
          list(conVar[testInd,,drop=FALSE],catFactor[testInd,,drop=FALSE])
        )
        
        # Initialize D matrix.
        dMat <- matrix(
          NaN,
          nrow = numInTest,
          ncol = numInTest
        )
        
        # Calculate D matrix.
        # replace with RCpp
        for (i in 1:(numInTest-1)) {
          for (j in (i+1):numInTest) {
            dMat[i,j] <- teIntoTr[i] == teIntoTr[j]
          }
        }
        
        # Initialize proportions to zero.
        psProps <- rep(0,numClust[ithNcInd])

        # Calculate prediction strength proportions.
        # replace with RCpp
        for (cl in 1:numClust[ithNcInd]) {
          clustN <- length(testIndList[[cl]])
	  if (clustN > 1) {
##################################
# Construct dMat separately within each cluster
# initialize dMat_cl
# dMat_cl[i,j] <- teIntoTr[testIndList[[cl]][i]] == teIntoTr[testIndList[[cl]][j]]
##################################
            for (i in 1:(clustN-1)) {
              for (j in (i+1):clustN) {
                psProps[cl] <- psProps[cl] + dMat[testIndList[[cl]][i], testIndList[[cl]][j]]
              }
            }
	  }
	  if (clustN < 2) {
	    # if cluster size is 1 or zero, not applicable
	    psProps[cl] <- NA
	  } else {
            # * 2 since only upper triangle of dMat is used.
            psProps[cl] <- psProps[cl] / (clustN*(clustN-1)) * 2
	  }
        }

        # Calculate and update prediction strength results.
	# Remove NaN/NAs for empty clusters.
	# min(...,na.rm=T) returns Inf if entire vector is NA.
	# Workaround is simple but WEIRD behavior.
        psCvRes[ithNcInd, cvRun] <- ifelse(
	  test = all(is.na(psProps)),
	  yes = NA,
	  no = min(psProps,na.rm=TRUE)
	)

      } # end clusters
    } # end cv runs

    # Calculate CV estimate of prediction strength for each cluster size.
    avgPredStr    <- apply(psCvRes,1,mean,na.rm=TRUE)
    stdErrPredStr <- apply(psCvRes,1,sd,na.rm=TRUE) / sqrt(numPredStrCvRun)

    # Calculate final number of clusters: largest # clust such that avg+sd
    # score is above the threshold.
    psValues <- avgPredStr + stdErrPredStr
    clustAboveThresh <- psValues > predStrThresh

    if (all(!clustAboveThresh)) {
      warning('No cluster size is above prediction strength threshold.
        Consider lowering the ps threshold; returning the cluster size
	corresponding to the highest ps value.')
      kfinal <- numClust[which.max(psValues)]
    } else {
      kfinal <- max(numClust[clustAboveThresh])
    }

    # Do the final clustering.
    outRes <- kamila(
      conVar = conVar,
      catFactor = catFactor,
      numClust=kfinal,
      numInit = numInit,
      conWeights = conWeights,
      catWeights = catWeights,
      maxIter = maxIter,
      conInitMethod = conInitMethod,
      catBw = catBw,
      verbose = FALSE
    )
    outRes$nClust <- list(
      bestNClust = kfinal,
      psValues = setNames(psValues, numClust),
      avgPredStr = avgPredStr,
      stdErrPredStr = stdErrPredStr,
      psCvRes = psCvRes
    )

    return(outRes)

  } else {
    stop('Currently calcNumClust must be either "none" or "ps"')
  }
}