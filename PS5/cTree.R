cTree <- function(formula, data, test, depth, minPoints = 1, costFnc = c("Entropy")){
  library(assertthat)
  library(dplyr)
  # Checking inputs
  assert_that(costFnc %in% c("Entropy", "Gini", "ME"))
  assert_that(!is.null(dim(data)))
  if(all.vars(formula)[2] != "."){
    assert_that(mean(all.vars(formula) %in% colnames(data)) == 1) # we have all the columns
  }
  
  
  target <- data[, all.vars(formula)[1]] # extracting the target
  if(all.vars(formula)[2] != "."){
    dependents <- which(colnames(data) %in% all.vars(formula)[-1]) # getting indices
    dependents.test <- which(colnames(test) %in% all.vars(formula)[-1])
  } else {
    dependents <- which(colnames(data) != all.vars(formula)[1]) # because Dom complained
    dependents.test <- which(colnames(test) != all.vars(formula)[1])
  }
  
  
  # Defining an error function that takes a vectot of probabilities
  getError <- function(prob, costFnc){
    if(costFnc == "Entropy") e <- -1 * sum(prob * log(prob))
    if(costFnc == "Gini") e <- sum(prob * (1 - prob))
    if(costFnc == "ME") e <- 1 - max(prob)
    
    return(e)
  }
  
  # Function to find best threshold
  findThreshold <- function(x, y, minPoints){
    # Want to work with the UNIQUE numerical representations to save time
    vals <- unique(as.numeric(x))
    errors <- sapply(vals, function(val){
      # Seeing the most popular classifications
      popL <- sort(table(y[x < val]), dec = T)
      popR <- sort(table(y[x >= val]), dec = T)
      
      if(sum(popL) < minPoints | sum(popR) < minPoints){
        err <- Inf # preventing a split
        return(err)
      } 
      
      probL <- popL / length(y[x < val]) # getting probabilities
      probR <- popR / length(y[x >= val])
      
      err <- getError(c(probL[1], probR[1]), costFnc) # getting error
      
      return(err)
    })
    
    thresh <- vals[which.min(errors)] # getting the minimum error 
    thresh <- sample(thresh, 1)
    
    probL <- sort(table(y[x < thresh]), dec = T) / length(y[x < thresh]) # now getting the preds
    probR <- sort(table(y[x >= thresh]), dec = T) / length(y[x >= thresh])
    
    splitPredictions <- c(names(probL[1]), names(probR[1])) %>% as.factor # splitting
    
    return(list(thresh = thresh, err = min(errors), split = splitPredictions))
  }
  
  # Here we actually get the tree and define the recursion
  getTree <- function(trainData, testData, indLR, indLR.test, target, nodes, minPoints, depth){
    # At start so that we count the number of splits
    nodes <- nodes + 1
    
    results <- as.data.frame(t(sapply(trainData, function(x){
      findThreshold(x[indLR], target[indLR], minPoints)
    })))
    
    err.ind <- which.min(unlist(results[,2])) # return index of lowest error - also gets column
    best.thresh <- results[err.ind, 1] # get threshold
    

    # Split the data using true / false labels - will be fed back in to keep track of splits
    indL <- ifelse(trainData[, err.ind] >= best.thresh | !indLR, FALSE, TRUE)
    indR <- ifelse(trainData[, err.ind] < best.thresh | !indLR, FALSE, TRUE)
    
    # Split the test in the SAME WAY as the train
    indL.test <- ifelse(testData[, err.ind] >= best.thresh | !indLR.test, FALSE, TRUE)
    indR.test <- ifelse(testData[, err.ind] < best.thresh | !indLR.test, FALSE, TRUE)
    
    # Do not split below the min number of points on the training set
    numPointsL <- sum(indL)
    numPointsR <- sum(indR)
    
    if(numPointsL > minPoints & numPointsR > minPoints & nodes != depth){
      splitL <- getTree(trainData, testData, indL, indL.test, target, nodes, minPoints, depth) 
      splitR <- getTree(trainData, testData, indR, indR.test, target, nodes, minPoints, depth)
      
      pointsList <- c(splitL$pointsList, splitR$pointsList)
      labelList <- c(splitL$labelList, splitR$labelList)
      probList <- c(splitL$probList, splitR$probList)
      
      # On the function call - we should be making "datalist" continuously smaller
      
      
      
      # Can just call on a dataframe of the original data - but having the TRUE / FALSE
      
    } else if(numPointsL <= minPoints & numPointsR > minPoints & nodes != depth){
      splitR <- getTree(trainData, testData, indR, indR.test, target, nodes, minPoints, depth)
      
      prob <- sort(table(target[indL]), dec = T) / length(target[indL])
      predLabs <- names(prob)[1]
      splitL <- list(pointsList = indL.test, labelList = predLabs, probList = prob[1])
      
      pointsList <- c(splitL$pointsList, splitR$pointsList)
      labelList <- c(splitL$labelList, splitR$labelList)
      probList <- c(splitL$probList, splitR$probList)
      
      
    } else if(numPointsL > minPoints & numPointsR <= minPoints & nodes != depth){
      splitL <- getTree(trainData, testData, indL, indL.test, target, nodes, minPoints, depth)
      
      prob <- sort(table(target[indR]), dec = T) / length(target[indR])
      predLabs <- names(prob)[1]
      splitR <- list(pointsList = indR.test, labelList = predLabs, probList = prob[1])
      
      pointsList <- c(splitL$pointsList, splitR$pointsList)
      labelList <- c(splitL$labelList, splitR$labelList)
      probList <- c(splitL$probList, splitR$probList)
      
    } else {
      prob <- sort(table(target[indL]), dec = T) / length(target[indL])
      predLabs <- names(prob)[1]
      splitL <- list(pointsList = indL.test, labelList = predLabs, probList = prob[1])
      
      prob <- sort(table(target[indR]), dec = T) / length(target[indR])
      predLabs <- names(prob)[1]
      splitR <- list(pointsList = indR.test, labelList = predLabs, probList = prob[1])
      
      pointsList <- c(splitL$pointsList, splitR$pointsList)
      labelList <- c(splitL$labelList, splitR$labelList)
      probList <- c(splitL$probList, splitR$probList)
    }
    
    return(list(pointsList = pointsList, labelList = labelList, probList = probList))
  
    
  }
  
  n <- nrow(test)
  
  indLR <- rep(TRUE, nrow(data))
  indLR.test <- rep(TRUE, n)
  
  tree <- getTree(data[,dependents], test[, dependents.test], indLR, indLR.test, target,
                  0, minPoints, depth)
  
  m <- length(tree$pointsList)
  
  splitVec <- as.vector(sapply(1:(m/n), rep, n))
  splitMat <- as.matrix(split(tree$pointsList, splitVec))
  splitNice <- t(sapply(splitMat, function(x){
    which(unlist(x) == max(unlist(x)))
  }))
  
  predLabs <- rep(0, nrow(test))
  probs <- rep(0, nrow(test))
  for(i in 1:length(tree$labelList)){
    predLabs[splitMat[[i]]] <- tree$labelList[i]
    probs[splitMat[[i]]] <- tree$probList[i]
  }
  
  return(list(predLabels = predLabs, prob = probs))
}
