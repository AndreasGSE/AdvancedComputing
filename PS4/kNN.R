# Features data must NOT include the labels, otherwise no constraint
# Test data should also just follow the same format as the features data

kNN <- function(features, labels, k, p, test = NA){
  # Function to return indexes of k near neighbours
  dist <- function(x, y, k, p){
    # Finding distance to points
    if(p %in% c(1,2)){
      d <- colSums((abs(x - t(y))^p)^(1/p))
    } else if(p == Inf){
      d <- apply(abs(x - y), 1, max)
    }
    
    points <- which(d %in% sort.int(d, method = "quick")[2:(k+1)]) # getting indexes
    return(points)
  }
  
  # Predict the label of a single point - x is the vector of nn indexes
  pred <- function(x, labels){
    tabed <- sort(table(labels[x]), decreasing = TRUE) # sorts tabulated data
    lab <- names(tabed)[1] # get the label with most occurances 
    probs <- tabed[1] / sum(tabed) # get the proportion / probability
    
    res <- list(lab = lab, probs = probs)
  }
  
  if(is.na(test)) test <- features # This is done to over-ride lack of test data
  
  # getting indexes of NNs
  if(k == 1){
    nnMat <- as.matrix(apply(test, 1, dist, features, k, p))
    
  } else {
    nnMat <- t(apply(test, 1, dist, features, k, p)) 
    
  }
  
  predictions <- unlist(apply(nnMat, 1, pred, labels)) # getting the labels and probs
  m <- length(predictions)
  predictionF <- data.frame(predLabels = predictions[seq(1,m,2)], 
                            prob = predictions[seq(2,m,2)])
  
  return(predictionF)
}
