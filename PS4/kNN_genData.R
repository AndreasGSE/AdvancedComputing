# Generating problem data
genData <- function(n = 1, seed = NA, muXY = c(0.5,0.5), sigmaXY = 0.1*diag(2), cat = 2,
                    csv = TRUE, pdf = TRUE){
  
  if (!require("mvtnorm")) install.packages("mvtnorm"); library(mvtnorm)
  if (!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)
  
  if(!is.na(seed)) set.seed(seed)
  probData <- matrix(0,n,2*cat) # Creating a blank matrix for the data
  
  for(i in 1:cat){
    angle <- (i-1)*(pi/2)/(cat-1) # Rotating the mean around for new data sets
    muXY <- as.vector(matrix(c(cos(angle),-sin(angle),sin(angle),cos(angle)),2,2) %*% as.matrix(muXY))
    sign <- ifelse(i %% 2 == 1,1,-1)
    modMU <- c(-muXY[1],-muXY[2])
    
    id <- 1 + (i-1)*2
    
    coords1 <- rmvnorm(n/2, mean = muXY, sigma = sigmaXY)
    coords2 <- rmvnorm(n/2, mean = modMU, sigma = sigmaXY)
    probData[1:(n/2),id] <- coords1[,1]; probData[1:(n/2),(id+1)] <- coords1[,2]
    probData[(n/2 + 1):n,id] <- coords2[,1]; probData[(n/2 + 1):n,(id+1)] <- coords2[,2]
  }
  
  cats <- as.vector(sapply(1:cat,function(i){
    rep((i-1),n)
  }))
  probDataF <- data.frame(x = as.vector(probData[,seq(1,2*cat,2)]), 
                          y = as.vector(probData[,seq(2,2*cat,2)]), cat = cats)
  
  if(csv){
    write.table(probDataF,"dataset.csv",sep=",", row.names = FALSE)
  }
  if(pdf){
    pdf("dataPlot.pdf")
    print(ggplot(data = probDataF,
                 aes(x = x, y = y, colour = cat)) +
            geom_point() +
            xlab("x value") +
            ylab("y value") +
            theme_bw() +
            scale_colour_gradient(low="red"))
    dev.off()
  }
  return(probDataF)
}

# kNN function, same as before
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

# Generating 1000 data points
loanDF <- genData(n = 1000, seed = 100, pdf = FALSE, csv = FALSE)

# Getting the classified points
kNNdata <- kNN(loanDF[,c(1,2)], labels = loanDF$cat, k = 1, p = 1)

# Getting a dataframe with all details
loanPreds <- data.frame(x = loanDF$x, y = loanDF$y, cat = loanDF$cat, 
                        predLabels = as.vector(kNNdata$predLabels), prob = kNNdata$prob)

# Saving CSV
write.csv(loanPreds, "predictions.csv", row.names = FALSE)

# Generating the grid to be used in the contour
x1 <- seq(-1.7, 1.7, length.out = 100)
x2 <- x1
grid <- expand.grid(x = x1, y = x2)

# Have to predict labels on the grid
grid$predLabels <- kNN(loanDF[,c(1,2)], test = grid, loanDF$cat, 10, 1)[,1]

grid$predLabels <- as.integer(grid$predLabels)

# Plotting contour
pdf("plot.pdf")
print(ggplot(data = loanPreds, aes(x = x, y = y, colour = as.factor(cat), z = predLabels)) +
        geom_point() +
        xlab("x value") +
        ylab("y value") +
        theme_bw() +
        stat_contour(data = grid, binwidth = 1, colour = "black", 
                     aes(x = x, y = y, z = predLabels)))
dev.off()

  