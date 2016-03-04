adaBoost <- function(formula, data, test, depth = 1, noTrees = 10){
  library(assertthat)
  library(rpart)
  
  vars <- all.vars(formula)
  
  # Checking inputs
  assert_that(!is.null(dim(data)))
  
  target <- data[, vars[1]] # extracting the target
  
  if(min(target) != -1) target <- ifelse(target == 0, -1, 1)
  
  assert_that(mean(unique(target) %in% c(-1, 1)) == 1) # Checking if coded as 1, -1
  
  # fixing up formula
  if(vars[2] == "."){
    features <- names(data)[which(names(data) != vars[1])]
    formula <- as.formula(paste0(vars[1],"~", paste(sprintf("`%s`", features), collapse = "+")))
  }
  
  # Boosting
  weightsMat <- matrix(0, noTrees, length(target))
  alpha <- rep(0, noTrees)
  testLabs <- matrix(3, noTrees, nrow(test)) # to store each model
  
  data[, vars[1]] <- as.factor(data[, vars[1]])
  
  for(i in 1:noTrees){
    print(i)
    if(i == 1){
      w <- rep(1, length(target))
    } else{
      w <- weightsMat[(i - 1),] # getting the weights for the training run
    }
    
    data$w <- w
    
    model <- rpart(formula, data, weights = w, 
                   control = rpart.control(maxdepth = depth))
    
    predLabs <- predict(model, data, type = "class") # getting our labels
    testLabs[i,] <- predict(model, test, type = "class")
    
    misClass <- ifelse(predLabs != target, 1, 0) # Vector of incorrect
    err <- sum(w * misClass) / sum(w) 
    
    alpha[i] <- log((1 - err) / err)
    weightsMat[i, ] <- w * exp(alpha[i] * misClass) # assigning weights for next run
  }
  
  # have to sum over cols as those are the different models
  finalClass <- sign(colSums(alpha * testLabs))
  
  return(list(predLabs = finalClass))
}

# read data
spamData <- read.table("spambase.data", sep = ",")
names <- read.table("spambase.names", comment.char = "|", skip = 4, sep = ":")
names <- row.names(names)
names(spamData) <- c(names, "spam")

form <- as.formula("spam~.")

# training and test split
samp <- sample(1:nrow(spamData), 4000)

trainSpam <- spamData[samp,]
testSpam <- spamData[-samp,]

# predictions
library(gbm)

testTarget <- ifelse(testSpam$spam == 0, -1, 1)

treeList <- c(30, 50, 70, 90, 100)
accMine <- rep(0, 5)
accGBM <- rep(0, 5)
for(i in 1:5){
  prediction <- adaBoost(form, trainSpam, testSpam, noTrees = treeList[i])
  accMine[i] <- mean(ifelse(prediction[[1]] == testTarget, 1, 0))
  
  boost <- gbm(form, distribution = "adaboost", data = trainSpam, n.trees = treeList[i])
  accGBM[i] <- mean((predict(boost, testSpam, n.trees = treeList[i]) > 0) != testSpam$spam)
}

error <- data.frame(Iterations = treeList, myError = (1-accMine), packageError = (1-accGBM))

library(ggplot2)

pdf("adaBoost.pdf")
print(ggplot(data = error, aes(x = Iterations, y = myError, colour = "Mine")) +
        geom_point(size = 2) +
        geom_point(aes(x = Iterations, y = packageError, colour = "gbm")) +
        scale_colour_manual(values = c("Mine" = "red", "gbm" = "blue"), name = "Function") +
        theme_bw() +
        ylab("Error"))
dev.off()