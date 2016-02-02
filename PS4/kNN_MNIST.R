train <- read.csv("MNIST_training.csv")
test <- read.csv("MNIST_test.csv")

if (!require("FNN")) install.packages("FNN"); library(FNN)

# Getting some cross validation function
Xval <- function(train, k, n=3000, seed = 123){
  set.seed(seed)
  subs <- sample(c(1:nrow(train)), n)
  subTest <- train[-subs,]
  subTrain <- train[subs,]
  
  cl <- as.factor(subTrain[,1])
  
  preds <- knn(subTrain[,-1], subTest[,-1], cl, k, prob = TRUE)[1:nrow(subTest)]
  
  acc <- mean(ifelse(preds == subTest[,1],1,0))
  
  return(acc)
}

# Optimising
acc <- rep(0,5)
for(i in 1:5){
  acc[i] <- Xval(train, k = i)
}
acc # Seems that 1nn is the best, for various training sets

# Now predicting with optimised lables
cl <- as.factor(train[,1])
predictions <- knn(train[,-1], test, cl, 1, prob = TRUE)


write.csv(as.integer(predictions[1:nrow(test)]),"MNIST_predictions.csv", row.names = FALSE,
          col.names = NA)