if (!require("mvtnorm")) install.packages("mvtnorm"); library(mvtnorm)
if (!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)

# Creating sigma
sigmaXY <- function(rho, sdX, sdY) {
  covTerm <- rho * sdX * sdY
  VCmatrix <- matrix(c(sdX^2, covTerm, covTerm, sdY^2),
                     2, 2, byrow = TRUE)
  return(VCmatrix)
}

# Generating values
genBVN <- function(n = 1, seed = NA, muXY=c(0,1), sigmaXY=diag(2)) {
  if(!is.na(seed)) set.seed(seed)
  rdraws <- rmvnorm(n, mean = muXY, sigma = sigmaXY)
  return(rdraws)
}

# Full data generation
loanData <- function(noApproved, noDenied, noUndecided, 
                     muApproved, muDenied, muUndecided, 
                     sdApproved, sdDenied, sdUndecided,
                     rhoApproved, rhoDenied, rhoUndecided,
                     seed=1111) {
  sigmaApproved <- sigmaXY(rho=rhoApproved, sdX=sdApproved[1], sdY=sdApproved[2])
  sigmaDenied <- sigmaXY(rho=rhoDenied, sdX=sdDenied[1], sdY=sdDenied[2])
  sigmaUndecided <- sigmaXY(rho=rhoUndecided, sdX=sdUndecided[1], sdY=sdUndecided[2])
  
  approved <- genBVN(noApproved, muApproved, sigmaApproved, seed = seed)
  denied <- genBVN(noDenied, muDenied, sigmaDenied, seed = seed+1)
  undecided <- genBVN(noUndecided, muUndecided, sigmaUndecided, seed = seed+2)
  
  loanDf <- as.data.frame(rbind(approved,denied,undecided))
  deny <- c(rep("Approved", noApproved), rep("Denied", noDenied), rep("Undecided", noUndecided))
  target1 <- c(rep(1, noApproved), rep(0, noDenied), rep(0, noUndecided))
  target2 <- c(rep(0, noApproved), rep(1, noDenied), rep(0, noUndecided))
  target3 <- c(rep(0, noApproved), rep(0, noDenied), rep(1, noUndecided))
  loanDf <- data.frame(loanDf, deny, target1, target2, target3)
  colnames(loanDf) <- c("PIratio", "solvency", "deny", "target1", "target2", "target3")
  return(loanDf)
}

# Creating the data set - three clusters
loanDf <- loanData(noApproved=50, noDenied=50, noUndecided = 50, c(4,150), c(12,100), c(5,75),
                   c(1,20), c(2,30), c(1,20), -0.1, 0.6, 0.3, 1221)

X <- as.matrix(cbind(ind=rep(1, nrow(loanDf)),
                     loanDf[,c("PIratio", "solvency")]))
Y <- as.matrix(loanDf[,c("target1","target2","target3")])

coeffsOpt <- solve(t(X)%*%X) %*% t(X) %*% Y # Getting separate coefs for each category

predictions <- X %*% coeffsOpt

predictionMatrix <- (predictions==apply(predictions, 1, max)) # so have a true / false for each

predictedLabel <- ifelse(predictionMatrix[,1], "Approved",
                         ifelse(predictionMatrix[,2],"Denied","Undecided"))

loanDf <- cbind(loanDf,predictions,predictedLabel)

write.table(loanDf,"predictions.csv",sep=",", row.names = FALSE)


# Now getting the boundary lines
x <- seq(min(loanDf$solvency), max(loanDf$solvency))
adBL <- (coeffsOpt[1,1]-coeffsOpt[1,2])/(coeffsOpt[2,2]-coeffsOpt[2,1]) + x*(coeffsOpt[3,1]-coeffsOpt[3,2])/(coeffsOpt[2,2]-coeffsOpt[2,1])
auBL <- (coeffsOpt[1,1]-coeffsOpt[1,3])/(coeffsOpt[2,3]-coeffsOpt[2,1]) + x*(coeffsOpt[3,1]-coeffsOpt[3,3])/(coeffsOpt[2,3]-coeffsOpt[2,1])
duBL <- (coeffsOpt[1,2]-coeffsOpt[1,3])/(coeffsOpt[2,3]-coeffsOpt[2,2]) + x*(coeffsOpt[3,2]-coeffsOpt[3,3])/(coeffsOpt[2,3]-coeffsOpt[2,2])

# Getting the bounds into data frames
bound1 <- data.frame(solvency = x, PIratio = adBL, deny = rep("App/Deny", length(x)))
bound2 <- data.frame(solvency = x, PIratio = auBL, deny = rep("App/Undec", length(x))) # this line seems reasonable
bound3 <- data.frame(solvency = x, PIratio = duBL, deny = rep("Den/Undec", length(x)))

pdf("discFunction3C.pdf")
print(ggplot() +
        geom_point(data = loanDf,
                   aes(x = solvency, y = PIratio, colour = deny, fill = deny)) +
        xlab("solvency") +
        ylab("PI ratio") +
        ylim(0,18) +
        geom_line(data = bound1, aes(x = solvency, y = PIratio, colour = deny)) +
        geom_line(data = bound2, aes(x = solvency, y = PIratio, colour = deny)) +
        geom_line(data = bound3, aes(x = solvency, y = PIratio, colour = deny)) +
        theme_bw() +
        scale_color_manual("deny",
                           values = c("App/Deny" = "black", "App/Undec" = "yellow", 
                                      "Den/Undec" = "grey",
                                      "Approved" = "green", "Denied" = "red",
                                      "Undecided" = "blue"))
)
dev.off()
