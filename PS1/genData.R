if (!require("mvtnorm")) install.packages("mvtnorm"); library(mvtnorm)
if (!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)


# Generating a set of data where each category has two focii, so the correct
# separation would be a cross (for two categories)
# Can specify the mean, sigma and number of categories. Only 2D allowed
genData <- function(n = 1, seed = NA, muXY = c(0.5,0.5), sigmaXY = 0.1*diag(2), cat = 2,
                    csv = TRUE, pdf = TRUE){
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