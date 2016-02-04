library(ggplot2)
library(mvtnorm)
#install.packages("shiny")
#install.packages('devtools')
#devtools::install_github('rstudio/shinyapps')
#devtools::install_github('rstudio/rsconnect')

# Sigma function
sigmaXY <- function(rho, sdX, sdY) {
  covTerm <- rho * sdX * sdY
  VCmatrix <- matrix(c(sdX^2, covTerm, covTerm, sdY^2),
                     2, 2, byrow = TRUE)
  return(VCmatrix)
}

# Generating data function
genBVN <- function(n = 1, seed = NA, muXY=c(0,1), sigmaXY=diag(2)) {
  if(!is.na(seed)) set.seed(seed)
  rdraws <- rmvnorm(n, mean = muXY, sigma = sigmaXY)
  return(rdraws)
}

# Getting the data
loanData <- function(noApproved, noDenied, muApproved, muDenied, sdApproved,
                     sdDenied, rhoApproved, rhoDenied, seed=1111) {
  sigmaApproved <- sigmaXY(rho=rhoApproved, sdX=sdApproved[1], sdY=sdApproved[2])
  sigmaDenied <- sigmaXY(rho=rhoDenied, sdX=sdDenied[1], sdY=sdDenied[2])
  approved <- genBVN(noApproved, muApproved, sigmaApproved, seed = seed)
  denied <- genBVN(noDenied, muDenied, sigmaDenied, seed = seed+1)
  loanDf <- as.data.frame(rbind(approved,denied))
  deny <- c(rep("Approved", noApproved), rep("Denied", noDenied))
  target = c(rep(0, noApproved), rep(1, noDenied))
  loanDf <- data.frame(loanDf, deny, target)
  colnames(loanDf) <- c("PIratio", "solvency", "deny", "target")
  return(loanDf)
}

# Creating the decision boundary
decBound <- function(data){
  datafit <- lm(target ~ solvency + PIratio + 1, data = data)
  coefs <- coef(datafit)
  
  x <- seq(min(data["PIratio"]), max(data["PIratio"]), # generating X and Y points
           length.out = nrow(data))
  y <- -(coefs["PIratio"]/coefs["solvency"])*x +
    (0.5-coefs[1])/coefs["solvency"]
  
  predictedLabels <- ifelse(predict(datafit) < 0.5, "Approved", "Denied") # predicting labels
  
  boundFrame <- data.frame(PIratio = x, solvency = y, 
                           deny = rep("Boundary", length(x)))
  
  l <- list(bound = boundFrame, predLab = predictedLabels)
  
  return(l)
}

# Displaying the output
shinyServer(function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    loanData(50, 50, c(input$piMA, input$solvMA), c(input$piMD, input$solvMD),
                     c(input$piSDA, input$solvSDA), c(input$piSDD, input$solvSDD),
             -0.1, 0.6) # Feeding the "loan data" as our input
  })
  
  # Calling the bound and predicted labels
  decisionBound <- reactive({
    decBound(selectedData())$bound
  })
  
  predictedLabels <- reactive({
    decBound(selectedData())$predLab
  })
  
  output$plot1 <- renderPlot({
    ggplot(data = selectedData(),
           aes(x = solvency, y = PIratio, colour=deny)) +
      geom_point() +
      xlab("solvency") +
      ylab("PI ratio") +
      theme_bw() +
      geom_line(data=decisionBound()) +
      scale_color_manual("",
                         values = c("Boundary" = "grey",
                                    "Approved" = "blue", "Denied" = "red"))
    
  })
  
  output$confusionmatrix <- renderTable({
    table(selectedData()$deny, predictedLabels())
  })
  
})

# We always want to plot the same thing, just changes the parameters
# I think what could do is have a list of parameters that feeds into 
# the function - then graph that