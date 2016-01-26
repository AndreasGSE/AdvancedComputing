# Creating the UI, allowing input for all elements
shinyUI(pageWithSidebar(
  
  headerPanel('Loan Data Classification'),
  
  sidebarPanel(
    
    numericInput('piMA', 'PI mean approved', 4),
    
    numericInput('solvMA', 'Solv mean approved', 150),
    
    numericInput('piMD', 'PI mean denied', 10),
    
    numericInput('solvMD', 'Solv mean denied', 100),
    
    numericInput('piSDA', 'PI SD approved', 1),
    
    numericInput('solvSDA', 'Solv SD approved', 20),
    
    numericInput('piSDD', 'PI SD denied', 2),
    
    numericInput('solvSDD', 'Solv SD denied', 30)
    
  ),
  
  # Displaying the graph and table
  mainPanel(
    h4('Separation of categories'),
    plotOutput('plot1'),
    
    h4('Confusion Matrix'),
    tableOutput('confusionmatrix')
  )
))