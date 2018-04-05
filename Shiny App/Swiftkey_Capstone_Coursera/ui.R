
library(shiny)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Word Prediction Model"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
   sidebarPanel(
            textInput("textinput1", "Enter Input here", value = "", width = NULL, placeholder = NULL)
  ),
    
    # Show a plot of the generated distribution
    mainPanel(
        h5("Predicted words"),
        textOutput("textoutput1")
    )
  )
))
