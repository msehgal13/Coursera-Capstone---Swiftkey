
library(shiny)
load("swiftkeydata.RData",envir = environment())
source("SwiftKeyPrediction.R")
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$textoutput1 <- reactive({
          prediction(input$textinput1,prof_data,en_US_Sample_Unigram_count,en_US_Sample_Bigram_count,en_US_Sample_Trigram_count)
  })
})
