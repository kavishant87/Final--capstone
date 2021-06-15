suppressPackageStartupMessages(c(
    library(shinythemes),
    library(shiny),
    library(tm),
    library(stringr),
    library(markdown),
    library(stylo)))

source("C:/Users/kavis/Documents/Kavi files/Git-R Files/datasciencecoursera/FinalProject_SwiftKey/Try/inputCleaner.R")
final4Data <- readRDS(file="C:/Users/kavis/Documents/Kavi files/Git-R Files/datasciencecoursera/FinalProject_SwiftKey/Try/quadgram.RData")
final3Data <- readRDS(file="C:/Users/kavis/Documents/Kavi files/Git-R Files/datasciencecoursera/FinalProject_SwiftKey/Try/trigram.RData")
final2Data <- readRDS(file="C:/Users/kavis/Documents/Kavi files/Git-R Files/datasciencecoursera/FinalProject_SwiftKey/Try/bigram.RData")


shinyServer(function(input, output) {
    
    wordPrediction <- reactive({
        text <- input$text
        textInput <- cleanInput(text)
        wordCount <- length(textInput)
        wordPrediction <- nextWordPrediction(wordCount,textInput)})
    
    output$predictedWord <- renderPrint(wordPrediction())
    output$enteredWords <- renderText({ input$text }, quoted = FALSE)
})