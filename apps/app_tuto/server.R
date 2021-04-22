library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(ggplot2)

shinyServer(function(input, output) {
  
  output$text <- renderText({
    input$title
  })

  output$plot <- renderPlot({
    x <- mtcars[, input$x]
    y <- mtcars[, input$y]
    plot(x, y, pch=16)
  })
})
