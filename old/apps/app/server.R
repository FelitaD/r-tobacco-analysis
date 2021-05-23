library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyverse)
library(xts)


shinyServer(function(input, output) {
  
  output$title <- renderText({
    input$title
  })
  
  output$text <- renderText({
    input$field
  })
  
  base <- reactive ({get(input$data) })
  
  output$plot <- renderPlot({
    mydata <- base()
    autoplot(mydata, facets = TRUE) # upward trend
    
  })
  
  output$plot2 <- renderPlot({

    # Sélection d'uniquement 1 variable
    d <- select(data, IYEAR, !!as.symbol(input$field))
    
    # Creation d'une variable contenant une valeur unique
    e <- d  %>%
      mutate(new = ifelse(!!as.symbol(input$field) == "true", 1, 0)) %>%
      select(-!!as.symbol(input$field)) %>%
      subset(!is.na(new))
    
    f <- e %>%
      aggregate(by=list(e$IYEAR), sum) %>% 
      select(-IYEAR)
    
    f$Group.1 <- as.Date(ISOdate(f$Group.1, 1, 1))
    
    # t <- xts(order.by=f$Group.1, f$new) 
    # t <- t[-c(nrow(t)),] 
    # # colnames(t) <-!!as.symbol(input$field)
    # plot(t)    
    
    ggplot(t) +
      geom_line(aes(x=f$Group.1, y = f$new), colour = f$new)
    
  # output$plot <- renderPlot({
  #   ggplot(subset(data, !is.na(input$field2)), aes(x = !!as.symbol(input$field), fill = !!as.symbol(input$field2))) + 
  #            geom_bar(position = "fill") +
  #            labs(title = "Proportion du tabagisme en fonction du niveau d'achèvement", fill = 'Statut fumeur') +
  #            xlab("Education")    
    
  })
})


