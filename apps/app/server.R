library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(ggplot2)

data$IYEAR 

shinyServer(function(input, output) {
  
  output$title <- renderText({
    input$title
  })
  
  output$text <- renderText({
    input$field
  })
  
  output$plot <- renderPlot({

    ggplot(count(data, X_RFSMOK3), aes(x=as.Date(data$IYEAR, origin="1970-01-01"), y=NUMADULT))  
    
    ggplot(data, aes(x=as.Date(IYEAR, origin="1970-01-01"), y=NUMADULT))

  # output$plot <- renderPlot({
  #   ggplot(subset(data, !is.na(input$field2)), aes(x = !!as.symbol(input$field), fill = !!as.symbol(input$field2))) + 
  #            geom_bar(position = "fill") +
  #            labs(title = "Proportion du tabagisme en fonction du niveau d'achèvement", fill = 'Statut fumeur') +
  #            xlab("Education")    
    
  })
})

library(tidyverse)
library(xts)

d <- select(data, IYEAR, X_RFSMOK3)

d <- d  %>%
  mutate(fumeur = ifelse(X_RFSMOK3 == "true", 1, 0)) %>%
  select(-X_RFSMOK3)
e <- subset(d, !is.na(fumeur))
agg <- aggregate(e, by=list(e$IYEAR), sum)

agg %>% select(-IYEAR)
agg$Group.1 <- as.Date(ISOdate(agg$Group.1, 1, 1))

t <- xts(order.by=agg$Group.1, agg$fumeur)
colnames(t)<-'Fumeur'

plot(t)

