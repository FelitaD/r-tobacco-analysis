library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(ggplot2)


shinyServer(function(input, output) {
  
  output$title <- renderText({
    input$title
  })
  
  output$text <- renderText({
    input$field
  })
  
  output$plot <- renderPlot({
    ggplot(subset(data, !is.na(input$field2)), aes(x = !!as.symbol(input$field), fill = !!as.symbol(input$field2))) + 
             geom_bar(position = "fill") +
             labs(title = "Proportion du tabagisme en fonction du niveau d'achèvement", fill = 'Statut fumeur') +
             xlab("Education")    
    
    
    
    # ggplot(subset(data, !is.na(X_EDUCAG), aes_string(x = input$field, fill = input$field2)) + 
    #   geom_bar(position = "fill") +
    #   labs(title = "Proportion du tabagisme en fonction du niveau d'achèvement", fill = 'Statut fumeur') +
    #   xlab("Education")
      
      # geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
      # theme_economist()+ 
      # labs(title = "Répartition des répondants par rapport à leur consommation de tabac", x = "Nombre de cigarettes consommées dans leur vie", y = "Proportion de répondants") + 
      # scale_fill_discrete(name="Fréquence") + 
      # scale_fill_brewer(palette = "Reds") + 
      # theme(axis.text.x=element_text(size=10,vjust=.8, hjust=0.8),legend.position="none")
  })
})
