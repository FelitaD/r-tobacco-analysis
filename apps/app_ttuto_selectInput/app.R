## Only run examples in interactive R sessions
if (interactive()) {


  # demoing group support in the `choices` arg
  shinyApp(
    ui = fluidPage(
      selectInput("theme", "Choose a state:",
                  list(`Consommation tabac` = list("tabac","pack_conso"),
                       `Prix tabac` = list("tax_rate", "tax_revenue", "pack_cost"),
                       `Socio-demographie` = list("famille", "education", "finances"),
                       `Sante` = list("sante")),
                  selected = "pack_cost"
      ),
      plotOutput("plot"),
      plotOutput("gglagplot"),
    ),
    
    server = function(input, output) {
      
      data <- reactive({ get(input$theme) })
      
      output$plot <- renderPlot({
        autoplot(data(), facets=TRUE, rownames.label = TRUE, colnames.label = TRUE)
      })
      output$gglagplot <- renderPlot({
        gglagplot(data())
      })
      
    }
  )
}