

shinyUI(fluidPage(
  
  titlePanel("TEST"),
  sidebarLayout(
    sidebarPanel(
      
      selectInput("field", "Choose a field :",
                  choices = names(data),
                  selected = "X_EDUCAG"),
      
      selectInput("field2", "Choose a field :",
                  choices = names(data),
                  selected = "X_SMOKER3")
    ),
    
    mainPanel(
      # tableOutput("table"),
      plotOutput("plot")
    )
  )
))

