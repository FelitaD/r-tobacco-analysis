

shinyUI(fluidPage(
  
  titlePanel("TEST"),
  sidebarLayout(
    sidebarPanel(

      dateRangeInput("date", strong("Date range"), start = "2013-01-01", end = "2020-12-31",
                     min = "2013-01-01", max = "2020-12-31"),
  
      hr(),
      fluidRow(column(4, verbatimTextOutput("value"))),
      
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

