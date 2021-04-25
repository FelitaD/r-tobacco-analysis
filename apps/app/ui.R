

shinyUI(fluidPage(
  
  titlePanel("TEST"),
  sidebarLayout(
    sidebarPanel(

      dateRangeInput("date", strong("Date range"), start = "2013-01-01", end = "2020-12-31",
                     min = "2013-01-01", max = "2020-12-31"),
  
      hr(),
      fluidRow(column(4, verbatimTextOutput("value"))),
      
      selectInput("data", "Choose a dataset :",
                  choices = c("age" = "age",
                              "tax_rate" = "tax_rate"),
                  selected = "age"),
      
      selectInput("field", "Choose a field :",
                  choices = choices,
                  selected = "X_RFSMOK3"),
    ),
    
    mainPanel(
      # tableOutput("table"),
      plotOutput("plot")
    )
  )
))

