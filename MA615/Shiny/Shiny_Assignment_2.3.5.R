###Shiny Assignment 2.3.5

library(shiny)
library(DT)  # Use DT for modern DataTables

ui <- fluidPage(
  titlePanel("Testing Output Types, Scatterplot, and Data Table"),
  
  fluidRow(
    column(6,
           h3("Using renderPrint with verbatimTextOutput"),
           verbatimTextOutput("print_summary"),
           verbatimTextOutput("print_ttest")
    ),
    column(6,
           h3("Using renderText with textOutput"),
           textOutput("text_greeting"),
           textOutput("text_model")
    )
  ),
  
  fluidRow(
    column(12,
           h3("Scatterplot with Custom Size and Accessibility"),
           div(
             plotOutput("scatterplot", 
                        width = "700px", 
                        height = "300px"),
             tags$p("A scatterplot of five random numbers.", class = "sr-only")
           )
    )
  ),
  
  fluidRow(
    column(12,
           h3("Data Table with Suppressed Controls"),
           DTOutput("table")  # Use DTOutput for the data table
    )
  )
)

server <- function(input, output, session) {
  # Text and Print Outputs
  output$print_summary <- renderPrint({
    summary(mtcars)
  })
  
  output$print_ttest <- renderPrint({
    t.test(1:5, 2:6)
  })
  
  output$text_greeting <- renderText({
    "Good morning!"
  })
  
  output$text_model <- renderText({
    str(lm(mpg ~ wt, data = mtcars))
  })
  
  # Scatterplot Output
  output$scatterplot <- renderPlot({
    x <- 1:5
    y <- runif(5)  # Generate random values for scatterplot
    plot(x, y, 
         main = "Scatterplot of Five Random Numbers",
         xlab = "Index", 
         ylab = "Random Value",
         pch = 19, col = "blue")
  }, res = 96)  # Match RStudio's resolution
  
  # Data Table Output
  output$table <- renderDT({
    datatable(mtcars, 
              options = list(
                searching = FALSE,   # Disable search box
                ordering = FALSE,    # Disable column sorting
                info = FALSE,        # Disable summary information
                lengthChange = FALSE, # Disable length selection
                paging = FALSE       # Disable pagination
              ))
  })
}

shinyApp(ui, server)

