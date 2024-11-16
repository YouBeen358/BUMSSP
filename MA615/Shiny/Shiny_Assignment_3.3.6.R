###Shiny_Assignment_3.3.6

#Q1
library(shiny)

ui <- fluidPage(
  textInput("name", "What's your name?"),
  textOutput("greeting")
)

server1 <- function(input, output) {
  output$greeting <- renderText({
    paste0("Hello ", input$name)
  })
}

shinyApp(ui, server1)

server2 <- function(input, output) {
  greeting <- reactive({
    paste0("Hello ", input$name)
  })
  output$greeting <- renderText({
    greeting()
  })
}

shinyApp(ui, server2)

server3 <- function(input, output) {
  output$greeting <- renderText({
    paste0("Hello ", input$name)
  })
}

shinyApp(ui, server3)


#Q2

# Install the package if not already installed
install.packages("jpeg")

# Load the library
library(jpeg)

# Read and display the image
img <- readJPEG("IMG_0247.jpg")
plot(1:2, type = "n", xlab = "", ylab = "", axes = FALSE)
rasterImage(img, 1, 1, 2, 2)

#Q3
#When using range and var as a reactive expression, we have to note that we are overwriting the base R function, which has the same names.
#Hence, this will lead to creating a conflict. When using without the parantheses, this will either lead to causing unexpected behaviour or be shadowed. 





