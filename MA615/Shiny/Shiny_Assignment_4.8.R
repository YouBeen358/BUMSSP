###Shiny_Assignment_4.8

#Q1

# Load the library
library(jpeg)

# Read and display the image
img <- readJPEG("IMG_0248.jpg")
plot(1:2, type = "n", xlab = "", ylab = "", axes = FALSE)
rasterImage(img, 1, 1, 2, 2)

#Q2
#It won't go as expected. If we flip fct_infreq() and fct_lump(), as fct_lump() is not a function that knows
# the most frequent level, unless using fct_infreq(), it will just give us a random answer. 

#Q3
dir.create("neiss")
#> Warning in dir.create("neiss"): 'neiss' already exists
download <- function(name) {
  url <- "https://raw.github.com/hadley/mastering-shiny/main/neiss/"
  download.file(paste0(url, name), paste0("neiss/", name), quiet = TRUE)
}
download("injuries.tsv.gz")
download("population.tsv")
download("products.tsv")

library(shiny)
library(vroom)
library(tidyverse)

injuries <- vroom::vroom("neiss/injuries.tsv.gz")
products <- vroom::vroom("neiss/products.tsv")
population <- vroom::vroom("neiss/population.tsv")

count_top <- function(df, var, n) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}

ui <- fluidPage(
  fluidRow(
    column(6, 
           selectInput("code", "Product", 
                       choices = setNames(products$prod_code, products$title))
    ),
    column(3, 
           numericInput("rows", "Number of rows to show:", value = 5, min = 1, max = 20)
    )
  ),
  fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  fluidRow(
    column(12, plotOutput("age_sex"))
  )
)

server <- function(input, output, session) {
  # Reactive dataset filtered by product code
  selected <- reactive(injuries %>% filter(prod_code == input$code))
  
  # Summary tables
  output$diag <- renderTable(
    count_top(selected(), diag, input$rows), width = "100%"
  )
  output$body_part <- renderTable(
    count_top(selected(), body_part, input$rows), width = "100%"
  )
  output$location <- renderTable(
    count_top(selected(), location, input$rows), width = "100%"
  )
  
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  
  output$age_sex <- renderPlot({
    summary() %>%
      ggplot(aes(age, n, colour = sex)) +
      geom_line() +
      labs(y = "Estimated number of injuries")
  }, res = 96)
}

shinyApp(ui, server)


#Q4


# Function to get top n rows
count_top <- function(df, var, n) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}

# UI
ui <- fluidPage(
  fluidRow(
    column(6, 
           selectInput("code", "Product", 
                       choices = setNames(products$prod_code, products$title))
    ),
    column(3, 
           numericInput("rows", "Number of rows to show:", value = 5, min = 1, max = 20)
    )
  ),
  fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  fluidRow(
    column(12, plotOutput("age_sex"))
  ),
  fluidRow(
    column(2, actionButton("prev", "Previous")),
    column(2, actionButton("next", "Next")),
    column(8, textOutput("narrative"))
  )
)

# Server
server <- function(input, output, session) {
  # Reactive dataset filtered by product code
  selected <- reactive(injuries %>% filter(prod_code == input$code))
  
  # Summary tables
  output$diag <- renderTable(
    count_top(selected(), diag, input$rows), width = "100%"
  )
  output$body_part <- renderTable(
    count_top(selected(), body_part, input$rows), width = "100%"
  )
  output$location <- renderTable(
    count_top(selected(), location, input$rows), width = "100%"
  )
  
  # Summary for plotting
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  
  # Plot
  output$age_sex <- renderPlot({
    summary() %>%
      ggplot(aes(age, n, colour = sex)) +
      geom_line() +
      labs(y = "Estimated number of injuries")
  }, res = 96)
  
  # Reactive value for the current narrative index
  current_index <- reactiveVal(1)
  
  # Update index when buttons are clicked
  observeEvent(input$next, {
    if (current_index() < nrow(selected())) {
      current_index(current_index() + 1)
    }
  })
  
  observeEvent(input$prev, {
    if (current_index() > 1) {
      current_index(current_index() - 1)
    }
  })
  
  # Render the current narrative
  output$narrative <- renderText({
    narrative <- selected()$narrative
    if (length(narrative) == 0) {
      return("No narrative available.")
    }
    narrative[current_index()]
  })
}

# Run the app
shinyApp(ui, server)



#Advanced
# Function to get top n rows
count_top <- function(df, var, n) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}

# UI
ui <- fluidPage(
  fluidRow(
    column(6, 
           selectInput("code", "Product", 
                       choices = setNames(products$prod_code, products$title))
    ),
    column(3, 
           numericInput("rows", "Number of rows to show:", value = 5, min = 1, max = 20)
    )
  ),
  fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  fluidRow(
    column(12, plotOutput("age_sex"))
  ),
  fluidRow(
    column(2, actionButton("prev", "Previous")),
    column(2, actionButton("next", "Next")),
    column(8, textOutput("narrative"))
  )
)

# Server
server <- function(input, output, session) {
  # Reactive dataset filtered by product code
  selected <- reactive(injuries %>% filter(prod_code == input$code))
  
  # Summary tables
  output$diag <- renderTable(
    count_top(selected(), diag, input$rows), width = "100%"
  )
  output$body_part <- renderTable(
    count_top(selected(), body_part, input$rows), width = "100%"
  )
  output$location <- renderTable(
    count_top(selected(), location, input$rows), width = "100%"
  )
  
  # Summary for plotting
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  
  # Plot
  output$age_sex <- renderPlot({
    summary() %>%
      ggplot(aes(age, n, colour = sex)) +
      geom_line() +
      labs(y = "Estimated number of injuries")
  }, res = 96)
  
  # Reactive value for the current narrative index
  current_index <- reactiveVal(1)
  
  # Update index when buttons are clicked
  observeEvent(input$next, {
    if (nrow(selected()) > 0) {
      current_index((current_index() %% nrow(selected())) + 1)
    }
  })
  
  observeEvent(input$prev, {
    if (nrow(selected()) > 0) {
      current_index(ifelse(current_index() == 1, nrow(selected()), current_index() - 1))
    }
  })
  
  # Render the current narrative
  output$narrative <- renderText({
    narrative <- selected()$narrative
    if (length(narrative) == 0) {
      return("No narrative available.")
    }
    narrative[current_index()]
  })
}

# Run the app
shinyApp(ui, server)

