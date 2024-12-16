library(shiny)
library(leaflet)
library(ggplot2)
library(forecast)
library(dplyr)
library(scales)

# Data for different categories
general_metrics <- data.frame(
  Metric = c("Land Size (km²)", "Population (2023)"),
  Taiwan = c(36197, 23588613),
  Singapore = c(729, 5917648)
)

gdp_metrics <- data.frame(
  Metric = c("GDP (2020)", "GDP (2021)", "GDP (2022)", "GDP (2023)"),
  Taiwan = c(668159000000, 722589000000, 759104000000, 755674000000),
  Singapore = c(349488382611, 434111559283, 498474540988, 501427500080)
)

gdp_per_capita_metrics <- data.frame(
  Metric = c("GDP Per Capita (2020)", "GDP Per Capita (2021)", 
             "GDP Per Capita (2022)", "GDP Per Capita (2023)"),
  Taiwan = c(28000, 30000, 32000, 32404),
  Singapore = c(61466.8, 79601.41, 88428.7, 84734.26)
)

# Data cleaning and forecasting functions
load_and_clean_data <- function(files) {
  all_data <- do.call(rbind, lapply(files, function(file) {
    df <- read.csv(file)
    df$Date <- as.Date(df$X)
    return(df)
  }))
  
  # Filter and aggregate data
  data_filtered <- all_data %>%
    filter(format(Date, "%Y") %in% 2020:2024) %>%
    select(Date, Tx) %>%
    na.omit()
  
  data_filtered$Month <- format(data_filtered$Date, "%Y-%m")
  monthly_data <- data_filtered %>%
    group_by(Month) %>%
    summarize(AvgTemperature = mean(Tx, na.rm = TRUE))
  
  return(ts(monthly_data$AvgTemperature, start = c(2020, 1), frequency = 12))
}

forecast_temperature <- function(monthly_ts) {
  arima_model <- auto.arima(monthly_ts)
  forecast(arima_model, h = 12)
}

# Simulated alternative forecast function
alternative_forecast <- function() {
  data.frame(
    Month = seq.Date(from = as.Date("2025-01-01"), by = "month", length.out = 12),
    Temperature = round(runif(12, min = 15, max = 30), 2) # Random values
  )
}

# File paths for the data (adjust these to your actual file paths)
files <- c("12J990_2020_monthly.csv", "12J990_2021_monthly.csv", 
           "12J990_2022_monthly.csv", "12J990_2023_monthly.csv", 
           "12J990_2024_monthly.csv")

# UI
ui <- fluidPage(
  titlePanel("Taiwan Analysis"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.tabs == 'Comparison'",
        h4("Select Category"),
        radioButtons(
          "category", "View Metrics:",
          choices = c("General" = "general",
                      "GDP" = "gdp",
                      "GDP Per Capita" = "gdp_per_capita"),
          selected = "general"
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel(
          "Overview",
          leafletOutput("map"), # Placeholder for the map
          h3("Key Facts About Taiwan"),
          tags$ul(
            tags$li("Government: Semi-presidential democracy."),
            tags$li("Economy: A global leader in technology and electronics manufacturing."),
            tags$li("Population: Approximately 23.59 million people as of 2023."),
            tags$li("Natural Environment: Known for its mountains, forests, and diverse ecosystems."),
            tags$li("History: Taiwan has a rich history, including periods of Dutch, Spanish, and Japanese colonization.")
          )
        ),
        tabPanel(
          "Projection",
          h3("Temperature Projections for 2025"),
          plotOutput("originalProjectionPlot"), # Original projection plot
          plotOutput("alternativeProjectionPlot"), # Alternative projection plot
          h4("Projection Table"),
          tableOutput("projectionTable") # Table of forecasted values
        ),
        tabPanel(
          "Comparison",
          h3("Detailed Comparison: Taiwan vs. Singapore"),
          tableOutput("comparisonMetricsTable")
        ),
        tabPanel(
          "SWOT Analysis",
          h3("SWOT Analysis"),
          p("**Strengths**: Strong technology sector, skilled workforce."),
          p("**Weaknesses**: International recognition issues."),
          p("**Opportunities**: Expansion in renewable energy."),
          p("**Threats**: Geopolitical tensions in East Asia.")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Render the map for Overview
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>% # English labels
      setView(lng = 121.5598, lat = 25.0911, zoom = 7) %>%
      addMarkers(lng = 121.5598, lat = 25.0911, popup = "Taipei")
  })
  
  # Load, clean, and forecast data
  monthly_ts <- load_and_clean_data(files)
  forecast_2025 <- forecast_temperature(monthly_ts)
  alt_forecast <- alternative_forecast()
  
  # Combine both projections into one table
  projection_table <- data.frame(
    Month = format(seq.Date(from = as.Date("2025-01-01"), by = "month", length.out = 12), "%Y-%m"),
    OriginalProjection = round(as.numeric(forecast_2025$mean), 2),
    AlternativeProjection = alt_forecast$Temperature
  )
  
  # Render the original projection plot
  output$originalProjectionPlot <- renderPlot({
    autoplot(forecast_2025) +
      labs(title = "Original Temperature Projection (2025)",
           x = "Month", y = "Average Temperature (°C)") +
      theme_minimal()
  })
  
  # Render the alternative projection plot
  output$alternativeProjectionPlot <- renderPlot({
    ggplot(alt_forecast, aes(x = Month, y = Temperature)) +
      geom_line(color = "blue") +
      geom_point(color = "red") +
      labs(title = "Alternative Temperature Projection (2025)",
           x = "Month", y = "Temperature (°C)") +
      theme_minimal()
  })
  
  # Render the projection table
  output$projectionTable <- renderTable({
    projection_table
  })
  
  # Render the comparison table based on selected category
  output$comparisonMetricsTable <- renderTable({
    if (input$category == "general") {
      general_metrics %>%
        mutate(
          Taiwan = scales::comma(Taiwan, accuracy = 1),
          Singapore = scales::comma(Singapore, accuracy = 1)
        )
    } else if (input$category == "gdp") {
      gdp_metrics %>%
        mutate(
          Taiwan = scales::comma(Taiwan, accuracy = 1),
          Singapore = scales::comma(Singapore, accuracy = 1)
        )
    } else if (input$category == "gdp_per_capita") {
      gdp_per_capita_metrics %>%
        mutate(
          Taiwan = scales::comma(Taiwan, accuracy = 1),
          Singapore = scales::comma(Singapore, accuracy = 1)
        )
    }
  }, striped = TRUE, bordered = TRUE, hover = TRUE, align = "l")
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
