library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet) 


accident_data <- read.csv("accident.csv") %>%
  mutate(
    HOUR = ifelse(HOUR == 99, NA, HOUR),
    WEATHERNAME = ifelse(WEATHERNAME == "Unknown", NA, WEATHERNAME)
  )


ui <- fluidPage(
  titlePanel("Enhanced Accident Data Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("state", "Select State:", 
                  choices = unique(accident_data$STATENAME), 
                  selected = "Alabama"),
      sliderInput("fatal_range", "Select Fatality Range:",
                  min = 0, max = max(accident_data$FATALS, na.rm = TRUE), 
                  value = c(0, max(accident_data$FATALS, na.rm = TRUE)))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Accident Hour Distribution", plotOutput("accidentPlot")),
        tabPanel("Map of Accidents", leafletOutput("accidentMap"))
      )
    )
  )
)


server <- function(input, output) {
  
 
  filtered_data <- reactive({
    accident_data %>%
      filter(
        STATENAME == input$state,
        FATALS >= input$fatal_range[1],
        FATALS <= input$fatal_range[2]
      )
  })
  
  
  output$accidentPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = HOUR)) +
      geom_histogram(binwidth = 1, fill = "steelblue", color = "white", alpha = 0.8) +
      labs(title = paste("Accident Count by Hour in", input$state),
           x = "Hour of Day", y = "Number of Accidents") +
      theme_minimal(base_size = 15) +
      scale_x_continuous(breaks = 0:23) + 
      theme(
        plot.title = element_text(face = "bold", color = "darkblue", size = 18),
        axis.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey80")
      )
  })
  
 
  output$accidentMap <- renderLeaflet({
    leaflet(data = filtered_data()) %>%
      addTiles() %>%
      addCircleMarkers(
        ~LONGITUD, ~LATITUDE,
        radius = ~sqrt(FATALS) + 3,
        color = "red",
        stroke = FALSE, fillOpacity = 0.5,
        label = ~paste("Fatalities:", FATALS)
      ) %>%
      setView(lng = -95.7129, lat = 37.0902, zoom = 4) 
  })
}


shinyApp(ui = ui, server = server)
