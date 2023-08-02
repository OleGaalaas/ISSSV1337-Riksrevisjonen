


library(shiny)
library(leaflet)
library(sf)
library(plotly)
library(dplyr)



# Load and preprocess the data 
Norway_all <- read.csv("NorwayDataset2.csv")

# Define the UI for the Shiny app
ui <- fluidPage(
  titlePanel("Interactive Map with Shiny"),
  
  # Input for the year
  numericInput("year_input", label = "Select Year", min = min(Norway_all$year), max = max(Norway_all$year), value = min(Norway_all$year)),
  
  # Input for the municipality
  selectInput("municipality_input", label = "Select Municipality", choices = unique(Norway_all$kommune)),
  
  leafletOutput("map") # Placeholder for the map
)

# Define the server for the Shiny app
server <- function(input, output) {
  
  # Filter data based on input$year_input and input$municipality_input
  filtered_data <- reactive({
    filter_data <- Norway_all[Norway_all$year == input$year_input & Norway_all$kommune == input$municipality_input, ]
    return(filter_data)
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(data = filtered_data(), ~lon, ~lat,
                 popup = ~paste("Municipality:", kommune, "<br>",
                                "School Name", enhetnavn.x,"<br>",
                                "Mean Grade:", mean_grade ,"<br>", 
                                "Number of Student", antall_elever,"<br>", 
                                "Dropout", andel,"<br>", 
                                "School Staff", antall_ansatte)) %>%
      setView(lng = 8.03, lat = 58.2, zoom = 10)
  })
}

# Run the Shiny app
shinyApp(ui, server)


