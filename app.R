# Load the necessary libraries
library(shiny)
library(dplyr)
library(leaflet)
library(sf)
library(plotly)
library(glmnet)
library(Matrix)
library(shinythemes)
library(tidyverse)

# Load your "bigdata" dataset (replace "path/to/your/data.csv" with the actual path to your dataset)
bigdata <- read.csv("Bigdata.csv")

# Calculate the mean of the 'mean_grade' column
mean_grade_mean <- mean(bigdata$mean_grade, na.rm = TRUE)

# Create the "Higher" column based on the 'mean_grade' variable
bigdata$Higher <- ifelse(bigdata$mean_grade > mean_grade_mean, "Higher", "Lower")

# Load other necessary data files
helper <- read.csv("Bigdata.csv")
imp_data <- read_csv("NorwayDataset copy.csv")

imp_data$aar <- substr(imp_data$aar, 1, 4)
imp_data$aar <- as.integer(imp_data$aar)

income <- read.csv("Income_no.csv")

income <- income %>% 
  rename("aar" = "year")

income$kommune <- str_to_upper(income$kommune)

imp_data <- imp_data %>% 
  left_join(income, by = c("kommune", "aar"))

imp_data$type.x <- ifelse(grepl("skule$|skole$", imp_data$enhetnavn.x), "Public", "Private")

merged_data <- merge(imp_data, helper, by = "enhetnavn.x", all.x = TRUE)
merged_data <- rename(merged_data, Centrality_index = Indeks.2023)
merged_data$Centrality_index[is.na(merged_data$Centrality_index)] <- 0

merged_data <- merged_data %>% 
  rename("Type" = "type.x.x")

final_data <- merged_data %>% 
  select(antall_ansatte.x, Centrality_index, Type, income, andel.x, antall_elever.x, mean_grade.x)

# Define the UI
ui <- fluidPage(theme = shinytheme("united"),
                titlePanel("Shiny App with Tabs"),
                tabsetPanel(
                  tabPanel("OLS Regression",
                           # UI for OLS Regression App
                           sidebarLayout(
                             sidebarPanel(
                               # Create a select input for dependent variable selection
                               selectInput("dependent_var2", "Select the dependent variable:", choices = colnames(bigdata)),
                               # Create a checkbox group for independent variable selection with a scrollbar
                               selectizeInput("independent_vars2", "Select independent variables:",
                                              choices = colnames(bigdata), multiple = TRUE,
                                              options = list(maxOptions = 100, placeholder = 'Choose variables')),
                               # Create a select input for choosing the size variable
                               selectInput("size_var", "Select the size variable:", choices = colnames(bigdata))
                             ),
                             mainPanel(
                               h3("Visualization:"),
                               # Output the plot
                               plotOutput("visualization")
                             )
                           )
                  ),
                  tabPanel("Interactive Map",
                           # UI for Interactive Map App
                           sidebarLayout(
                             sidebarPanel(
                               # Input for the year
                               numericInput("year_input", label = "Select Year", min = min(imp_data$aar), max = max(imp_data$aar), value = min(imp_data$aar)),
                               # Input for the municipality
                               selectInput("municipality_input", label = "Select Municipality", choices = unique(imp_data$kommune))
                             ),
                             mainPanel(
                               # Placeholder for the map
                               leafletOutput("map")
                             )
                           )
                  ),
                  tabPanel("Predict Mean Grade",
                           # UI for Predict Mean Grade App
                           sidebarLayout(
                             sidebarPanel(
                               HTML("<h3>Input parameters</h3>"),
                               sliderInput("antall_ansatte.x", label = "Antall Ansatte:", min = 2, max = 500, value = 100),
                               sliderInput("Centrality_index", label = "Indeks.2023:", min = 300, value = 100, max = 1000),
                               selectInput("Type", label = "Type:", choices = unique(final_data$Type)),
                               sliderInput("income", label = "Inntekt:", min = 500000, max = 941000, value = 500000),
                               sliderInput("andel.x", label = "Percent of Dropout:", min = 0, value = 50, max = 100),
                               sliderInput("antall_elever.x", label = "Antall Elever:", min = 2, value = 100, max = 2500),
                               actionButton("submitbutton", "Submit", class = "btn btn-primary")
                             ),
                             mainPanel(
                               tags$label(h3('Prediction')), # Output Text Box
                               verbatimTextOutput('prediction_result')
                             )
                           )
                  )
                )
)

# Define the server
server <- function(input, output, session) {
  # Create the visualization plot
  output$visualization <- renderPlot({
    dependent_var2 <- input$dependent_var2
    independent_vars2 <- input$independent_vars2
    size_var <- input$size_var
    
    if (!is.null(dependent_var2) && length(independent_vars2) > 0 && !is.null(size_var)) {
      # Filter the selected variables from the dataset
      selected_vars2 <- c(dependent_var2, independent_vars2, size_var)
      plot_data <- bigdata[, selected_vars2]
      
      # Remove rows with any NA values
      plot_data <- na.omit(plot_data)
      
      # Create the ggplot with geom_point and geom_smooth (method = "lm")
      ggplot(plot_data, aes_string(x = independent_vars2, y = dependent_var2, size = size_var)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE) +
        labs(x = independent_vars2, y = dependent_var2)
    }
  })
  
  # Filter data based on input$year_input and input$municipality_input for Interactive Map
  filtered_data <- reactive({
    filter_data <- imp_data[imp_data$aar == input$year_input & imp_data$kommune == input$municipality_input, ]
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
  
  # Predict mean grade based on input parameters for Predict Mean Grade
  prediction_result <- reactive({
    final_data$Type <- factor(final_data$Type, levels = c("Public", "Private"))
    
    new_data <- data.frame(
      antall_ansatte.x = input$antall_ansatte.x,
      Centrality_index = input$Centrality_index,
      Type = input$Type,
      income = input$income,
      andel.x = input$andel.x,
      antall_elever.x = input$antall_elever.x
    )
    
    model <- lm(mean_grade.x ~ ., data = final_data)
    
    predicted_mean_grade <- predict(model, newdata = new_data)
    return(predicted_mean_grade)
  })
  
  output$prediction_result <- renderText({
    if (input$submitbutton > 0) {
      prediction_result()
    } else {
      return("Press the 'Submit' button to see the prediction.")
    }
  })
}

# Run the Shiny app
shinyApp(ui, server)
