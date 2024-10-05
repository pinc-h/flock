# Alex Pinch
# Last updated October 5 2024
library(shiny)
library(tidyverse)


ui <- fluidPage(
  titlePanel("Flock"),
  
  mainPanel(
    plotOutput("scatterPlot", width = "600px", height = "600px"),
    textInput("guess", "Estimate the flock size", value = ""),  # Input box for guess
    actionButton("submit", "Submit Guess"),
    textOutput("result"),
    checkboxInput("show_grid", "Show Grid", value = FALSE),  # Checkbox to toggle grid
    textOutput("averageScore")
  )
)


server <- function(input, output, session) {
  num_points <- reactiveVal(sample(0:3000, 1))  # random number of points
  
  # init score variables
  total_score <- reactiveVal(0)
  guess_count <- reactiveVal(0)
  
  output$scatterPlot <- renderPlot({
    # random data
    data <- tibble( 
      x = runif(num_points(), min = 0, max = 100),
      y = runif(num_points(), min = 0, max = 100)
    )
    
    plot <- ggplot(data, aes(x = x, y = y)) +
      geom_point(color = "blue", size = 3) +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank() 
      )
    
    # turning checkbox on and off (checking checkbox state)
    if (input$show_grid) {
      plot <- plot + theme(panel.grid.major = element_line(color = "gray"),  # Major grid lines
                     panel.grid.minor = element_line(color = "lightgray"))  # Minor grid lines
    } else {
      plot <- plot + theme(panel.grid.major = element_blank(),  # Remove major grid lines
                     panel.grid.minor = element_blank())  # Remove minor grid lines
    }

    print(plot)
    
  })
  

  observeEvent(input$submit, {
    guess <- as.numeric(input$guess) # Get the user's guess
    
    actual_points <- num_points()
    differential <- abs(guess - actual_points)
    
    total_score(total_score() + differential)  # Add the differential to total score
    guess_count(guess_count() + 1)          
    
    # Round answer and guess only if the actual points are above 1000
    rounded_guess <- ifelse(actual_points > 1000, round(guess, -2), round(guess, -1))
    rounded_actual <- ifelse(actual_points > 1000, round(actual_points, -2), round(actual_points, -1))
    
    if (!is.na(rounded_guess) && rounded_guess == rounded_actual) { # check if rounded answer and guess are equal
      output$result <- renderText("Correct!")
    } else {
      output$result <- renderText(paste("Incorrect, the correct rounded number is", rounded_actual))
    }
    
    average_score <- total_score() / guess_count()
    output$averageScore <- renderText(paste("Average differential:", round(average_score, 2)))
    
    updateActionButton(session, "submit")
    
    # Reset the number of points for the next guess
    num_points(sample(0:3000, 1))  # Randomly set the number of points between 0 and 3000
  })
}

# Run the application
shinyApp(ui = ui, server = server)