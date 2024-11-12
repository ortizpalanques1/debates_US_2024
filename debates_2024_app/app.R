library(shiny)
library(tidyverse)
library(tidyr)
load("data/data_debate_2024.RData")
# Files to retrive
# debates_2024_uw_clean_ss = All the words after stop_words
# candidates_data
# person_colors

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput("candidate_1",
                      "Candidate 1",
                      choices = candidates_data$selection),
          
          selectInput("candidate_2",
                      "Candidate 2",
                      choices = NULL),
          
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  candidate_alter <- reactive({
    filter(candidates_data, selection != input$candidate_1)
  })
  
  observeEvent(candidate_alter(), {
    choices <- candidate_alter()$selection
    updateSelectInput(inputId = "candidate_2", choices = choices)
  })
  
  

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
