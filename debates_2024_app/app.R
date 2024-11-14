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
    titlePanel("United States Presidential Debates (2024)"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput("candidate_1",
                      "Candidate 1",
                      choices = candidates_data$selection),
          
          selectInput("candidate_2",
                      "Candidate 2",
                      choices = NULL)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("candidates"),
          textOutput("correlation")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Reactive IU for the Selection N. 2
  candidate_alter <- reactive({
    filter(candidates_data, selection != input$candidate_1)
  })
  
  observeEvent(candidate_alter(), {
    choices <- candidate_alter()$selection
    updateSelectInput(inputId = "candidate_2", choices = choices)
  })
  
  # Data frame with the two selected names
  candidate_both <- reactive({
    candidate_x <- debates_2024_uw_clean_ss_proprotions %>% 
      ungroup() %>% 
      filter(selection == input$candidate_1) %>% 
      select(word, proportion, selection)
    
    candidate_y <- debates_2024_uw_clean_ss_proprotions %>% 
      ungroup() %>% 
      filter(selection == input$candidate_2) %>% 
      select(word, proportion, selection)
    
    data <- full_join(candidate_x, candidate_y, by = "word") 
  })
 
  
  # Plot
  output$candidates <- renderPlot({
    ggplot(candidate_both(), aes(x = proportion.x, y = proportion.y, color = abs(proportion.y - proportion.x)))+
      geom_abline(color = "darkgreen", lty =2)+
      geom_point(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3)+
      geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5)+
      scale_x_log10(labels = scales::percent)+
      scale_y_log10(labels = scales::percent)+
      theme(
        legend.position = "none"
      )
  })
  
  # Correlation
  output$correlation <- renderText({
    lado_1 <- candidate_both()$proportion.x %>% 
      replace(is.na(.), 0)
    lado_2 <- candidate_both()$proportion.y %>% 
      replace(is.na(.), 0)
    # print(length(lado_1))
    # print(length(lado_2))
    
    the_test <- cor.test(lado_1, lado_2, method = "pearson")
    paste0(round(the_test$estimate, 2), " ", the_test$p.value)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
