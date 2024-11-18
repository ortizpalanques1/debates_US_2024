library(shiny)
library(tidyverse)
library(tidyr)
load("data/data_debate_2024.RData")
source("www/functions.R")
# Files to retrive
# debates_2024_uw_clean_ss = All the words after stop_words
# candidates_data
# person_colors

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  class = "column_selection",
  fluidRow(
    h1(
      "United States Presidential Debates (2024)",
      )
  ),
  fluidRow(
    column(
      2,
      #class = "column_selection",
      h2(selector_title),
      selectInput("candidate_1",
                  "Candidate 1",
                  choices = candidates_data$selection),
      
      selectInput("candidate_2",
                  "Candidate 2",
                  choices = NULL)
    ),
    column(
      10,
      fluidRow(
        column(
          6,
          plotOutput("candidates")
        ),
        column(
          2,
          h3(explain_graph_title),
          p(explain_graph)
        )
      ),
      fluidRow(
        column(
          2,
          fluidRow(
            h3(correlatio),
          ),
          fluidRow(
            class = "minitext",
            textOutput("correlation")
          )
        ),
        column(
          2,
          fluidRow(
            h3(the_pvalue)
          ),
          fluidRow(
            class = "minitext",
            textOutput("pvalue")
            # ,
            # textOutput("word01")
          )
        )
      )
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
  
  # Candidate name
  candidate_x_name <- reactive({
    candidate_both()$selection.x[1]
  })
  candidate_y_name <- reactive({
    candidate_both()$selection.y[1]
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
      geom_point(alpha = 0.05, size = 2.5, color = "#ff6500")+
      geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5, color = "black")+
      scale_x_log10(labels = scales::percent)+
      scale_y_log10(labels = scales::percent)+
      labs(
        title = "Comparison in The Use of Words",
        x = candidate_x_name(),
        y = candidate_y_name()
      )+
      theme(
        axis.title = element_text(family =  "TT Times New Roman", face = "bold", color = "black", size = 14),
        axis.text = element_text(family =  "TT Times New Roman", face = "bold", color = "black", size = 10),
        legend.position = "none",
        panel.background = element_rect(fill = "#fdfd96"),
        panel.grid.major = element_line(color = "grey"),
        panel.grid.minor = element_line(color = "white"),
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(family =  "TT Times New Roman", face = "bold", color = "black", size = 24)
      )
  })
  
  # Correlation the Test
  the_correlatio <- reactive({
    lado_x <- candidate_both()$proportion.x %>%
      replace(is.na(.), 0)
    lado_y <- candidate_both()$proportion.y %>%
      replace(is.na(.), 0)

    the_test <- cor.test(lado_x, lado_y, method = "pearson")
  })
  
  #Correlation
  output$correlation <- renderText({
    rvalue(the_correlatio()$estimate)
  })
  
  #p Value
  output$pvalue <- renderText({
    pvalue(the_correlatio()$p.value)
  })
  
  # deborah <- reactive({
  #   most_used_word(candidate_both(), candidate_both()$proportion.x, "word")
  # })
  # 
  # output$word01 <- renderText({
  #     deborah()
  # })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
