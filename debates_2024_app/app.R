library(shiny)
library(tidyverse)
library(tidyr)
library(tidytext)
library(forcats)
library(SemNetCleaner)
library(RMariaDB)
library(odbc)
library(DBI)
library(bslib)


load("data/data_debate_2024.RData")
source("www/functions.R")
negative_words <- df_query("english_negations")
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
  tabsetPanel(
    tabPanel("General View",
      fluidRow(
        h1(title_tabs)
      ),
      fluidRow(
        column(
          2,
          #class = "column_selection",
          h3(selector_title),
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
            h2(title_section_01)
          ),
          fluidRow(
            column(
              6,
              plotOutput("candidates")
            ),
            column(
              2,
              h4(explain_graph_title),
              p(explain_graph)
            ),
            column(
              4,
              h3(most_used_word_text),
              fluidRow(
                column(
                  6,
                  fluidRow(
                    h4(textOutput("name_x"))
                  ),
                  fluidRow(
                    class = "muw",
                    textOutput("muw_x")
                  )
                ),
                column(
                  6,
                  fluidRow(
                    h4(textOutput("name_y"))
                  ),
                  fluidRow(
                    class = "muw",
                    textOutput("muw_y")
                  )
                )
              )
            )
          ),
          fluidRow(
            column(
              2,
              fluidRow(
                h4(correlatio),
              ),
              fluidRow(
                class = "minitext",
                textOutput("correlation")
              )
            ),
            column(
              2,
              fluidRow(
                h4(the_pvalue)
              ),
              fluidRow(
                class = "minitext",
                textOutput("pvalue")
              )
            )
          ),
          fluidRow(
            h2(title_section_02)
          ),
          fluidRow(
            column(
              6,
              fluidRow(
                h3("Proportion of Sentiment")
              ),
              fluidRow(
                column(
                  6,
                  fluidRow(
                    h4(textOutput("name_proportion_x"))
                  ),
                  fluidRow(
                    class = "table",
                    tableOutput("proportion_x")
                  )
                ),
                column(
                  6,
                  fluidRow(
                    h4(textOutput("name_proportion_y"))
                  ),
                  fluidRow(
                    class = "table",
                    tableOutput("proportion_y")
                  )
                )
              )
            ),
            column(
              6,
              fluidRow(
                h3("Sentiment: Ten Most Common Words")
              ),
              column(
                6,
                plotOutput("sentiment_graph_x")
              ),
              column(
                6,
                plotOutput("sentiment_graph_y")
              )
            )
          )
        )
      )
    ),
    tabPanel("Vocabulary Usage",
      fluidRow(
        h1(title_tabs)
      ),
      fluidRow(
        h2(title_section_03),
        column(
          2,
          fluidRow(
            selectInput("vocabulary_analysis",
                        "Select the Synthesis",
                        choices = vocabulary_selector,
                        selected = NULL)
          ),
          fluidRow(
            h4(
              textOutput("title_explain_synthesis_vocabulary")
            )
          ),
          fluidRow(
            class = "explanation",
            textOutput("explain_synthesis_vocabulary")
          )
        ),
        column(
          8,
          plotOutput("vocabulary_analysis_plot")
        )
      ),
      fluidRow(
        h2(title_section_04),
        column(
          2,
          fluidRow(
            h3(tf_idf_title)
          ),
          fluidRow(
            class = "explanation",
            explain_tf_idf
          ),
          fluidRow(
            column(
              12,
              align = "center",
              downloadButton(
                "download_graph_tf_idf",
                label = "Download this Graph",
                class = "button"
              )
            )
          )
        ),
        column(
          4,
          plotOutput("tf_idf", width = "480px", height = "853px")
        ),
        column(
          2,
          fluidRow(
            h3(search_tf_idf)
          ),
          fluidRow(
            uiOutput("search_it_idf_box")
          ),
          fluidRow(
            column(
              12,
              align = "center",
              downloadButton(
                "download_table_tf_idf",
                label = "Download this Table",
                class = "button"
              )
            )
          )
        ),
        column(
          4,
          fluidRow(
            h3(textOutput("search_tf_idf_word"))
          ),
          fluidRow(
            class = "table",
            tableOutput("search_it_idf_table")
          )
        )
      )
    ),
    tabPanel("Sentiment",
      fluidRow(
        h1(title_tabs)
      ),
      fluidRow(
        h2(title_section_05)
      ),
      fluidRow(
        column(
          2,
          fluidRow(
            h3(sentiment_dictionaries)
          ),
          fluidRow(
            column(
              12,
              align = "center",
              selectInput(
                "dictionaries", 
                "Dictionaries", 
                choices = vector_query("meta_data", "tables_dictio", "include_sentiments", "TRUE")
              ),
              card(
                max_height = 250,
                card_header(textOutput("title_dictionary")),
                card_body(textOutput("dictionary_description"))
              ),
              fluidRow(
                h3(edit_dictionary),
                selectInput("edit_dictionary", 
                            "Select Words", 
                            choices = NULL,
                            selected = NULL, 
                            multiple = TRUE, 
                            selectize = TRUE, 
                            width = NULL, 
                            size = NULL
                )
              ),
              downloadButton(
                "download_table_total_sentiment_sentence",
                label = "Download this Table",
                class = "button"
              )
            )
          )
        ),
        column(
          8,
          class = "selector_pad",
          DT::dataTableOutput(
            "sentimental_table"
          )
        ),
        column(
          2,
        )
      ),
      fluidRow(
        div(style = "height:100px;"),
        h3(sentiment_synthesis_graph)
      ),
      fluidRow(
        column(
          2,
          align = "center",
          downloadButton(
            "download_graph_sentiment_sentence",
            label = "Download this Graph",
            class = "button"
          )
        ),
        column(
          8,
          fluidRow(
            plotOutput("sentiment_graph_1")
          )
        )
      ),
      fluidRow(
        div(style = "height:100px;"),
        h3(sentiment_synthesis_table) 
      ),
      fluidRow(
        column(
          2,
          align = "center",
          downloadButton(
            "download_table_sentiment_sentence",
            label = "Download this Table",
            class = "button"
          )
        ),
        column(
          8,
          fluidRow(
            class = "selector_pad",
            DT::dataTableOutput(
              "sentimental_table_grouped"
            )
          )
        )
      )
    )
  )
)

# Define server #
server <- function(input, output) {
  
  # TAB 1 #
  
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
      geom_point(alpha = 0.05, size = 2.5, color = "#a00000")+
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
        panel.background = element_rect(fill = "#afd8d8"),
        panel.grid.major = element_line(color = "#b8b8b8"),
        panel.grid.minor = element_line(color = "#afd8d8"),
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
  
  # Most used word
  output$name_x <- renderText({
    nombre_x <- candidates_data$person[candidates_data$selection == candidate_x_name()]
    nombre_x
  })
  
  output$name_y <- renderText({
    nombre_y <- candidates_data$person[candidates_data$selection == candidate_y_name()]
    nombre_y
  })
  
  output$muw_x <- renderText({
    deborah <- most_used_word(candidate_both()$word, candidate_both()$proportion.x)
    deborah[which(!is.na(deborah))]
  })
  
  output$muw_y <- renderText({
    rachel <- most_used_word(candidate_both()$word, candidate_both()$proportion.y)
    rachel[which(!is.na(rachel))]
  })
  
  # Sentiment Analysis
  #Proportion of sentiment
  output$name_proportion_x <- renderText({
    candidate_x_name()
  })
  
  output$name_proportion_y <- renderText({
    candidate_y_name()
  })
  
  output$proportion_x <- renderTable({
    sentiment_x <- sentiment_total(input$candidate_1, debates_2024_uw_clean_nss, bing_sentiment)
    sentiment_x
  },
  bordered = TRUE,
  digits = 5,
  hover = TRUE
  )
  
  output$proportion_y <- renderTable({
    sentiment_y <- sentiment_total(input$candidate_2, debates_2024_uw_clean_nss, bing_sentiment)
    sentiment_y
  },
  bordered = TRUE,
  digits = 5,
  hover = TRUE
  )
  
  #Sentiment Graph
  output$sentiment_graph_x <- renderPlot({
    sentiment_g_x <- sentiment_graph(input$candidate_1, debates_2024_uw_clean_nss, bing_sentiment)
    sentiment_g_x
  })
  
  output$sentiment_graph_y <- renderPlot({
    sentiment_g_y <- sentiment_graph(input$candidate_2, debates_2024_uw_clean_nss, bing_sentiment)
    sentiment_g_y
  })
  
  # TAB 2 #
  # Vocabulary Synthesis
  output$vocabulary_analysis_plot <- renderPlot({
    analytical_graph <- if(input$vocabulary_analysis == vocabulary_selector[1]){
      word_counter_by_participant_gr(word_counter_by_participant(debates_2024), person_colors)
    } else if(input$vocabulary_analysis == vocabulary_selector[2]){
      unique_words_graph(unique_words(debates_2024, all_participants), person_colors)
    } else if(input$vocabulary_analysis == vocabulary_selector[3]){
      vocabulary_diversity_graph(vocabulary_diversity(word_counter_by_participant(debates_2024), number_of_unique_words_person_and_date(unique_words(debates_2024, all_participants))), person_colors)
    }
    analytical_graph
  })
  
  output$title_explain_synthesis_vocabulary <- renderText({
    X <- if(input$vocabulary_analysis == vocabulary_selector[1]){
      vocabulary_selector[1]
    } else if(input$vocabulary_analysis == vocabulary_selector[2]){
      vocabulary_selector[2]
    } else if(input$vocabulary_analysis == vocabulary_selector[3]){
      vocabulary_selector[3]
    }
    X
  })
  
  # TF-IDF
  # General Table
  output$explain_synthesis_vocabulary <- renderText({
    X <- if(input$vocabulary_analysis == vocabulary_selector[1]){
      explain_1_number_of_words
    } else if(input$vocabulary_analysis == vocabulary_selector[2]){
      explain_2_unique_words
    } else if(input$vocabulary_analysis == vocabulary_selector[3]){
      explain_3_vocabulary_diversity
    }
    X
  })
  
  output$tf_idf <- renderPlot({
    X <- tf_idf_gr(tf_idf_table(word_counter_neat(debates_2024, person)), person_colors)
    X
  })
  
  # Search by Word
  output$search_it_idf_box <- renderUI({
    important_words <- unique(select(tf_idf_table(word_counter_neat(debates_2024, person)), word))
    selectInput(inputId = "search_it_idf_box",
                label = "Search", 
                choices = sort(important_words$word)
    )
  })
  
  # Search by Word Table
  output$search_it_idf_table <- renderTable({
    search_by_word_table(tf_idf_table(word_counter_neat(debates_2024, person)), input$search_it_idf_box)
  },
  bordered = TRUE,
  digits = 5,
  hover = TRUE)
  
  # Title of the IF-IDF table
  searched_word <- reactive({
    input$search_it_idf_box
  })
  
  observeEvent(
    input$search_it_idf_box, 
    {output$search_tf_idf_word <- renderText({
      paste0(
        search_tf_idf_table_1, 
        str_to_title(searched_word())
        )
      })
  })
  
  output$download_graph_tf_idf <- downloadHandler(
    filename <- function(){
      paste("tf_idf_graph", Sys.Date(), ".png", sep = "")
    },
    content = function(con){
      ggsave(
        con,
        plot = tf_idf_gr(tf_idf_table(word_counter_neat(debates_2024, person)), person_colors),
        device = "png",
        scale = 1
      )
    }
  )
  
  output$download_table_tf_idf <- downloadHandler(
    filename <- function(){
      paste("tf_idf_table", Sys.Date(),"_", input$search_it_idf_box, ".csv", sep = "")
    },
    content = function(con){
      write.csv(
        search_by_word_table(tf_idf_table(word_counter_neat(debates_2024, person)),  input$search_it_idf_box),
        con
      )
    }
  )
  
  # Third tab
  # working_dictionary <- reactive({
  #   vector_query(input$dictionaries)
  # })
  
  # Sentiments Selector
  output$title_dictionary <- renderText({
    cap_letter(input$dictionaries)
  })
  
  dictionary_description <- reactive({
    vector_query("meta_data", "description", "tables_dictio", input$dictionaries)
  })
  
  observeEvent(dictionary_description(), {
     descriptio <- dictionary_description()
     output$dictionary_description <- renderText({descriptio})
  })
  
  selected_sentiments <- reactive({
    df_query(input$dictionaries)
  })
  
  
  # Create the sentiments' table
  collected_sentiments <- reactive({
    collect_sentiments(debates_2024, selected_sentiments(), negative_words)
  })
  
  # Displaying the sentiments' table
  output$sentimental_table <- DT::renderDataTable(collected_sentiments())
  
  # Graphic of the table
  # Grouped table
  collected_sentiments_grouped <- reactive({
    grouped_table_sentiments(collected_sentiments(), person)
  })
  
  output$sentimental_table_grouped <- DT::renderDataTable(collected_sentiments_grouped())
  
  output$sentiment_graph_1 <- renderPlot(grouped_graph_sentiments(collected_sentiments_grouped()))
  
  output$download_table_total_sentiment_sentence <- downloadHandler(
    filename <- function(){
      paste("total_sentiment_sentence_table_", Sys.Date(), ".csv", sep = "")
    },
    content = function(con){
      write.csv(
        collected_sentiments(),
        con
      )
    }
  )
  
  output$download_graph_sentiment_sentence <- downloadHandler(
    filename <- function(){
      paste("sentiment_sentence_graph_", Sys.Date(), ".png", sep = "")
    },
    content = function(con){
      ggsave(
        con,
        plot = grouped_graph_sentiments(collected_sentiments_grouped()),
        device = "png",
        units = "cm",
        width = 32,
        height = 18,
        dpi = 300,
        scale = 1
      )
    }
  )
  
  output$download_table_sentiment_sentence <- downloadHandler(
    filename <- function(){
      paste("sentiment_sentence_table_", Sys.Date(), ".csv", sep = "")
    },
    content = function(con){
      write.csv(
        collected_sentiments_grouped(),
        con
      )
    }
  )
  
  observeEvent(input$dictionaries,{
    choices <- edit_dictionay_f("word", input$dictionaries)
    updateSelectInput(inputId = "edit_dictionary", choices = choices)
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
