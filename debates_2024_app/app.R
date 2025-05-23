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
              actionButton("activate_dictionary", label = "Create Table",  class = "button_editor"),
              card(
                max_height = 250,
                card_header(textOutput("title_dictionary")),
                card_body(textOutput("dictionary_description"))
              ),
              fluidRow(
                h3(edit_dictionary)
              ),
              fluidRow(
                style = "height: 200px; overflow-y: auto; padding-left: 20px;", 
                align = "left",
                checkboxGroupInput("edit_dictionary", 
                            "Select Words", 
                            choices = NULL,
                            selected = NULL, 
                            width = NULL, 
                ),
              ),
              fluidRow(
                actionButton("editor", label = "Without These Words")
              ),
              fluidRow(
                downloadButton(
                  "download_table_total_sentiment_sentence",
                  label = "Download this Table",
                  class = "button"
                )
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
          class = "edit_area",
          2,
          uiOutput("edit_sentiment_table")
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
  
  # Third tab ###################################################
  
  #########             #
  #########             #
        ###   ###    ####
        ###   #      #  #
  #########   #      #### 
  #########
         ##
         ##
  #########
  #########
  
  # Sentiments Selector ##########################################
  
  
  {
    # CREATES THE MAIN TABLE AND ITS CORRESPONDING UI
    observeEvent(input$activate_dictionary,{
      #GENERAL SECTION
      # Function & Query: data frame with the sentiments of the selected dictionary
      selected_sentiments <- df_query(input$dictionaries)
      # Function: data frame with the sentiment measures
      collected_sentiments <- collect_sentiments(debates_2024, selected_sentiments, negative_words)
      # Shiny output of collected_sentiments
      output$sentimental_table <- DT::renderDataTable(collected_sentiments, selection = list(mode = "single", target = "cell"))
      # Variable: title of the dictionary in the description area
      output$title_dictionary <- renderText({cap_letter(input$dictionaries)})
      # Function & Query: description of the dictionary in the description area
      dictionary_description <- vector_query("meta_data", "description", "tables_dictio", input$dictionaries)
      # Shiny output of the description of the dictionary in the description area
      output$dictionary_description <- renderText({dictionary_description})
      # Function: collected sentiments grouped by theme (for instance: positive and negative)
      collected_sentiments_grouped <- grouped_table_sentiments(collected_sentiments, person)
      # Shiny output: table with the sentiments already grouped
      output$sentimental_table_grouped <- DT::renderDataTable(collected_sentiments_grouped)
      # Shiny output: graphic of the sentiments already grouped
      output$sentiment_graph_1 <- renderPlot(grouped_graph_sentiments(collected_sentiments_grouped))
      # Shiny output: download collected sentiments table
      output$download_table_total_sentiment_sentence <- downloadHandler(
        filename <- function(){
          paste("total_sentiment_sentence_table_", Sys.Date(), ".csv", sep = "")
        },
        content = function(con){
          write.csv(
            collected_sentiments,
            con
          )
        }
      )
      # Shiny output: download collected sentiments grouped table
      output$download_graph_sentiment_sentence <- downloadHandler(
        filename <- function(){
          paste("sentiment_sentence_graph_", Sys.Date(), ".png", sep = "")
        },
        content = function(con){
          ggsave(
            con,
            plot = grouped_graph_sentiments(collected_sentiments_grouped),
            device = "png",
            units = "cm",
            width = 32,
            height = 18,
            dpi = 300,
            scale = 1
          )
        }
      )
      # Shiny output: download collected sentiments grouped graphic
      output$download_table_sentiment_sentence <- downloadHandler(
        filename <- function(){
          paste("sentiment_sentence_table_", Sys.Date(), ".csv", sep = "")
        },
        content = function(con){
          write.csv(
            collected_sentiments_grouped,
            con
          )
        }
      )
      # END GENERAL SECTION
      
      # UI CREATED
      # Shiny output: dictionary selector for edition of sentiments
      output$edit_sentiment_table <- renderUI(
        if(input$dictionaries == "bing"){
          tagList(
            actionButton("start_table_editor", label = "Edit This Table", class = "button_editor"),
            selectInput(inputId = "all_outputs",
                        label = "Edit the Assessment Column",
                        choices = sort(bing_table_output)
            ),
            actionButton("sentiment_table_editor", label = "Change", class = "button_editor"),
          )
        }else if(input$dictionaries == "afinn"){
          print("This dictionary cannot be edited. Afinn")
        } else {
          print("This dictionary is not editable. NRC")
        }
      )
      # END UI CREATED
    })
    # EDIT ROUTINES
  
    # Variable: empty data frame to start the edit routine
    editable_sentimental_table <- reactiveValues(
      df_data = NULL
    )
    # Table is filled with the function
    observeEvent(input$start_table_editor,{
      selected_sentiments <- df_query(input$dictionaries)
      editable_sentimental_table$df_data <- collect_sentiments(debates_2024, selected_sentiments, negative_words)
    })
    # Change values in table
    observeEvent(input$sentiment_table_editor, {
      editor_table <- editable_sentimental_table$df_data
      output$sentimental_table <- DT::renderDataTable(editor_table, selection = list(mode = "single", target = "cell"))
      editor_table[input$sentimental_table_cells_selected[1], input$sentimental_table_cells_selected[2]] <- input$all_outputs
      collected_sentiments_grouped_edited <- reactive({
        grouped_table_sentiments(editor_table, person)
      })
      
      
      output$sentimental_table_grouped <- DT::renderDataTable(collected_sentiments_grouped_edited())
      output$sentiment_graph_1 <- renderPlot(grouped_graph_sentiments(collected_sentiments_grouped_edited()))
      editable_sentimental_table$df_data <- editor_table
      
      output$download_table_total_sentiment_sentence <- downloadHandler(
        filename <- function(){
          paste("total_sentiment_sentence_table_", Sys.Date(), ".csv", sep = "")
        },
        content = function(con){
          write.csv(
            editor_table,
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
            plot = grouped_graph_sentiments(collected_sentiments_grouped_edited()),
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
            collected_sentiments_grouped_edited(),
            con
          )
        }
      )
    }
    )
  }    
  
  
  
  # Selecting Edit Dictionary Section ###########################################
  # Selecting words to be edited from a dictionary
  observeEvent(input$activate_dictionary,{
    choices <- edit_dictionay_f("word", input$dictionaries)
    updateCheckboxGroupInput (inputId = "edit_dictionary", choices = choices$word)
  })
  
  # Button to give the new values in general table, grouped table and graph once
  # values to eliminate are selected
  observeEvent( input$editor, {
    isolate({
      the_edited_vector <- as.vector(input$edit_dictionary)
    })
    #print(the_edited_vector)
    #print(class(the_edited_vector))
    my_selected_vector <- edited_dictionay_f("*", input$dictionaries, "word", the_edited_vector)
    #print(my_selected_vector[1:10,])
    collected_sentiments_dictionary <- reactive({
      collect_sentiments(debates_2024, my_selected_vector, negative_words)
    })
    output$sentimental_table <- DT::renderDataTable(collected_sentiments_dictionary(), selection = list(mode = "single", target = "cell"))
    collected_sentiments_grouped <- reactive({
      grouped_table_sentiments(collected_sentiments_dictionary(), person)
    })
    output$sentimental_table_grouped <- DT::renderDataTable(collected_sentiments_grouped())
    output$sentiment_graph_1 <- renderPlot(grouped_graph_sentiments(collected_sentiments_grouped()))
    output$download_table_total_sentiment_sentence <- downloadHandler(
      filename <- function(){
        paste("total_sentiment_sentence_table_", Sys.Date(), ".csv", sep = "")
      },
      content = function(con){
        write.csv(
          collected_sentiments_dictionary(),
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
    ############################################
    # Sub routine Edit inside Dictionary #######
    output$edit_sentiment_table <- renderUI(
      if(input$dictionaries == "bing"){
        tagList(
          actionButton("start_table_editor_dictionary", label = "Edit This Table", class = "button_editor"),
          selectInput(inputId = "all_outputs",
                      label = "Edit the Assessment Column",
                      choices = sort(bing_table_output)
          ),
          actionButton("sentiment_table_editor_dictionary", label = "Change", class = "button_editor")
        )
      }else if(input$dictionaries == "afinn"){
        print("This dictionary cannot be edited. Afinn")
      } else {
        print("This dictionary is not editable. NRC")
      }
    )
    
    # Bing edition functions
    # Empty table
    editable_sentimental_table <- reactiveValues(
      df_data = NULL
    )
    
    # Table is filled with the function 
    observeEvent(input$start_table_editor_dictionary,{
      editable_sentimental_table$df_data <- collect_sentiments(debates_2024, my_selected_vector, negative_words)
    })
    
    # Change values in table
    observeEvent(input$sentiment_table_editor_dictionary, {
      editor_table <- editable_sentimental_table$df_data
      output$sentimental_table <- DT::renderDataTable(editor_table, selection = list(mode = "single", target = "cell"))
      editor_table[input$sentimental_table_cells_selected[1], input$sentimental_table_cells_selected[2]] <- input$all_outputs
      collected_sentiments_grouped_edited <- reactive({
        grouped_table_sentiments(editor_table, person)
      })
      output$sentimental_table_grouped <- DT::renderDataTable(collected_sentiments_grouped_edited())
      output$sentiment_graph_1 <- renderPlot(grouped_graph_sentiments(collected_sentiments_grouped_edited()))
      editable_sentimental_table$df_data <- editor_table
      
      output$download_table_total_sentiment_sentence <- downloadHandler(
        filename <- function(){
          paste("total_sentiment_sentence_table_", Sys.Date(), ".csv", sep = "")
        },
        content = function(con){
          write.csv(
            editor_table,
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
            plot = grouped_graph_sentiments(collected_sentiments_grouped_edited()),
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
            collected_sentiments_grouped_edited(),
            con
          )
        }
      )
    }
    )
    
    # End sub routine Edit inside Dictionary #######
    ########################################################################
   }
  )
  
# FINAL  
} # Run the application 
shinyApp(ui = ui, server = server)
