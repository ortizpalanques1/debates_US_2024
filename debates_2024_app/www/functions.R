# Functions ####

pvalue <- function(x){
  tryCatch(
    {
      if(x < 0.001){
        this_text <- "p < 0.001"
      }else{
        this_text <- paste0("p = ", x)
      }
    },
      error=function(e) {
        message('Working')
        cat("Working")
      }
  )
}

rvalue <- function(x){
  paste0("r = ", round(x,2))
}


most_used_word <- function(data, columna){
  parlatio <- data[columna == max(columna, na.rm = TRUE)]
  return(parlatio)
}


sentiment_total <- function(this_debate, texto, lexic_00){
  
  this_name <- str_extract(this_debate, "(?:(?! [[:digit:]]).)*")
  #print(this_name)
  this_date <- str_extract(this_debate, "[[:digit:]].*")
  #print(this_date)
  
  this_debate <- texto %>% 
    filter(person == this_name & the_date == this_date)
  #print(this_debate)
  
  total_words <- sum(this_debate$n, na.rm = TRUE)
  #print(total_words)
  
  sentimental_words <- this_debate %>% 
    inner_join(lexic_00, by = "word") %>% 
    group_by(sentiment) %>% 
    summarise(n = sum(n)) %>% 
    mutate(percentage = round((n/total_words*100),2))
  #print(sentimental_words)
  
  colnames(sentimental_words) <- c("Sentiment", "Words", "Percentage")
  
  return(sentimental_words)
}

sentiment_graph <- function(this_debate, texto, lexic_00){
  
  this_name <- str_extract(this_debate, "(?:(?! [[:digit:]]).)*")
  #print(this_name)
  this_date <- str_extract(this_debate, "[[:digit:]].*")
  #print(this_date)
  
  this_debate <- texto %>% 
    filter(person == this_name & the_date == this_date)
  #print(this_debate)
  
  total_words <- sum(this_debate$n, na.rm = TRUE)
  #print(total_words)
  
  all_the_sentiment <- c("positive","negative")
  all_the_sentiment_colors <- c("#00ff41","#ebecf0")
  sentiment_colors <- setNames(all_the_sentiment_colors, all_the_sentiment)
  
  sentimental_graph <- this_debate %>% 
    inner_join(lexic_00, by = "word") %>% 
    group_by(sentiment) %>% 
    arrange(desc(n)) %>% 
    slice_head(n = 10) %>% 
    ungroup() %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(n, word, fill = sentiment)) +
    geom_col(show.legend = FALSE, width = .5) +
    facet_wrap(~sentiment, scales = "free_y", ncol = 1) +
    scale_fill_manual(values = sentiment_colors)+
    labs(title = paste0(this_name, " in ", this_date),
         x = NULL,
         y = NULL)+
    theme(
      axis.text = element_text(family =  "TT Times New Roman", face = "bold", color = "white", size = 14),
      panel.background = element_rect(fill = "#1E2952", color = "#1E2952"),
      panel.grid.major = element_line(color = "#1E2952"),
      panel.grid.minor = element_line(color = "#1E2952"),
      plot.background = element_rect(fill = "#1E2952"),
      plot.caption = element_text(hjust = 0, face= "italic"),
      plot.title.position = "plot",
      strip.background = element_rect(fill = "#1E2952"),
      strip.text = element_text(family =  "TT Times New Roman", colour = "white", face = "bold", size = 14),
      title = element_text(family =  "TT Times New Roman", face = "bold", color = "white", size = 14, hjust = 0)
    )
  
  return(sentimental_graph)
}

# Texts
correlatio <- "Pearson's Correlation"
most_used_word_text <- "Candidates' Most Used Word"
selector_title <- "Select Two Candidates"
explain_graph <- "This graph shows the proportion of relevant words for each candidate.\nBecause we use a logarithmic scale many dots seem closer than they really are.\nFor this reason, the intensity of the dot's color denotes how close both proportions are."
explain_graph_title <- "What This Graph Represents"
the_pvalue <- "p Value"
title_section_01 <- "General Comparison"
title_section_02 <- "Sentiment Analysis"