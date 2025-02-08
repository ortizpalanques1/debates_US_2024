# Functions ####

# p Value
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

# Most Used Word (Only One)
most_used_word <- function(data, columna){
  parlatio <- data[columna == max(columna, na.rm = TRUE)]
  return(parlatio)
}

# Sentiment Analysis
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

#Sentiment Graph
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

# Word Counter by Participant
word_counter_by_participant <- function(file){
  the_processed_file <- file %>% 
    filter(!grepl("^\\(", transcript)) %>% 
    unnest_tokens(word, transcript) %>% 
    group_by(the_date, person) %>% 
    summarise(Words = n()) %>% 
    arrange(desc(Words), .by_group = TRUE)
  return(the_processed_file)
}

# Word Counter by Participant Graph
word_counter_by_participant_gr <- function(file, the_colors){
  the_graph <- ggplot(file, aes(x = reorder(person, Words), y = Words, fill = person, label = Words))+
    geom_col(width = 0.9)+
    scale_y_continuous(expand = c(0,0), limits = c(0,9000))+
    facet_wrap(~the_date, nrow = 3, scales = "free")+
    scale_fill_manual(values = the_colors)+
    coord_flip()+
    geom_text(aes(y=700), fontface="bold", color="white", size = 6, hjust = 1)+
    labs(
      title = "2024 US Elections: Words by Candidate/Debate",
      caption = "Sources: CNN, MSN, Usa Today"
    )+
    theme(
      axis.title.y = element_blank(),
      axis.text = element_text(family =  "TT Times New Roman", face = "bold", color = "black", size = 10),
      axis.ticks = element_blank(),
      legend.position = "off",
      strip.background = element_rect(fill = "#1E2952"),
      strip.text = element_text(family =  "TT Times New Roman", face = "bold", color = "white", size = 14),
      panel.background = element_rect(fill = "white"),
      panel.grid = element_line(color = "white"),
      plot.background = element_rect(fill = "white"),
      plot.title = element_text(family =  "TT Times New Roman", face = "bold", color = "black", size = 24, hjust = 0.5),
      plot.title.position = "plot",
      title = element_text(color = "black")
    )
  return(the_graph)
}

# Unique Words
unique_words <- function(file, people){
  debates_2024_uw <- file %>% 
    filter(!grepl("^\\(", transcript)) %>% 
    filter(!person %in% people$person[people$candidacy == "Journalist"]) %>% 
    unnest_tokens(word, transcript) %>% 
    #anti_join(stop_words) %>% 
    group_by(the_date, person) %>% 
    count(word) %>% 
    summarise(unique_words = n())
  return(debates_2024_uw)
}

#Unique Words Graph
unique_words_graph <- function(file, the_colors){
  debates_2024_uw_graph <- file %>% 
    mutate(candidate_debate = paste0(person,
                                     "\n",
                                     format(as.Date(the_date, format="%Y-%m-%d"),"%B"))) %>% 
    ggplot(aes(x = reorder(candidate_debate, unique_words), y = unique_words, fill = person, label = unique_words))+
    geom_col()+
    geom_text(aes(y=700), fontface="bold", color="white", hjust = 1, size = 10)+
    scale_y_continuous(expand = c(0,0), limits = c(0,1600))+
    labs(
      title = "Number of Unique Words by Candidate and Debate",
      y = "Unique Words"
    )+
    scale_fill_manual(values = the_colors)+
    coord_flip()+
    theme(
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(family =  "TT Times New Roman", face = "bold", color = "black", size = 14),
      legend.position = "off",
      panel.background = element_rect(fill = "white"),
      panel.grid = element_line(color = "white"),
      plot.background = element_rect(fill = "white"),
      plot.title = element_text(family =  "TT Times New Roman", face = "bold", color = "black", size = 24, hjust = 0.5),
      plot.title.position = "plot",
      title = element_text(color = "black")
    )
  return(debates_2024_uw_graph)
}

# Number of Unique words (debates_2024_uw_special)
# The parameter for this function is: unique_words(debates_2024, all_participants)
number_of_unique_words_person_and_date <- function(file){
  debates_2024_uw_special <- file %>% 
    mutate(candidate_debate_join = paste0(person, " ", the_date))
  return(debates_2024_uw_special)
}

# Vocabulary Diversity
vocabulary_diversity <- function(total_words, unique_words){
  debates_2024_vocabulary_diversity_V2 <- total_words %>% 
    mutate(candidate_debate_join = paste0(person, " ", the_date)) %>% 
    right_join(., unique_words[, c("unique_words", "candidate_debate_join")], by = "candidate_debate_join") %>% 
    mutate(vocabulary_diversity = round(unique_words/Words,2),
           candidate_debate = paste0(person,
                                     "\n",
                                     format(as.Date(the_date, format="%Y-%m-%d"),"%B")))
  return(debates_2024_vocabulary_diversity_V2)
}

# Vocabulary Diversity Graph
vocabulary_diversity_graph <- function(file, the_colors){
  debates_2024_vocabulary_diversity_graph <- ggplot(
    file, 
    aes(
      x = reorder(candidate_debate, -vocabulary_diversity),
      y = vocabulary_diversity,
      fill = person,
      label = vocabulary_diversity)
  )+
    geom_col()+
    geom_text(aes(y=0.10), fontface="bold", color="white", hjust = 0.5, size = 8)+
    scale_fill_manual(values = the_colors)+
    scale_y_continuous(expand = c(0,0), limits = c(0,max(file$vocabulary_diversity)*1.05))+
    labs(
      title = "Vocabulary Diversity. 2024 US Candidates/Debate",
      subtitle = "Number of Unique Words Divided by the Total Number of Words",
      y = "Vocabulary Diversity"
    )+
    theme(
      axis.title.x = element_blank(),
      axis.text = element_text(family =  "TT Times New Roman", face = "bold", color = "black"),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 16),
      axis.ticks.y = element_line(color = "black"),
      legend.position = "off",
      panel.background = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white"),
      plot.title = element_text(family =  "TT Times New Roman", face = "bold", color = "black", size = 24, hjust = 0.5),
      plot.subtitle = element_text(family =  "TT Times New Roman", face = "bold", color = "black", hjust = 0.5),
      plot.title.position = "plot",
      title = element_text(color = "black")
    )
  return(debates_2024_vocabulary_diversity_graph)
}

# Word Frequency
# 1. The parameter for this function is a data frame containing: text and identifiers 
# (author, name of the book, etc.). the file "debates_2024" for the present case.
# 2. The grouping variable: person in the examples
word_counter_neat <- function(file, the_grouping_var){
  
  this_grouping_var <- deparse(substitute(the_grouping_var))
  
  the_processed_file <- file %>% 
    filter(!grepl("^\\(", transcript)) %>% 
    unnest_tokens(word, transcript) %>% 
    mutate(word = str_replace(word, "’", "'")) %>% 
    count(person, word, sort = TRUE)
  
  total_words <- the_processed_file %>%
    group_by(get(this_grouping_var)) %>%
    summarize(total = sum(n)) 
  
  colnames(total_words) <- c(this_grouping_var, "total")
   
  document_words <- left_join(the_processed_file, total_words, by = this_grouping_var)
  
  return(document_words)
}

# TF*IDF Function
# The parameter is the result of the function "word_counter_neat."
# Example: tf_idf_table(word_counter_neat(debates_2024, person))
tf_idf_table <- function(file){
  colnames(file)[1] <- "X"
  this_tf_idf <- file %>% 
    bind_tf_idf(word, X, n) %>% 
    arrange(desc(tf_idf))
  return(this_tf_idf)
}

# Graph TF*IDF
# Example: tf_idf_gr(tf_idf_table(word_counter_neat(debates_2024, person)), person_colors)
# Reference: https://juliasilge.github.io/tidytext/reference/reorder_within.html
tf_idf_gr <- function(file, the_colors){
  this_graph <- file %>%
    #mutate(word = reorder_within(word, tf_idf, X))  %>% 
    group_by(X) %>%
    slice_max(tf_idf, n = 4) %>%
    ungroup() %>%
    ggplot(aes(tf_idf, reorder_within(word, tf_idf, X), fill = X)) +
    geom_col(show.legend = FALSE, width = 0.8, position = position_dodge(1.5)) +
    facet_wrap(~X, ncol = 2, scales = "free_y") +
    scale_fill_manual(values = the_colors)+
    scale_y_reordered()+
    labs(
      title = "Most Relevant Words by Participant",
      subtitle = "Four Highest tf-idf values",
      x = "tf-idf", 
      y = NULL
    )+
    theme(
      axis.text = element_text(family =  "TT Times New Roman", face = "bold", color = "black"),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 16),
      axis.ticks.y = element_line(color = "black"),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      plot.subtitle = element_text(size = 16, hjust = 0.5),
      plot.title = element_text(family =  "TT Times New Roman", face = "bold", color = "black", size = 24, hjust = 0.5),
      plot.title.position = "plot", 
      strip.background = element_rect(fill = "#1E2952"),
      strip.text = element_text(family =  "TT Times New Roman", face = "bold", color = "white", size = 14)
    )
  return(this_graph)
}

# Search by Word Table
# Example: search_by_word_table(tf_idf_table(word_counter_neat(debates_2024, person)), selection)
# Notice that the grouping variable was determined in the function tf_idf_table
search_by_word_table <- function(file, selection){
  this_table <- file %>% 
    group_by(X) %>% 
    mutate(rank = row_number()) %>% 
    ungroup() %>% 
    filter(word == selection) %>% 
    select(X, rank, tf_idf)
  colnames(this_table) <- c("Group", "Rank", "TF-IDF")
  return(this_table)
}




# Texts ####
# Tab 1 and global texts
correlatio <- "Pearson's Correlation"
explain_graph <- "This graph shows the proportion of relevant words for each candidate.\nBecause we use a logarithmic scale many dots seem closer than they really are.\nFor this reason, the intensity of the dot's color denotes how close both proportions are."
explain_graph_title <- "What This Graph Represents"
most_used_word_text <- "Candidates' Most Used Word"
selector_title <- "Select Two Candidates"
selector_title_vocabulary <- "General Analysis"
the_pvalue <- "p Value"
title_tabs <- "United States Presidential Debates (2024)"
title_section_01 <- "General Comparison"
title_section_02 <- "Sentiment Analysis"


#Tab2
title_section_03 <- "Synthesis of the Vocabulary"
title_section_04 <- "TF-IDF Analysis"
explain_1_number_of_words <- "The numbers in each bar represent the total number of words used by each participant in the debates."
explain_2_unique_words <- "The numbers in each bar represent the total number of unique words pronunced by each candidate. This means that if you use the word \'people\' 10 times in your discourse and \'atomic'\ just once, Unique Words will count each as one regardless their frequency."
explain_3_vocabulary_diversity <- "Vocabulary Diversity is a rough measure of the proportion of unique words. It is obtained by simply dividing Unique Words between Total Words (UW/TW). The measure ranges between 1 (all the words were different) and 1/TW (the person repeated just one word)."
search_tf_idf <- "Search TF-IDF of a Word"

# Variables ####
# Selector Vocabulary tab
vocabulary_selector <- c("Number of Words", "Unique Words", "Vocabulary Diversity")
