# Libraries ####
library(tidyverse)
library(tidytext)


# Files ####
negative_words <- read.csv("texts/english_negations.txt", header = TRUE)
bing_sentiment <- get_sentiments("bing")

# Obtaining sentences ####
# Function to create a tibble with sentences
# Prerequisites
# 1. The column with the text must have the name 'transcript'
# 2. The column with the document level text must have the name 'person'
sentences_corpus <- function(this_corpus){
  this_corpus %>%
    unnest_tokens(
      output = sentence,
      input = transcript,
      token = "sentences",
      to_lower = TRUE,
      drop = TRUE,
      collapse = NULL
    ) %>% 
    mutate(
      N_words = str_count(sentence, "\\S+"),
      T_words = sum(N_words),
      Sen_Cor = round((N_words/T_words), 5)
    ) %>% 
    group_by(person) %>% # This name represents the 'document' level
    mutate(
      N_document = sum(N_words),
      Sen_Doc = round((N_words/N_document), 5)
    ) %>% 
    ungroup() %>%
    mutate(
      sentimiento = NA,
      sentence_ID = row_number()
    )
}


# Assessment of the sentence
# This function runs inside 'collect_sentiments' (next function)
evaluador_palabras <- function(the_list, the_position, the_negative_position, the_dictionary){
  # 1. the_list = character (quoted) input with the name of the list.
  # 2. the_position = numeric input, element of the list that contains the words
  # 3. the_negative_position = numeric input, element of the list that marks the existence of a negation.
  #    the pointed vector contains only TRUE or FALSE.
  # 4. the_dictionary = data_frame with the sentiments: 
  #     check the column with words must have the name "word" and the column with sentiment, the
  #     name "sentiment"
  this_list <- get(the_list)[[the_position]]
  this_negative <- get(the_list)[[the_negative_position]]
  partial_evaluations <- ifelse(this_list %in% the_dictionary$word[the_dictionary$sentiment == "positive"],1,0)
  #print(paste0("list: ", this_list, "Negative: ", this_negative, "Result: ", partial_evaluations))
  veredict <- if(length(unique(partial_evaluations)) > 1){
    "indecisive"
  }else if(unique(partial_evaluations) == 1 & this_negative == FALSE){
    "likely positive"
  }else if(unique(partial_evaluations) == 1 & this_negative == TRUE){
    "likely negative"
  }else if(unique(partial_evaluations) == 0 & this_negative == TRUE){
    "likely positive"
  }else{
    "likely negative"
  }
  return(veredict)
}


# List of Rows with Sentiments ####
# Prerequisites
# 1. Use the function: sentence_corpus to create the 'by_sentence' data frame
# 2. The column for the words that are assessed in the sentiment dictionary must be
#    called 'word'
# 3. The negative words list is constant
collect_sentiments <- function(by_sentence, this_dictionary){
  those_sentiments_beta <- list()
  y = 0
  for(i in 1:nrow(by_sentence)){
    this_sentence <- c(unlist(str_split(by_sentence$sentence[i], "\\s+")))

    auxiliar_list <- list()
    if(length(this_sentence[this_sentence %in% this_dictionary$word]) > 0){
      y = y + 1
      auxiliar_list[[1]] <- by_sentence$sentence_ID[i]
      auxiliar_list[[2]] <- this_sentence[this_sentence %in% this_dictionary$word]
      print(auxiliar_list[[2]])
      auxiliar_list[[3]] <- ifelse(length(this_sentence[this_sentence %in% negative_words$word]) > 0, TRUE, FALSE)
      auxiliar_list[[4]] <- this_sentence[this_sentence %in% negative_words$word]
      print(auxiliar_list)
      auxiliar_list_beta <<- auxiliar_list
      auxiliar_list[[5]] <- evaluador_palabras("auxiliar_list_beta", 2, 3, bing_sentiment)
      those_sentiments_beta[[y]] <- auxiliar_list
    }
  }
  return(those_sentiments_beta)
}


# Running the functions ####
collected_sentiments <- collect_sentiments(sentences_corpus(debates_2024), bing_sentiment) 
collected_sentiments_df <- tibble(
  "sentence_ID" = sapply(collected_sentiments,"[[",1),
  "Negation" = sapply(collected_sentiments,"[[",3), 
  "Assessment" = sapply(collected_sentiments,"[[",5)
)  

collected_sentiments_final <- left_join(collected_sentiments_df, by_sentence_df, by = "sentence_ID") %>% 
  select(sentence_ID, person, sentence, Assessment, Negation, Sen_Cor, Sen_Doc)

# Test: failed and successful ####
# Run "sentences_corpus" as:
#by_sentence_df <- sentences_corpus(debates_2024)  

#evaluador_palabras("auxiliar_list", 2, 3, bing_sentiment)

# Extracting the Row Numbers for the filter ====
# Sentiment
# extraction_test <- sapply(those_sentiments,"[[",1)
# sentence_debate_with_sentiment <- sentences_debate %>% 
#   filter(sentence_ID %in% extraction_test) 
# 
# # Negative words
# extract_negative_sentiment <- NULL
# for(l in 1:length(those_sentiments)){
#   if(those_sentiments[[l]][[3]] == TRUE){
#     the_negation <- those_sentiments[[l]][[1]]
#     extract_negative_sentiment <- c(extract_negative_sentiment, the_negation)
#   }
# }

# sentence_debate_with_sentiment <- sentence_debate_with_sentiment %>% 
#   mutate(Negative = ifelse(sentence_debate_with_sentiment$sentence_ID %in% extract_negative_sentiment, TRUE, FALSE))

# Test: Creating string to assess ####
# the_master_filter <- 4
# the_master_filter_02 <- 8
# the_final_master_filter <- c(the_master_filter, the_master_filter_02)
# the_filter <- paste0("dplyr::filter(mtcars, carb !=  ", the_master_filter, ")")
# the_filter_02 <- paste0("mtcars[!(mtcars$carb %in% c(", paste(the_final_master_filter, collapse = ", "), ")),]")

# test_filters <-   eval(parse(text = paste0(the_filter)))
# test_filters <-   eval(parse(text = paste0(the_filter_02)))
# 
# 
# test_vector <- c("welcome", "greed")
# partial_evaluations <- ifelse(test_vector %in% bing_sentiment$word[bing_sentiment$sentiment == "positive"],1,0)



test_numeric <- c(5,4,6,3)
partial_evaluations <- ifelse(test_numeric > 4 ,1,0)
