# Libraries ####
library(tidyverse)
library(tidytext)

# Obtaining sentences ####

sentences_debate <- debates_2024 %>%
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
  group_by(person) %>% 
  mutate(
    N_document = sum(N_words),
    Sen_Doc = round((N_words/N_document), 5)
    #,
    #verifico = sum(Sen_Doc)
  ) %>% 
  ungroup() %>%
  mutate(
    sentimiento = NA,
    sentence_ID = row_number()
  )
  # mutate(
  #   verifico_Sen_Cor = sum(Sen_Cor)
  # ) 


# List of Rows with Sentiments ####
with_sentiment <- matrix(ncol = 1, nrow = length(sentences_debate$N_words))
these_sentiments <- list()
those_sentiments <- list()
y = 0
for(i in 1:length(sentences_debate$N_words)){
  this_sentence <- c(unlist(str_split(sentences_debate$sentence[i], "\\s+")))
  verifico <- ifelse(length(this_sentence[this_sentence %in% bing_sentiment$word]) > 0, TRUE, FALSE)
  verifico_string <- this_sentence[this_sentence %in% bing_sentiment$word]
  print(verifico)
  with_sentiment[i,1] <- verifico
  these_sentiments[[i]] <- verifico_string
  
  auxiliar_list <- list()
  if(length(this_sentence[this_sentence %in% bing_sentiment$word]) > 0){
    y = y + 1
    auxiliar_list[[1]] <- sentences_debate$line_number[i]
    auxiliar_list[[2]] <- this_sentence[this_sentence %in% bing_sentiment$word]
    those_sentiments[[y]] <- auxiliar_list
  }
}

# Test: Creating string to assess ####
the_master_filter <- 4
the_filter <- paste0("dplyr::filter(mtcars, carb ==  ", the_master_filter, ")")

test_filters <-   eval(parse(text = paste0(the_filter)))
