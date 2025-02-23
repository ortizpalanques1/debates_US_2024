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

for(i in 1:length(sentences_debate$N_words)){
  this_sentence <- c(unlist(str_split(sentences_debate$sentence[362], "\\s+")))
  verifico <- this_sentence[this_sentence %in% bing_sentiment$word]
}
