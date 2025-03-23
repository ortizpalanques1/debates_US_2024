# Libraries ####
library(tidyverse)
library(tidytext)


# Files ####
negative_words <- read.csv("texts/english_negations.txt", header = TRUE)
bing_sentiment <- get_sentiments("bing")

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
  group_by(person) %>% # This name must be changed with a parameter for the function
  mutate(
    N_document = sum(N_words),
    Sen_Doc = round((N_words/N_document), 5)
  ) %>% 
  ungroup() %>%
  mutate(
    sentimiento = NA,
    sentence_ID = row_number()
  )


# List of Rows with Sentiments ####
those_sentiments <- list()
y = 0
for(i in 1:length(sentences_debate$N_words)){
  this_sentence <- c(unlist(str_split(sentences_debate$sentence[i], "\\s+")))
  
  auxiliar_list <- list()
  if(length(this_sentence[this_sentence %in% bing_sentiment$word]) > 0){
    y = y + 1
    auxiliar_list[[1]] <- sentences_debate$sentence_ID[i]
    auxiliar_list[[2]] <- this_sentence[this_sentence %in% bing_sentiment$word]
    auxiliar_list[[3]] <- ifelse(length(this_sentence[this_sentence %in% negative_words$word]) > 0, TRUE, FALSE)
    auxiliar_list[[4]] <- this_sentence[this_sentence %in% negative_words$word]
    those_sentiments[[y]] <- auxiliar_list
  }
}

sentiments_table <- data.frame(
  "sentence_id" = sapply(those_sentiments,"[[",1),
  "Negation" = sapply(those_sentiments,"[[",3)
)

# Extracting the Row Numbers for the filter ####
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

sentence_debate_with_sentiment <- sentence_debate_with_sentiment %>% 
  mutate(Negative = ifelse(sentence_debate_with_sentiment$sentence_ID %in% extract_negative_sentiment, TRUE, FALSE))

# Test: Creating string to assess ####
the_master_filter <- 4
the_master_filter_02 <- 8
the_final_master_filter <- c(the_master_filter, the_master_filter_02)
the_filter <- paste0("dplyr::filter(mtcars, carb !=  ", the_master_filter, ")")
the_filter_02 <- paste0("mtcars[!(mtcars$carb %in% c(", paste(the_final_master_filter, collapse = ", "), ")),]")

test_filters <-   eval(parse(text = paste0(the_filter)))
test_filters <-   eval(parse(text = paste0(the_filter_02)))

## Quitar ####
# "vice", "trump"
