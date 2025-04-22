# Libraries ####
library(tidyverse)
library(tidytext)
library(RMariaDB)
library(odbc)
library(DBI)


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
collect_sentiments <- function(corpus, this_dictionary){
  by_sentence <- sentences_corpus(corpus)
  those_sentiments_beta <- list()
  y = 0
  for(i in 1:nrow(by_sentence)){
    this_sentence <- c(unlist(str_split(by_sentence$sentence[i], "\\s+")))

    auxiliar_list <- list()
    if(length(this_sentence[this_sentence %in% this_dictionary$word]) > 0){
      y = y + 1
      auxiliar_list[[1]] <- by_sentence$sentence_ID[i]
      auxiliar_list[[2]] <- this_sentence[this_sentence %in% this_dictionary$word]
      auxiliar_list[[3]] <- ifelse(length(this_sentence[this_sentence %in% negative_words$word]) > 0, TRUE, FALSE)
      auxiliar_list[[4]] <- this_sentence[this_sentence %in% negative_words$word]
      auxiliar_list_beta <<- auxiliar_list
      auxiliar_list[[5]] <- evaluador_palabras("auxiliar_list_beta", 2, 3, bing_sentiment)
      those_sentiments_beta[[y]] <- auxiliar_list
    }
  }
  collected_sentiments_df <- tibble(
    "sentence_ID" = sapply(those_sentiments_beta,"[[",1),
    "Negation" = sapply(those_sentiments_beta,"[[",3), 
    "Assessment" = sapply(those_sentiments_beta,"[[",5)
  ) 
  collected_sentiments_df <- left_join(collected_sentiments_df, by_sentence, by = "sentence_ID") %>% 
    select(sentence_ID, person, sentence, Assessment, Negation, Sen_Cor, Sen_Doc)
  return(collected_sentiments_df)
}


# Running the functions ####
collected_sentiments <- collect_sentiments(debates_2024, bing_sentiment) 

# The graph ####
this_grouped_file <- collected_sentiments %>% 
  group_by(person) %>% 
  summarise(Positive = sum(Sen_Doc[Assessment == "likely positive"]),
            Negative = sum(Sen_Doc[Assessment == "likely negative"]))

# Create sentiment dictionaries in MariaDB ####
# First, connection:
con <- dbConnect(
  drv = RMariaDB::MariaDB(), 
  dbname = "sentiments_dictionaries",
  username = "root",
  password = "Ciencia54", 
  host = "localhost", 
  port = 3306
)

# Afinn
# Download data frame
afinn <- get_sentiments("afinn") %>% 
  mutate("sentiment" = ifelse(value <= 0, "negative", "positive"),
         "value_abs" = abs(value))

# create database in mariaDB
the_query <- ("CREATE TABLE afinn ( 
              id_af INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
              word VARCHAR(30),
              value INT,
              sentiment VARCHAR(30),
              value_abs INT
              );")
dbSendQuery(con, the_query)

# upload dataframe to database
dbAppendTable(con, "afinn", afinn, row.names = NULL)


# NRC
# Download data frame
nrc <- get_sentiments("nrc")

# create database in mariaDB
the_query <- ("CREATE TABLE nrc ( 
              id_nr INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
              word VARCHAR(30),
              sentiment VARCHAR(30)
              );")
dbSendQuery(con, the_query)

# upload dataframe to database
dbAppendTable(con, "nrc", nrc, row.names = NULL)

# Append both tables in meta_data table
tables <- c("afinn", "nrc")
language <- "English"
types <- "Sentiment"
descriptio_afinn <- " \"AFINN is a list of words rated for valence with an integer between minus five (negative) and plus five (positive). Sentiment analysis is performed by cross-checking the string tokens (words, emojis) with the AFINN list and getting their respective scores. The comparative score is simply: sum of each token / number of tokens\" (https://www.npmjs.com/package/sentiment). URL: http://www2.imm.dtu.dk/pubdb/views/publication_details.php?id=6010. License: Open Database License (ODbL) v1.0 "
descriptio_nrc <- " \"The NRC Emotion Lexicon is a list of English words and their associations with eight basic emotions (anger, fear, anticipation, trust, surprise, sadness, joy, and disgust) and two sentiments (negative and positive). The annotations were manually done by crowdsourcing\" (https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm). Visit the page to understand the \"TERMS OF USE\""
descriptions <- c(descriptio_afinn, descriptio_nrc)

append_data <- data.frame(
  tables_dictio = tables,
  description = descriptions,
  language_dictio = language,
  type_dictio = "Sentiment",
  include_sentiments = TRUE
)
dbAppendTable(con, "meta_data", append_data, row.names = NULL)
