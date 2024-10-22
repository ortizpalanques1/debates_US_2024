# Task: Most Used Words
# Libraries ####
library(tidyverse)
library(tidytext)
data("stop_words")

# Most Used Words After stop_words ####
debates_2024_muw <- debates_2024 %>% 
  filter(!grepl("^\\(", transcript)) %>% 
  filter(!person %in% all_participants$person[all_participants$candidacy == "Journalist"]) %>% 
  unnest_tokens(word, transcript) %>% 
  filter(!grepl("[[:digit:]]+", word)) %>% 
  anti_join(stop_words) %>% 
  group_by(the_date, person) %>% 
  count(word) %>% 
  arrange(desc(n), .by_group = TRUE) 
