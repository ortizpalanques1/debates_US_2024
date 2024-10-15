# Task: Count the words
# Libraries ####
library(tidyverse)
library(tidytext)

# Create the tibble by word ####
debates_2024_by_word <- debates_2024 %>% 
  filter(!grepl("^\\(", transcript)) %>% 
  unnest_tokens(word, transcript) %>% 
  group_by(the_date, person) %>% 
  summarise(Words = n()) %>% 
  arrange(desc(Words), .by_group = TRUE) %>% 
  ggplot(aes(reorder(person, Words), Words))+
  geom_col()+
  facet_wrap(~the_date, nrow = 3, scales = "free")+
  coord_flip()
  
