# 3. Number of relevant words.
# Libraries
source("scripts/most_used_words.R")
library(tidyverse)
library(tidytext)
data("stop_words")

# 3.1. Improve the stop words: no numbers, no personal pronouns, keep the verbs.
# For this task we have two files: "stop_words" from tidyverse and "texts/stopwords.txt." In their current form
# their utility is low. We need verbs to state or deny actions.
# Additionally, in general, we have to get rid of numbers, pronouns with an apostrophe and join the words with Saxo 
# Genitive with their corresponding original.

# 3.1.1. Keep Verbs. Is it pointless to clean the stop words from verbs.
# 3.1.2. Get rid of numbers: We don't touch the stop words files. Instead, we introduce a regex to avoid any Arabic cardinal number.
# 3.1.3. Delete pronouns with apostrophe.
# 3.1.4. Join the words with Saxo Genitive with their corresponding original.

# Create the stop words special instead of using the usual stop words. This new stop words will contain:
# 3.1.3.1. Pronouns with an apostrophe (contracted pronouns) for to be, to have, would (only included modal verb). But not their negative forms.
# 3.1.3.2. Prepositions
# 3.1.3.3. Conjunctions
# 3.2.3.4. Verbs that can be used as auxiliary verbs: to be, to have, to do. 

# Since some of them includes a contrast (e.g. "Against") we are using those that do not include this feature. 
stop_words_special <- read_delim("texts/stop_words_special.csv", col_names = TRUE)

# This script uses the stop words created for this project (although we are not using it)
debates_2024_uw_clean <- debates_2024 %>% 
  mutate(transcript = gsub("Donald Trump", "donald_trump", transcript)) %>% 
  mutate(transcript = gsub("Trump", "donald_trump", transcript)) %>%
  mutate(transcript = gsub("Kamala Harris", "kamala_harris", transcript)) %>% 
  mutate(transcript = gsub("Harris", "kamala_harris", transcript)) %>% 
  mutate(transcript = gsub("Joe Biden", "joe_biden", transcript)) %>% 
  mutate(transcript = gsub("Biden", "joe_biden", transcript)) %>% 
  mutate(transcript = gsub("Tim_Walz", "tim_walz", transcript)) %>%
  mutate(transcript = gsub("Walz", "tim_walz", transcript)) %>% 
  mutate(transcript = gsub("Tim", "tim_walz", transcript)) %>% 
  mutate(transcript = gsub("Senator", "senator", transcript)) %>%
  mutate(transcript = gsub("senator Vance", "jd_vance", transcript)) %>% 
  mutate(transcript = gsub("Vance", "jd_vance", transcript)) %>%
  mutate(transcript = gsub("jobs", "job", transcript, ignore.case = TRUE)) %>% 
  filter(!grepl("^\\(", transcript)) %>% 
  filter(!person %in% all_participants$person[all_participants$candidacy == "Journalist"]) %>% 
  unnest_tokens(word, transcript) %>%
  mutate(word = str_replace(word, "’", "'")) %>% 
  mutate(word = ifelse(grepl("'s$", word) | grepl("’s$", word),
                       str_sub(word, 1, (nchar(word)-2)),
                       word)) %>%  
  group_by(the_date, person) %>% 
  count(word) %>% 
  filter(!grepl("^\\d+", word)) %>% 
  anti_join(stop_words_special[stop_words_special$contrast == FALSE,]) %>% 
  arrange(desc(n), .by_group = TRUE) 
  
 
# This script uses the stop-words file from tidyverse and we are using it.
debates_2024_uw_clean_ss <- debates_2024 %>% 
  mutate(transcript = gsub("Donald Trump", "donald_trump", transcript)) %>% 
  mutate(transcript = gsub("Trump", "donald_trump", transcript)) %>%
  mutate(transcript = gsub("Kamala Harris", "kamala_harris", transcript)) %>% 
  mutate(transcript = gsub("Harris", "kamala_harris", transcript)) %>% 
  mutate(transcript = gsub("Joe Biden", "joe_biden", transcript)) %>% 
  mutate(transcript = gsub("Biden", "joe_biden", transcript)) %>% 
  mutate(transcript = gsub("Tim_Walz", "tim_walz", transcript)) %>%
  mutate(transcript = gsub("Walz", "tim_walz", transcript)) %>% 
  mutate(transcript = gsub("Tim", "tim_walz", transcript)) %>% 
  mutate(transcript = gsub("Senator", "senator", transcript)) %>%
  mutate(transcript = gsub("senator Vance", "jd_vance", transcript)) %>% 
  mutate(transcript = gsub("Vance", "jd_vance", transcript)) %>%
  mutate(transcript = gsub("jobs", "job", transcript, ignore.case = TRUE)) %>% 
  filter(!grepl("^\\(", transcript)) %>% 
  filter(!person %in% all_participants$person[all_participants$candidacy == "Journalist"]) %>% 
  unnest_tokens(word, transcript) %>%
  mutate(word = str_replace(word, "’", "'")) %>% 
  mutate(word = ifelse(grepl("'s$", word) | grepl("’s$", word),
                       str_sub(word, 1, (nchar(word)-2)),
                       word)) %>%  
  group_by(the_date, person) %>% 
  count(word) %>% 
  filter(!grepl("^\\d+", word)) %>% 
  anti_join(stop_words) %>% 
  arrange(desc(n), .by_group = TRUE) 

# Unique words

# 
debates_2024_uw_clean_ss_graph <- debates_2024_uw_clean_ss %>% 
  slice(1:20)
  

 


