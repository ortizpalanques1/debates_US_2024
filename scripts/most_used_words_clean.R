# 3. Number of relevant words.
# Libraries
source("scripts/most_used_words.R")
library(textdata)
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
  mutate(transcript = gsub("Governor Walz", "tim_walz", transcript)) %>% 
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
  mutate(transcript = gsub("Governor Walz", "tim_walz", transcript)) %>% 
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
  mutate(word = str_replace(word, "women", "woman")) %>%
  mutate(word = str_replace(word, "men", "man")) %>%
  mutate(word = str_replace(word, "’", "'")) %>% 
  mutate(word = ifelse(grepl("'s$", word) | grepl("’s$", word),
                       str_sub(word, 1, (nchar(word)-2)),
                       word)) %>%  
  group_by(the_date, person) %>% 
  count(word) %>% 
  filter(!grepl("^\\d+", word)) %>% 
  anti_join(stop_words) %>% 
  arrange(desc(n), .by_group = TRUE) 

# Graph with the most used words
debates_2024_uw_clean_ss_graph <- debates_2024_uw_clean_ss %>% 
  slice(1:20)

# This three date will cut the different sections and will be helpful for the loop
date_debate <- unique(debates_2024_uw_clean_ss_graph$the_date)

for(i in 1:length(date_debate)){
  this_name <- paste0("debate_", date_debate[i])
  this_data <- debates_2024_uw_clean_ss_graph[debates_2024_uw_clean_ss_graph$the_date == date_debate[i],] %>% 
    ungroup() %>%
    mutate(word_2 = ifelse(person == person[1], paste0(word, "_d"), paste0(word, "_b"))) %>% 
    ggplot(aes(y = reorder(word_2, n), x = n, fill = person, label = paste(word, "/", n)))+
    facet_wrap(~person,  scales = "free_y")+
    geom_col()+
    geom_text(aes(x = 1), fontface="bold", color="white", hjust = 0)+
    scale_fill_manual(values = person_colors)+
    theme(
      axis.text = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title = element_blank(),
      legend.position = "off",
      panel.background = element_rect(fill = "#2c3840"),
      panel.grid = element_line(color = "#2c3840"),
      plot.background = element_rect(fill = "#2c3840"),
      strip.background = element_rect(fill = "black"),
      strip.text = element_text(colour = "#e5b80b", face = "bold"),
    )
  ggsave(paste0("graph/",this_name,".png"),this_data, units = "cm", width = 16, height = 16, device = "png")
}

# Data base for the app ####

# Candidates data
candidates_data <- debates_2024 %>% 
  select(-c(transcript, line_number)) %>% 
  unique() %>% 
  filter(candidacy != "Journalist") %>% 
  mutate(selection = paste0(person, " ", the_date)) 


# Proportion of word by candidate/debate
debates_2024_uw_clean_ss_proprotions <- debates_2024_uw_clean_ss %>% 
  mutate(proportion = n/sum(n),
         selection = paste0(person, " ", the_date)) 

# All words with proportions. No Stop words
debates_2024_uw_clean_nss <- debates_2024 %>% 
  mutate(transcript = gsub("Donald Trump", "donald_trump", transcript)) %>% 
  mutate(transcript = gsub("Trump", "donald_trump", transcript)) %>%
  mutate(transcript = gsub("Kamala Harris", "kamala_harris", transcript)) %>% 
  mutate(transcript = gsub("Harris", "kamala_harris", transcript)) %>% 
  mutate(transcript = gsub("Joe Biden", "joe_biden", transcript)) %>% 
  mutate(transcript = gsub("Biden", "joe_biden", transcript)) %>% 
  mutate(transcript = gsub("Governor Walz", "tim_walz", transcript)) %>% 
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
  mutate(word = str_replace(word, "women", "woman")) %>%
  mutate(word = str_replace(word, "men", "man")) %>%
  mutate(word = str_replace(word, "’", "'")) %>% 
  mutate(word = ifelse(grepl("'s$", word) | grepl("’s$", word),
                       str_sub(word, 1, (nchar(word)-2)),
                       word)) %>%  
  group_by(the_date, person) %>% 
  count(word) %>% 
  filter(!grepl("^\\d+", word)) %>% 
  #anti_join(stop_words) %>% 
  arrange(desc(n), .by_group = TRUE) 

# Bing sentiment
bing_sentiment <- get_sentiments("bing")

# Save file and sent to debates_2024_app/data  
save(all_participants,
     person_colors, 
     candidates_data, 
     debates_2024,
     debates_2024_uw_clean_ss_proprotions, 
     debates_2024_uw_clean_nss,
     bing_sentiment,
     file = "data_debate_2024.RData")
