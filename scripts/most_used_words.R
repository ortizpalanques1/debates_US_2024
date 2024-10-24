# Task: Most Used Words
# Libraries ####
library(tidyverse)
library(tidytext)
source("scripts/word_count.R")
data("stop_words")

# Task No. 1. Number of Unique Words by Candidate/Debate ####
debates_2024_uw <- debates_2024 %>% 
  filter(!grepl("^\\(", transcript)) %>% 
  filter(!person %in% all_participants$person[all_participants$candidacy == "Journalist"]) %>% 
  unnest_tokens(word, transcript) %>% 
  anti_join(stop_words) %>% 
  group_by(the_date, person) %>% 
  count(word) %>% 
  summarise(unique_words = n())

debates_2024_uw_graph <- debates_2024_uw %>% 
  mutate(candidate_debate = paste0(person,
                                     "\n",
                                     format(as.Date(the_date, format="%Y-%m-%d"),"%B"))) %>% 
  ggplot(aes(x = reorder(candidate_debate, unique_words), y = unique_words, fill = person, label = unique_words))+
  geom_col()+
  geom_text(aes(y=700), fontface="bold", color="white", hjust = 1, size = 8)+
  scale_y_continuous(expand = c(0,0), limits = c(0,1200))+
  labs(
    title = "Number of Unique Words by Candidate and Debate",
    y = "Unique Words"
  )+
  scale_fill_manual(values = person_colors)+
  coord_flip()+
  theme(
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(color = "white", face = "bold"),
    legend.position = "off",
    strip.background = element_rect(fill = "#3D0C02"),
    strip.text = element_text(colour = "white", face = "bold"),
    # panel.background = element_rect(alpha("#C5B6B3", 1)),
    # panel.grid = element_line(alpha("#C5B6B3", 1)),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#2c3840"),
    plot.title = element_text(hjust = 0.5),
    plot.title.position = "plot",
    title = element_text(color = "white")
  )
  
ggsave("graph/word_unique_words.png", debates_2024_uw_graph, units = "cm", width = 16, height = 16, device = "png")

# Task No. 2


# filter(!grepl("[[:digit:]]+", word)) %>% 
#   arrange(desc(n), .by_group = TRUE) 