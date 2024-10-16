# Task: Count the words
# Libraries ####
library(tidyverse)
library(tidytext)

# Create the tibble by word ####
# Vector with names
# all_the_names. Already created.

# Vector with colors
all_the_names_colors <- c("#58d963","#58d963","#00aef3","#e81b23","#58d963","#58d963","#00aef3","#58d963","#58d963","#00aef3","#e81b23")

# Named vector
person_colors <- setNames(all_the_names_colors, all_the_names)

debates_2024_by_word <- debates_2024 %>% 
  filter(!grepl("^\\(", transcript)) %>% 
  unnest_tokens(word, transcript) %>% 
  group_by(the_date, person) %>% 
  summarise(Words = n()) %>% 
  arrange(desc(Words), .by_group = TRUE) %>% 
  ggplot(aes(x = reorder(person, Words), y = Words, fill = person, label = Words))+
  geom_col()+
  facet_wrap(~the_date, nrow = 3, scales = "free")+
  scale_fill_manual(values = person_colors)+
  coord_flip()+
  geom_text(aes(y=700), fontface="bold", color="white", hjust = 1)+
  labs(
    title = "2024 US Elections: Words by Candidate/Debate",
    caption = "Sources: CNN, MSN, Usa Today"
  )+
  theme(
    axis.title.y = element_blank(),
    axis.text = element_text(color = "white", face = "bold"),
    legend.position = "off",
    strip.background = element_rect(fill = "#3D0C02"),
    strip.text = element_text(colour = "white", face = "bold"),
    panel.background = element_rect(fill = "#4D4D4D"),
    panel.grid = element_line(color = "#4D4D4D"),
    plot.background = element_rect(fill = "#0C090A"),
    plot.title = element_text(hjust = 0.5),
    plot.title.position = "plot",
    title = element_text(color = "white")
  )

ggsave("graph/word_count.png", debates_2024_by_word, units = "cm", width = 16, height = 16, device = "png")
  
