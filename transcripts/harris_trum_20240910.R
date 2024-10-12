# Libraries ####
library(tidytext)
library(tidyverse)

# Loading the Text and Creating the Data Frame ####

debate_20240910 <- read.delim("texts/harris_trump_20240910.txt", header = FALSE) %>% 
  mutate(line_number = row_number(),
         person = str_extract(V1, "^[[:upper:]]+.*?(?=:)"),
         V1 = str_replace(V1, "^[[:upper:]]+.*?: ", "")) %>% 
  rename(transcript = V1) %>% 
  relocate(line_number, person, transcript) %>% 
  tibble()

