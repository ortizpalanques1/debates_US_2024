# Libraries ####
library(tidytext)
library(tidyverse)

# Loading the Text and Creating the Data Frame ####

# 1. Biden Trump 27 June 2024
biden_trump_20240627 <- read.delim("texts/biden_trump_20240627.txt", header = FALSE) %>% 
  mutate(line_number = row_number(),
         person = str_extract(V1, "^[[:upper:]][[:upper:]]+.*?(?=:)"))

participants_20240627 <- unique(biden_trump_20240627$person)
names <- str_extract(unique(biden_trump_20240627$person), "^[[:upper:]]+ [[:upper:]]+?(?=,)")
names <- names[!is.na(names)]
recode(biden_trump_20240627$person, )

# 2. Harris Trump 10 September 2024
harris_trump_20240910 <- read.delim("texts/harris_trump_20240910.txt", header = FALSE) %>% 
  mutate(line_number = row_number(),
         person = str_extract(V1, "^[[:upper:]]+.*?(?=:)"),
         V1 = str_replace(V1, "^[[:upper:]]+.*?: ", "")) %>% 
  rename(transcript = V1) %>% 
  relocate(line_number, person, transcript) %>% 
  tibble()

