# Libraries ####
library(tidytext)
library(tidyverse)

# Load Text ####

debate_20240910 <- read.delim("texts/harris_trump_20240910.txt", header = FALSE) %>% 
  mutate(line_number = row_number()) %>% 
  tibble()

