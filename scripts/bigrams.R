# Libraries ####
library(tidyverse)
library(tidytext)

# Obtaining sentences 

sentences_debate <- debates_2024 %>%
  unnest_tokens(
    output = sentence,
    input = transcript,
    token = "sentences",
    to_lower = TRUE,
    drop = TRUE,
    collapse = NULL
  )
