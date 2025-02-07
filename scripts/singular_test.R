# Libraries ####
library(textdata)
library(tidyverse)
library(tidytext)
library(SemNetCleaner)

# Creating a sample to test ####
test <- tf_idf_table(word_counter_neat(debates_2024, person))
test <- unique(test$word)

# Singularizing ####
singular_test <- sapply(test, singularize)

# Data Frame ####
singular_plural <- data.frame("Original" = test, "Final" = singular_test)
row.names(singular_plural) <- seq(1:length(singular_plural$Original))
singular_plural$Difference <- ifelse(singular_plural$Original == singular_plural$Final, 0, 1)

# False Positives ####
false_positive <- singular_plural %>% 
  filter(Difference == 1)
