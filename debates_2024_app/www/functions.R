# Functions ####

pvalue <- function(x){
  tryCatch(
    {
      if(x < 0.001){
        this_text <- "p < 0.001"
      }else{
        this_text <- paste0("p = ", x)
      }
    },
      error=function(e) {
        message('Working')
        cat("Working")
      }
  )
}

rvalue <- function(x){
  paste0("r = ", round(x,2))
}


most_used_word <- function(data, columna){
  parlatio <- data[columna == max(columna, na.rm = TRUE)]
  return(parlatio)
}


sentiment_total <- function(this_debate, texto, lexic_00){
  
  this_name <- str_extract(this_debate, "(?:(?! [[:digit:]]).)*")
  #print(this_name)
  this_date <- str_extract(this_debate, "[[:digit:]].*")
  #print(this_date)
  
  this_debate <- texto %>% 
    filter(person == this_name & the_date == this_date)
  #print(this_debate)
  
  total_words <- sum(this_debate$n, na.rm = TRUE)
  #print(total_words)
  
  sentimental_words <- this_debate %>% 
    inner_join(lexic_00, by = "word") %>% 
    group_by(sentiment) %>% 
    summarise(n = sum(n)) %>% 
    mutate(percentage = round((n/total_words*100),2))
  #print(sentimental_words)
  
  colnames(sentimental_words) <- c("Sentiment", "Words", "Percentage")
  
  return(sentimental_words)
}

# Texts
correlatio <- "Pearson's Correlation"
most_used_word_text <- "Candidates' Most Used Word"
selector_title <- "Select Two Candidates"
explain_graph <- "This graph shows the proportion of relevant words for each candidate.\nBecause we use a logarithmic scale many dots seem closer than they really are.\nFor this reason, the intensity of the dot's color denotes how close both proportions are."
explain_graph_title <- "What This Graph Represents"
the_pvalue <- "p Value"