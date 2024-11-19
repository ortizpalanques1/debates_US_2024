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

# Texts
correlatio <- "Pearson's Correlation"
most_used_word_text <- "Candidates' Most Used Word"
selector_title <- "Select Two Candidates"
explain_graph <- "This graph shows the proportion of relevant words for each candidate.\nBecause we use a logarithmic scale many dots seem closer than they really are.\nFor these reason, the intensity of the dot's color denotes how close both proportions are."
explain_graph_title <- "What This Graph Represents"
the_pvalue <- "p Value"