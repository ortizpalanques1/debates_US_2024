# Functions ####
pvalue <- function(x){
  if(x < 0.001){
    this_text <- "p < 0.001"
  }else{
    this_text <- paste0("p = ", x)
  }
}

# Texts
correlatio <- "Pearson's Correlation"
the_pvalue <- "p Value"
selector_title <- "Select Two Candidates"

