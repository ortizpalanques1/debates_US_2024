# This project's limited goal is to create csv files and create a connection
# 1. Libraries ####
library(tidytext)
library(RMariaDB)
library(odbc)
library(DBI)

# 2. Create csv. Examples do not run ####
# stopwords.txt
stopwords <- read.delim("data_CSV/stopwords.txt", header = FALSE)
stopwords$sw_id <- seq(1, nrow(stopwords))
colnames(stopwords) <- c("sw_id", "stopword")
stopwords <- stopwords[, c(2,1)]
write.csv(stopwords, "data_CSV/stopwords.csv", row.names = FALSE)

# english_negations.txt
english_negations <- read.delim("data_CSV/english_negations.txt", header = TRUE, sep = ",")
english_negations$ee_id <- seq(1, nrow(english_negations))
english_negations <- english_negations[, c(4,1,2,3)]
write.csv(english_negations, "data_CSV/english_negations.csv", row.names = FALSE)

# 
bing <- get_sentiments('bing')
bing$bi_id <- seq(1, nrow(bing))
bing <- bing[,c(3,1,2)]
write.csv(bing, "data_CSV/bing.csv", row.names = FALSE)

# Meta data
meta_data <- data.frame(
  "md_id" = c(1:4),
  "tables" = c("bing", 
               "english_negations", 
               "stopwords",
               "stop_words_special"),
  "description" = c("Sentiment lexicon that classifies words as Negative or positive. Source: https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html. Details: This lexicon was first published in: Minqing Hu and Bing Liu, ``Mining and summarizing customer reviews.``, Proceedings of the ACM SIGKDD International Conference on Knowledge Discovery & Data Mining (KDD-2004), Seattle, Washington, USA, Aug 22-25, 2004. Words with non-ASCII characters were removed.",
                    "List of English words that denotes the absence of an attribute",
                    "List of words that are considered low value in the extraction of meaning from texts",
                    "Experimental list of stop words."),
  "language" = c("English",
                 "English",
                 "English",
                 "English"),
  "type" = c("Sentiment",
             "Negation",
             "Stopword",
             "Stopword"),
  "include_sentimens" = c(TRUE,
                FALSE,
                FALSE,
                FALSE)
)
write.csv(meta_data, "data_CSV/meta_data.csv", row.names = FALSE)


# 3. Connection ####
# 3. 1. Connection
con <- dbConnect(
  drv = RMariaDB::MariaDB(), 
  dbname = "sentiments_dictionaries",
  username = "root",
  password = "Ciencia54", 
  host = "localhost", 
  port = 3306
)

# 3.2. To disconnect
dbDisconnect()

# 3.3. Some queries
# 3.3.1. Vector with tables
vector_with_tables <- dbListTables(con)

# 3.3.2. Elements of a query
this_table <- dbSendQuery(con, "SELECT * FROM bing")
working_data <- dbFetch(this_table)
dbClearResult(this_table)


{this_row <- dbSendQuery(con, "SELECT * FROM bing WHERE bi_id = 10")
row_10 <- dbFetch(this_row)
dbClearResult(this_row)}
