# Loading the Text and Creating the Data Frame
# 1. Libraries ####
library(tidytext)
library(tidyverse)

# 2. Functions ####
name_filler <- function(x){
  this_vector <- x
  for(i in 1:length(this_vector)){
    if(!is.na(this_vector[i])){
      this_vector[i] <- this_vector[i]
    }else{
      this_vector[i] <- this_vector[i-1]
    }
  }
  return(this_vector)
}

# 3. Biden Trump 27 June 2024 ####
# Load data and creation of two new columns: line_number and person 
biden_trump_20240627 <- read.delim("texts/biden_trump_20240627.txt", header = FALSE) %>% 
  mutate(line_number = row_number(),
         person = str_extract(V1, "^[[:upper:]][[:upper:]]+.*?(?=:)"),
         V1 = str_replace(V1, "^[[:upper:]][[:upper:]]+.*?: ", ""),
         the_date = as.Date("2024-06-27")) %>%
  rename(transcript = V1) %>% 
  relocate(line_number, person, transcript) %>% 
  tibble()

# Standardization of participants

# Getting raw names
participants_20240627 <- unique(biden_trump_20240627$person) 

#Only names
names_20240627 <- str_extract(unique(biden_trump_20240627$person), "^[[:upper:]]+ [[:upper:]]+?(?=,)") 

# Delete NA
names_20240627 <- names_20240627[!is.na(names_20240627)] 

#Replace with standard names_20240627
biden_trump_20240627$person <- recode(biden_trump_20240627$person, 
                                      `JAKE TAPPER, CNN MODERATOR` = names_20240627[1],
                                      `DANA BASH, CNN MODERATOR` = names_20240627[2],
                                      TAPPER = names_20240627[1],
                                      BASH = names_20240627[2],
                                      `JOE BIDEN, PRESIDENT OF THE UNITED STATES` = names_20240627[3],
                                      BIDEN = names_20240627[3],
                                      `DONALD TRUMP, FORMER PRESIDENT OF THE UNITED STATES AND CURRENT U.S. PRESIDENTIAL CANDIDATE` = names_20240627[4],
                                      TRUMP = names_20240627[4]
                                      )

# Fill NA with name of the speaker
for(i in 1:nrow(biden_trump_20240627)){
  if(!is.na(biden_trump_20240627$person[i])){
    biden_trump_20240627$person[i] <- biden_trump_20240627$person[i]
  }else{
    biden_trump_20240627$person[i] <- biden_trump_20240627$person[i-1]
  }
}

# Alternatively, you can use the function name_filler
# biden_trump_20240627$person <- name_filler(biden_trump_20240627$person)
  
# 4. Harris Trump 10 September 2024 ####

# Load data and creation of two new columns: line_number and person 
harris_trump_20240910 <- read.delim("texts/harris_trump_20240910.txt", header = FALSE) %>% 
  mutate(line_number = row_number(),
         person = str_extract(V1, "^[[:upper:]]+.*?(?=:)"),
         V1 = str_replace(V1, "^[[:upper:]]+.*?: ", ""),
         the_date = as.Date("2024-09-10")) %>% 
  rename(transcript = V1) %>% 
  relocate(line_number, person, transcript) %>% 
  tibble()

# Standardization of participants

# Getting raw names
participants_20240910 <- unique(harris_trump_20240910$person) 

#Only names
names_20240910 <- str_extract(unique(harris_trump_20240910$person)[1:4], "[[:upper:]]+ [[:upper:]]+?$") 

#Replace with standard names_20240910
harris_trump_20240910$person <- recode(harris_trump_20240910$person,
                                       `VICE PRESIDENT KAMALA HARRIS` = names_20240910[3],
                                       `FORMER PRESIDENT DONALD TRUMP` = names_20240910[4],
                                       `FORMER PRESIDENT TRUMP` = names_20240910[4],
                                       `LINDSEY DAVIS` = names_20240910[2],
                                       `VICE PRESIDENT HARRIS` = names_20240910[3]
                                       )

# Fill NA with name of the speaker
for(i in 1:nrow(harris_trump_20240910)){
  if(!is.na(harris_trump_20240910$person[i])){
    harris_trump_20240910$person[i] <- harris_trump_20240910$person[i]
  }else{
    harris_trump_20240910$person[i] <- harris_trump_20240910$person[i-1]
  }
}

# Alternatively, you can use the function name_filler
# harris_trump_20240910$person <- name_filler(harris_trump_20240910$person)

# 5. Vance Walz 1 October 2024 ####
# Load data and creation of two new columns: line_number and person 
# Although it is the shortest scrip, some manual cleaning was needed. We removed some prerecorded statements and voice-off segments and adjusted some wrongly attributed paragraphs.
vance_walz_20241001 <- read.delim("texts/vance_walz_20241001.txt", header = FALSE) %>% 
  mutate(line_number = row_number(),
         person = str_extract(V1, "^[[:upper:]]+.*?(?=:)"),
         V1 = str_replace(V1, "^[[:upper:]]+.*?: ", ""),
         the_date = as.Date("2024-10-01")) %>% 
  rename(transcript = V1) %>% 
  relocate(line_number, person, transcript) %>% 
  tibble()

# Getting raw names
participants_20241001 <- unique(vance_walz_20241001$person)

#Only names
names_20241001 <- participants_20241001[1:4]
names_20241001 <- gsub("NORAH O' ", "NORAH O'", names_20241001)

#Replace with standard names_20240910
vance_walz_20241001$person <- recode(vance_walz_20241001$person,
                                       `NORAH O' DONNELL` = names_20241001[1],
                                       `NORAH O’DONNELL` = names_20241001[1]
)

# 6. Unique data frame with all data ====

# Candidacies and political alignment
all_the_names <- unique(c(names_20240627, names_20240910, names_20241001))
position <- c("Journalist", "President", "Vice-President")
party <- c("Democrat", "Republican", "Neutral")

all_participants <- data.frame(
  person = all_the_names,
  candidacy = c(position[1], position[1], position[2], position[2], position[1], position[1], position[2], position[1], position[1], position[3], position[3]),
  alignment = c(party[3], party[3], party[1], party[2], party[3], party[3], party[1], party[3], party[3], party[1], party[2])
)

# Bind data frames and join with all_participants
debates_2024 <- bind_rows(biden_trump_20240627, harris_trump_20240910, vance_walz_20241001) %>% 
  left_join(all_participants)
