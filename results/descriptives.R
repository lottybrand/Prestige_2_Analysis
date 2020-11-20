#descriptives

pptData <- full_data[!duplicated(full_data$u_origin),]

setwd("~/Desktop/Postdoc/Lottys_dallinger/Prestige_2_Analysis/results/demographics")

# libraries
library(jsonlite)
library(data.table)
library(dplyr)

# 1) First, make a transformation function that works for a single entry
f <- function(json, id){
  # transform json to list
  tmp    <- jsonlite::fromJSON(json)
  # transform list to data.frame
  tmp    <- as.data.frame(tmp)
  # add id
  tmp$id <- id
  # return
  return(tmp)
}


load_file <- function(file, condition) {
  questions <- read.csv(file, stringsAsFactors = FALSE)
  
  # parse the JSON using: https://stackoverflow.com/questions/41988928/how-to-parse-json-in-a-dataframe-column-using-r 
  # using the second option (non-tidyverse): 
  
  
  # 2) apply it via mapply 
  json_dfs <- 
    mapply(f, questions$response, questions$participant_id, SIMPLIFY = FALSE)
  # 3) combine the fragments via rbindlist
  clean_df <- 
    data.table::rbindlist(json_dfs)
  
  #cleanup
  my_data <- clean_df
  clean_df <- NULL
  rm(json_dfs)
  return(my_data)
}

file_names <- c("question_April1_2020_C.csv", "question_April2_2020_C.csv","question_April2_2020_A.csv","question_April03_B1.csv","question_April03_B_2.csv","question_Oct02_CondD_group1.csv","question_Oct02_CondD_groups.csv","question_OCT12_COND_D_extra2.csv")
condition <- c("c", "c", "a", "b", "b", "d", "d", "d")
loaded_files <- list()
for (i in 1:length(file_names)) {
  loaded_files[[i]] <- load_file(file_names[i], condition[i])
}

#assign first dataset to questions data_data:
questions_data <- loaded_files[[1]]

#combine all 
for (i in 2:length(loaded_files)) {
  this_table <- loaded_files[[i]]
  questions_data <- bind_rows(questions_data, this_table)
}

questions_data$age <- as.integer(questions_data$age)
questions_data <- questions_data[!questions_data$age=="NA",]
mean(questions_data$age)
table(questions_data$gender)
