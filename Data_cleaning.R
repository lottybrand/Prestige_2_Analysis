
# Data prep for Dallinger analysis
# SKIP TO LINE 113 <<- ONLY NEED TO RUN THIS
# source('data_inputting.R')

#####
#assign first dataset to full_data:
full_data <- loaded_files[[1]]

#assign first dataset unique info ids and network ids
full_data$uid <- full_data$id
full_data$u_network <- full_data$Network
full_data$u_origin <- full_data$Origin

#give unique ids to each subsequent datasets by adding the max of the previous id's 
for (i in 2:length(loaded_files)) {
  this_table <- loaded_files[[i]]
  this_table$uid <- this_table$id + max(full_data$uid)
  this_table$u_origin <- this_table$Origin + max(full_data$u_origin)
  this_table$u_network <- this_table$Network + max(full_data$u_network)
  full_data <- bind_rows(full_data, this_table)
}

#make u_origins contiguous and starting from 1. 
current_u_origins <- unique(full_data$u_origin)
full_data$u_origin <- match(full_data$u_origin, current_u_origins)
#dont we need to do this for network as well?!

#####
##### Calculating the cumulative score, cumulative copies, and if they copied the top scorer and most copied : 
#####

#figure out cumulative ASOCIAL score in Round 1 
#(equivalent to asoc_score in property1 of node table) 
# now need to do this per topic (can we not get this from the node table?)
full_data$c_a_score_r1 <- rep(-666, nrow(full_data))
u_origins <- unique(full_data$u_origin)
for (i in 1:length(u_origins)) {
  u_origin <- u_origins[i]
  relevant_rows <- c(1:nrow(full_data))[full_data$u_origin == u_origin]
  subset <- full_data[relevant_rows,]
  c_a_score_r1 <- cumsum(subset$score * (1-subset$copying) * (subset$round==1))
  full_data$c_a_score_r1[relevant_rows] <- c_a_score_r1
}


#figure out Cumulative Asocial GEOGRAPHY score in Round 1 
#(equivalent to asoc_score_geog in property1 of node table) 
# now need to do this per topic (can we not get this from the node table?)
full_data$c_a_Geography <- rep(-666, nrow(full_data))
u_origins <- unique(full_data$u_origin)
for (i in 1:length(u_origins)) {
  u_origin <- u_origins[i]
  relevant_rows <- c(1:nrow(full_data))[full_data$u_origin == u_origin]
  subset <- full_data[relevant_rows,]
  c_a_Geography <- cumsum(subset$score * (1-subset$copying) * (subset$round==1) * (subset$topic=="Geography"))
  full_data$c_a_Geography[relevant_rows] <- c_a_Geography
}

#figure out Cumulative Asocial LANGUAGE score in Round 1 
#(equivalent to asoc_score_lang in property1 of node table) 
# now need to do this per topic (can we not get this from the node table?)
full_data$c_a_Language <- rep(-666, nrow(full_data))
u_origins <- unique(full_data$u_origin)
for (i in 1:length(u_origins)) {
  u_origin <- u_origins[i]
  relevant_rows <- c(1:nrow(full_data))[full_data$u_origin == u_origin]
  subset <- full_data[relevant_rows,]
  c_a_Language <- cumsum(subset$score * (1-subset$copying) * (subset$round==1) * (subset$topic=="Language"))
  full_data$c_a_Language[relevant_rows] <- c_a_Language
}

#figure out Cumulative Asocial WEIGHT score in Round 1 
#(equivalent to asoc_score_weight in property1 of node table) 
# now need to do this per topic (can we not get this from the node table?)
full_data$c_a_Weight <- rep(-666, nrow(full_data))
u_origins <- unique(full_data$u_origin)
for (i in 1:length(u_origins)) {
  u_origin <- u_origins[i]
  relevant_rows <- c(1:nrow(full_data))[full_data$u_origin == u_origin]
  subset <- full_data[relevant_rows,]
  c_a_Weight <- cumsum(subset$score * (1-subset$copying) * (subset$round==1) * (subset$topic=="Weight"))
  full_data$c_a_Weight[relevant_rows] <- c_a_Weight
}

#figure out Cumulative Asocial ART score in Round 1 
#(equivalent to asoc_score_art in property1 of node table) 
# now need to do this per topic (can we not get this from the node table?)
full_data$c_a_Art <- rep(-666, nrow(full_data))
u_origins <- unique(full_data$u_origin)
for (i in 1:length(u_origins)) {
  u_origin <- u_origins[i]
  relevant_rows <- c(1:nrow(full_data))[full_data$u_origin == u_origin]
  subset <- full_data[relevant_rows,]
  c_a_Art <- cumsum(subset$score * (1-subset$copying) * (subset$round==1) * (subset$topic=="Art"))
  full_data$c_a_Art[relevant_rows] <- c_a_Art
}

#figure out total score including copies 
#(equivalent to score in property1 of node table)
full_data$t_score <- rep(-666, nrow(full_data))
u_origins <- unique(full_data$u_origin)
for (i in 1:length(u_origins)) {  
  u_origin <- u_origins[i]
  relevant_rows <- c(1:nrow(full_data))[full_data$u_origin == u_origin]
  subset <- full_data[relevant_rows,]
  t_score <- cumsum(subset$score)
  full_data$t_score[relevant_rows] <- t_score
}

#figure out their cumulative copies:
#(equivalent to n_copies in property1 of node table)
#is_model_id also created here
full_data$c_copies <- rep(-666, nrow(full_data))
all_node_ids <- unique(full_data$Origin)
full_data$is_model_id <- (full_data$copying == TRUE & full_data$Contents %in% all_node_ids)
copying_decisions <- full_data[full_data$is_model_id == TRUE,]
for (i in 1:nrow(full_data)) {
  full_data$c_copies[i] <- nrow(copying_decisions[
    copying_decisions$round == 1 &
    copying_decisions$u_network == full_data$u_network[i] &
    copying_decisions$uid < full_data$uid[i] & 
    copying_decisions$Contents == as.character(full_data$Origin[i])
  ,])
}


#create function for cumulative topic copies:
topic_copies <- function(x) {
  full_data[,paste("c_copies_",x, sep="")] <<- rep(-666, nrow(full_data))
  for (i in 1:nrow(full_data)) {
    full_data[i,paste("c_copies_",x, sep="")] <<- nrow(copying_decisions[
      copying_decisions$round == 1 &
        copying_decisions$u_network == full_data$u_network[i] &
        copying_decisions$topic == x &
        copying_decisions$uid < full_data$uid[i] & 
        copying_decisions$Contents == as.character(full_data$Origin[i])
      ,])
  }
}
topics <- c("Geography", "Art", "Language", "Weight")
for (topic in topics) {
  topic_copies(topic)
}

#had to split the below into several chunks per topic instead of one function like the above:
#figure out if they copied the highest scorer for Geography: 
full_data$copied_successful_geog <- rep(NA, nrow(full_data))
potential_models <- full_data[full_data$copying == FALSE & full_data$Contents != "Ask Someone Else" & full_data$topic =="Geography",]
for (i in 1:nrow(full_data)) {
  if (full_data$is_model_id[i] == TRUE) {
    models <- potential_models[potential_models$number == full_data$number[i] & potential_models$u_network == full_data$u_network[i],]
    if (nrow(models) > 1) {
      if (length(unique(models$c_a_Geography)) != 1) {
        model <- models[as.character(models$Origin) == full_data$Contents[i],]
        full_data$copied_successful_geog[i] <- (model$c_a_Geography == max(models$c_a_Geography))*1
      }
    }
  }
}

#figure out if they copied the highest scorer for Language:
full_data$copied_successful_lang <- rep(NA, nrow(full_data))
potential_models <- full_data[full_data$copying == FALSE & full_data$Contents != "Ask Someone Else" & full_data$topic =="Language",]
for (i in 1:nrow(full_data)) {
  if (full_data$is_model_id[i] == TRUE) {
    models <- potential_models[potential_models$number == full_data$number[i] & potential_models$u_network == full_data$u_network[i],]
    if (nrow(models) > 1) {
      if (length(unique(models$c_a_Language)) != 1) {
        model <- models[as.character(models$Origin) == full_data$Contents[i],]
        full_data$copied_successful_lang[i] <- (model$c_a_Language == max(models$c_a_Language))*1
      }
    }
  }
}

#figure out if they copied the highest scorer for Weight:
full_data$copied_successful_weight <- rep(NA, nrow(full_data))
potential_models <- full_data[full_data$copying == FALSE & full_data$Contents != "Ask Someone Else" & full_data$topic =="Weight",]
for (i in 1:nrow(full_data)) {
  if (full_data$is_model_id[i] == TRUE) {
    models <- potential_models[potential_models$number == full_data$number[i] & potential_models$u_network == full_data$u_network[i],]
    if (nrow(models) > 1) {
      if (length(unique(models$c_a_Weight)) != 1) {
        model <- models[as.character(models$Origin) == full_data$Contents[i],]
        full_data$copied_successful_weight[i] <- (model$c_a_Weight == max(models$c_a_Weight))*1
      }
    }
  }
}

#figure out if they copied the highest scorer for Art:
full_data$copied_successful_art <- rep(NA, nrow(full_data))
potential_models <- full_data[full_data$copying == FALSE & full_data$Contents != "Ask Someone Else" & full_data$topic =="Art",]
for (i in 1:nrow(full_data)) {
  if (full_data$is_model_id[i] == TRUE) {
    models <- potential_models[potential_models$number == full_data$number[i] & potential_models$u_network == full_data$u_network[i],]
    if (nrow(models) > 1) {
      if (length(unique(models$c_a_Art)) != 1) {
        model <- models[as.character(models$Origin) == full_data$Contents[i],]
        full_data$copied_successful_art[i] <- (model$c_a_Art == max(models$c_a_Art))*1
      }
    }
  }
}

#merge these columns (by overwriting each column with subsequent non NA entries)
#using simple solution from this page: https://stackoverflow.com/questions/14563531/combine-column-to-remove-nas

full_data$copied_successful <- full_data$copied_successful_geog  
full_data$copied_successful[!is.na(full_data$copied_successful_lang)] <- full_data$copied_successful_lang[!is.na(full_data$copied_successful_lang)]  # merge with lang
full_data$copied_successful[!is.na(full_data$copied_successful_weight)] <- full_data$copied_successful_weight[!is.na(full_data$copied_successful_weight)]  # merge with weight
full_data$copied_successful[!is.na(full_data$copied_successful_art)] <- full_data$copied_successful_art[!is.na(full_data$copied_successful_art)]  # merge with art



test_set <- full_data[,c("number","round","Contents","Origin","topic","copied_successful_geog","copied_successful_weight","copied_successful_art","copied_successful_lang","copied_successful","is_model_id")]
test_set <- test_set[test_set$round==1,]
test_set <- test_set[test_set$is_model_id==TRUE,]



#figure out if they copied the most copied:
full_data$copied_prestigious <- rep(NA, nrow(full_data))
potential_models <- full_data[full_data$copying == FALSE]
for (i in 1:nrow(full_data)) {
  if (full_data$is_model_id[i] == TRUE) {
    models <- full_data[full_data$number == full_data$number[i] & full_data$u_network == full_data$u_network[i],]
    if (nrow(models) > 1) {
      if (length(unique(models$c_copies)) != 1) {
        model <- models[as.character(models$Origin) == full_data$Contents[i],]
        full_data$copied_prestigious[i] <- (model$c_copies == max(models$c_copies))*1
      }
    }
  }
}


#figure out total copied (how many times they copied others across both rounds)
#note this doesn't correspond to c_copies, because that is how many times others copied them in round 1 only. 
full_data$t_copied <- rep(-666, nrow(full_data))
u_origins <- unique(full_data$u_origin)
for (i in 1:length(u_origins)) {  
  u_origin <- u_origins[i]
  relevant_rows <- c(1:nrow(full_data))[full_data$u_origin == u_origin & full_data$is_model_id == FALSE]
  subset <- full_data[relevant_rows,]
  t_copied <- cumsum(subset$copying)
  full_data$t_copied[relevant_rows] <- t_copied
}


#####
##### SKIP TO HERE IN FUTURE
#write.csv(full_data, file="full_data.csv", row.names = FALSE)
#full_data <- read.csv("full_data.csv")
# saveRDS(full_data, "full_data")

#####
##### SUBSET OF ASOCIAL ONLY: 
#####
asocialOnly <- full_data[full_data$copying=="FALSE",]

#####
##### SUBSET OF COPYING ONLY:
#####
# this is redundant when can use model_ids instead
copyOnly <-full_data[full_data$copying=="TRUE",]

#####
##### Subset for seeing score info (Prediction 2)
#####

# need to subset only copying instances and only when score info is visible 

# first is it a copying decision rather than a copied answer:
model_ids <- full_data[full_data$is_model_id==TRUE,]
# then is it not condition a, and is it in round 1 (of B & C) OR  was info chosen score in R2... 
#scoreChoice <- model_ids[((!model_ids$condition=="a")&(model_ids$round==1))|(model_ids$info_chosen=="Total Score in Round 1"),]                                     


#####
##### Subset for seeing prestige info (Prediction 3)
#####

#prestigeChoice <- model_ids[model_ids$info_chosen =="Times chosen in Round 1",]


#####
##### Subset of info chosen for Predictions 4,5,6 (model 3 ):
#####

#is info choice:
infoChosen <- model_ids[model_ids$round==2,]

#predicted infos:
domain_spec <- c("Times Chosen on This Topic")
domain_gen <- c("Times Chosen Altogether")

infoChosen$chosePredicted <- ifelse((infoChosen$info_chosen%in%domain_spec & infoChosen$condition=='a'),1,
                                    ifelse((infoChosen$info_chosen%in%domain_gen & infoChosen$condition=='b'),1,
                                           ifelse((infoChosen$info_chosen%in%domain_spec & infoChosen$condition=='c'),1,0)))
                                    

##### 
##### Prediction 6 (model4): 
#####

# have copied or not be 0/1 for asocial choices (this includes any 'ask someone else's)

asocialOnly$copied <- ifelse(asocialOnly$Contents=="Ask Someone Else",1,0)
asocialOnly_2 <- asocialOnly[asocialOnly$round==2,]

#change baseline to condition to A if not using ulam method:
#asocialOnly$condB <- ifelse(asocialOnly$condition=="b",1,0)
#asocialOnly$condC <- ifelse(asocialOnly$condition=="c",1,0)

#####
##### Prediction 7 (model 5):
#####

#taking just the accumulated score for the final question for each participant
finalScore <- asocialOnly[asocialOnly$number==100,]
finalScoreBC <- finalScore[!finalScore$condition=="a",]
finalScoreA <- finalScore[finalScore$condition=="a",]

#what about just round 2's total scores:
full_data_R2 <- full_data[full_data$round==2,]
full_data_R2$t_score_r2 <- rep(-666, nrow(full_data_R2))
u_origins <- unique(full_data_R2$u_origin)
for (i in 1:length(u_origins)) {  
  u_origin <- u_origins[i]
  relevant_rows <- c(1:nrow(full_data_R2))[full_data_R2$u_origin == u_origin]
  subset <- full_data_R2[relevant_rows,]
  t_score_r2 <- cumsum(subset$score)
  full_data_R2$t_score_r2[relevant_rows] <- t_score_r2
}

asocialOnly_2 <- full_data_R2[full_data_R2$copying=="FALSE",]

finalScore_R2 <- asocialOnly_2[asocialOnly_2$number==100,]
