
# should this just be round 1? 
# looks like c_a_Weight etc are just calculated for round 1, should it be across both?
# check how finalScore and c_a_Geography etc calculated in data_cleaning.R - is it just round1?

# so if we want to compare R1 asocial scores with R2 asocial scores as we did for exp1, we 
# will need to calculate asocial score for R2 here 

topicTable <- finalScore[,c("number","round","u_network","u_origin","topic","c_a_Geography","c_a_Weight","c_a_Art","c_a_Language","c_a_score_r1")]

# so calculate c_a_Geog_r2  etc below: 

#figure out cumulative ASOCIAL score in Round 2 
# as opposed to R1 already calcuated 
# to compare for correlations test

full_data$c_a_score_r2 <- rep(-666, nrow(full_data))
u_origins <- unique(full_data$u_origin)
for (i in 1:length(u_origins)) {
  u_origin <- u_origins[i]
  relevant_rows <- c(1:nrow(full_data))[full_data$u_origin == u_origin]
  subset <- full_data[relevant_rows,]
  c_a_score_r2 <- cumsum(subset$score * (1-subset$copying) * (subset$round==2))
  full_data$c_a_score_r2[relevant_rows] <- c_a_score_r2
}


#figure out Cumulative Asocial GEOGRAPHY score in Round 2 
#as opposed to Round 1 already calculated
# for comparison for the correlations test

full_data$c_a_Geog_r2 <- rep(-666, nrow(full_data))
u_origins <- unique(full_data$u_origin)
for (i in 1:length(u_origins)) {
  u_origin <- u_origins[i]
  relevant_rows <- c(1:nrow(full_data))[full_data$u_origin == u_origin]
  subset <- full_data[relevant_rows,]
  c_a_Geog_r2 <- cumsum(subset$score * (1-subset$copying) * (subset$round==2) * (subset$topic=="Geography"))
  full_data$c_a_Geog_r2[relevant_rows] <- c_a_Geog_r2
}

#figure out Cumulative Asocial LANGUAGE score in Round 2 
#as opposed to Round 1 already calculated
# for comparison for the correlations test

full_data$c_a_Lang_r2 <- rep(-666, nrow(full_data))
u_origins <- unique(full_data$u_origin)
for (i in 1:length(u_origins)) {
  u_origin <- u_origins[i]
  relevant_rows <- c(1:nrow(full_data))[full_data$u_origin == u_origin]
  subset <- full_data[relevant_rows,]
  c_a_Lang_r2 <- cumsum(subset$score * (1-subset$copying) * (subset$round==2) * (subset$topic=="Language"))
  full_data$c_a_Lang_r2[relevant_rows] <- c_a_Lang_r2
}

#figure out Cumulative Asocial WEIGHT score in Round 2 
#as opposed to Round 1 which is already calculated
#for comparison for the correlations check

full_data$c_a_Weight_r2 <- rep(-666, nrow(full_data))
u_origins <- unique(full_data$u_origin)
for (i in 1:length(u_origins)) {
  u_origin <- u_origins[i]
  relevant_rows <- c(1:nrow(full_data))[full_data$u_origin == u_origin]
  subset <- full_data[relevant_rows,]
  c_a_Weight_r2 <- cumsum(subset$score * (1-subset$copying) * (subset$round==2) * (subset$topic=="Weight"))
  full_data$c_a_Weight_r2[relevant_rows] <- c_a_Weight_r2
}

#figure out Cumulative Asocial ART score in Round 2 
#as opposed to Round 1 which was already calculated
# for comparison with Round 1 in the correlations check 

full_data$c_a_Art_r2 <- rep(-666, nrow(full_data))
u_origins <- unique(full_data$u_origin)
for (i in 1:length(u_origins)) {
  u_origin <- u_origins[i]
  relevant_rows <- c(1:nrow(full_data))[full_data$u_origin == u_origin]
  subset <- full_data[relevant_rows,]
  c_a_Art_r2 <- cumsum(subset$score * (1-subset$copying) * (subset$round==2) * (subset$topic=="Art"))
  full_data$c_a_Art_r2[relevant_rows] <- c_a_Art_r2
}

#Now create finalScore table (and asocialOnly table) again as this was created with previous full_data from data_cleaning.R file

finalScore <- asocialOnly[asocialOnly$number==100,]

topicTable <- finalScore[,c("number","round","u_network","u_origin","topic","c_a_Geography","c_a_Weight","c_a_Art","c_a_Language","c_a_score_r1", "c_a_Geog_r2","c_a_Weight_r2","c_a_Art_r2","c_a_Lang_r2","c_a_score_r2")]


#predicting Art scores:
cor.test(finalScore$c_a_Art, finalScore$c_a_Art_r2)$estimate[[1]]
cor.test(finalScore$c_a_Geography, finalScore$c_a_Art_r2)$estimate[[1]]
cor.test(finalScore$c_a_Language, finalScore$c_a_Art_r2)$estimate[[1]]
cor.test(finalScore$c_a_Weight, finalScore$c_a_Art_r2)$estimate[[1]]
cor.test(finalScore$c_a_score_r1, finalScore$c_a_Art_r2)$estimate[[1]]

#predicting Language scores:
cor.test(finalScore$c_a_Language, finalScore$c_a_Lang_r2)$estimate[[1]]
cor.test(finalScore$c_a_Art, finalScore$c_a_Lang_r2)$estimate[[1]]
cor.test(finalScore$c_a_Geography, finalScore$c_a_Lang_r2)$estimate[[1]]
cor.test(finalScore$c_a_Weight, finalScore$c_a_Lang_r2)$estimate[[1]]
cor.test(finalScore$c_a_score_r1, finalScore$c_a_Lang_r2)$estimate[[1]]

#predicting Geog scores:
cor.test(finalScore$c_a_Geography, finalScore$c_a_Geog_r2)$estimate[[1]]
cor.test(finalScore$c_a_Art, finalScore$c_a_Geog_r2)$estimate[[1]]
cor.test(finalScore$c_a_Language, finalScore$c_a_Geog_r2)$estimate[[1]]
cor.test(finalScore$c_a_Weight, finalScore$c_a_Geog_r2)$estimate[[1]]
cor.test(finalScore$c_a_score_r1, finalScore$c_a_Geog_r2)$estimate[[1]]

#predicting Weight scores:
cor.test(finalScore$c_a_Weight, finalScore$c_a_Weight_r2)$estimate[[1]]
cor.test(finalScore$c_a_Art, finalScore$c_a_Weight_r2)$estimate[[1]]
cor.test(finalScore$c_a_Geography, finalScore$c_a_Weight_r2)$estimate[[1]]
cor.test(finalScore$c_a_Language, finalScore$c_a_Weight_r2)$estimate[[1]]
cor.test(finalScore$c_a_score_r1, finalScore$c_a_Weight_r2)$estimate[[1]]





