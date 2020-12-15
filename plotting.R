#plotting 
# run relevant sections of analysis_script first to get e.g. finalScore dataframe


hist(finalScore$c_copies)
hist(finalScore$t_copied)


prestigePlot + theme_bw() + 
  theme(text = element_text(size=12), axis.title.y=element_text(margin=margin(0,12,0,0))) +
  scale_y_continuous(limits=c(0,30), expand = c(0,0)) +
  scale_x_continuous(limits=c(0,70), expand= c(0,0)) +
  xlab("Number of times copied") + ylab("Number of Participants")

plot(finalScore$c_copies ~ finalScore$u_origin)

finalScore$groupIndex <- as.factor(finalScore$groupIndex)
presPlot2<- ggplot(data = finalScore, mapping = aes(x = u_origin, y = c_copies, color = groupIndex)) + 
  geom_point(size=3)


presPlot2 + theme_bw() + 
  geom_vline(aes(xintercept=100), linetype="dashed", show.legend=FALSE) + 
  geom_vline(aes(xintercept=200), linetype="dashed", show.legend=FALSE) +
  geom_vline(aes(xintercept=300), linetype="dashed", show.legend=FALSE) +
  annotate("text", x=50, y=80, label=paste("Condition\nC")) +
  annotate("text", x=150, y=80, label=paste("Condition\nA")) +
  annotate("text", x=250, y=80, label=paste("Condition\nB")) +
  annotate("text", x=350, y=80, label=paste("Condition\nD")) +
  theme(text = element_text(size=12), axis.title.y=element_text(margin=margin(0,12,0,0)), plot.title = element_text(hjust=0.5)) +
  labs(color = "Group ID") +
  xlab("Participant ID") + ylab("Final Prestige Score")



plot(finalScore$c_copies ~ finalScore$t_score)

scorePresPlot <- ggplot(data = finalScore, mapping = aes(x = t_score, y = c_copies)) + 
  geom_point() + 
  geom_smooth() + theme_bw() + xlab("Total Score") + ylab("Prestige Score")
scorePresPlot



#plot raw counts
round2 <- copyOnly[copyOnly$round==2,]

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(data = round2) + 
  geom_bar(mapping = aes(x = condition, fill = info_chosen)) +
  labs(fill = "Participant Choice") +
  xlab("Conditions") + ylab("Total times chosen") +
  theme_bw() +
  theme(text = element_text(size=16), axis.text = element_text(size=14)) + 
  scale_fill_manual(labels = c("Random Cue", "Domain-general prestige","Cross-domain prestige","Domain-specific prestige"), values=cbbPalette)

#plot raw counts
ggplot(infoChosen, aes(x=condition, fill=info_chosen)) +
  geom_histogram(position="dodge", stat="count") + 
  theme_bw()

table(round2$info_chosen, round2$condition)
