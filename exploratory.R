## Exploratory 

# EXPLORATORY: 
# model E1 , t_score <- t_copied - throughout or round 2? all conds. 
# below is from First Analysis, so just includes B and C where copying was informative but we want all. 

finalScore_list <- list(
  t_score = finalScore$t_score,
  groupIndex = finalScore$groupIndex,
  t_copied = finalScore$t_copied
)


modelE1 <- map2stan(
  alist(
    t_score ~ dnorm(mu, sigma),
    mu <- a + b*t_copied + g[groupIndex],
    a ~ dnorm(50,10),
    b ~ dnorm(0,1),
    g[groupIndex] ~ dnorm(0,1),
    sigma ~ dexp(1)
  ), data = finalScore_list, chains=3)

precis(modelE1)

# control condition: 

finalScoreA <- as.data.frame(finalScoreA)

Ngroups = length(unique(finalScoreA$u_network))
Oldgroup <- finalScoreA$u_network
groupIndex <- array(0,length(finalScoreA$u_network))
for (index in 1:Ngroups){
  groupIndex[Oldgroup == unique(Oldgroup)[index]] = index
}
finalScoreA$groupIndex <- groupIndex
finalScoreA$groupIndex <- as.integer(finalScoreA$groupIndex)

finalScoreA$t_score <- as.integer(finalScoreA$t_score)

finalScoreA_list <- list(
  t_score = finalScoreA$t_score,
  groupIndex = finalScoreA$groupIndex,
  t_copied = finalScoreA$t_copied
)

model5.2.1 <- map2stan(
  alist(
    t_score ~ dnorm(mu, sigma),
    mu <- a + b*t_copied + g[groupIndex],
    a ~ dnorm(50,10),
    b ~ dnorm(0,1),
    g[groupIndex] ~ dnorm(0,1),
    sigma ~ dexp(1)
  ), data = finalScoreA_list, chains=3)

precis(model5.2.1)

# EXPLORATORY: 
# model 5.3 , c_copies <- t_score


finalScoreBC <- as.data.frame(finalScoreBC)


Ngroups = length(unique(finalScoreBC$u_network))
Oldgroup <- finalScoreBC$u_network
groupIndex <- array(0,length(finalScoreBC$u_network))
for (index in 1:Ngroups){
  groupIndex[Oldgroup == unique(Oldgroup)[index]] = index
}
finalScoreBC$groupIndex <- groupIndex
finalScoreBC$groupIndex <- as.integer(finalScoreBC$groupIndex)

Nconds = length(unique(finalScoreBC$condition))
Oldconds <- finalScoreBC$condition
condsIndex <- array(0,length(finalScoreBC$condition))
for (index in 1:Nconds){
  condsIndex[Oldconds == unique(Oldconds)[index]] = index
}
finalScoreBC$condsIndex <- condsIndex
finalScoreBC$condsIndex <- as.integer(finalScoreBC$condsIndex)

finalScoreBC$t_score <- as.integer(finalScoreBC$t_score)
finalScoreBC$c_copies <- as.integer(finalScoreBC$c_copies)

finalScoreBC_list <- list(
  t_score = finalScoreBC$t_score,
  groupIndex = finalScoreBC$groupIndex,
  condsIndex = finalScoreBC$condsIndex,
  c_copies = finalScoreBC$c_copies
)


model5.3 <- map2stan(
  alist(
    c_copies ~ dnorm(mu, sigma),
    mu <- a + b*t_score + g[groupIndex],
    a ~ dnorm(6,10),
    b ~ dnorm(0,0.5),
    g[groupIndex] ~ dnorm(0,0.5),
    sigma ~ dexp(1)
  ), data = finalScoreBC_list, chains=3)

precis(model5.3)
