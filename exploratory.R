## Exploratory 

# EXPLORATORY: 
# model E1 , t_score <- t_copied  is score determined by copying?

finalScore_list <- list(
  t_score = finalScore$t_score,
  groupIndex = finalScore$groupIndex,
  t_copied = finalScore$t_copied,
  c_copies = finalScore$c_copies
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

#mean   sd  5.5% 94.5% n_eff Rhat
#a     66.03 0.69 64.93 67.11  4763    1
#b      0.24 0.03  0.20  0.28  4498    1
#sigma 10.69 0.41 10.05 11.34  6885    1
 

# 
# model E2 , c_copies <- t_score  is prestige determined by quiz score?


modelE2 <- map2stan(
  alist(
    c_copies ~ dnorm(mu, sigma),
    mu <- a + b*t_score + g[groupIndex],
    a ~ dnorm(20,10),
    b ~ dnorm(50,10),
    g[groupIndex] ~ dnorm(0,1),
    sigma ~ dexp(1)
  ), data = finalScore_list, chains=3)

precis(modelE2)

#mean   sd   5.5% 94.5% n_eff Rhat
#a     -5.99 3.64 -11.67 -0.14  1678    1
#b      0.22 0.05   0.14  0.30  1681    1
#sigma 12.12 0.46  11.40 12.86  3345    1

plot(finalScore$c_copies ~ finalScore$t_score)

scorePresPlot <- ggplot(data = finalScore, mapping = aes(x = t_score, y = c_copies)) + 
  geom_point() + 
  geom_smooth() + theme_bw() + xlab("Total Score") + ylab("Prestige Score")
scorePresPlot

