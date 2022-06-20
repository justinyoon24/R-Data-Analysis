install.packages('markovchain', dependencies = TRUE)
library("markovchain")
stateNames = c("susceptible", "infected", "immune", "dead")
contagionRate = 0.10
contagionRate = 0.05
fatalityRate = 0.000325
fatalityRate = 0.0013
fatalityRate = 0.00845
transitionMatrix = matrix(c((1-contagionRate),contagionRate, 0, 0,
                            0, 0, (1-fatalityRate),fatalityRate,
                            0, 0, 1, 0,
                            0, 0, 0, 1),
                          byrow = TRUE, nrow = 4,
                          dimnames = list(stateNames, stateNames))
epidemic = new("markovchain", transitionMatrix = transitionMatrix)
print(epidemic)
initialState <- c(0.99,0.01,0.00,0.00)
for (i in 0:60){
  state = initialState * (epidemic ^ i)
  cat('time:', i, ' state:',round(state, 6), '\n')}
for (i in 0:10){
  state = initialState * (epidemic ^ (2^i))
  cat('time:',(2^i), ' state:',round(state, 3), '\n')
}
initialPop <- c(899000,1000,100000,0);
n.steps <- 100;
timeSteps.Under50 <- matrix(NA,nrow=n.steps,ncol=4);
for (j in 1:n.steps){
  timeSteps.Under50[j,] <- initialPop*(epidemic^j)
}
timeSteps.Under50
