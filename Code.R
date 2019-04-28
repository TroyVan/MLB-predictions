library(boot)
library(plyr)
library(dplyr)
library(gbm)
library(ggplot2)
library(readr)
library(rjags)

# In all RNG operations a seed of 454 is used for reproducibility

# Code to read the data
gamestates <- read_csv("gamestates.csv", col_types = cols(
  homeScore = col_integer(),
  homeWin = col_logical(),
  innings = col_integer(),
  visitorScore = col_integer()
))
teststates <- read_csv("teststates.csv", col_types = cols(
  homeScore = col_integer(),
  homeWin = col_logical(),
  innings = col_integer(),
  visitorScore = col_integer()
))

# Extra fields
gamestates$diff = gamestates$homeScore - gamestates$visitorScore
gamestates$trailScore = pmin(gamestates$visitorScore, gamestates$homeScore)

teststates$diff = teststates$homeScore - teststates$visitorScore
teststates$trailScore = pmin(teststates$visitorScore, teststates$homeScore)

# Brier score function
brier = function(y, p) {
  return(mean((p - y)^2))
}

# Breakdown by inning and difference
breakdown = count(gamestates, innings, diff)
breakdown$homeWins = rep(0, nrow(breakdown))
for(i in 1:nrow(breakdown)) {
  breakdown$homeWins[i] = nrow(filter(gamestates,
                                      innings == breakdown$innings[i],
                                      diff == breakdown$diff[i],
                                      homeWin == TRUE
                                      ))
}
breakdown$hprob = breakdown$homeWins / breakdown$n

# Breakdown by difference and trailing score
magbreakdown = count(gamestates, trailScore, diff)
magbreakdown$homeWins = rep(0, nrow(magbreakdown))
for(i in 1:nrow(magbreakdown)) {
  magbreakdown$homeWins[i] = nrow(filter(gamestates,
                                      trailScore == magbreakdown$trailScore[i],
                                      diff == magbreakdown$diff[i],
                                      homeWin == TRUE
  ))
}
magbreakdown$hprob = magbreakdown$homeWins / magbreakdown$n

# --------------------------------
# Model 1: using difference

# In this model:
# y = binary indicating winning team (0 for visitor win, 1 for home win)
# x = difference in score (negative for visitor lead, positive for home lead)
# p = probability of home win in the given state
# b0 = home field advantage effect
# b1 = effect of a run

# DEFINE
model_1 <- "model{  
    # Data
    for(i in 1:length(y)) {
        y[i] ~ dbin(p[i], 1)
        logit(p[i]) = b0 + b1 * x[i]
    }

    # Priors
    b0 ~ dnorm(0, 1/100)
    b1 ~ dnorm(0, 1/100)
}"

# COMPILE
model_1_jags <- jags.model(textConnection(model_1), 
                          data = list(y = gamestates$homeWin, x = gamestates$diff),
                          inits=list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 454))

# SIMULATE
model_1_sim <- coda.samples(model_1_jags,
                           variable.names = c("b0", "b1"),
                           n.iter = 10000)

# STORE CHAINS
model_1_chains <- data.frame(model_1_sim[[1]])

# Visualize distribution
M1 = data.frame(diff = -10:10, p = inv.logit(mean(model_1_chains$b0) + mean(model_1_chains$b1) * -10:10))
ggplot(M1, aes(x = diff, y = p)) +
  geom_line()

# Produce predictions
set.seed(454)
for(i in 1:nrow(teststates)) {
  teststates$M1[i] = mean(rbinom(nrow(model_1_chains), 1, inv.logit(model_1_chains$b0 + model_1_chains$b1 * teststates$diff[i])))
}

# Test predictions
calibrate.plot(teststates$homeWin,teststates$M1, shade.col = "lightblue")
brier(teststates$homeWin,teststates$M1)

# --------------------------------
# Model 2: using difference and inning

# In this model:
# y = binary indicating winning team (0 for visitor win, 1 for home win)
# x = difference in score (negative for visitor lead, positive for home lead)
# t = number of innings completed
# p = probability of home win in the given state
# b0 = home field advantage effect
# b1 = effect of a run
# b2 = effect time in innings
# b3 = effect of a run * time in innings

# DEFINE
model_2 <- "model{  
  # Data
  for(i in 1:length(y)) {
    y[i] ~ dbin(p[i], 1)
    logit(p[i]) = b0 + b1 * x[i] + b2 * t[i] + b3 * x[i] * t[i]
  }

  # Priors
  b0 ~ dnorm(0, 1/100)
  b1 ~ dnorm(0, 1/100)
  b2 ~ dnorm(0, 1/100)
  b3 ~ dnorm(0, 1/100)
}"

# COMPILE
model_2_jags <- jags.model(textConnection(model_2), 
                           data = list(y = gamestates$homeWin, x = gamestates$diff, t = gamestates$innings),
                           inits=list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 454))

# SIMULATE
model_2_sim <- coda.samples(model_2_jags,
                            variable.names = c("b0", "b1", "b2", "b3"),
                            n.iter = 10000)

# STORE CHAINS
model_2_chains <- data.frame(model_2_sim[[1]])

# Visualize distribution
M2 = data.frame(diff = rep(-10:10, each = 9), innings = rep(0:8, times = 21))
M2$p = inv.logit(
    mean(model_2_chains$b0) +
    mean(model_2_chains$b1) * M2$diff +
    mean(model_2_chains$b2) * M2$innings +
    mean(model_2_chains$b3) * M2$diff * M2$innings
  )
ggplot(M2, aes(x = innings, y = p, group = diff, color = diff)) +
  geom_line()

# Produce predictions
set.seed(454)
for(i in 1:nrow(teststates)) {
  teststates$M2[i] = mean(rbinom(nrow(model_2_chains), 1, inv.logit(
    model_2_chains$b0 +
      model_2_chains$b1 * teststates$diff[i] +
      model_2_chains$b2 * teststates$innings[i] +
      model_2_chains$b3 * teststates$diff[i] * teststates$innings[i]
  )))
}

# Test predictions
calibrate.plot(teststates$homeWin,teststates$M2, shade.col = "lightblue")
brier(teststates$homeWin,teststates$M2)
brier(filter(teststates, innings > 8)$homeWin,filter(teststates, innings > 8)$M2) # Extra innings

# --------------------------------
# Model 2A: 2 with 9 innings only

# In this model:
# y = binary indicating winning team (0 for visitor win, 1 for home win)
# x = difference in score (negative for visitor lead, positive for home lead)
# t = number of innings completed
# p = probability of home win in the given state
# b0 = home field advantage effect
# b1 = effect of a run
# b2 = effect time in innings
# b3 = effect of a run * time in innings

# DEFINE
model_2A <- "model{  
  # Data
  for(i in 1:length(y)) {
    y[i] ~ dbin(p[i], 1)
    logit(p[i]) = b0 + b1 * x[i] + b2 * t[i] + b3 * x[i] * t[i]
  }

  # Priors
  b0 ~ dnorm(0, 1/100)
  b1 ~ dnorm(0, 1/100)
  b2 ~ dnorm(0, 1/100)
  b3 ~ dnorm(0, 1/100)
}"

nineinnstates = filter(gamestates, innings < 9)

# COMPILE
model_2A_jags <- jags.model(textConnection(model_2A), 
                            data = list(y = nineinnstates$homeWin, x = nineinnstates$diff, t = nineinnstates$innings),
                            inits=list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 454))

# SIMULATE
model_2A_sim <- coda.samples(model_2A_jags,
                            variable.names = c("b0", "b1", "b2", "b3"),
                            n.iter = 10000)

# STORE CHAINS
model_2A_chains <- data.frame(model_2A_sim[[1]])

# Visualize distribution
M2A = data.frame(diff = rep(-10:10, each = 9), innings = rep(0:8, times = 21))
M2A$p = inv.logit(
  mean(model_2A_chains$b0) +
    mean(model_2A_chains$b1) * M2A$diff +
    mean(model_2A_chains$b2) * M2A$innings +
    mean(model_2A_chains$b3) * M2A$diff * M2A$innings
)
ggplot(M2A, aes(x = innings, y = p, group = diff, color = diff)) +
  geom_line()

# Produce predictions
nineInnTestStates = filter(teststates, innings < 9)

set.seed(454)
for(i in 1:nrow(nineInnTestStates)) {
  nineInnTestStates$M2A[i] = mean(rbinom(nrow(model_2A_chains), 1, inv.logit(
    model_2A_chains$b0 +
      model_2A_chains$b1 * nineInnTestStates$diff[i] +
      model_2A_chains$b2 * nineInnTestStates$innings[i] +
      model_2A_chains$b3 * nineInnTestStates$diff[i] * nineInnTestStates$innings[i]
  )))
}

# Test predictions
calibrate.plot(nineInnTestStates$homeWin,nineInnTestStates$M2A, shade.col = "lightblue")
brier(nineInnTestStates$homeWin,nineInnTestStates$M2A)

# --------------------------------
# Model 2B: 2 with extra innings only

# Extra innings always start with the score tied, so there is no diff

# In this model:
# y = binary indicating winning team (0 for visitor win, 1 for home win)
# t = number of innings completed
# p = probability of home win in the given state
# b0 = home field advantage effect
# b2 = effect time in innings

# DEFINE
model_2B <- "model{  
  # Data
  for(i in 1:length(y)) {
    y[i] ~ dbin(p[i], 1)
    logit(p[i]) = b0 + b2 * t[i]
  }

  # Priors
  b0 ~ dnorm(0, 1/100)
  b2 ~ dnorm(0, 1/100)
}"

extraInnStates = filter(gamestates, innings >= 9)

# COMPILE
model_2B_jags <- jags.model(textConnection(model_2B), 
                           data = list(y = extraInnStates$homeWin, t = extraInnStates$innings),
                           inits=list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 454))

# SIMULATE
model_2B_sim <- coda.samples(model_2B_jags,
                            variable.names = c("b0", "b2"),
                            n.iter = 10000)

# STORE CHAINS
model_2B_chains <- data.frame(model_2B_sim[[1]])

# Visualize distribution
M2B = data.frame(innings = 9:20)
M2B$p = inv.logit(mean(model_2B_chains$b0) + mean(model_2B_chains$b2) * M2B$innings)
ggplot(M2B, aes(x = innings, y = p)) +
  geom_line()

# Produce predictions
extraInnTestStates = filter(teststates, innings >= 9)

set.seed(454)
for(i in 1:nrow(extraInnTestStates)) {
  extraInnTestStates$M2B[i] = mean(rbinom(nrow(model_2B_chains), 1, inv.logit(
    model_2B_chains$b0 +
      model_2B_chains$b2 * extraInnTestStates$innings[i]
  )))
}

# Test predictions
calibrate.plot(extraInnTestStates$homeWin,extraInnTestStates$M2B, shade.col = "lightblue")
brier(extraInnTestStates$homeWin,extraInnTestStates$M2B)

# --------------------------------
# Model 2C: 2 but extra innings are deemed as 9th inning

# In this model:
# y = binary indicating winning team (0 for visitor win, 1 for home win)
# x = difference in score (negative for visitor lead, positive for home lead)
# t = number of innings completed
# p = probability of home win in the given state
# b0 = home field advantage effect
# b1 = effect of a run
# b2 = effect time in innings
# b3 = effect of a run * time in innings

# Definition same as model 2

modGameStates = gamestates
modGameStates$innings = pmin(modGameStates$innings, rep(8,nrow(modGameStates)))

# COMPILE
model_2C_jags <- jags.model(textConnection(model_2), 
                           data = list(y = modGameStates$homeWin, x = modGameStates$diff, t = modGameStates$innings),
                           inits=list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 454))

# SIMULATE
model_2C_sim <- coda.samples(model_2C_jags,
                            variable.names = c("b0", "b1", "b2", "b3"),
                            n.iter = 10000)

# STORE CHAINS
model_2C_chains <- data.frame(model_2C_sim[[1]])

# Visualize distribution
M2C = data.frame(diff = rep(-10:10, each = 9), innings = rep(0:8, times = 21))
M2C$p = inv.logit(
  mean(model_2C_chains$b0) +
    mean(model_2C_chains$b1) * M2C$diff +
    mean(model_2C_chains$b2) * M2C$innings +
    mean(model_2C_chains$b3) * M2C$diff * M2C$innings
)
ggplot(M2C, aes(x = innings, y = p, group = diff, color = diff)) +
  geom_line()

# Produce predictions
set.seed(454)
for(i in 1:nrow(teststates)) {
  teststates$M2C[i] = mean(rbinom(nrow(model_2C_chains), 1, inv.logit(
    model_2C_chains$b0 +
      model_2C_chains$b1 * teststates$diff[i] +
      model_2C_chains$b2 * min(teststates$innings[i], 8) +
      model_2C_chains$b3 * teststates$diff[i] * min(teststates$innings[i], 8)
  )))
}

# Test predictions
calibrate.plot(teststates$homeWin,teststates$M2C, shade.col = "lightblue")
brier(teststates$homeWin,teststates$M2C)
brier(filter(teststates, innings > 8)$homeWin,filter(teststates, innings > 8)$M2C) # Extra innings

# --------------------------------
# Naive model N1: using difference

N1 = count(gamestates, diff)
N1$homeWins = rep(0, nrow(N1))
for(i in 1:nrow(N1)) {
  N1$homeWins[i] = nrow(filter(gamestates,
                               diff == N1$diff[i],
                               homeWin == TRUE
  ))
}
N1$hprob = N1$homeWins / N1$n

for(i in 1:nrow(teststates)) {
  # If the score difference is greater than 10, a definite prediction is given
  if(teststates$diff[i] < -10) {
    teststates$N1[i] = 0
  } else if(teststates$diff[i] > 10) {
    teststates$N1[i] = 1
  } else {
    teststates$N1[i] = N1$hprob[which(teststates$diff[i] == N1$diff)[[1]]]
  }
}

# Test predictions
calibrate.plot(teststates$homeWin,teststates$N1, shade.col = "lightblue")
brier(teststates$homeWin,teststates$N1)

# --------------------------------
# Naive model N2: using difference and inning

N2 = count(gamestates, diff, innings)
N2$homeWins = rep(0, nrow(N2))
for(i in 1:nrow(N2)) {
  N2$homeWins[i] = nrow(filter(gamestates,
                               diff == N2$diff[i],
                               innings == N2$innings[i],
                               homeWin == TRUE
  ))
}
N2$hprob = N2$homeWins / N2$n

for(i in 1:nrow(teststates)) {
  # If the score difference is 10 runs or more, a definite prediction is given
  if(teststates$diff[i] <= -10) {
    teststates$N2[i] = 0
  } else if(teststates$diff[i] >= 10) {
    teststates$N2[i] = 1
  } else if(teststates$innings[i] > max(N2$innings)){
    # For an inning beyond the maximum in the training set, the 10th inning frequency is used
    teststates$N2[i] = N2$hprob[which((teststates$diff[i] == N2$diff) & (9 == N2$innings))[[1]]]
  } else {
    teststates$N2[i] = N2$hprob[which(
      (teststates$diff[i] == N2$diff) &
      (teststates$innings[i] == N2$innings)
    )[[1]]]
  }
}

# Test predictions
calibrate.plot(teststates$homeWin,teststates$N2, shade.col = "lightblue")
brier(teststates$homeWin,teststates$N2)
