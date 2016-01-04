sd <- 1
mi <- 5
N <- 10
experimentReps <- 100000

inInterval <- 0
# Repeat the experiment multiple times
for(expIdx in 1:experimentReps){
  # generate a random sample
  sample <- rnorm(n = N, mean = mi, sd = sd)
  # compute the interval based on the sample
  interval <- mean(sample) + qnorm(0.975) * c(-1,1) * (sd/sqrt(N))
  # see if the true value 5 falls inside it
  if(mi >= interval[1] && mi <= interval[2]){
    inInterval <- inInterval + 1    
  }
}

# coverage probability
covaerageProb <- inInterval/experimentReps
cat(paste0('Coverage probability for frequentist is ', covaerageProb))

# Now, pick a prior on mi
# Question 1: Compute the coverage probability for the Bayesian confidence interval in the above setting.
inIntervalBayes <- 0
priorMi <- 0
priorSd <- 1
# Repeat the experiment multiple times
for(expIdx in 1:experimentReps){
  # generate a random sample
  sample <- rnorm(n = N, mean = mi, sd = sd)
  
  postMi <- (sd^2*priorMi/N+priorSd^2*mean(sample))/(sd^2/N+priorSd^2)
  postSd <- sqrt(1/(N/sd^2 + 1/priorSd^2))
  
  # compute the interval based on the sample
  interval <- postMi + qnorm(0.975) * c(-1,1) * postSd
  # see if the true value 5 falls inside it
  if(mi >= interval[1] && mi <= interval[2]){
    inIntervalBayes <- inIntervalBayes + 1    
  }
}

# coverage probability
covaerageProbBayes <- inIntervalBayes/experimentReps
cat(paste0('Coverage probability for the Bayesian is ', covaerageProbBayes))

# changing procedure to compute 95%
inIntervalBayesChanged <- 0
# now more vague prior
priorMi <- 1
priorSd <- 1000
bayesInterval <- 0
# Repeat the experiment multiple times
for(expIdx in 1:experimentReps){
  # generate a random sample
  sample <- rnorm(n = N, mean = mi, sd = sd)
  
  # posterior the same
  postMi <- (sd^2*priorMi/N+priorSd^2*mean(sample))/(sd^2/N+priorSd^2)
  postSd <- sqrt(1/(N/sd^2 + 1/priorSd^2))
  
  # now computing interval based on posterior distribution
  interval <- c(qnorm(0.025, postMi, postSd),qnorm(0.975, postMi, postSd))
  bayesInterval <- bayesInterval + interval[2] - interval[1]
  # see if the true value 5 falls inside it
  if(mi >= interval[1] && mi <= interval[2]){
    inIntervalBayesChanged <- inIntervalBayesChanged + 1    
  }
}
bayesInterval <- bayesInterval / experimentReps

# coverage probability
covaerageProbBayesChanged <- inIntervalBayesChanged/experimentReps
cat(paste0('Coverage probability computed using modified procedure for the Bayesian is ', covaerageProbBayesChanged))

# Question 2 Coverage probability of the classical confidence interval under the modified procedure
# It doesn't change. The bayes is different because of the inclusion of the prior.

# Question 3: What are the widths of both types of intervals?
cat(paste0('Frequntist interval width is ', 2 * qnorm(0.975) * (sd/sqrt(N))))
cat(paste0('Bayesian interval width is ', bayesInterval))

# Question 4: Based on the above experiments, can you describe the difference between classical and Bayesian confidence intervals?
# Classical confidence intervals are random and can contain fixed estimated value. Bayesian confidence intervals contain 
# probability distribution mass of the parameter which is random variable abd intervals are fixed.
