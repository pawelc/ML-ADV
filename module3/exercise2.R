library(rjags)
library(coda)

X <- c(4, 5, 4, 0, 1, 4, 3, 4, 0, 6, 3, 3, 4, 0, 2, 6,
       3, 3, 5, 4, 5, 3, 1, 4, 4, 1, 5, 5, 3, 4, 2, 5,
       2, 2, 3, 4, 2, 1, 3, 2, 2, 1, 1, 1, 1, 3, 0, 0,
       1, 0, 1, 1, 0, 0, 3, 1, 0, 3, 2, 2, 0, 1, 1, 1,
       0, 1, 0, 1, 0, 0, 0, 2, 1, 0, 0, 0, 1, 1, 0, 2,
       3, 3, 1, 1, 2, 1, 1, 1, 1, 2, 4, 2, 0, 0, 1, 4,
       0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1)

# Question 1

mod.string <- "model {
  #prior on 2 regimes
  mu1 ~ dexp(1)
  mu2 ~ dexp(1)
  # uniform prior on change index
  t ~ dunif(0,N)
  for(i in 1:N) {
    # based on change time choose correct regime
    mu[i] <- ifelse(i<=t, mu1, mu2)
    # sample number of disasters from the regime
    x[i] ~ dpois(mu[i])
  }
}"

jagsModel <- jags.model(textConnection(mod.string), n.chains=1, n.adapt=100, data=list("N"=length(X),"x"=X))
update( jagsModel , n.iter=2000 )
samples <- coda.samples(jagsModel, c("t","mu1","mu2"), 3000, thin=1)
# show posterior on change t and lambda parameters of 2 Poissons
plot(samples)

# Question 2

mod.string <- "model {
  # now we have three regimes
  for(i in 1:3) {
    mu[i] ~ dexp(1)
  }

  # we sample 2 possible change points
  change1 ~ dunif(0,N-1)
  change2 ~ dunif(round(change1),N)
  
  for(i in 1:N) {
    # choose correct regime
    lambda[i] <- ifelse(i<=change1, mu[1], ifelse(i<=change2, mu[2], mu[3]) )
    x[i] ~ dpois(lambda[i])
  }
}"
jagsModel <- jags.model(textConnection(mod.string), n.chains=1, n.adapt=100, data=list("N"=length(X),"x"=X))
update( jagsModel , n.iter=2000 )
samples <- coda.samples(jagsModel, c("change1","change2"), 3000, thin=1)
# show posterior on change 2 possible changes in regime
plot(samples)