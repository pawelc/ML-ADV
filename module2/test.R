#install.packages("rjags", dependencies=TRUE)
require(rjags)
library(coda)

mod.string <- "model {
  for(i in 1:N) {
    X[i] ~ dexp(1.0)
  }
  Y <- sum(X)
  Y_e ~ dnorm(Y, 1000)
}"

N <- 10 # how many r.v.’s to add?

# UNCONDITIONAL
jags <- jags.model(textConnection(mod.string),
                   n.chains=1, n.adapt=100, data=list("N"=N))
j <- coda.samples(jags, c("Y"), 1000, thin=1)
plot(j)
autocorr.plot(j)

# CONDITION on Y=5
jags <- jags.model(textConnection(mod.string),
                   n.chains=1, n.adapt=10000,
                   data=list("N"=N, "Y_e"=5))
j <- coda.samples(jags, c("X[1]"), 100000, thin=100)
plot(j)
autocorr.plot(j)
j[[1]] # actual samples