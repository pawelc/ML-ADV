library(rjags)
library(coda)

mod.string <- "model {

  p1[1]<-1
  p1[2]<-1

  p2[1]<-1
  p2[2]<-10

  for(i in 1:3){
    x[i] ~ dcat(p1)
    y[i] ~ dcat(p2)

    z[i] <- ifelse(i==2, x[i], y[i])
  }
}"

jags <- jags.model(textConnection(mod.string),
                   n.chains = 4,
                   n.adapt = 100)

update(jags, 1000)

x <- coda.samples(jags, c("z"), 500, thin=1)

plot(x)