# image denoising using Markov networks

# prepare image
#----------------
#library(png)
#I<-t(png::readPNG("letting_go.png")[,,1])
#I <- I[,seq(201,1,-1)]
#image(I)
## add noise
#e = 0.1
#In <- I
#for (i in seq(1,dim(I)[1])) {
#  for (j in seq(1,dim(I)[2])) {
#    if(runif(1) < e)
#      In[i,j] = 1-In[i,j]
#  }
#}
#image(In)
#save(In, file="noisyIm.RData")

# load noisy image and denoise using JAGS
#----------------------------------------
load("noisyIm.RData")
image(In)

nx <- dim(In)[1]
ny <- dim(In)[2]

bugs <- "
model {
...................
fill in bugs model here
...................
}
"

# model parameters
a <- 5
b <- 2

library(rjags)
jags <- jags.model(textConnection(bugs),
                   data = list('X' = In,
                               'a' = a, 'b'=b,
                               'nx' = nx, 'ny'=ny),
                   n.chains = 1,
                   n.adapt = 100)
print(jags)
cs <- jags.samples(jags, c("Y"), n.iter=1000)

# take average image: we don't have maximization in JAGS
x11()
Idenoised <- summary(cs$Y,mean)$stat
image(Idenoised)

# write PNG image
#library(png)
#writePNG(Idenoised, paste("/tmp/denoised_", a, "_", b, ".png", sep=""))
