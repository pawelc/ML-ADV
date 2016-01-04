#I built jags model and tried it on poliblog example. It looks the it takes even this amount of data is too much for jags to run 
#experiments quickly

library(rjags)
library(coda)

library(lda)
data(poliblog.documents)
poliblog.documents[[1]]
data(poliblog.vocab)

#transform data in poliblog so it is good for jags
#find maximum of words in any of the documents
maxWordsInDoc <- max(unlist(lapply(poliblog.documents, function(tab){sum(tab[2,])})))

#matrix w will hold all words for given document
#w<-matrix(nrow=length(poliblog.documents), ncol = maxWordsInDoc)
words <- array(NA,dim=c(length(poliblog.documents),maxWordsInDoc));
# nw holds number of words in given document
nw<-rep(0,length(poliblog.documents))
for(d in 1:length(poliblog.documents)){
  #words in this doc
  doc <- unlist(apply(poliblog.documents[[d]],2,function(col){rep(col[1],col[2])}))
  words[d,1:length(doc)] <- doc+1#they have to be 1 indexed for jags
  nw[d]<-length(doc)
}

mod.string <- "model {
  # K dirchlets for topics modelling distribution of words within topics
  for(i in 1:K) {
    phi[i,1:V] ~ ddirch(beta[])
  }

  for(d in 1:D) {
    # D dirchlets for distribution of topics within each document
    theta[d,1:K] ~ ddirch(alpha[])

    for(w in 1:NW[d]){
          #chooses topic for given word in the document
          z[d,w] ~ dcat(theta[d,])

          #word is selected based words distribution within selected topic
          words[d,w] ~ dcat(phi[z[d,w],])
    }
  }
}"

# Model hyperparameters
K<-10 #number of topics
V<-length(poliblog.vocab)
beta<-rep(0.01,V)
D<-length(poliblog.documents)
alpha<-rep(0.01,K)

jagsModel <- jags.model(textConnection(mod.string), n.chains=1, n.adapt=100, 
                        data=list("K"=K,"beta"=beta,"D"=D,"alpha"=alpha,"NW"=nw,"V"=V,"words"=words))
update( jagsModel , n.iter=2000 )
samples <- coda.samples(jagsModel, c("phi"), 3000, thin=1)
