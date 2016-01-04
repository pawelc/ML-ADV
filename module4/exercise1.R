library(lda)

data(poliblog.documents)
data(poliblog.vocab)
data(poliblog.ratings)

doc.length <- sapply(poliblog.documents, function(x) sum(x[2, ]))  # number of tokens per document 

term.frequency <- rep(0,length(poliblog.vocab))
for(i in 1:length(poliblog.documents)){
  term.frequency[poliblog.documents[[i]][1,]+1] <- term.frequency[poliblog.documents[[i]][1,]+1] + poliblog.documents[[i]][2,]
}

K <- 10 ## Num clusters
G <- 1000
alpha <- 0.001
eta <- 0.001

fit <- lda.collapsed.gibbs.sampler(documents = poliblog.documents, K = K, vocab = poliblog.vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)

## Get the top words in the cluster
top.words <- top.topic.words(fit$topics, 5, by.score=TRUE)

theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

poliblog <- list(phi = phi,
                     theta = theta,
                     doc.length = doc.length,
                     vocab = poliblog.vocab,
                     term.frequency = term.frequency)

library(LDAvis)

json <- createJSON(phi = poliblog$phi, 
                   theta = poliblog$theta, 
                   doc.length = poliblog$doc.length, 
                   vocab = poliblog$vocab, 
                   term.frequency = poliblog$term.frequency)

serVis(json, out.dir = 'vis', open.browser = FALSE)
