#see marginal probabilities for all nodes
querygrain(g)
#see marginal some nodes
querygrain(g, nodes=c("bc","ac1"), type="marginal")
#joint
querygrain(g,nodes=c("bc","ac1"), type="joint")

#probabilty of that child has A blood type when mother and father has B
g1 <- setEvidence(g, evidence=list(bc="A",bm="B",bf="B"))
pEvidence( g1 )
#probabilty of that child has A blood type when mother has A and father has B
g2 <- setEvidence(g, evidence=list(bc="A",bm="A",bf="B"))
pEvidence( g2 )
querygrain( g2)

#sample 10 examples from the model
simulate(g, nsim=10)

#show some CPTs
cptlist$am1
cptlist$am2
cptlist$bm