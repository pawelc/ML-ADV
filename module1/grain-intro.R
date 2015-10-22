yn <- c("yes","no")
a <- cptable(~asia, values=c(1,99),levels=yn)
t.a <- cptable(~tub|asia, values=c(5,95,1,99),levels=yn)
s <- cptable(~smoke, values=c(5,5), levels=yn)
l.s <- cptable(~lung|smoke, values=c(1,9,1,99), levels=yn)
b.s <- cptable(~bronc|smoke, values=c(6,4,3,7), levels=yn)
e.lt <- cptable(~either|lung:tub,values=c(1,0,1,0,1,0,0,1),levels=yn)
x.e <- cptable(~xray|either, values=c(98,2,5,95), levels=yn)
d.be <- cptable(~dysp|bronc:either, values=c(9,1,7,3,8,2,1,9), levels=yn)

plist <- compileCPT(list(a, t.a, s, l.s, b.s, e.lt, x.e, d.be))
plist
plist$tub
plist$lung
plist$either ## Notice: a logical node
net1 <- grain(plist)
net1

querygrain(net1, nodes=c("lung","bronc"), type="marginal")

net12 <- setEvidence(net1, evidence=list(asia="yes", dysp="yes"))
pEvidence( net12 )
querygrain( net12, nodes=c("lung","bronc") )
querygrain( net12, nodes=c("lung","bronc"), type="joint" )

net13 <- setEvidence(net1,nodes=c("either", "tub"), states=c("no","yes"))
pEvidence( net13 )
querygrain( net13, nodes=c("lung","bronc"), type="joint" )

tt <- querygrain( net1, type="joint")
sum(tt==0)/length(tt)

sum(tableSlice(tt, c("either","tub"), c("no","yes")))