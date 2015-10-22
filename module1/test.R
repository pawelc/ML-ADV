source("http://bioconductor.org/biocLite.R")
biocLite(c("graph","RBGL","Rgraphviz"))
install.packages("gRain", dependencies=TRUE)

require(gRain)
yn   <- c("yes","no")
ynm  <- c("yes","no","maybe")
a    <- cptable( ~ asia, values=c(1,99), levels=yn)
t.a  <- cptable( ~ tub : asia, values=c(5,95,1,99,1,999),  levels=ynm)
d.a  <- cptable( ~ dia : asia, values=c(5,5,1,99,100,999), levels=ynm)
cptlist <- compileCPT(list(a,t.a,d.a))
g<-grain(cptlist)
iplot(g)

## Example: Specifying conditional probabilities as a matrix
bayes.levels  <- c('Enzyme', 'Keratine', 'unknown')
root.node     <- cptable( ~R, values=c( 1, 1, 1 ), levels=bayes.levels)
cond.prob.tbl <- t(matrix( c( 1, 0, 0, 0, 1, 0, 0.5, 0.5, 0 ),
                           nrow=3, ncol=3, byrow=TRUE, dimnames=list(bayes.levels, bayes.levels)))
cond.prob.tbl
## Notice above: Columns represent parent states; rows represent child states
query.node    <- cptable( ~ Q | R, values=cond.prob.tbl, levels=bayes.levels )
sister.node   <- cptable( ~ S | R, values=cond.prob.tbl, levels=bayes.levels )
## Testing 
compile(grain(compileCPT(list( root.node, query.node, sister.node ))), propagate=TRUE)


querygrain(g)
querygrain(g, c(d.a)) # marginals of all nodes
querygrain(g, nodes, type = "joint") # joint of nodes
querygrain(g, nodes, type = "conditional") # first variable condtional on all others