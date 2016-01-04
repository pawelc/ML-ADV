
#installing graph kernels
source("http://bioconductor.org/biocLite.R")
biocLite("Rchemcpp")

require(Rchemcpp)

m <- list()
m[[1]] <- createRMolecule(c("C","C"), matrix(c(0,1,1,0),nrow=2))
m[[2]] <- createRMolecule(c("C","C","C"), matrix(c(0,1,1,1,0,1,1,1,0),nrow=3))
m[[3]] <- createRMolecule(c("C","C","C"), matrix(c(0,1,0,1,0,1,0,1,0),nrow=3))
set <- new(Rmoleculeset)
set$addMoleculeCopy(m[[1]])
set$addMoleculeCopy(m[[2]])
set$addMoleculeCopy(m[[3]])
s <- sd2gramSpectrum(set)


m <- list()
m[[1]] <- createRMolecule(c("C","C"), matrix(c(0,1,1,0),nrow=2))
m[[2]] <- createRMolecule(c("C","C","C"), matrix(c(0,1,1,1,0,1,1,1,0),nrow=3))
m[[3]] <- createRMolecule(c("C","C","C"), matrix(c(0,1,0,1,0,1,0,1,0),nrow=3))
set <- new(Rmoleculeset)
set$addMoleculeCopy(m[[1]])
set$addMoleculeCopy(m[[2]])
set$addMoleculeCopy(m[[3]])
s <- sd2gramSpectrum(set)
print(s)

require(igraph)
t<-graph.tree(15,2,mode="undirected")
adjMat <- get.adjacency(t,sparse=0)
adjMat[1,4]<-1
adjMat[4,1]<-1
t<-graph.adjacency(adjMat)
plot(t, layout = layout.reingold.tilford(t, root=1))

m[[1]] <- createRMolecule(rep('c',15), get.adjacency(t,sparse=0))

install.packages("kernlab")
require(kernlab)
m <- ksvm(as.kernelMatrix(s),c(1,0,0),type="C-svc",kernel="matrix")
ypred <- predict(m,as.kernelMatrix(s))

source("execrcise7.R")
generateGraphsGramWithLabels(100)
