require(igraph)
require(kernlab)
require(Rchemcpp)
require(pROC)
set.seed(1)

generateGraphsGramWithLabels<-function(n){
  set <- new(Rmoleculeset)
  labels<-c()
  for(i in 1:n){#generate n graphs
    #of random number of vertices
    numVert <- sample(3:4, 1)
    #generate tree
    t<-graph.tree(numVert,2,mode="undirected")
    #get its adjecency matrix
    adjMat <- get.adjacency(t,sparse=0)
    
    if(runif(1)>0.5){#adding cycle half of the times
      #adding one edge add cycle to the tree
      #find missing edges
      zeroIndx <- which(adjMat==0)
      #remove self edges
      zeroIndx = zeroIndx[!zeroIndx%in%c((0:(numVert-1))*numVert + (1:numVert))]
      edgeToAddIdx <- sample(zeroIndx,1)
      
      row <- ceiling(edgeToAddIdx/numVert)
      col <- (edgeToAddIdx-1)%%numVert+1
      adjMat[row,col]<-1
      adjMat[col,row]<-1
      #label for graph with cycle
      labels<-c(labels,1)
    }else{
      #label for graph without cycle
      labels<-c(labels,-1)
    }
      
    set$addMoleculeCopy(createRMolecule(rep('c',numVert), adjMat))
  }
  list(gram=sd2gramSpectrum(set,kernelType="spectrum"),labels=labels)
}

<<<<<<< HEAD
n=50
=======
n=40
>>>>>>> d21510ee1210f87cafb20ff928d302f12757ee60
dataK <- generateGraphsGramWithLabels(n)

# Split the data into training set and test set
ntrain <- round(n*0.3) # number of training examples
tindex <- sample(n,ntrain) # indices of training samples
kernelMatrix<-as.kernelMatrix(dataK$gram)
model <- ksvm(kernelMatrix[tindex,tindex],dataK$labels[tindex],type="C-svc",kernel='matrix',prob.model=TRUE)

# remove train
testK <- kernelMatrix[-tindex,tindex]
# extract the SVs from the train
testK <- testK[,SVindex(model),drop=FALSE]
# predict with the SVM
# convert the matrix testK to a kernelMatrix
ypred <- predict(model,as.kernelMatrix(testK),type="probabilities")

plot(roc(ifelse(dataK$labels[-tindex]==-1,0,1) ,ypred[,2]))
