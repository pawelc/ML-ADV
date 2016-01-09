# Errors:
# multinomial logistic: 0.2231497
# classical neural network with one hidden layer: 0.1558152
# deep network with one hidden layer: 0.1519199
# deep network with 3 hidden layers: 0.03561491
#
# Additional depth improved test accuracy. It increased expressivness of the model but also more training time was required
# and more hyper parameters had to be chosen.

require(nnet)
require(deepnet)

# Load train and test data
digits_trainX<-read.csv('digits/digits_trainX.csv', header = F)
digits_trainX.mat<-as.matrix(digits_trainX)
digits_trainY<-read.csv('digits/digits_trainY.csv', header = F)
digits_train <- cbind(digits_trainX,digits_trainY)
digits_trainY.ind <- class.ind(digits_trainY[,])
colnames(digits_train)[65]<-'dig'

digits_testX<-read.csv('digits/digits_testX.csv', header = F)
digits_testX.mat<-as.matrix(digits_testX)
digits_testY<-read.csv('digits/digits_testY.csv', header = F)
digits_test <- cbind(digits_testX,digits_testY)
digits_testY.ind <- class.ind(digits_testY[,])
colnames(digits_test)[65]<-'dig'
#image(matrix(digits_testX[6,],ncol = 8, byrow = F), col = grey(seq(0, 1, length = 256)))


#1. Build a multinomial logistic regression and compute test error
multimodel <- multinom(dig~., digits_train)
predicted.multi <- predict(multimodel, digits_testX, type = "class")
error.multi <- sum(digits_testY[,]!=predicted.multi)/length(predicted.multi)

#2. Build a classical neural network with one hidden layer (use e.g. neuralnet or nnet package).
onehidden.model <- nnet(digits_trainX, digits_trainY.ind,size=10,softmax=TRUE)
predicted.onehidden <- as.numeric(predict(onehidden.model, digits_testX,type="class"))
error.onehidden <- sum(digits_testY[,]!=predicted.onehidden)/length(predicted.multi)

#3. Build a deep network with one hidden layer
deep.onehidden.model <- nn.train(digits_trainX.mat, digits_trainY.ind, hidden=c(10),output="softmax",numepochs = 50,hidden_dropout=0.1,visible_dropout=0)
predicted.deep.onehidden <- max.col(nn.predict(deep.onehidden.model, digits_testX.mat))-1
error.deep.onehidden <- sum(digits_testY[,]!=predicted.deep.onehidden)/length(predicted.multi)

#4. Build a deep network with many hidden layers.
deep.manyhidden.model <- nn.train(digits_trainX.mat, digits_trainY.ind, hidden=c(50,30,20),output="softmax",numepochs = 300,hidden_dropout=0.1,visible_dropout=0,activationfun="sigm")
predicted.deep.manyhidden <- max.col(nn.predict(deep.manyhidden.model, digits_testX.mat))-1
error.deep.manyhidden <- sum(digits_testY[,]!=predicted.deep.manyhidden)/length(predicted.multi)
