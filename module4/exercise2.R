#Load data and LDA package
library(lda)

data(poliblog.documents)
data(poliblog.vocab)
data(poliblog.ratings)

# split classes so the test and traning set contain the same proportion of classes
liberalIdx <- sample(which(-100 == poliblog.ratings))
conservativeIdx <- sample(which(100 == poliblog.ratings))

liberalTrainTestSplitIdx <- floor(length(liberalIdx)*0.7)
conservativeTrainTestSplitIdx <- floor(length(conservativeIdx)*0.7)

trainIdx <- sample(c(liberalIdx[1:liberalTrainTestSplitIdx],conservativeIdx[1:conservativeTrainTestSplitIdx]))
testIdx <- sample(c(liberalIdx[(liberalTrainTestSplitIdx+1):length(liberalIdx)],
                    conservativeIdx[(conservativeTrainTestSplitIdx+1):length(conservativeIdx)]))


#build LDA model
K <- 10 # Num clusters
G <- 5000 #Num of iterations 0.
alpha <- 0.1 
eta <- 0.1

#build features for training set
fitTrain <- lda.collapsed.gibbs.sampler(documents = poliblog.documents[trainIdx], K = K, vocab = poliblog.vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)

#the features for the training set
topicFreqPerDocTrain <- t(apply(fitTrain$document_sums, 2, function(x) x/sum(x)))

#build features for test set
fitTest <- lda.collapsed.gibbs.sampler(documents = poliblog.documents[testIdx], K = K, vocab = poliblog.vocab, 
                                        num.iterations = G, alpha = alpha, 
                                        eta = eta, initial=list(topics = fitTrain$topics, topic_sums = fitTrain$topic_sums), 
                                        freeze.topics=TRUE)
topicFreqPerDocTest <- t(apply(fitTest$document_sums, 2, function(x) x/sum(x)))

#build data frame so it can be used in glm procedure
trainData <- data.frame(cbind(topicFreqPerDocTrain,poliblog.ratings[trainIdx]))
colnames(trainData)<-c(paste0(rep('topic',K),1:K),'group')
trainData[trainData[,K+1]==-100,K+1] <- 0
trainData[trainData[,K+1]==100,K+1] <- 1

model <- glm(group ~.,family=binomial(link='logit'),data=trainData)
#anova(model, test="Chisq")

#checking accuracy of the model
testData <- data.frame(cbind(topicFreqPerDocTest,poliblog.ratings[testIdx]))
colnames(testData)<-c(paste0(rep('topic',K),1:K),'group')
testData[testData[,K+1]==-100,K+1] <- 0
testData[testData[,K+1]==100,K+1] <- 1

fitted.results <- predict(model,newdata=testData[,1:K],type='response')
fitted.results.prediction <- prediction(fitted.results, testData$group)
#fitted.results <- ifelse(fitted.results > 0.5,1,0)

#misClasificError <- mean(fitted.results != testData$group)
#print(paste('Accuracy',1-misClasificError))

#ROC curve and AUC
library(ROCR)
prf <- performance(fitted.results.prediction, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(fitted.results.prediction, measure = "auc")
auc <- auc@y.values[[1]]
auc

