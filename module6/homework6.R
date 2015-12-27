set.seed(1)

NUM_ATTR = 1000
NUM_REC = 10000

sigmoid <- function(X,W){
    1/(1+exp(-X%*%W))
}

data <- matrix(runif(n = NUM_ATTR*NUM_REC, -1, 1), nrow = NUM_REC, ncol = NUM_ATTR)
data <- cbind(rep(1,NUM_REC),data)
W <- matrix(rnorm(NUM_ATTR+1),nrow = NUM_ATTR+1)

class<-ifelse(sigmoid(data,W) > 0.5,1,0)
class_one_min_one<-class*2-1

test.data <- matrix(runif(n = NUM_ATTR*NUM_REC, -1, 1), nrow = NUM_REC, ncol = NUM_ATTR)
test.data <- cbind(rep(1,NUM_REC),test.data)
test.class<-ifelse(sigmoid(test.data,W) > 0.5,1,0)
test.class_one_min_one<-test.class*2-1

start.time <- Sys.time()
model <- glm.fit(data, class,family=binomial(link='logit'))
end.time <- Sys.time()
time.taken_r <- as.numeric(end.time - start.time, units = "secs")

accuracy <- function(coef){
    fitted.results <- ifelse(sigmoid(test.data,coef) > 0.5,1,0) 
    misClasificError <- mean(fitted.results != test.class)
    1-misClasificError
}

acc_r <- accuracy(model$coefficients)

print(paste('Accuracy',acc_r))

n_avg<-10
time_accuracy = data.frame(time=c(),accuracy=c())
WA <- matrix(rnorm(NUM_ATTR+1),nrow = NUM_ATTR+1)
WA_mat <- matrix(nrow = NUM_ATTR+1,ncol=n_avg)
n<-0
start.time <- Sys.time()
for(epoch in 1:300){
    samples <- sample(NUM_REC)
    t<-1
    for(s in samples){
        x<-data[s,]
        y<-class_one_min_one[s]
        WA <- WA + 1/sqrt(t)*y*x/(1+exp(y*t(WA)%*%x))
        WA_mat[,n+1] <- WA 
        WA <- rowMeans(WA_mat,na.rm = TRUE)
        n <- (n + 1)%%n_avg
        t<-t+1
    }
    end.time <- Sys.time()
    time.taken <- as.numeric(end.time - start.time, units = "secs")
    acc<-accuracy(WA)
    time_accuracy <- rbind(time_accuracy,data.frame(time=time.taken,accuracy=acc))
    print(paste('Accuracy',acc))
}
time_accuracy_r <- data.frame(time=time.taken_r,accuracy=acc_r)
save(time_accuracy, time_accuracy_r, file = "data.RData")

library(ggplot2)

p <- ggplot() + 
    geom_point(data = time_accuracy, aes(x = time, y = accuracy, color="ASGD")) +
    geom_point(data = time_accuracy_r, aes(x = time, y = accuracy, color="R"), size=4)  +
    ggtitle("test set accuracy versus computation time") +
    labs(color="accuracy") +
    xlab("time[s]")
p
