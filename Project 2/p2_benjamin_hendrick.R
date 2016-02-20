# CS 4821 Project 2
# Part A

# Problem 1

minMaxNorm <- function (train,test,l,u){
  normalized <- (test-min(train))/(max(train)-min(train))*(u-l)+l # Patro and Kumar
  return(normalized)
}

zScoreNorm <- function (train,test,type){
  if (type == 1) {
    z <- (test-mean(train))/sd(train)
  }
  if (type == 2) {
    z <- (test - mean(train)) / (length(test)^-1 * sum(abs(test-mean(train))))
  }
  return(z)
}

# Problem 2
data <- c(200,300,400,600,1000)

## Part A
minMaxNorm(data,data,0,1)

## Part B
zScoreNorm(data,data,1)

## Part C
zScoreNorm(data,data,2)

# Problem 3
iris <- read.csv("/var/folders/6j/_n_8wtg912559sqd775wfctw0000gn/T//Rtmpmv0LxE/data1b074f253bfa", header=FALSE)
data <- c(1.6, 2.8, 3.72,4.97)

## Part A
minMaxNorm(iris$V3,data,-1,1)

## Part B
zScoreNorm(iris$V3,data,1)
zScoreNorm(iris$V3,data,2)

# Problem 4
predictionEvaluate <- function(yPred, yTrue, thresh){
  for (i in 1:length(yPred)) {
    if (1-yPred[i] < thresh) {
      yPred[i] <- 1
    } else {
      yPred[i] <- 0
    }
  }
  
  tp <- 0
  for (i in 1:length(yTrue)) {
    if (yTrue[i] == 1) {
      if(yTrue[i] == yPred[i]){
        tp <- tp + 1
      }
    }
  }
  
  tn <- 0
  for (i in 1:length(yTrue)) {
    if (yTrue[i] == 0) {
      if(yTrue[i] == yPred[i]){
        tn <- tn + 1
      }
    }
  }
  
  fp <- 0
  for (i in 1:length(yTrue)) {
    if (yTrue[i] == 1) {
      if(yTrue[i] != yPred[i]){
        fp <- fp + 1
      }
    }
  }
  
  fn <- 0
  for (i in 1:length(yTrue)) {
    if (yTrue[i] == 0) {
      if(yTrue[i] != yPred[i]){
        fn <- fn + 1
      }
    }
  }
  
  tpr <- tp/(tp+fp)
  tnr <- tn/(fp+tn)
  sens <- tp/(tp+tn)
  spec <- tn/(fp+tn)
  prec <- tp/(tp+fp)
  rec <- tp/(tp+fn)
  acc <- (tp+tn)/(tp+fp+fn+tn)
  
  results <- list()
  results$tp <- tp
  results$tn <- tn
  results$fp <- fp
  results$fn <- fn
  results$tpr <- tpr
  results$tnr <- tnr
  results$sens <- sens
  results$spec <- spec
  results$prec <- prec
  results$rec <- rec
  results$acc <- acc
  return (results)
}

# Problem 5
yPred <- c(0.98,0.92,0.85,0.77,0.71,0.64,0.50,0.39,0.34,0.31)
yTrue <- c(1,0,1,1,0,0,1,0,1,0)
predictionEvaluate(yPred,yTrue,0.5)

# Problem 6
sens.list <- c() 
thresh.list <- c()
for (i in 1:10) {
  tmp <- predictionEvaluate(yPred, yTrue, 0.01*i)
  sens.list[i] <- tmp$sens
  thresh.list[i] <- 0.1*i
}

plot(thresh.list, sens.list,
     xlab = "False Positive Rate",
     ylab = "True Positive Rate",
     main = "ROC Curve",
     type = "b")

# Problem 7
m1 <- c(30.5, 32.2, 20.7, 20.6, 31.0, 41.0, 27.7, 26.0, 21.5, 26.0)
m2 <- c(22.4, 14.5, 22.4, 19.6, 20.7, 20.4, 22.1, 19.4, 16.2, 35.0)
t.test(m1, m2, alternative = "two.sided", mu = 0, conf.level = 0.99)

## Part A
N <- c(100,50,60)
N11 <- c(62,8,0)
N12 <- c(38,42,60)
N21 <- c(65,20,0)
N22 <- c(21,19,20)
N23 <- c(14,11,40)

gini.N11 <- 1 - (N11[1]/(N11[1]+N12[1]))^2 - (N11[2]/(N11[2]+N12[2]))^2 - (N11[3]/(N11[3]+N12[3]))^2  
gini.N12 <- 1 - (N12[1]/(N11[1]+N12[1]))^2 - (N12[2]/(N11[2]+N12[2]))^2 - (N12[3]/(N11[3]+N12[3]))^2  
gini.N1x <- sum(N11)/sum(N11,N12) * gini.N11 + sum(N12)/sum(N11,N12) * gini.N12

gini.N21 <- 1 - (N21[1]/(N21[1]+N22[1]+N23[1]))^2 - (N21[2]/(N21[2]+N22[2]+N23[2]))^2 - (N21[3]/(N21[3]+N22[3]+N23[3]))^2
gini.N22 <- 1 - (N22[1]/(N21[1]+N22[1]+N23[1]))^2 - (N22[2]/(N21[2]+N22[2]+N23[2]))^2 - (N22[3]/(N21[3]+N22[3]+N23[3]))^2
gini.N23 <- 1 - (N23[1]/(N21[1]+N22[1]+N23[1]))^2 - (N23[2]/(N21[2]+N22[2]+N23[2]))^2 - (N23[3]/(N21[3]+N22[3]+N23[3]))^2
gini.N2x <- sum(N21)/sum(N21,N22,N23) * gini.N21 + sum(N22)/sum(N21,N22,N23) * gini.N22 + sum(N23)/sum(N21,N22,N23) * gini.N23

gini.N2x-gini.N1x


## Part B

## Part C
ent.N1x <- -sum(N11)/sum(N11,N12) * log(sum(N11)/sum(N11,N12),base = 2) - sum(N12)/sum(N11,N12) * log(sum(N12)/sum(N11,N12),base = 2) 
ent.N2x <- -sum(N21)/sum(N21,N22,N23) * log(sum(N21)/sum(N21,N22,N23),base = exp(2)) - sum(N22)/sum(N21,N22,N23) * log(sum(N22)/sum(N21,N22,N23),base = exp(2)) - sum(N23)/sum(N21,N22,N23) * log(sum(N23)/sum(N21,N22,N23),base = exp(2))

## Part D

# Problem 2

## Part A
spam <- read.csv("/var/folders/6j/_n_8wtg912559sqd775wfctw0000gn/T//Rtmpmv0LxE/data1b071254b98b")
spam <- spam[,-c(1,2,6,19,20,11)]

## Part B
library(caret)
set.seed(100)
rows <- createDataPartition(spam$name, p = 0.8, list = FALSE)
spamTrain <- spam[rows,]
spamTest <- spam[-rows,]

## Part C
library(rpart)
spam.fit <- rpart(spam~., 
                  data = spamTrain, 
                  method = "class")

## Part D
install.packages("AUC")
library(AUC)
plot(spam.fit, uniform=TRUE, 
     main="Classification Tree for Spam")
text(spam.fit,cex=0.75)
print(spam.fit)

## Part E
spam.fit.Train <- predict(spam.fit,spamTrain,"class")
spam.fit.Test <- predict(spam.fit,spamTest,"class")

train.cm <- confusionMatrix(data=spam.fit.Train,
                            reference=spamTrain$spam,
                            positive=c("yes"))
test.cm <- confusionMatrix(data=spam.fit.Test,
                           reference = spamTest$spam,
                           positive=c("yes"))

train.auc <- auc(sensitivity(spam.fit.Train, spamTrain$spam))
test.auc <- auc(sensitivity(spam.fit.Test, spamTest$spam))

## Part F
spam.pfit <- prune(spam.fit, cp= 0.015)

spam.pfit.Train <- predict(spam.pfit,spamTrain,"class")
spam.pfit.Test <- predict(spam.pfit,spamTest,"class")

ptrain.cm <- confusionMatrix(data=spam.pfit.Train,
                             reference=spamTrain$spam,
                             positive=c("yes"))
ptest.cm <- confusionMatrix(data=spam.pfit.Test,
                            reference = spamTest$spam,
                            positive=c("yes"))

ptrain.auc <- auc(sensitivity(spam.pfit.Train, spamTrain$spam))
ptest.auc <- auc(sensitivity(spam.pfit.Test, spamTest$spam))

spam.pfit2 <- prune(spam.fit, cp= 0.2)

spam.pfit.Train2 <- predict(spam.pfit2,spamTrain,"class")
spam.pfit.Test2 <- predict(spam.pfit2,spamTest,"class")

ptrain.cm2 <- confusionMatrix(data=spam.pfit.Train2,
                              reference=spamTrain$spam,
                              positive=c("yes"))
ptest.cm2 <- confusionMatrix(data=spam.pfit.Test2,
                             reference = spamTest$spam,
                             positive=c("yes"))

ptrain.auc2 <- auc(sensitivity(spam.pfit.Train2, spamTrain$spam))
ptest.auc2 <- auc(sensitivity(spam.pfit.Test2, spamTest$spam))


# Problem 3
## Part A
music <- read.csv("/var/folders/6j/_n_8wtg912559sqd775wfctw0000gn/T//Rtmpmv0LxE/data1b076228f72f")
music <- music[,-c(2,3,4,5)]

## Part B
library(caret)
set.seed(100)
musicSplit <- createDataPartition(music$Top10, p = 0.8, list = TRUE, time=10)

trainDistZero <- c()
trainDistOne <- c()

for (i in 1:10) {
  musicTrain <- music[musicSplit[[i]],]
  trainDistZero[i] <- prop.table(table(musicTrain$Top10))[1]
  trainDistOne[i] <- prop.table(table(musicTrain$Top10))[2]
}

testDistZero <- c()
testDistOne <- c()

for (i in 1:10) {
  musicTest <- music[-musicSplit[[i]],]
  testDistZero[i] <- prop.table(table(musicTest$Top10))[1]
  testDistOne[i] <- prop.table(table(musicTest$Top10))[2]
}


## Part C
require(class)
kValue = 1
for (i in 1:10){
  musicTrain <- music[musicSplit[[i]],]
  musicTest <- music[-musicSplit[[i]],]
  
  model <- knn(musicTrain[,1:34],
               musicTest[,1:34],
               cl=musicTrain[,35],
               k=kValue)
  
  knn.cm <- confusionMatrix(data=model,
                            reference = musicTest$Top10,
                            positive=c("1"))
  print(knn.cm$overall[[1]])
  print(1 - knn.cm$overall[[1]])
  
  knn.pred <- prediction(as.numeric(model)-1, as.numeric(musicTest$Top10)-1)
  knn.auc <- performance(knn.pred,"auc")
  print(knn.auc@y.values[[1]])
}

kValue = 5
for (i in 1:10){
  musicTrain <- music[musicSplit[[i]],]
  musicTest <- music[-musicSplit[[i]],]
  
  model <- knn(musicTrain[,1:34],
               musicTest[,1:34],
               cl=musicTrain[,35],
               k=kValue)
  
  knn.cm <- confusionMatrix(data=model,
                            reference = musicTest$Top10,
                            positive=c("1"))
  print(knn.cm$overall[[1]])
  print(1 - knn.cm$overall[[1]])
  
  knn.pred <- prediction(as.numeric(model)-1, as.numeric(musicTest$Top10)-1)
  knn.auc <- performance(knn.pred,"auc")
  print(knn.auc@y.values[[1]])
}

kValue = 9
for (i in 1:10){
  musicTrain <- music[musicSplit[[i]],]
  musicTest <- music[-musicSplit[[i]],]
  
  model <- knn(musicTrain[,1:34],
               musicTest[,1:34],
               cl=musicTrain[,35],
               k=kValue)
  
  knn.cm <- confusionMatrix(data=model,
                            reference = musicTest$Top10,
                            positive=c("1"))
  print(knn.cm$overall[[1]])
  print(1 - knn.cm$overall[[1]])
  
  knn.pred <- prediction(as.numeric(model)-1, as.numeric(musicTest$Top10)-1)
  knn.auc <- performance(knn.pred,"auc")
  print(knn.auc@y.values[[1]])
}

## Part D
for (i in 1:10){
  musicTrain <- music[musicSplit[[i]],]
  musicTest <- music[-musicSplit[[i]],]
  
  model <- rpart(Top10 ~ ., 
                 data = musicTrain, 
                 method = "class")
  model.fit <- predict(model,musicTest,"class")
  tree.cm <- confusionMatrix(data=model.fit,
                             reference = musicTest$Top10,
                             positive=c("1"))
  print(tree.cm$overall[[1]])
  print(1 - tree.cm$overall[[1]])
  
  tree.pred <- prediction(as.numeric(model.fit)-1, as.numeric(musicTest$Top10)-1)
  tree.auc <- performance(tree.pred,"auc")
  print(tree.auc@y.values[[1]])
}

for (i in 1:10){
  musicTrain <- music[musicSplit[[i]],]
  musicTest <- music[-musicSplit[[i]],]
  
  model <- rpart(Top10 ~ ., 
                 data = musicTrain, 
                 method = "class")
  model.prune <- prune(model, cp= 0.2)
  model.fit <- predict(model.prune,musicTest,"class")
  tree.cm <- confusionMatrix(data=model.fit,
                             reference = musicTest$Top10,
                             positive=c("1"))
  print(tree.cm$overall[[1]])
  print(1 - tree.cm$overall[[1]])
  
  tree.pred <- prediction(as.numeric(model.fit)-1, as.numeric(musicTest$Top10)-1)
  tree.auc <- performance(tree.pred,"auc")
  print(tree.auc@y.values[[1]])
}
