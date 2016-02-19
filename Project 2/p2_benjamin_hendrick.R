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
minMaxNorm(iris$V3,data,-1,1)
zScoreNorm(iris$V3,data,1)
zScoreNorm(iris$V3,data,2)

## Part A
minMaxNorm(iris[,3],-1,1)[rows]

## Part B
zScoreNorm(iris[,3],1)[rows]

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
     type = "o")

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
spam.fit <- rpart(spam~day.of.week+time.of.day+size.kb+domain+local+digits+name+special+credit+sucker+porn+chain+username+large.text, 
                  data = spamTrain, 
                  method = "class")
plot(spam.fit, uniform=TRUE, 
     main="Classification Tree for Spam")
text(spam.fit, use.n=TRUE, all=TRUE, cex=.8)

## Part D

## Part E
printcp(spam.fit)
performance(spam.fit)

## Part F
spam.pfit <- prune(spam.fit, cp= 0.1)

plot(spam.pfit, uniform=TRUE, 
     main="Pruned Classification Tree for Kyphosis")
text(spam.pfit, use.n=TRUE, all=TRUE, cex=.8)

## Problem 3
music <- read.csv("/var/folders/6j/_n_8wtg912559sqd775wfctw0000gn/T//Rtmpmv0LxE/data1b076228f72f")
music <- music[,-c(3,4,5)]

## Part A
library(caret)
set.seed(100)
music.rows <- createDataPartition(music$year, p = 0.8, list = FALSE)
music.train <- spam[music.rows,]
music.test <- spam[-music.rows,]

## Part B