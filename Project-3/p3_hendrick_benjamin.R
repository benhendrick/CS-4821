# CS 4821 
# Project 3
# Started 2/22/2016

# Problem 1

## Part A

# Load all files
#dataDir <- '//homedir.mtu.edu/home/Desktop/plays/plays'
dataDir <- '/Users/benhendrick/GitHub/CS 4821/Project-3/plays/'

docs <- list()
files <- dir(path = dataDir, full.names = TRUE)
for (i in 1:length(files)) {
  docs[[i]] <- tolower(scan(files[i], what = "", quiet = TRUE))
}

## Part B

# Load stopwords.txt
#filePath <- '//homedir.mtu.edu/home/Desktop/stopwords.txt'
filePath <- '/Users/benhendrick/GitHub/CS 4821/Project-3/stopwords.txt'
stopWords <- scan(filePath, what = 'c')

num <- length(docClass)

# Remove stopwords from text files 
for (i in 1:num){
  docs[[i]] <- docs[[i]][!docs[[i]] %in% stopWords]
}

## Part C

library(tm)
# Read the text and perform pre-processing
words <- Corpus(VectorSource(docs[-c(11:20)]))
words <- tm_map(words, stripWhitespace)
words <- tm_map(words, removePunctuation)

for (i in 1:20) {
  if (i %in% 1:10){
    words[[i]]$meta$genre = "comedy"
  }
  if (i %in% 11:20){
    words[[i]]$meta$genre = "tragedy"
  }
}


# Sample indicies of training data
words.test <- c(sample(c(2,12)))

words.train.dtm <- DocumentTermMatrix(words[-words.test])
words.train.dtm <- removeSparseTerms(words.train.dtm, 0.9)

words.dict <- dimnames(words.train.dtm)[[2]]
words.test.dtm <- DocumentTermMatrix(words[words.test], list(dictionary = words.dict))

# Convert training data
words.train.dtm.bin <- inspect(words.train.dtm)
words.train.dtm.bin <- words.train.dtm.bin > 0
words.train.dtm.bin <- as.data.frame(words.train.dtm.bin)

# Convert testing data
words.test.dtm.bin <- inspect(words.test.dtm)
words.test.dtm.bin <- words.test.dtm.bin > 0
words.test.dtm.bin <- as.data.frame(words.test.dtm.bin)

# extract class labels
words.lab <- as.vector(unlist(lapply(words,meta,tag="genre")))
words.lab <- as.factor(words.lab)

# Bernouli Model
trainBernouliNB <- function (C, D) {
  V <- dimnames(words.test.dtm)[[2]]
  N <- 2
  prior <- c()
  condprob <- data.frame(matrix(NA, nrow = length(V), ncol = 2))
  
  for (c in 1:2) {
    Nc <- 1
    prior <- c(prior, Nc/N)
    
    for (t in 1:length(V)) {
      ifelse(words.test.dtm.bin[c,t] == 'TRUE',
             Ntc <- 1,
             Ntc <- 0)
      condprob[t,c] <- (Ntc + 1)/(Nc + 2)
    }
  }
  output <- list()
  output$V <- V
  output$prior <- prior
  output$condprob <- condprob
  
  return(output)
}

bern.train <- trainBernouliNB(unique(words.lab), docs[words.test])

applyBernoulliNB <- function (C, V, prior, condprob, d) {
  Vd <- colnames(words.test.dtm.bin[d, which(words.test.dtm.bin[d,]== 'TRUE')])
  score <- c()
  score <- c(score, log(prior[d]))
    
  for (t in 1:length(V)) {
    ifelse(dimnames(words.train.dtm)[[2]][t] %in% Vd == TRUE,
           score <- score + log(condprob[t,d]),
           score <- score + log(1-condprob[t,d]))
  }
  return (max(score))
}

bern.com <- applyBernoulliNB(unique(words.lab), docs[words.test], bern.train$prior, bern.train$condprob, 1)
bern.trag <- applyBernoulliNB(unique(words.lab), docs[words.test], bern.train$prior, bern.train$condprob, 2)
exp(bern.com)
exp(bern.trag)

# Multinomial model
trainMultinomialNB <- function(C,D) {
  V <- dimnames(words.test.dtm)[[2]]
  N <- 2
  prior <- c()
  condprod <- c()
  for (c in 1:2) {
    textC <- c()
    Nc <- 1
    prior <- c(prior, Nc/N)
  }
  
  Tct.com <- data.frame(inspect(words.test.dtm))[1, which(data.frame(inspect(words.test.dtm))[1,] != 0)]
  Tct.trag <- data.frame(inspect(words.test.dtm))[2, which(data.frame(inspect(words.test.dtm))[2,] != 0)]
  condprod.com <- (Tct.com + 1)/(sum(Tct.com + 1))
  condprod.trag <- (Tct.trag + 1)/(sum(Tct.trag + 1))
  
  
  output <- list()
  output$V <- V
  output$prior <- prior
  output$condprod.com <- condprod.com
  output$condprod.trag <- condprod.trag
  
  return (output)
}  

mult.train <- trainMultinomialNB(unique(words.lab), docs[words.test])


applyMultinomailNB <- function(C, V, prior, condprod, d) {
  score <- log(prior[d])
  score <- score + log(condprod)
  max(score)
}

mult.com <- applyMultinomailNB(unique(words.lab), mult.train$V, 
                               mult.train$prior, mult.train$condprod.com, 1)
mult.trag <- applyMultinomailNB(unique(words.lab), mult.train$V, 
                               mult.train$prior, mult.train$condprod.trag, 2)
exp(mult.com)
exp(mult.trag)


# Problem 3
## Part A
filePath <- '/Users/benhendrick/GitHub/CS 4821/Project-3/spam.csv'
spam <- read.csv(filePath)
spam <- spam[,-c(1,2,7,11,19,20)]

## Part B
library(caret)
rows <- createDataPartition(spam$name, p = 0.8, list = FALSE)
spam.train <- spam[rows,]
spam.test <- spam[-rows,]

## Part C
spam.nb <- naiveBayes(spam.train[,15],spam.train$spam)
spam.pred <- predict(spam.nb, spam.test)

library(AUC)

test.cm <- confusionMatrix(data=spam.pred,
                           reference=spam.test$spam,
                           positive=c("yes"))
test.auc <- auc(sensitivity(spam.pred, spam.test$spam))

## Part D
library(rpart)
tree.fit <- rpart(spam~., 
                  data = spam.train, 
                  method = "class")
tree.fit.test <- predict(tree.fit,spam.test,"class")
tree.test.cm <- confusionMatrix(data=tree.fit.test,
                                reference = spam.test$spam,
                                positive=c("yes"))
tree.test.auc <- auc(sensitivity(tree.fit.test, spam.test$spam))

## Part E
library(randomForest)
rf.fit <- randomForest(spam~., 
                       data = spam.train, 
                       method = "class")
rf.fit.test <- predict(rf.fit,spam.test,"class")
rf.test.cm <- confusionMatrix(data=rf.fit.test,
                              reference = spam.test$spam,
                              positive=c("yes"))
rf.test.auc <- auc(sensitivity(rf.fit.test, spam.test$spam))

## Part F
svm.cost <- c(0.01,0.1,1,10,100)
svm.acc <- c()
svm.err <- c()
svm.auc <- c()
for (i in svm.cost){
  library(kernlab)
  svm.fit <- ksvm(spam~., 
                  data = spam.train,
                  kernel ="rbfdot", 
                  kpar = "automatic",
                  C = i, epsilon = 0.1)
  svm.fit.test <- predict(svm.fit,spam.test[,-15])
  library(caret)
  svm.test.cm <- confusionMatrix(data=svm.fit.test,
                                 reference = spam.test$spam,
                                 positive=c("yes"))
  svm.acc <- c(svm.acc,svm.test.cm$overall[1])
  svm.err <- c(svm.err,(1-svm.test.cm$overall[1]))
  library(AUC)
  svm.test.auc <- auc(sensitivity(svm.fit.test, spam.test$spam))
  svm.auc <- c(svm.auc, svm.test.auc)
}

svm.summary <- data.frame(svm.cost,svm.acc,svm.err,svm.auc)
svm.cost.best <- svm.summary[which(svm.summary$svm.auc==max(svm.summary$svm.auc)),"svm.cost"]

library(kernlab)
svm.fit <- ksvm(spam~., 
                data = spam.train,
                kernel ="rbfdot", 
                kpar = "automatic",
                C = svm.cost.best, epsilon = 0.1)
svm.fit.test <- predict(svm.fit,spam.test[,-15])

## Part G
spam.ens <- data.frame(spam.test$spam, spam.pred, tree.fit.test, rf.fit.test, svm.fit.test)
spam.ens.pred <- c()
k <- 0
for (i in 1:length(spam.ens$spam.pred)) {
  for(j in 2:5) {
    if (spam.ens[i,j] == 'yes') {
      k = k + 1
    }
  }
  
  if (k >= 3) {
    spam.ens.pred <- c(spam.ens.pred, "yes")
  } else{
    spam.ens.pred <- c(spam.ens.pred, "no")
  }
  
  k <- 0
}

spam.ens.cm <- confusionMatrix(data=spam.ens.pred,
                               reference = spam.test$spam,
                               positive=c("yes"))
spam.ens.auc <- auc(sensitivity(spam.ens.pred, spam.test$spam))

## Part H
install.packages("adabag")
library(adabag)
bag.fit <- bagging(spam~., data = spam.train, mfinal = 100, method = "class")
bag.fit.test <- predict.bagging(bag.fit,spam.test)
bag.fit.test$error

## Part I
boost.fit <- boosting(spam~., data = spam.train, mfinal = 100, method = "class")
boost.fit.test <- predict.boosting(boost.fit,spam.test)
boost.fit.test$error
