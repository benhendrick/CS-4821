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

# Load class labels
docClass <- c()
for (i in 1:10) {
  docClass[i] <- 1
}
for (i in 11:20) {
  docClass[i] <- 2
}
for (i in 21:30) {
  docClass[i] <- 3
}

## Part B

# Load stopwords.txt
# filePath <- '//homedir.mtu.edu/home/Desktop/stopwords.txt'
filePath <- '/Users/benhendrick/GitHub/CS 4821/Project-3/stopwords.txt'
stopWords <- scan(filePath, what = 'c')

num <- length(docClass)

# Remove stopwords from text files 
for (i in 1:num){
  docs[[i]] <- docs[[i]][!docs[[i]] %in% stopWords]
}

## Part C

# Remove testing documents
docs.test <- docs[c(2,22)]
docs.train <- docs[-c(2,22,11:20)]

### i
install.packages("tm")
library(tm)

# Create a term document matrix
td.train <- t(TermDocumentMatrix(Corpus(VectorSource(docs.train))))
td.test <- t(TermDocumentMatrix(Corpus(VectorSource(docs.test))))

install.packages("e1071")
library(e1071)

nb.model <- naiveBayes(as.matrix(td.train), as.factor(docs.train))
                       
nb.train.fit <- predict(nb.model, docs.train)
nb.test.fit <- predict(nb.model, docs.test)


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
