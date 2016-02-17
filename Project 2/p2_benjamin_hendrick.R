# CS 4821 Project 2
# Part A

# Problem 1

minMaxNorm <- function (x,l,u){
  normalized <- (x-min(x))/(max(x)-min(x))*(u-l)+l # Patro and Kumar
  return(normalized)
}

zScoreNorm <- function (x,type){
  if (type == 1) {
    z <- (x-mean(x))/sd(x)
    return(z)
  }
  if (type == 2) {
    (x - mean(x)) / (length(x)^-1 * sum(abs(x-mean(x))))
  }
}

# Problem 2
data <- c(200,300,400,600,1000)

## Part A
minMaxNorm(data,0,1)

## Part B
zScoreNorm(data,1)

## Part C
zScoreNorm(data,2)

# Problem 3
iris <- read.csv("/var/folders/6j/_n_8wtg912559sqd775wfctw0000gn/T//RtmprXguJK/datad7f2133b486", 
                 header=FALSE)

## Part A
minMaxNorm(iris[,3],-1,1)

## Part B
zScoreNorm(iris[,3],1)

# Problem 4
predictionEvaluate <- function(yPred, yTrue){
  yPredNorm <- minMaxNorm(yPred,0,1)
  for (i in 1:length(yPredNorm)) {
    if (1-yPredNorm[i] < 0.5) {
      yPredNorm[i] <- 1
    } else {
      yPredNorm[i] <- 0
    }
  }

  tp <- 0
  for (i in 1:length(yTrue)) {
    if (yTrue[i] == 1) {
      if(yTrue[i] == yPredNorm[i]){
        tp <- tp + 1
      }
    }
  }
  
  tn <- 0
  for (i in 1:length(yTrue)) {
    if (yTrue[i] == 0) {
      if(yTrue[i] == yPredNorm[i]){
        tn <- tn + 1
      }
    }
  }
  
  fp <- 0
  for (i in 1:length(yTrue)) {
    if (yTrue[i] == 1) {
      if(yTrue[i] != yPredNorm[i]){
        fp <- fp + 1
      }
    }
  }
  
  fn <- 0
  for (i in 1:length(yTrue)) {
    if (yTrue[i] == 0) {
      if(yTrue[i] != yPredNorm[i]){
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
predictionEvaluate(yPred,yTrue)

# Problem 6

# Problem 7
m1 <- c(30.5, 32.2, 20.7, 20.6, 31.0, 41.0, 27.7, 26.0, 21.5, 26.0)
m2 <- c(22.4, 14.5, 22.4, 19.6, 20.7, 20.4, 22.1, 19.4, 16.2, 35.0)
t.test(m1, m2, alternative = "two.sided", mu = 0, conf.level = 0.99)



# Part B


