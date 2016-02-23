# CS 4821 
# Project 3
# Started 2/22/2016

# Problem 1

## Part A

# Load all files
dataDir <- '//homedir.mtu.edu/home/Desktop/plays/plays'

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
stopWords <- scan('//homedir.mtu.edu/home/Desktop/stopwords.txt', what = 'c')

num <- length(docClass)

# Remove stopwords from text files 
for (i in 1:num){
  docs[[i]] <- docs[[i]][!docs[[i]] %in% stopWords]
}

## Part C

# Remove testing documents
docs.test <- docs[c(10,20,30)]
docs <- docs[-c(10,20,30)]

### i
install.packages("tm")
library(tm)

# Create a term document matrix
td <- TermDocumentMatrix(Corpus(VectorSource(docs)))
