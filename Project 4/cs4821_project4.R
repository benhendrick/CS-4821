# CS 4281 Project 4

library(ggplot2)

# Question 1
x1 <- c(2,1,0,2,5,6,3,5)
x2 <- c(4,3,4,5,1,2,1,2)
Group <- c(1,1,2,2,1,2,1,2)
q1Data <- data.frame(x1,x2,Group)

# Part A
ggplot(data = q1Data,
       aes(x = x1,
           y = x2,
           color = as.character(Group))) +
  geom_point() + 
  labs(color = "Group", x = "X1", y = "X2", title = "Scatter Plot of Sample Data")

# Part B
y1 <- q1Data[which(q1Data$Group == 1),c(1,2)]
y2 <- q1Data[which(q1Data$Group == 2),c(1,2)]

y1.x1 <- mean(y1[,1])
y1.x2 <- mean(y1[,2]) 
y1.centroid <- c(y1.x1,y1.x2)

y2.x1 <- mean(y2[,1])
y2.x2 <- mean(y2[,2]) 
y2.centroid <- c(y2.x1,y2.x2)

# Part C
label <- c()
for (i in 1:8) {
  y1.eucDist <- sqrt(abs(q1Data[i,1]-y1.centroid[1])^2+
                       abs(q1Data[i,2]-y1.centroid[2])^2)
  y2.eucDist <- sqrt(abs(q1Data[i,1]-y2.centroid[1])^2+
                       abs(q1Data[i,2]-y2.centroid[2])^2)
  
  ifelse(y1.eucDist < y2.eucDist, label <- c(label,1), label <- c(label,2))
}

# Part D
q1Data[,3] <- label
y1 <- q1Data[which(q1Data$Group == 1),c(2,3)]
y2 <- q1Data[which(q1Data$Group == 2),c(2,3)]

y1.x1 <- mean(y1[,1])
y1.x2 <- mean(y1[,2]) 
y1.centroid <- c(y1.x1,y1.x2)

y2.x1 <- mean(y2[,1])
y2.x2 <- mean(y2[,2]) 
y2.centroid <- c(y2.x1,y2.x2)

label2 <- c()
for (i in 1:8) {
  y1.eucDist <- sqrt(abs(q1Data[i,1]-y1.centroid[1])^2+
                       abs(q1Data[i,2]-y1.centroid[2])^2)
  y2.eucDist <- sqrt(abs(q1Data[i,1]-y2.centroid[1])^2+
                       abs(q1Data[i,2]-y2.centroid[2])^2)
  
  ifelse(y1.eucDist < y2.eucDist, label2 <- c(label2,1), label2 <- c(label2,2))
}

q1Data[,3] <- label2
y1 <- q1Data[which(q1Data$Group == 1),c(1,2)]
y2 <- q1Data[which(q1Data$Group == 2),c(1,2)]

y1.x1 <- mean(y1[,1])
y1.x2 <- mean(y1[,2]) 
y1.centroid <- c(y1.x1,y1.x2)

y2.x1 <- mean(y2[,1])
y2.x2 <- mean(y2[,2]) 
y2.centroid <- c(y2.x1,y2.x2)

label3 <- c()
for (i in 1:8) {
  y1.eucDist <- sqrt(abs(q1Data[i,1]-y1.centroid[1])^2+
                       abs(q1Data[i,2]-y1.centroid[2])^2)
  y2.eucDist <- sqrt(abs(q1Data[i,1]-y2.centroid[1])^2+
                       abs(q1Data[i,2]-y2.centroid[2])^2)
  
  ifelse(y1.eucDist < y2.eucDist, label3 <- c(label3,1), label3 <- c(label3,2))
}

q1Data[,3] <- label3
y1 <- q1Data[which(q1Data$Group == 1),c(1,2)]
y2 <- q1Data[which(q1Data$Group == 2),c(1,2)]

y1.x1 <- mean(y1[,1])
y1.x2 <- mean(y1[,2]) 
y1.centroid <- c(y1.x1,y1.x2)

y2.x1 <- mean(y2[,1])
y2.x2 <- mean(y2[,2]) 
y2.centroid <- c(y2.x1,y2.x2)

label4 <- c()
for (i in 1:8) {
  y1.eucDist <- sqrt(abs(q1Data[i,1]-y1.centroid[1])^2+
                       abs(q1Data[i,2]-y1.centroid[2])^2)
  y2.eucDist <- sqrt(abs(q1Data[i,1]-y2.centroid[1])^2+
                       abs(q1Data[i,2]-y2.centroid[2])^2)
  
  ifelse(y1.eucDist < y2.eucDist, label4 <- c(label4,1), label4 <- c(label4,2))
}

q1Data[,3] <- label4

# Part E
ggplot(data = q1Data,
       aes(x = x1,
           y = x2,
           color = as.character(Group))) +
  geom_point() + 
  geom_point(data = aggregate(cbind(x1,x2)~Group,q1Data,mean), size = 5) +
  labs(color = "Group", x = "X1", y = "X2", title = "Scatter Plot of Sample Data with Centroids")

# Question 2
D <- c(0, 0.3, 0.4,0.7,0.6,
       0.3,0,0.5,0.8,0.2,
       0.4,0.5,0,0.45,0.4,
       0.7,0.8,0.45,0,0.35,
       0.6,0.2,0.4,0.35,0)
D <- as.dist(matrix(D,5,5))

# Part A
tree.comp <- hclust(D, method = "complete")
plot(tree.comp, hang = -1e-10,xlab="")
abline(h=0.5, lty=3, lwd=2)

# Part B
tree.single <- hclust(D, method = "single")
plot(tree.single, hang = -1e-10,xlab="")
abline(h=0.325, lty=3, lwd=2)

# Part C
# abline() functions from Part A and Part B


# Question 3
college <- read.csv("~/GitHub/CS 4821/Project 4/college.csv")
college.num <- college[,-c(1:3)]
library(cluster)

# Part A
college.dist <- dist(college.num)
tree.college <- hclust(college.dist, method = "complete")
plot(tree.college, hang = -1e-10, xlab="")

# Part B
college2 <- college[-c(1,6,7,8,14,15,20),]
college2.num <-college2[,-c(1:3)]

college2.dist <- dist(college2.num)
tree.college2.comp <- hclust(college2.dist, method = "complete")
plot(tree.college2.comp, hang = -1e-10, xlab="")

tree.college2.avg <- hclust(college2.dist, method = "average")
plot(tree.college2.avg, hang = -1e-10, xlab = "")

# Part C
tree.college2.single <- hclust(college2.dist, met?hhod = "single")
plot(tree.college2.single, hang = -1e-10, xlab="")
abline(h = 30000, lty = 3, lwd = 2)

# Part D