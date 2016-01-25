install.packages("ggplot2")
library(ggplot2)
# 2.b.i
adulttest[adulttest == " ?"] <- NA
apply(is.na(adulttest),2,sum)/nrow(adulttest)*100

# 2.b.ii
hist(apply(is.na(adulttest),1,sum),
     xlab = "Number of Missing Values Per Row",
     ylab = "Frequency",
     main = "Number of Missing Values",
     breaks = 3,
     col = "blue")
# 2.c
adultnumeric <- adulttest[,c(1,3,5,11,12,13)]
adultnumeric[is.na(adultnumeric)] <- 0

# 2.d.i
for(i in 1:length(adultnumeric)){
  ifelse(length(unique(adultnumeric[,i])) < 100,
         hist(adultnumeric[,i], 
              xlab = names[i],
              breaks = length(unique(adultnumeric[,i]))),
         hist(adultnumeric[,i], breaks = 100))
}

# 2.d.ii

# 3.a
nfl.passing.2014 <- read.csv("~/Google Drive/Academic/Spring 2016/CS 4821/nfl-passing-2014.csv")

#3.a.i
mean(nfl.passing.2014$TD)
median(nfl.passing.2014$TD)
names(sort(-table(nfl.passing.2014$TD)))[1]

mean(nfl.passing.2014$Int)
median(nfl.passing.2014$Int)
names(sort(-table(nfl.passing.2014$Int)))[1]

#3.a.ii
quantile(nfl.passing.2014$Yds)
quantile(nfl.passing.2014$Rate)

#3.a.iii
a <- fivenum(nfl.passing.2014$Cmp.)
b <- fivenum(nfl.passing.2014$Yds)

#3.a.iv
hist(nfl.passing.2014$Yds,
     xlab = "Passing Yards",
     main = "Histogram of Passing Yards",
     breaks = 4)

hist(nfl.passing.2014$Yds,
     xlab = "Passing Yards",
     main = "Histogram of Passing Yards",
     breaks = 8)

hist(nfl.passing.2014$Yds,
     xlab = "Passing Yards",
     main = "Histogram of Passing Yards",
     breaks = 12)

#3.a.v
lessthan12 <- nfl.passing.2014[which(nfl.passing.2014$Int < 12), ]
hist(lessthan12$Yds,
     xlab = "Passing Yards",
     main = "Distribution of Passing Yards")

#3.a.vi
plot(x = nfl.passing.2014$TD,
     y = nfl.passing.2014$Int,
     xlab = "Passing Touchdowns",
     ylab = "Passing Interceptions",
     main = "Passing Touchdowns vs. Interceptions")

#3.b
'2014w' <- read.csv("~/Google Drive/Academic/Spring 2016/CS 4821/2014w.csv")

#3.b.i

#3.c
wc.data.2014 <- read.csv("~/Google Drive/Academic/Spring 2016/CS 4821/wc-data-2014.csv")

#3.c.i
wc.data.2014$home_score - wc.data.2014$away_score




