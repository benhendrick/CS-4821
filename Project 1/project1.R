adultdata <- read.csv("~/Desktop/adultdata.txt")

# 2.b.i
adultdata[adultdata == " ?"] <- NA
apply(is.na(adultdata),2,sum)/nrow(adultdata)*100

# 2.b.ii
hist(apply(is.na(adultdata),1,sum),
     xlab = "Number of Missing Values Per Row",
     ylab = "Frequency",
     main = "Number of Missing Values",
     breaks = 3,
     col = "blue")
# 2.c
adultnumeric <- adultdata[,c(1,3,5,11,12,13)]
adultcat <- adultdata[,-c(1,3,5,11,12,13)]
names(adultnumeric) # numeric
names(adultcat) # chategorical

# 2.d.i
apply(adultnumeric, 2, function(x) ifelse(length(unique(x)) < 100,
              hist(x, breaks = length(unique(x))),
              hist(x, breaks = 100)))

# 2.d.ii
lesser <- adultdata[which(adultdata$income == " <=50K"), ]
greater <- adultdata[which(adultdata$income == " >50K"), ]

for(i in c(1,3,5,11,12,13)){
par(mfrow=c(1,2))
hist(lesser[,i],
     xlab = names(lesser)[i],
     ylab = "Frequency",
     main = "",
     col = "blue")
hist(greater[,i],
     xlab = names(greater)[i],
     ylab = "Frequency",
     main = "",
     col = "red")
}

#2.d.iii
for(i in c(1,3,5,11,12,13)){
  par(mfrow=c(1,2))
  boxplot(lesser[,i],
       xlab = names(lesser)[i],
       ylab = "Frequency",
       main = "",
       col = "blue")
  boxplot(greater[,i],
       xlab = names(greater)[i],
       ylab = "Frequency",
       main = "",
       col = "red")
}

par(mfrow = c(1,1))

#2.d.iv

#2.e.i
for(i in c(2,4,6,7,8,9,10,14,15)){
barplot(table(adultdata[,i]), 
        las = 2, 
        cex.names = 0.65,
        main = "",
        xlab = names(adultcat)[i],
        col = "blue")
}

#2.3.ii
for(i in c(2,4,6,7,8,9,10,14,15)){
  par(mfrow=c(1,2))
  barplot(table(lesser[,i]), 
          las = 2, 
          cex.names = 0.65,
          main = "",
          xlab = names(lesser)[i],
          col = "blue")
  barplot(table(greater[,i]), 
          las = 2, 
          cex.names = 0.65,
          main = "",
          xlab = names(greater)[i],
          col = "red")
}

par(mfrow = c(1,1))

#2.f.i
plot(y = adultdata$education.num, x = adultdata$age,
     xlab = "Age",
     ylab = "Education",
     main = "Education vs. Age")

cor(adultdata$education.num, adultdata$age)

#2.f.i
plot(x = adultnumeric$capital.gain, y = adultnumeric$hours.per.week,
     xlab = "Capital Gain",
     ylab = "Hours Per Week",
     main = "Capital Gain vs. Hours Per Week")

cor(adultnumeric$capital.gain, adultnumeric$hours.per.week)

# 3.a
nfl.passing.2014 <- read.csv("~/Google Drive/Academic/Spring 2016/CS 4821/nfl-passing-2014.csv")

#3.a.i
summary(nfl.passing.2014$TD)
names(sort(-table(nfl.passing.2014$TD)))[1]

summary(nfl.passing.2014$Int)
names(sort(-table(nfl.passing.2014$Int)))[1]

#3.a.ii
quantile(nfl.passing.2014$Yds)
quantile(nfl.passing.2014$Rate)

#3.a.iii
fivenum(nfl.passing.2014$Cmp.)
fivenum(nfl.passing.2014$Yds)

#3.a.iv
hist(nfl.passing.2014$Yds,
     xlab = "Passing Yards",
     main = "Histogram of Passing Yards",
     col = "blue",
     breaks = 4)

hist(nfl.passing.2014$Yds,
     xlab = "Passing Yards",
     main = "Histogram of Passing Yards",
     col = "blue",
     breaks = 8)

hist(nfl.passing.2014$Yds,
     xlab = "Passing Yards",
     main = "Histogram of Passing Yards",
     col = "blue",
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
tennis <- read.csv("~/Google Drive/Academic/Spring 2016/CS 4821/2014w.csv")

#3.b.i

twenty <- tennis[tennis$Winner %in% names(subset(table(tennis$Winner), table(tennis$Winner) >= 20)), ]
twenty <- twenty[, c(10, 16,18,20)]
twenty$avg <- rowMeans(twenty[,c(2,3,4)], na.rm = TRUE)
head(twenty[order(-twenty$avg),])

#3.c
wc.data.2014 <- read.csv("~/Google Drive/Academic/Spring 2016/CS 4821/wc-data-2014.csv")

#3.c.i
groupStage <- wc.data.2014[1:48,]
groupStage$home_dif <- groupStage$home_score - groupStage$away_score
groupStage$away_dif <- groupStage$away_score - groupStage$home_score

sort(tapply(groupStage$home_dif, groupStage$home, FUN=sum) + 
       tapply(groupStage$away_dif, groupStage$away, FUN=sum),
     decreasing = TRUE)

#3.c.ii
sort(tapply(groupStage$home_dif, groupStage$home_continent, FUN=sum) + 
       tapply(groupStage$away_dif, groupStage$away_continent, FUN=sum),
     decreasing = TRUE)

#3.c.iii
