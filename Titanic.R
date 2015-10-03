library(randomForest)
library(gplots)
library(ROCR)
set.seed(415)

data <- read.csv("data.csv" ,  header=T, na.strings = "" )
summary(data)


data$Fare[is.na(data$Fare)] <- 0
str(data)

data$Name <- as.character(data$Name)

data$Title <- trim(sapply(data$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]}))

data$TktCount <- sapply(data$Ticket, function(x) length(which(data$Ticket == x)))
length(data$TktCount)
data$nFare <- rep(0, 1309)
data$nFare <- data$Fare/data$TktCount

data$FareR <- c( "0", "1-10","10-20", "20-30", "30-40", "40-50", "50-100", "100-200", "200-550")[ findInterval( data$nFare, c(0,1, 10,20, 30, 40, 50, 100,200, 600) ) ]
data$FareR <- as.factor(data$FareR)
data$Title <- as.factor(data$Title)

which(is.na(data$Embarked))
data$Embarked[62] <- 'S'
data$Embarked[830] <- 'S'
data$Embarked[which(is.na(df.data$Embarked))] <- 'S' #short cut

which(is.na(data$Age))

install.packages('Hmisc', dependencies=TRUE)
library(Hmisc)

options(digits=2)
require(Hmisc)
bystats(data$Age, data$Title,  fun=function(x)c(Mean=mean(x),Median=median(x)))


titles.na.data <- c("Dr", "Master", "Mrs", "Miss", "Mr", "Ms")

data$Age[which(data$Title=="Dr")]
data$Age <- imputeMedian(data$Age, data$Title,  titles.na.data)
summary(data)
str(data)

#data <- as.data.frame(trainD)
data$Survived <- as.factor(data$Survived)


dataCopy <- data

data$PassengerId <- NULL
data$Name <- NULL
data$nFare <- NULL
data$TktCount <- NULL
data$Cabin <- NULL
data$Ticket <- NULL
data$Fare <- NULL

data1 <- subset(data, Survived %in% c("0","1"))
data2 <- subset(data, is.na(data$Survived))

#data22 <- subset(data, is.na(dataCopy$Survived))

bestmtry <- tuneRF(trainD[-1], trainDD$Survived, ntreeTry=100,stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE, dobest=FALSE)

bestmtry <- tuneRF( data1$Pclass + data1$Sex + data1$Age + data1$SibSp + data1$Parch + data1$FareR + data1$Embarked + data1$Title  , data1$Survived, data = data1
	ntreeTry=100,stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE, dobest=FALSE)



fit  <-randomForest(Survived~.,data=data1, mtry=3, ntree=2000, keep.forest=TRUE, importance=TRUE)
varImpPlot(fit)  # what variables are important
#The response has five or fewer unique values.  Are you sure you want to do regression?

fit12 <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + FareR + Embarked + Title 
	, data=data1, mtry=3, ntree=2000, keep.forest=TRUE, importance=TRUE)
varImpPlot(fit12)

testDData <- as.data.frame(testData)
testDData$PassengerId <- NULL
testDData$PassengerId <- test$PassengerId

Predic121 <- predict(fit12, data2)
submit <- data.frame(PassengerId = data2$PassengerId, Survived = Predic121)
write.csv(submit, file = "firstforest121.csv", row.names = FALSE)

write.csv(data1, file = "data1.csv", row.names = FALSE)
write.csv(data2, file = "data2.csv", row.names = FALSE)

plot(Predic12, log="y")
plot(fit)
varImpPlot(fit)

getTree(fit)

plot(fit, uniform=TRUE)

print(fit) # view results
importance(fit) # importance of each predictor

#==================== forest of conditional inference trees===================

install.packages('party')
library(party)
set.seed(415)


fit122 <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + FareR + Embarked + Title 
	, data=data1,controls=cforest_unbiased(ntree=2000, mtry=3))
varImpPlot(fit122)

Predic122 <- predict(fit122, data2)
submit <- data.frame(PassengerId = data2$PassengerId, Survived = Predic122)
write.csv(submit, file = "firstforest122.csv", row.names = FALSE)