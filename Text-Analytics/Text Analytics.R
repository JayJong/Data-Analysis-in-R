energy <- read.csv("energy_bid.csv", stringsAsFactors = FALSE)
strwrap(energy$email[1])

table(energy$responsive)

library(tm)
corpus <- Corpus(VectorSource(energy$email))
as.character(corpus[[4]])
strwrap(as.character(corpus[[4]]))

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stemDocument)
strwrap(as.character(corpus[[4]]))

freq <- DocumentTermMatrix(corpus)
freq
freq <- removeSparseTerms(freq, 0.97)
energysparse <- as.data.frame(as.matrix(freq))
energysparse$responsive <- energy$responsive
library(caTools)
set.seed(1978)
spl <- sample.split(energysparse$responsive, SplitRatio = 0.7)
train <- subset(energysparse, spl == TRUE)
test <- subset(energysparse, spl == FALSE)

library(rpart)
model1 <- rpart(as.factor(responsive)~., data = train)
library(rpart.plot)
prp(model1, type=4, extra=2)
printcp(model1)
predict1 <- predict(model1, newdata = test, type = "class")
table(predict1, test$responsive)

library(randomForest)
model2 <- randomForest(as.factor(responsive)~., data = train)


colnames(energysparse) <- make.names(colnames(energysparse))
spl <- sample.split(energysparse$responsive, SplitRatio = 0.7)
train <- subset(energysparse, spl == TRUE)
test <- subset(energysparse, spl == FALSE)
model2 <- randomForest(as.factor(responsive)~., data = train)
predict2 <- predict(model2, newdata = test, type = "class")
table(predict2, test$responsive)

varUsed(model2)
order2 <- sort(varUsed(model2), index.return=TRUE)
tail(order2$ix)
names(test[,tail(order2$ix)])

install.packages("e1071")
library(e1071)
model3 <- naiveBayes(as.factor(responsive)~., data = train)
summary(model3)
predict3 <- predict(model3, newdata = test, type = "class")
table(predict3, test$responsive)






