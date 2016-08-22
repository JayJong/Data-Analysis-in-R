supreme <- read.csv("supreme.csv")
head(supreme)

stevens <- subset(supreme[,c("docket", "term", "stevdir", "petit", "respon", "circuit", "unconst", "lctdir", "issue")], supreme$stevdir != 9)
str(stevens)
stevens$rev <- as.integer((stevens$lctdir=="conser" & stevens$stevdir == 0) | (stevens$lctdir=="liberal") & stevens$stevdir == 1)

library(caTools)
set.seed(1)
spl <- sample.split(stevens$rev, SplitRatio = 0.7)
train <- subset(stevens, spl == TRUE)
test <- subset(stevens, spl == FALSE)

model1 <- glm(rev~petit+respon+circuit+issue+lctdir+unconst, data = train, family = binomial)
predict1 <- predict(model1, newdata = test, type = "response")

table(train$issue)
table(test$issue)
# there is no "IR" in training set and therefore we change the test set

test <- subset(test, test$issue != "IR")
predict1 <- predict(model1, newdata = test, type = "response")

table(predict1 >= 0.5, test$rev)

install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

cart1 <- rpart(rev~petit+respon+circuit+issue+unconst+lctdir, data = train)
cart1
summary(cart1)

cart1 <- rpart(rev~petit+respon+circuit+issue+unconst+lctdir, data = train, method = "class")
prp(cart1)
prp(cart1, type = 4)
prp(cart1, type = 4, extra = 4)

predictcart1 <- predict(cart1, newdata = test, type = "class")
table(predictcart1, test$rev)
#predictcart1  0  1
#           0 54 33
#           1 28 69

table(train$rev)
#0   1 
#195 239 
table(test$rev)
#0   1 
#82 102

printcp(cart1)

cart2 <- prune(cart1, 0.036)
cart2
predictcart2 <- predict(cart2, newdata = test, type = "class")
table(predictcart2, test$rev)

install.packages("randomForest")
library(randomForest)
forest1 <- randomForest(as.factor(rev) ~ petit+respon+circuit+issue+unconst+lctdir, data=train, ntree=200,nodesize=5)
predictforest1 <- predict(forest1, newdata = test, type = "class")
table(predictforest1, test$rev)

