Hitters <- read.csv("Hitters.csv")
str(Hitters)
?na.omit
Hitters <- na.omit(Hitters)
str(Hitters)

install.packages("leaps")
library(leaps)

Hitters <- Hitters[,2:21]
model1 <- regsubsets(Salary~., data=Hitters, nvmax = 19)
summary(model1)
summary(model1)$rsq
plot(summary(model1)$rsq)
plot(summary(model1)$rss)
plot(summary(model1)$adjr2)
which.max(summary(model1)$adjr2)
points(11,summary(model1)$adjr2[11], col="red",pch=20)

model2 <- regsubsets(Salary~., data=Hitters, nvmax = 19, method = "forward")

which.max(summary(model2)$adjr2)
coef(model1,11)
coef(model2,11)

install.packages("glmnet")
library(glmnet)
# translates into a quantitative format
x <- model.matrix(Salary~.,Hitters)
y <- Hitters$Salary
grid <- 10^seq(10, -2, length=100)

set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
modellasso <- glmnet(x[train,], y[train], alpha = 1, lambda = grid)
modellasso
plot(modellasso, xvar="lambda")

# -train take all the rows other than the training rows
predictlasso1 <- predict(modellasso, newx = x[-train,], s=100)
mean((predictlasso1-y[-train])^2)

predictlasso2 <- predict(modellasso, newx = x[-train,], s=1000)
mean((predictlasso2-y[-train])^2)

predictlasso3 <- predict(modellasso, newx = x[-train,], s=0, exact = TRUE)
mean((predictlasso3-y[-train])^2)

set.seed(2)

cvlasso <- cv.glmnet(x[train,],y[train],alpha = 1)

predictlassocv <- predict(modellasso, s=22.18, newx=x[-train,])
mean((predictlassocv-y[-train])^2)
lassocoef <- predict(modellasso,s=22.18,type="coefficient")
lassocoef
