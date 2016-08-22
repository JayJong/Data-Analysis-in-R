o <- read.csv("oscars.csv")

o$Ch <- 2-o$Ch

tapply(o$Nom[o$PP==1], o$Ch[o$PP==1], mean)
# 0        1 
6.780702 9.526316


tapply(o$Nom[o$PP==1], o$Ch[o$PP==1], var)
# 0        1 
5.264472 5.075188 



t.test(o$Nom[o$PP==1&o$Ch==1], o$Nom[o$PP==1&o$Ch==0],alternative = c("greater"))

Welch Two Sample t-test

data:  o$Nom[o$PP == 1 & o$Ch == 1] and o$Nom[o$PP == 1 & o$Ch == 0]
t = 8.1994, df = 87.361, p-value = 9.479e-13
alternative hypothesis: true difference in means is greater than 0
95 percent confidence interval:
  2.188922      Inf
sample estimates:
  mean of x mean of y 
9.526316  6.780702



table(o$Dir[o$PP==1&o$Ch==1])
0  1 
1 56

which(o$Dir==0&o$PP==1&o$Ch==1)
# 362

o[which(o$Dir==0&o$PP==1&o$Ch==1),c("Year", "Name")]
Year    Name
362 1989 Driving

table(o$Gdr[o$PP==1&o$Ch==1] + o$Gmc[o$PP==1&o$Ch==1])

install.packages("mlogit")

library(mlogit)

opp <- subset(o, PP==1)

opp$gg <- opp$Gdr + opp$Gmc

d <- mlogit.data(subset(opp, Year <= 2006), choice = "Ch", shape = "long", alt.var = "Mode")

model1 <- mlogit(Ch~Days + Dir + Nom + Afl + Length + gg + PGA + Aml-1 , data = d)
# the 1 means that we dont wan the constant

model2 <- mlogit(Ch~Dir + Nom + gg + PGA -1 , data = d)

rho <- 1 - ((-38.171) / (56*log(1/5)))
# 0.5764826

aicmodel2 <- 2*4 - 2 * (-38.171)

aicmodel1 <- 2*8 - 2 * (-36.722)

predict2 <- predict(model2, newdata = subset(opp, Year==2007))
#  1          2          3          4          5 
#0.01339511 0.04886810 0.09316376 0.72905382 0.11551921

d <- mlogit.data(opp, choice = "Ch", shape = "long", alt.var = "Mode")

model4 <- mlogit(Ch~Dir + Nom + gg + PGA -1 , data = d)

predict4 <- predict(model4, newdata = d)

P <- as.vector(t(predict4))

Fail <- 0

Predict <- NULL

Coefficients <- NULL

omm <- subset(o, MM==1)



for (i in 1960:2006){
  d <- mlogit.data(subset(omm, Year <= i), choice ="Ch", shape="long", alt.var="Mode")
  m <- mlogit(Ch~Pic+Gm1+Gm2+PrNl+PrWl-1, data=d)
  Coefficients <- rbind(Coefficients, m$coeff)
  p <- predict(m, newdata=subset(omm, Year==i+1))
  Predict <- rbind(Predict, p)
  Fail <- Fail + as.logical(which.max(p)-which.max(subset(omm, Year==i+1)$Ch)) # identify among the predictor probablilty which one is maximum
}