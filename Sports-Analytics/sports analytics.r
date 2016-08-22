baseball <- read.csv("baseball.csv")

#baseball2002 is the years before 2002
baseball2002 <- subset(baseball, Year < 2002)

plot(baseball2002$W, baseball2002$Team, col = ifelse(baseball2002$Playoffs == 1, "red", "blue"))

baseball2002$RD <- baseball2002$RS - baseball2002$RA

#linear regression with wins against runs difference(RD)
model1 <- lm(W~RD, data = baseball2002) 

plot(baseball2002$RD, baseball2002$W)

abline(model1)

lm(RS~BA, data = baseball2002)

summary(lm(RS~BA, data = baseball2002))

model2 <- lm(RS~OBP+SLG, data = baseball2002)

abline(model2)

model3 <- lm(RA~OOBP+OSLG, data = baseball2002)






