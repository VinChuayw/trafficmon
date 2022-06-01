# Graphical display of the observed data.
aadt_raw <- read.table('c:/aadt.txt',header=FALSE)
tm <- data.frame(y=aadt_raw$V1,x1=aadt_raw$V2,x2=aadt_raw$V3,x3=aadt_raw$V4,x4=aadt_raw$V5)
plot(tm)

#Fit a MLR
mlr <- lm(y~x1+x2+x3+x4,data=tm)
summary(mlr)
#structure of MLR
names(mlr)
mlrs <- summary(mlr)
names(mlrs)

#Normality checking
qqnorm(residuals(mlr),ylab='Residuals')
qqline(residuals(mlr))

#Draw some plots of residuals
par(mfrow=c(1,6))
plot(residuals(mlr),ylab='Residuals',xlab='Time')
plot(residuals(mlr),fitted(mlr),ylab='Residuals',xlab='Fitted values')
plot(residuals(mlr),tm$x1,ylab='Residuals',xlab='x1')
plot(residuals(mlr),tm$x2,ylab='Residuals',xlab='x2')
plot(residuals(mlr),tm$x3,ylab='Residuals',xlab='x3')
plot(residuals(mlr),tm$x4,ylab='Residuals',xlab='x4')
par(mfrow=c(1,1))

#Durbin-Watson tests
library(lmtest)
dwtest(y ~ x1+x2+x3+x4, data=tm)

#Some F-tests
#test x3 predictor = 0
mlr1 <- lm(y ~ x1+x2+x4,data=tm)
summary(mlr1)
anova(mlr1,mlr)
#test if coefficient of x3 is constant
mlr3 <- lm(y ~ x1+x2+offset(100.3*x3)+x4,data=tm)
summary(mlr3)
anova(mlr3,mlr)

#prediction x1=50000  x2=3  x3=60, x4=2
con <- (c(1,50000,3,60,2))
lhat <- sum(con*coef(mlr))
lhat
t05 <- qt(0.975,116)
bm <- t05*mlrs$sigma*sqrt(con%*%mlrs$cov.unscaled%*%con) 
c(lhat-bm,lhat+bm)
c3 <- 1
bm <- t05*mlrs$sigma*sqrt(con%*%mlrs$cov.unscaled%*%con+c3)
c(lhat-bm,lhat+bm)
con <- data.frame(x1=50000,x2=3,x3=60,x4=2)
predict(mlr,con,interval='confidence',level=0.95)
predict(mlr,con,interval='prediction',level=0.95)
