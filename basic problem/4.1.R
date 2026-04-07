#다중 선형회귀 분석 4.1
#예제 4.1 - airquality에 대한 다중회귀분석

data(airquality)
airquality[1:3,]

pairs(airquality[,1:4], panel=panel.smooth)

lm.a<- lm(Ozone~ Solar.R + Wind + Temp, data=airquality)
summary(lm.a)
lm.ab<- lm(log(Ozone)~ Solar.R + Wind + Temp, data=airquality)
summary(lm.ab)

op=par(mfrow=c(1,2))
plot(fitted(lm.a), residuals(lm.a), xlab="fitted", ylab="residual")
plot(fitted(lm.ab), residuals(lm.ab), xlab="fitted", ylab="residual")
abline(h=0)
par(op)

shapiro.test(residuals(lm.a)) 
shapiro.test(residuals(lm.ab))

library(lmtest)
dwtest(Ozone~ Solar.R + Wind + Temp, data= na.omit(airquality))
dwtest(log(Ozone)~ Solar.R + Wind + Temp, data= na.omit(airquality))



lm.ab<- lm(log(Ozone)~ Solar.R + Wind + Temp, data=airquality)
confint(lm.ab)                    # 95% CI
coef(lm.ab)                       # parameter coeff

library(ellipse)
op=par(mfrow=c(1,3))
plot(ellipse(lm.ab,c(2,3)), type="l")
points(coef(lm.ab)[2], coef(lm.ab)[3], pch=18)
abline(v=confint(lm.ab)[2,], lty=2)
abline(h=confint(lm.ab)[3,], lty=2)
plot(ellipse(lm.ab,c(2,4)), type="l")
points(coef(lm.ab)[2], coef(lm.ab)[4], pch=18)
plot(ellipse(lm.ab,c(3,4)), type="l")
points(coef(lm.ab)[3], coef(lm.ab)[4], pch=18)
par(op)

x0<- data.frame(Solar.R=170, Wind=8, Temp=70, Month=0, Day=0)
x0
predict(lm.ab, x0,interval="confidence")
predict(lm.ab, x0,interval="prediction")

conf<-predict(lm.ab, airquality,interval="confidence")
matplot( seq(1,length(conf[,1])), conf,type="lll", lty=c(1,2,2))

pred<-predict(lm.ab, airquality,interval="prediction")
pred


getwd()
setwd("C:/Users/user/Documents/R_DA")
library(readxl)
data <- read_excel("table4.4_cholesterol.xlsx") 
head(data)
tibble::glimpse(data)      # 데이터 구조 한눈에 보기
str(data)                 # 구조 및 변수 타입 확인
summary(data)   

plot(data$x1, data$y, xlab="몸무게", ylab="콜레스테롤", main="(X1,Y) 산점도")
plot(data$x2, data$y, xlab="나이", ylab="콜레스테롤", main="(X2,Y) 산점도")
model1 <- lm(y ~ x1, data=data)
summary(model1)

model2 <- lm(y ~ x2, data=data)
summary(model2)

model <- lm(y ~ x1 + x2, data=data)
summary(model)

summary(model)$r.squared
plot(fitted(model), resid(model), main="잔차 vs 적합값", xlab="적합값", ylab="잔차")
abline(h=0, col="red")

qqnorm(resid(model))
qqline(resid(model), col="red")

shapiro.test(resid(model))

library(lmtest)
dwtest(y ~ x1 + x2, data = na.omit(data))

predict(model, newdata=data.frame(x1=84, x2=54))
