getwd()
setwd("C:/Users/user/Documents/R_DA")
install.packages("readxl")
library(readxl)

data <- read_excel("table3.5_campaign.xlsx") 
# 데이터 확인
head(data)

#a 산점도
plot(data$X, data$Y,
     main = "캠페인 비용과 선거참여비율 산점도",
     xlab = "캠페인 비용(X)",
     ylab = "선거참여비율(Y)")

#b 상관계수
cor(data$X, data$Y)
cor.test(data$X, data$Y)

#c 단순회귀모형 적합
model <- lm(Y ~ X, data = data)
summary(model)

#d 회귀식 그리기
plot(data$X, data$Y,
     main = "캠페인 비용과 선거참여비율 산점도 및 회귀직선",
     xlab = "캠페인 비용(X)",
     ylab = "선거참여비율(Y)")
abline(model, col = "blue", lwd = 2)
#ggplot 사용했을 때
install.packages("ggplot2")
library(ggplot2)
ggplot(data, aes(x = X, y = Y)) + 
  geom_point(color='darkblue', size=2) +
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE, color = "black", size=1) +
  labs(title = "캠페인 비용 vs. 선거참여비율 회귀분석",
       x = "캠페인 비용(X)",
       y = "선거참여비율(Y)")

#e c의 summary 확인(p-value)

#f 신뢰구간
confint(model, level = 0.95)

#g 결정계수
summary(model)$r.squared

#h 잔차&잔차제곱합
residuals <- resid(model)
residuals
sum(residuals^2)  # 잔차제곱합 (Residual Sum of Squares: RSS)

#i 오차의 독립성
plot(fitted(model), residuals,
     xlab = "적합값 (Fitted Values)",
     ylab = "잔차 (Residuals)")
abline(h = 0, col = "red")

#j 정규 QQ plot
qqnorm(residuals)
qqline(residuals, col = "red")

#k Y값 예측(X=50)
predict(model, newdata = data.frame(X = 50))
