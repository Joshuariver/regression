rm(list=ls())
setwd("~/R/Regression/exercise")

# 단순 회귀분석 연습
# 광고비와 매출액간의 상관관계를 보기 위해서 최근 10개월간의 데이터를 수집한 자료

rt <- read.csv("1_sr_text.csv")
str(rt)
summary(rt)

qqnorm(rt$sales)


model<-lm(sales~ promo, data=rt)
summary(model)
model_simple<-lm(sales~ promo, data=rt)

anova(model, model_simple)


new_efficiency <-rnorm(10, mean=mean(rt$sales), sd=sd(rt$sales))
new_efficiency <-as.data.frame(new_efficiency)
colnames(new_efficiency)<-c("promo")
predict(model_simple, new_efficiency, interval="prediction")

