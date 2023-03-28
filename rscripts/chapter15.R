# Chapter 15

## 실습: 단순 선형 회귀분석 수행
## 단계 1: 데이터 가져오기 (전처리 NA)
product <- read.csv("./data/product.csv", header = T, fileEncoding = "euc-kr")
str(product)
View(product)
## 단계 2: 독립변수와 종속변수 생성
y = product$제품_만족도 
x = product$제품_적절성
df <- data.frame(x, y)

# 벡터 - 선형, 컬럼을 만들 때는 벡터를 써라.
# 매트릭스 - 표는 벡터들의 모임
# 테이블 - 헤드가 있는 표
# 데이터프레임 - 

## 단계 3: 단순 선형회귀 '모델'(절편과 기울기) 생성
result.lm <- lm(formula = y ~ x, data = df)

## 단계 4: 회귀분석의 절편과 기울기
result.lm

## Coefficients(계수):
# (Intercept - 절편)             x  
#     0.7789                0.7393  

## 단계 5: 모델의 잔차 보기 
# names(result.lm)
residuals(result.lm)[1:2]
#  1          2 
# -0.7359630 -0.9966869 

## 단계 5-1: 적합값 보기 
fitted.values(result.lm)[1:2]

## 단계 5-2: 관측값 보기 
head(df, 1)

# 단계 5-3: 회귀방정식을 적용하여 모델의 적합값 계산
Y = 0.7789 + 0.7393 * 4
Y

## 3.735963 2.996687 

## 단계 5-4: 잔차(오차) 계산
3 - 3.735963


## 단계 5-5: 모델의 잔차 보기 
residuals(result.lm)[1:2]

## 단계 5-6: 모델의 잔차와 회귀방정식에 의한 적합값으로부터 관측값 계산
-0.7359630 + 3.735963


# 실습 2: 선형 회귀분석 모델 시각화
# 단계 1: x, y 산점도 그리기 
plot(formula = y ~ x, data = product)


# 단계 2: 선형 회귀모델 생성
result.lm <- lm(formula = y ~ x, data = product)

# 단계 3: 회귀선
abline(result.lm, col = "red")


# 단계 4: 선형 회귀분석 결과보기 
summary(result.lm)

# 예제 : 캘리포니아 집값 데이터
# y = median_house_value
# x = ???

library(tidyverse)
library(reshape2)

# 데이터 윤리 문제에 주의.
# 1. 통계청 출산 데이터 (시계열, 회귀)
# 2. 캘리포니아 집값 데이터 (회귀 / 시각화) - 보스턴은 쓰지 마시오(데이터 편향 문제)
# 3. 손글씨 분류 (분류 - 0~9분류) / iris (분류)
# tensorflow fashion

house <- read.csv("./data/housing.csv")
head(house)
melt(house)

# households = 세대수

# 데이터 분석
summary(house)

# 데이터 시각화(데이터 확인을 위해서)
# 2, 5칸으로 분리 => 30개씩 bins로 묶어서 사용
par(mfrow = c(2,5))
colnames(house)
# ggplot()
ggplot(data = melt(house), mapping = aes(x = value) + geom_histogram(bins=30) + facet_wrap(-variable, scale = 'free_x'))
# ggplot(df, aes(gp, y)) + geom_point() + geom_point(data = ds, aes(y = mean), colour = 'red', size = 3)

## total_rooms와 total_bedrooms 결측치 처리
house$mean_bedrooms = house$total_bedrooms / house$households
house$mean_rooms = house$total_rooms / house$households
# 기존 값 드랍
drop = c('total_bedrooms', 'total_rooms')
# 결측치 처리
# 데이터가 너무 커서 가중치를 다 먹고 있기 때문에 처리한다.(정규화에 더 가깝다)
house = house[ , !(names(house) %in% drop)]
head(house)

## ocean_proximity chr를 num으로 바꾸기.
categories = unique(house$ocean_proximity)

cat_house = data.frame(ocean_proximity = house$ocean_proximity)
# for문 형식 체크.
for(cat in categories) {
  cat_house[,cat] = rep(0, times = nrow(cat_house))
}
head(cat_house)

## 해안만의 가격이 비싸다는 것을 확인할 전처리 코드
## 상식을 사용해서 가정에 대한 데이터를 별도로 분리
## 가중치를 부여할 곳에 1 부여.
for(i in 1:length(cat_house$ocean_proximity)) {
  cat = as.character(cat_house$ocean_proximity[i])
  cat_house[,cat][i] = 1
}
head(cat_house)

# NEAR BAY = 우리가 말하는 해안만
# NEAR OCEAN = 집 앞이 바다

