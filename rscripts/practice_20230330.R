# 직장인 => 생산성과 협력 => 보고서(이해가능, 설명가능)

# 회귀 => 값을 예측하는 것
## 단순 회귀
## 절차상 제일 간단함.

# 순서 정하기
# 1. 데이터 불러오기 확인 => 시각적으로 확인
# 2. 전처리 과정 (NA 지우기)
# 2_1. 후처리 과정 (표준화와 정규화)
# 3. 데이터 분리 -> 학습 데이터와 검증 데이터의 분리
# 4. 학습 -> 기울기와 절편을 구해야 한다.
# 5. 검증 -> 모델을 검증

# 캘리포니아 집값예측 (사이킷런의 첫 예제)
# 데이터 불러오기
housing = read.csv("./data/housing.csv")
# str 사용은 위험할 수 있다.
str(housing)
# head 로 데이터를 일부 확인한다.
head(housing)
# 기술 통계 기반 정보 확인
# Median(중앙값)과 Mean(평균)을 알아야 오차에 대응할 수 있다.
# 조직에서 정한 y를 가지고 판단해보자.
summary(housing)

# 3가지 과제.
# 결측치 처리를 해야 한다.
# 자료형을 정리해야 한다.
# total_rooms와 total_bedrooms의 값이 너무 커서 정리가 필요하다.

# 컬럼 이름 확인
colnames(housing)


# ggplot
library(tidyverse)
library(reshape2)

ggplot(data = melt(housing), mapping = aes(x = value)) + geom_histogram(bins = 30) + facet_wrap(~variable, scales = 'free_x')
# 한장씩 나오면, ctrl + enter로 한줄씩 적은 코드들을 한번에 실행한다.

# 전처리
# 합산된 값들이 나머지 값들을 먹어치울 것 같으면, 평균을 쓰지 말자.
# 그러므로, NA는 중앙값으로 처리하도록 하자.
# 양이 많아지면 시간이 오래 걸릴 수 있다.
housing$total_bedrooms[is.na(housing$total_bedrooms)] <- median(housing$total_bedrooms, na.rm = T)
sum(is.na(housing))

# 후처리
# 데이터 변수 10개 중 3개는 연속적(소수점 단위 - 산점도)이고, 6개는 불연속적(정수 단위 - 히스토그램)이다.
housing$mean_bedrooms = housing$total_bedrooms / housing$households
housing$mean_rooms = housing$total_rooms / housing$households
head(housing)

# 불필요한 데이터 삭제(범주형 데이터 정리 -> 러닝 돌리기)
# 데이터가 너무 커서 가중치를 다 먹고 있기 때문에 처리한다.(정규화에 더 가깝다)
drop = c('total_bedrooms', 'total_rooms')
housing = housing[ , !(names(housing) %in% drop)]
head(housing)

## 범주형 변수 처리를 위한 데이터프레임 생성
## ocean_proximity chr를 num으로 바꾸기.
categories = unique(housing$ocean_proximity)

cat_housing = data.frame(ocean_proximity = housing$ocean_proximity)
# for문 형식 체크.
for(cat in categories) {
  cat_housing[,cat] = rep(0, times = nrow(cat_housing))
}
head(cat_housing)

## 해안만의 가격이 비싸다는 것을 확인할 전처리 코드
## 상식을 사용해서 가정에 대한 데이터를 별도로 분리
## 가중치를 부여할 곳에 1 부여.
for(i in 1:length(cat_housing$ocean_proximity)) {
  cat = as.character(cat_housing$ocean_proximity[i])
  cat_housing[,cat][i] = 1
}
head(cat_housing)
# NEAR BAY = 우리가 말하는 해안만
# NEAR OCEAN = 집 앞이 바닷물

# 기존 특징은 사용하지 않기 때문에 삭제
cat_columns = names(cat_housing)
keep_columns = cat_columns[cat_columns != 'ocean_proximity']
cat_housing = select(cat_housing,one_of(keep_columns))
tail(cat_housing)

colnames(housing)
drops = c('ocean_proximity','median_house_value')
housing_num =  housing[ , !(names(housing) %in% drops)]
head(housing_num)
scaled_housing_num = scale(housing_num)
head(scaled_housing_num)

cleaned_housing = cbind(cat_housing, scaled_housing_num, median_house_value=housing$median_house_value)
head(cleaned_housing)

# 데이터를 분리 -> 학습과 검증
set.seed(42) # Set a random seed so that same sample can be reproduced in future runs
sample = sample.int(n = nrow(cleaned_housing), size = floor(.8*nrow(cleaned_housing)), replace = F)
train = cleaned_housing[sample, ] #just the samples
test  = cleaned_housing[-sample, ] #everything but the samples

# 분리된 데이터가 전체 데이터를 반영하고 있는지 확인
head(train)
nrow(train) + nrow(test) == nrow(cleaned_housing)

## 예측 모델 생성 및 평가
## 단순 선형 모델

# 간단한 선형 모형 테스트를 위해 아래 3개 변수를 선택하여 분석에 적용
# - 소득(중앙값) : `median_income`
# - 방 수(평균값) : `mean_rooms`
# - 인구 : `population`


# 모델의 과적합(`overfit`) 문제를 피하기 위해 `cv.glm`함수를 이용하여 교차 검증(k_fold)를 수행하며, 
# 여기서는 모델 테스트에 전처리된 데이터 자체를 사용합니다.
glm_house = glm(median_house_value~median_income+mean_rooms+population, data=cleaned_housing)
# 잔차값(에러값)을 확인하기 위해 5번 학습해라.
k_fold_cv_error = cv.glm(cleaned_housing , glm_house, K=5)
# 원시 교차검증 추정치
k_fold_cv_error$delta

# 조정된 교차검증 수정치
glm_cv_rmse = sqrt(k_fold_cv_error$delta)[1]
glm_cv_rmse #off by about $83,000... it is a start

#what parts of the model are callable? 
names(glm_house) 
glm_house$coefficients

# 랜덤 포레스트를 써서 개선한다.
names(train)
set.seed(42)
train_y = train[,'median_house_value']
train_x = train[, names(train) !='median_house_value']
head(train_y)
head(train_x)

# ntree의 값은 내가 정한다. for 루프로 500부터 1500까지 루프를 돌려보는 것도 좋다.
rf_model = randomForest(train_x, y = train_y , ntree = 500, importance = TRUE)
names(rf_model) #these are all the different things you can call from the model.
rf_model$importance


oob_prediction = predict(rf_model) #leaving out a data source forces OOB predictions
train_mse = mean(as.numeric((oob_prediction - train_y)^2))
oob_rmse = sqrt(train_mse)
oob_rmse

test_y = test[,'median_house_value']
test_x = test[, names(test) !='median_house_value']


y_pred = predict(rf_model , test_x)
test_mse = mean(((y_pred - test_y)^2))
test_rmse = sqrt(test_mse)
test_rmse
