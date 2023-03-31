# 실습: 의사결정 트리 생성: ctree() 함수 이용 
# 단계 1: party 패키지 설치 
install.packages("party")
library(party)

# 단계 2: airquality 데이터 셋 로딩
# install.packages("datasets")
library(datasets)
str(airquality)

# 단계 3: formula 생성 (y ~ x) 
# x의 요소 3가지(Solar.R + Wind + Ozone)는 Temp를 결정하는 데 사용된다.
formula <- Temp ~ Solar.R + Wind + Ozone

# 단계 4: 분류모델 생성 - formula를 이용하여 분류모델 생성
air_ctree <- ctree(formula, data = airquality)
air_ctree

# 단계 5: 분류분석 결과
plot(air_ctree)
# 이진 트리 형식 -> 제일 위의 값(OZone)이 제일 중요하고, 위의 값에 따라 두 갈래로 나뉜다.
# 데이터에서 가장 중요하다는 점이 아님에 주의하라.
# ex) OZone의 값 37을 기준으로 두 갈래로 나뉜다.


# 실습: 학습데이터와 검정데이터 샘플링으로 분류분석 수행
# 단계 1: 학습데이터와 검정데이터 샘플링
set.seed(1234)
# 7 : 3
idx <- sample(1:nrow(iris), nrow(iris) * 0.7)
train <- iris[idx, ]
test <- iris[-idx, ]

# 단계 2: formula(공식) 생성 (y ~ x) (y는 x를 따른다.)
formula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width

# 단계 3: 학습데이터 이용 분류모델 생성
iris_ctree <- ctree(formula, data = train)
iris_ctree

# 단계 4: 분류모델 플로팅
# 단계 4-1: 간단한 형식으로 시각화 
plot(iris_ctree, type = "simple")

# 단계 4-2: 의사결정 트리로 플로팅
plot(iris_ctree)


# 단계 5: 분류모델 평가
# 단계 5-1: 모델의 예측치 생성과 혼돈 매트릭스 생성
pred <- predict(iris_ctree, test)

table(pred, test$Species)

# 단계 5-2: 분류 정확도 - 96%
(16 + 15 + 12) / nrow(test)

# 실습: 랜덤 포레스트 기본 모델 생성
# 단계 1: 패키지 설치 및 데이터 셋 가져오기 
install.packages("randomForest")
library(randomForest)
data(iris)

# 단계 2: 랜덤 포레스트 모델 생성 (Species를 찾기 위해 다른 모든 값을 다 써라)
model <- randomForest(Species ~ ., data = iris)
model

# 실습: 파라미터 조정 - 트리 개수 300개, 변수 개수 4개 지정  
model2 <- randomForest(Species ~ ., data = iris,
                       ntree = 300, mtry = 4, na.action = na.omit)
model2

# 실습: 중요 변수를 생성하여 랜덤 포레스트 모델 생성 
# 단계 1: 중요 변수로 랜덤 포레스트 모델 생성
model3 <- randomForest(Species ~ ., data = iris,
                       importance = T, na.action = na.omit)

# 단계 2: 중요 변수 보기 
importance(model3)
 
# 단계 3: 중요 변수 시각화
varImpPlot(model3)

# 실습: 다향 분류 xgboost 모델 생성 (이게 제일 중요함!!!!)
# 단계 1: 패키지 설치
install.packages("xgboost")
library(xgboost)

# 단계 2: y 변수 생성
iris_label <- ifelse(iris$Species == 'setosa', 0,
                     ifelse(iris$Species == 'versicolor', 1, 2))
table(iris_label)
iris$label <- iris_label
 
# 단계 3: 데이터 셋 생성
idx <- sample(nrow(iris), 0.7 * nrow(iris))
train <- iris[idx, ] 
test <- iris[-idx, ]

# 단계 4: matrix 객체 변환
train_mat <- as.matrix(train[-c(5:6)])
dim(train_mat)

train_lab <- train$label
length(train_lab)


# 단계 5: xgb.DMatrix 객체 변환
# xgb.DMatrix(data = 데이터, label = 러닝)
dtrain <- xgb.DMatrix(data = train_mat, label = train_lab)

# 단계 6: model 생성 - xgboost matrix 객체 이용
xgb_model <- xgboost(data = dtrain, max_depth = 2, eta = 1,
                     nthread = 2, nrounds = 2,
                     objective = "multi:softmax", 
                     num_class = 3,
                     verbose = 0)
xgb_model

# 단계 7: testset 생성
test_mat <- as.matrix(test[-c(5:6)])
dim(test_mat)
test_lab <- test$label
length(test_lab)

# 단계 8: model prediction
pred_iris <- predict(xgb_model, test_mat)
pred_iris

# 단계 9: confusion matrix
table(pred_iris, test_lab)

# 단계 10: 모델 성능평가1 - Accuracy
(19 + 13 + 12) / length(test_lab)

# 단계 11: model의 중요 변수(feature)와 영향력 보기 
importance_matrix <- xgb.importance(colnames(train_mat), 
                                    model = xgb_model)
importance_matrix

# 단계 12: 중요 변수 시각화 
xgb.plot.importance(importance_matrix)



# 실습: 간단한 인공신경망 모델 생성
# 단계 1: 패키지 설치 
install.packages("nnet")
library(nnet)

# 단계 2: 데이터 셋 생성
df = data.frame(    # 데이터프레임 생성 - 입력 변수(x)와 출력변수(y)
  x2 = c(1:6),
  x1 = c(6:1),
  y = factor(c('no', 'no', 'no', 'yes', 'yes', 'yes'))
  )

str(df)

# 단계 3: 인공신경망 모델 생성
model_net = nnet(y ~ ., df, size = 1)

# 단계 4: 모델 결과 변수 보기 
model_net

# 단계 5: 가중치(weights)보기 
summary(model_net)

# 단계 6: 분류모델의 적합값 보기 
model_net$fitted.values

# 단계 7: 분류모델의 예측치 생성과 분류 정확도
p <- predict(model_net, df, type = "class")
table(p, df$y)



# 실습: iris 데이터 셋을이용한 인공신경망 모델 생성
# 단계 1: 데이터 셋 생성
data(iris)
idx = sample(1:nrow(iris), 0.7 * nrow(iris))
training = iris[idx, ]
testing = iris[-idx, ]
nrow(training)
nrow(testing)

# 단계 2: 인공신경망 모델(은닉층 1개와 은닉층 3개) 생성
model_net_iris1 = nnet(Species ~ ., training, size = 1)
model_net_iris1
model_net_iris3 = nnet(Species ~ ., training, size = 3)
model_net_iris3

# 단계 3: 가중치 네트워크 보기 - 은닉층 1개 신경망 모델 
summary(model_net_iris1)

# 단계 4:가중치 네트워크 보기 - 은닉층 3개 신경망 모델 
summary(model_net_iris3)


# 단계 5: 분류모델 평가 
table(predict(model_net_iris1, testing, type = "class"), testing$Species)

table(predict(model_net_iris3, testing, type = "class"), testing$Species)



# 실습: neuralnet 패키지를 이용한 인공신경망 모델 생성
# 단계 1: 패키지 설치 
install.packages("neuralnet")
library(neuralnet)

# 단계 2: 데이터 셋 생성
data("iris")
idx = sample(1:nrow(iris), 0.7 * nrow(iris))
training_iris = iris[idx, ]
testing_iris = iris[-idx, ]
dim(training_iris)
dim(testing_iris)

# 단계 3: 수치형으로 칼럼 생성
training_iris$Species2[training_iris$Species == 'setosa'] <- 1
training_iris$Species2[training_iris$Species == 'versicolor'] <- 2
training_iris$Species2[training_iris$Species == 'virginica'] <- 3

training_iris$Species <- NULL
head(training_iris)

testing_iris$Species2[testing_iris$Species == 'setosa'] <- 1
testing_iris$Species2[testing_iris$Species == 'versicolor'] <- 2
testing_iris$Species2[testing_iris$Species == 'virginica'] <- 3

testing_iris$Species <- NULL
head(testing_iris)


# 단계 4: 데이터 정규화
# 단계 4-1: 정규화 함수 정의 
normal <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# 단계 4-2: 정규화 함수를 이용하여 학습데이터와/검정데이터 정규화
training_nor <- as.data.frame(lapply(training_iris, normal))
summary(training_nor)


# 단계 5: 인공신경망 모델 생성 - 은닉 노드 1개
model_net = neuralnet(Species2 ~ Sepal.Length + Sepal.Width + 
                        Petal.Length + Petal.Width,
                      data = training_nor, hidden = 1)
model_net
plot(model_net)


# 단계 6: 분류모델 성능 평가
# 단계 6-1: 모델의 예측치 생성 - compute() 함수 이용 
model_result <- compute(model_net, testing_nor[c(1:4)])
model_result$net.result

# 단계 6-2: 상관관계 분석 - 상관계수로 두 변수 간 선형관계의 강도 측정
cor(model_result$net.result, testing_nor$Species2)

# 단계 7: 분류모델 성능 향상 - 은닉층 노드 2개 지정, backprop 속성 적용
# 단계 7-1: 인공신경망 모델 생성
model_net2 = neuralnet(Species2 ~ Sepal.Length + Sepal.Width +
                         Petal.Length + Petal.Width, 
                       data = training_nor, hidden = 2, 
                       algorithm = "backprop", learningrate = 0.01)

# 단계 7-2: 분류모델 예측치 생성과 평가 
model_result <- compute(model_net, testing_nor[c(1:4)])
cor(model_result$net.result, testing_nor$Species2)


