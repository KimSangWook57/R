# ch07

# 작업 폴더 확인.
getwd()
# 데이터 끌고 오기.
dataset <- read.csv("./data/dataset.csv", header = T)
dataset

print(dataset)
# 데이터 보기 형식 추천.
View(dataset)
# 머리, 꼬리 데이터 확인.
head(dataset)
tail(dataset)
# 데이터 변수명 확인.
names(dataset)
# 모든 속성값 확인.
attributes(dataset)

# 데이터 자료형 확인.
str(dataset)

# age 컬럼으로 접근.
dataset$age

# 변수 저장하기.
x <- dataset$age

# 데이터 값 그래프로.
plot(dataset$price)

# [1:300] 형식 = 배열
# 데이터["변수명"] 형식 = 딕셔너리(맵) = 키:밸류
# R은 배열을 1부터 센다. 그리고 키값으로만 조회한다.
dataset[1]

# 2개 이상의 컬럼 조회.
dataset[c("job", "price")]

# 결측치 확인.
# summary()에서 NA가 몇개인지 확인한다.
summary(dataset)

# 기존 코드는 계산 불가.
sum(dataset$price)
# 결측치 제거.
# na.rm = T == 'NA를 지워라.'
sum(dataset$price, na.rm = T)

# 미리 결측치를 제거해서 집어넣는다.
price2 <- na.omit(dataset$price)

# 결측치를 지우지 말아야 할 때는 데이터가 너무 적을 때.
# 그렇다면 보관해야 한다. na를 0으로 했다가는 평균이 바뀌어 버린다.
# 그러므로 중간값을 집어넣어서 평균을 희석시킨다.

# 극단치 처리.
# 찾으면 지운다.







# 코딩 변경 전과 변경 후의 칼럼 보기 
dataset2[c("resident", "resident2")]

# 실습: 가독성을 위해 job 칼럼을 대상으로 코딩 변경하기
# boolean 매트릭스 => 이중 for 루프 구문.
dataset2$job2[dataset2$job == 1] <- '공무원'
dataset2$job2[dataset2$job == 2] <- '회사원'
dataset2$job2[dataset2$job == 3] <- '개인사업'

# 코딩 변경 전과 변경 후의 칼럼 보기 
dataset2[c("job", "job2")]

# 나이를 나타내는 age 칼럼을 대상으로 코딩 변경하기 
dataset2$age2[dataset2$age <= 30] <- "청년층"
dataset2$age2[dataset2$age > 30 & dataset2$age <= 55] <- "중년층"
dataset2$age2[dataset2$age > 55 ] <- "장년층"
head(dataset2)

