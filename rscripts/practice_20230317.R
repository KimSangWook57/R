# EDA/시각화 과제(연습문제)



# 7장 연습문제.

# 2. dataset2의 resident 칼럼을 대상으로 NA 값을 제거한 후 dataset2 변수에 저장하시오.

# dataset2를 na가 아닌 resident 항목만 다시 갖도록 구성했다.
dataset2 <- subset(dataset2, !is.na(dataset2$resident))
dataset2
 
# 3. dataset2의 gender 칼럼을 대상으로 1->"남자", 2->"여자" 형태로 코딩 변경하여 gender2 칼럼에 추가하고,
# 파이 차트로 결과를 확인하시오. 
dataset2$gender2[dataset2$gender == 1] <- "남자"
dataset2$gender2[dataset2$gender == 2] <- "여자"
# 테이블을 이용한 파이 차트 만들기.
pie(table(dataset2$gender2))

# 4. 나이를 30세 이하 -> 1, 31~55 -> 2, 56이상 -> 3 으로 리코딩하여 
# age3 칼럼에 추가한 후 age, age2, age3 칼럼만 확인하시오. 
dataset2$age3[dataset2$age <= 30] <- 1
dataset2$age3[dataset2$age > 30 & dataset2$age <= 55] <- 2
dataset2$age3[dataset2$age >= 56] <- 3
dataset2[c("age", "age2", "age3")]



# 8장 연습문제.
# 01. 다음 조건에 맞게 quakes 데이터 셋의 수심(depth)과 리히터규모(mag)가 동일한 패널에
# 지진의 발생지를 산점도로 시각화하시오.

# 데이터 확인.
head(quakes)

# 조건1) 수심 3개 영역으로 범주화.
# 수심(depth)의 범주 확인.
range(quakes$depth)
# equal.count(수심, 3개 영역, overlap = 0)를 사용하여 범주화.
# number랑 overlap을 생략해도 될까?
depthgroup <- equal.count(quakes$depth, number = 3, overlap = 0)

# 조건2) 리히터규모 2개 영역으로 범주화.
# 리히터규모(mag)의 범주 확인.
range(quakes$mag)
# equal.count(리히터규모, 2개 영역, overlap = 0)를 사용하여 범주화.
magnitudegroup <- equal.count(quakes$mag, number = 2, overlap = 0)

# 조건3) 수심과 리히터규모가 3행 2열 구조의 패널로 산점도 그래프 그리기.
# xyplot(y축 ~ x축 | 열 * 행, data=데이터, main="제목", ylab="y축이름", xlab="x축이름", 
# pch="포인트(@로 표시)", col=c("첫번째컬러", "두번째컬러"))
xyplot(lat ~ long | magnitudegroup * depthgroup, data = quakes, main = "Fiji Earthquakes", 
       ylab = "latitude", xlab = "longitude", pch = "@", col = c("red","blue"))

# 02. latticeExtra패키지에서 제공되는 SeatacWeather 데이터 셋에서 월 별로 
# 최저기온과 최고기온을 선 그래프로 플로팅 하시오. 
# 힌트) lattice 패키지의 xyplot() 함수 이용
# 힌트) 선 그래프 : type="l"

# install.packages("latticeExtra")
# library(latticeExtra)

# 데이터 확인.
head(SeatacWeather)
# xyplot(y1축 + y2축 ~ x축 | 조건(월), data=데이터, main="제목", type="l", 
# ylab = "y축이름", xlab = "x축이름", col = c("첫번째컬러", "두번째컬러"), layout = c(열, 행))
xyplot(min.temp + max.temp ~ day | month, data = SeatacWeather, main = "temperature",
       type="l", ylab = "minmaxtemp", xlab = "days", col = c("red","blue"), layout = c(3,1)) 
