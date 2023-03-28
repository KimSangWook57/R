# 12장 연습문제
# 1. 교육수준과 흡연률 간의 관련성을 분석하시오.
# 독립 - 교육수준, 종속 - 흡연률

smoke <- read.csv("./data/smoke.csv", header = T)
head(smoke)

# 전처리 과정
smoke$education2[smoke$education==1] <- "1. 대졸"
smoke$education2[smoke$education==2] <- "2. 고졸"
smoke$education2[smoke$education==3] <- "3. 중졸"

smoke$smoking2[smoke$smoking==1] <- "1. 과대흡연"
smoke$smoking2[smoke$smoking==2] <- "2. 보통흡연"
smoke$smoking2[smoke$smoking==3] <- "3. 비흡연"

table(smoke$education2, smoke$smoking2)

# 라이브러리 가져오기
# packages에서 검색해서 클릭해도 적용 가능
library(gmodels)

CrossTable(smoke$education2, smoke$smoking2, chisq = T)

# p =  0.0008182573 이므로, 가설이 아주 잘 맞다고 볼 수 있다.

# 2. 나이와 직위 간의 관련성을 단계별로 분석하시오.

# data 변수는 가급적 쓰지 말것.
# euc-kr = cp949 = ko-KR -> euc-KR -> unicode  
data <- read.csv("./data/cleanData.csv", fileEncoding = "euc-kr")
str(data)

x <- data$position
y <- data$age3

plot(x, y, abline(lm(y~x)))

CrossTable(x, y, chisq = T)

# p =  1.548058e-57 는 매우 자명하다는 뜻이지만, 
# d.f. = 8이므로, 자유도가 높음에 주의해야 한다. (조금씩 다를 확률이 높다.)

# 3. 직업 유형에 따른 응답 정도에 차이가 있는가를 단계별로 검정하시오.
# 동질성 검정 <- 이 답변이 믿을 만 한가?
result <- read.csv("./data/response.csv", header = T)
str(result)

result$job2[result$job==1] <- "1. 학생"
result$job2[result$job==2] <- "2. 직장인"
result$job2[result$job==3] <- "3. 주부"

result$response2[result$response==1] <- "1. 무응답"
result$response2[result$response==2] <- "2. 낮음"
result$response2[result$response==3] <- "3. 높음"

table(result$job2, result$response2)

# 집단 동질성(올바른 응답인가?)이 의심스러움.
# 그러므로 카이제곱식을 이용해 검증해 보았다.
chisq.test(result$job2, result$response2)
# p-value = 6.901e-12이고 d.f. = 4이므로, 일단 믿어 보자.







