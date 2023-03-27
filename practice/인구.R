install.packages("tidyverse")
install.packages("ggplot2")

library(tidyverse)
library(readxl)

df <- read_excel("./practice/시군구_성_월별_출생_19972021.xlsx")

View(df)

# 원 데이터는 건들면 안된다.
df2 <- df %>% 
  filter(!is.na(시점)) %>%
  select(시점, 전국) %>%
  separate(시점, into = c("년도", "월"))

# 데이터 분석 기법 디폴트.
df2 <- df2 %>% 
  group_by(월) %>%
  summarise(평균출생수 = mean(전국))

# 시각화
df2 %>% 
  qplot(x = 월, y = 평균출생수, data=.)


