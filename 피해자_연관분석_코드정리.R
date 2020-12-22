## package 설치
install.packages("dplyr")
install.packages("arules")
install.packages("arulesViz")
install.packages("RColorBrewer")

## library 불러오기
library(dplyr)
library(arules)
library(arulesViz)
library(RColorBrewer)

# 원본 데이터 불러오기
data <- read.csv("C:\\Users\\alswh\\Desktop\\minjong\\2020년 4학년 1학기\\2020데이터청년캠퍼스\\팀프로젝트\\가해자차종 또는 피해자차종이 이륜차인 교통사고 정보(2015~2019년).csv")

# 원본 데이터 불러오기 (첨부 파일중 "가해자차종 또는 피해자차종이 이륜차인 교통사고 정보(2015~2019년).csv" 파일 선택)
data <- read.csv(file.choose())
## 미리보기
data %>% head()

# 필요변수 선택
data.use <- select(data, 요일, 가해자법규위반, 노면상태, 기상상태, 가해당사자종별, 가해자신체상해정도, 가해자보호장구, 피해당사자종별, 피해자신체상해정도, 피해자보호장구)

# 신체상해정도(중상, 경상, 부상신고)를 부상으로 바꿔주기 => (사망, 부상, 상해없음)
data.use$가해자신체상해정도 <- ifelse(data.use$가해자신체상해정도=="중상"|data.use$가해자신체상해정도=="경상"|data.use$가해자신체상해정도=="부상신고", "부상", data.use$가해자신체상해정도)
data.use$가해자신체상해정도 %>% table()

data.use$피해자신체상해정도 <- ifelse(data.use$피해자신체상해정도=="중상"|data.use$피해자신체상해정도=="경상"|data.use$피해자신체상해정도=="부상신고", "부상", data.use$피해자신체상해정도)
data.use$피해자신체상해정도 %>% table()

# 신체상해정도가 "기타불명"인 case 제거
data.use <- filter(data.use, data.use$가해자신체상해정도!="기타불명"&data.use$피해자신체상해정도!="기타불명")

# 변수들 factor로 바꿔주기
data.use$요일 <- data.use$요일 %>% as.factor()
data.use$가해자법규위반 <- data.use$가해자법규위반 %>% as.factor()
data.use$노면상태 <- data.use$노면상태 %>% as.factor()
data.use$기상상태 <- data.use$기상상태 %>% as.factor()
data.use$가해당사자종별 <- data.use$가해당사자종별 %>% as.factor()
data.use$가해자신체상해정도 <- data.use$가해자신체상해정도 %>% as.factor()
data.use$가해자보호장구 <- data.use$가해자보호장구 %>% as.factor()
data.use$피해당사자종별 <- data.use$피해당사자종별 %>% as.factor()
data.use$피해자신체상해정도 <- data.use$피해자신체상해정도 %>% as.factor()
data.use$피해자보호장구 <- data.use$피해자보호장구 %>% as.factor()

######################################################
# 피해자 데이터 전처리 
######################################################

# 피해자당사자종별이 "이륜차"인 데이터 뽑기
data.use.del <- filter(data.use, data.use$피해당사자종별 == "이륜차")

# 피해자신체상해정도가 "없음"인 경우 제외하기
data.use.del <- filter(data.use.del, data.use.del$피해자신체상해정도 != "없음")

# 결과변수 타입을 character로 바꾼후 factor로 변환
data.use.del$피해자신체상해정도 <- data.use.del$피해자신체상해정도 %>% as.character()
data.use.del$피해자신체상해정도 <- data.use.del$피해자신체상해정도 %>% as.factor()
data.use.del$피해자신체상해정도 %>% str()
data.use.del$피해자신체상해정도 %>% table()

# 사용 할 변수만 뽑기("피해당사자종별"을 제거 => 이미 피해자가 이륜차인 데이터만 뽑음)
data.apri <- select(data.use.del, 가해자신체상해정도, 피해자보호장구, 가해자법규위반, 요일, 가해당사자종별, 기상상태, 노면상태, 가해자보호장구, 피해자신체상해정도)

# 피해자보호장구가 "착용" or "미착용"인 데이터(연관분석 돌릴 데이터)
data.apri <- filter(data.apri, data.apri$피해자보호장구=='착용'|data.apri$피해자보호장구=='미착용')

# 피해자보호장구 변수 타입을 character로 바꾼후 factor로 변환
data.apri$피해자보호장구 <- data.apri$피해자보호장구 %>% as.character()
data.apri$피해자보호장구 <- data.apri$피해자보호장구 %>% as.factor()
data.apri$피해자보호장구 %>% str()
data.apri$피해자보호장구 %>% table()

# 연관규칙 만들기 (lhs => 피해자신체상해정도, rhs => 피해자보호장구)
rules <- apriori(data.apri, control = list(verbose = T), parameter = list(minlen = 2, maxlen = 2,supp = 0.001, conf = 0.001),
                 appearance = list(rhs = c("피해자신체상해정도=사망","피해자신체상해정도=부상","피해자신체상해정도=상해없음"),
                                   lhs = c("피해자보호장구=착용","피해자보호장구=미착용")))
# 향상도(lift) 기준으로 정렬
rules.sort <- sort(rules, by = "lift")

# 정렬된 규칙확인
inspect(rules.sort)

# 연관규칙 시각화로 한눈에 보기
plot(rules.sort, method = "matrix", control = list(type = "items",col=brewer.pal(11,"Spectral")), engine = "htmlwidget")
