## package 설치
install.packages("caret")
install.packages("dplyr")
install.packages("randomForest")
install.packages("ggplot2")


## library 불러오기
library(caret)
library(dplyr)
library(randomForest)
library(ggplot2)


# 원본 데이터 불러오기
data <- read.csv("C:\\Users\\alswh\\Desktop\\minjong\\2020년 4학년 1학기\\2020데이터청년캠퍼스\\팀프로젝트\\가해자차종 또는 피해자차종이 이륜차인 교통사고 정보(2015~2019년).csv")

# 원본 데이터 불러오기 (첨부 파일 중 "가해자차종 또는 피해자차종이 이륜차인 교통사고 정보(2015~2019년).csv" 파일 선택)
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



# 범주형 변수들 factor로 바꿔주기
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
############################################################################################################
############################################################################################################
############## 전체 전처리 ################
############################################################################################################
############################################################################################################









############################################################################################################
############################################################################################################
# 가해자 데이터 전처리 
############################################################################################################
############################################################################################################

# 가해자당사자종별이 "이륜차"인 데이터 뽑기
data.use.ga <- filter(data.use, data.use$가해당사자종별 == "이륜차")


# 결과변수 각 카테고리별 데이터 셋 분리
dam_1 <- data.use.ga[data.use.ga$가해자신체상해정도=="사망",]
dam_2 <- data.use.ga[data.use.ga$가해자신체상해정도=="상해없음",]
dam_3 <- data.use.ga[data.use.ga$가해자신체상해정도=="부상",]


# (사망, 부상, 상해없음) oversample ("부상"카테고리의 수만큼 oversample) # 일정한 표본을 뽑기위해 set.seed로 설정(822)
set.seed(822)
dam_sam1 <- sample_n(dam_1 , 45066, replace = T) # 사망인 데이터 oversampling
dam_sam2 <- sample_n(dam_2 , 45066, replace = T) # 상해없음인 데이터 oversampling
dam_sam3 <- sample_n(dam_3 , 45066, replace = T) # 부상인 데이터 oversampling


# oversampling한 데이터 병합시키기
data.use.ga.bind <- rbind(dam_sam1, dam_sam2, dam_sam3)


# 가해자 데이터 전처리 -끝-


# randomforest를 이용한 가해자모델만들기  # 일정한 표본을 뽑기위해 set.seed로 설정(822)
set.seed(822)
fitRF_ga <- 
  randomForest(가해자신체상해정도~요일+가해자법규위반+노면상태+기상상태+가해자보호장구+
                          피해당사자종별+피해자신체상해정도+피해자보호장구, data = data.use.ga.bind, importance = TRUE, ntree = 300)


fitRF_ga #  oob estimate of error rate = 26.14%  accuracy : (1-oob rate) = 0.7386



# 가해자 그래프 그리기 설정
imp <- varImpPlot(fitRF_ga)
imp <- as.data.frame(imp)
imp$varnames <- rownames(imp) # row names to column
rownames(imp) <- NULL  
imp$var_categ <- c(1,1,1,1,2,1,1,1)


## 가해자 그래프 실행하기 ##
ggplot(imp, aes(x=reorder(varnames, MeanDecreaseAccuracy), weight=MeanDecreaseAccuracy)) +
  geom_bar()+
  geom_bar(data = subset(imp, var_categ=="2"), fill = "#ff0000")+
  scale_fill_discrete(name="Variable Group") +
  ylab("MeanDecreaseAccuracy")+
  xlab("")+
  coord_flip()+
  theme(axis.text.y = element_text(size = 20, face = "bold"))
############################################################################################################
#####################################################  - 가해자 분석 끝 -
############################################################################################################





############################################################################################################
############################################################################################################
# 피해자 데이터 전처리 
############################################################################################################
############################################################################################################


# 피해자당사자종별이 "이륜차"인 데이터 뽑기
data.use.pi <- filter(data.use, data.use$피해당사자종별 == "이륜차")


# 피해자신체상해정도가 "없음"인 경우 제거 (가해자가 혼자 사고 낸 경우이므로 피해자가 없음, 그리하여 피해자분석에서 제거)
data.use.pi <- filter(data.use.pi, data.use.pi$피해자신체상해정도 != "없음")

data.use.pi$피해자신체상해정도 %>% table()


# 결과변수의 타입을 character로 바꾼 후 factor로 변환
data.use.pi$피해자신체상해정도 <- data.use.pi$피해자신체상해정도 %>% as.character()
data.use.pi$피해자신체상해정도 <- data.use.pi$피해자신체상해정도 %>% as.factor()
data.use.pi %>% str()


# 결과변수 각 카테고리별 데이터 셋 분리
dam_1 <- data.use.pi[data.use.pi$피해자신체상해정도=="사망",]
dam_2 <- data.use.pi[data.use.pi$피해자신체상해정도=="상해없음",]
dam_3 <- data.use.pi[data.use.pi$피해자신체상해정도=="부상",]


# (사망, 부상, 상해없음) oversample ("부상"카테고리의 수만큼 oversample) # 일정한 표본을 뽑기위해 set.seed로 설정(822)
set.seed(822)
dam_sam1 <- sample_n(dam_1 , 45066, replace = T) # 사망인 데이터 oversampling
dam_sam2 <- sample_n(dam_2 , 45066, replace = T) # 상해없음인 데이터 oversampling
dam_sam3 <- sample_n(dam_3 , 45066, replace = T) # 부상인 데이터 oversampling


# oversampling한 데이터 병합시키기
data.use.pi.bind <- rbind(dam_sam1, dam_sam2, dam_sam3)

# 피해자 데이터 전처리 -끝-



# randomforest를 이용한 피해자모델만들기  # 재실행시 동일한 결과을 뽑기위해 set.seed로 설정(822)
set.seed(822)
fitRF_pi <- 
  randomForest(피해자신체상해정도~요일+가해자법규위반+노면상태+기상상태+가해당사자종별+가해자보호장구
                          +가해자신체상해정도+피해자보호장구, data = data.use.pi.bind, importance = TRUE, ntree = 300)


fitRF_pi # oob estimate of error rate = 29.37%, 정분류율 : (1- oob rate) = 0.7063


# 피해자 그래프 그리기 설정
imp <- varImpPlot(fitRF_pi)
imp <- as.data.frame(imp)
imp$varnames <- rownames(imp) # row names to column
rownames(imp) <- NULL  
imp$var_categ <- c(1,1,1,1,1,1,1,2)


## 피해자 그래프 실행하기 ## 
ggplot(imp, aes(x=reorder(varnames, MeanDecreaseAccuracy), weight=MeanDecreaseAccuracy)) +
  geom_bar()+
  geom_bar(data = subset(imp, var_categ=="2"), fill = "#ff0000")+
  scale_fill_discrete(name="Variable Group") +
  ylab("MeanDecreaseAccuracy")+
  xlab("")+
  coord_flip()+
  theme(axis.text.y = element_text(size = 25, face = "bold"))
############################################################################################################
#####################################################  - 피해자 분석 끝 -
############################################################################################################

