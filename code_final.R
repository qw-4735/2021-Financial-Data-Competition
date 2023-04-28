rm(list=ls())
gc(reset = T)
setwd("C:/Users/landw/Desktop")
library(tidyverse)
data <- read.csv('2021금융데이터경진대회전체조인파일0829.csv')
data2 <- data %>% group_by(법정동리명,기준년도,기준년월,마케팅업종세세분류명, 성별구분,연령대,순위) %>%
  mutate(매출건수=sum(매출건수), 회원수 = sum(회원수)) %>%
  select(-c('체크카드보유여부','신용카드보유여부'))
data3 <- data2 %>% distinct(기준년도,기준년월,법정동리명,한글시군구명.x, 소비자물가지수,한글시도명.x,마케팅업종중분류명, 마케팅업종세분류명,마케팅업종세세분류명,성별구분, 연령대,회원수,매출건수,순위,미세먼지,초미세먼지, price,지하철역하차,학교,역,대학교,세대수,유동10대, 유동20대,유동30대,유동40대,유동50대,유동60대, 유동총인구수,주거10대,주거20대,주거30대,주거40대, 주거50대,주거60대이상,주거총인구)
#write.csv(data3,"조인2.csv",row.names = F)
rm(data,data2,data3)
data3 <- read.csv("조인2.csv",stringsAsFactors = T)
data_seoul <- data3 %>% filter(한글시도명.x=="서울특별시")
data_gg <- data3 %>% filter(한글시도명.x=="경기도")
data_gg <- data_gg %>% select(-c(23:29))
data_seoul$성별구분 <- substr(data_seoul$성별구분,4,5)
data_seoul$연령대 <- substr(data_seoul$연령대,3,4)
data_gg$성별구분 <- substr(data_gg$성별구분,4,5)
data_gg$연령대 <- substr(data_gg$연령대,3,4)

##시간별
library(zoo)
data_seoul$date <- paste0(data_seoul$기준년도,"-",data_seoul$기준년월)
data_seoul$date <- as.Date(as.yearmon(data_seoul$date))
data_gg$date <- paste0(data_gg$기준년도,"-",data_gg$기준년월)
data_gg$date <- as.Date(as.yearmon(data_gg$date))
data3$date <- paste0(data3$기준년도,"-",data3$기준년월)
data3$date <- as.Date(as.yearmon(data3$date))


########서울
#부스팅
library(xgboost)
head(data_seoul)
train_x = data.matrix(data_seoul[,c(5,9,10,11,15,16,17,19,20,21,22,24:28,30:34)])
train_y = seoul[,13]
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgbc = xgboost(data = xgb_train, max.depth = 7, nrounds = 20)
xgb.importance(model = xgbc)

##소비자물가지수와 매출건수가 연관있나?
data_seoul %>%
  ggplot()+
  geom_jitter(aes(x=소비자물가지수,y=매출건수,color=한글시군구명.x),width = 0.06)

#소비자물가지수에 따라서 매출건수가 다르다 가설검정?
model <- lm(매출건수~소비자물가지수,data=data_seoul)
summary(model)

##지역의 집값과 매출건수가 연관있나?
data_seoul %>%
  ggplot()+
  geom_jitter(aes(x=price,y=매출건수),width = 10)

#집값에 따라서 매출건수가 다르다 가설검정?
model <- lm(매출건수~price,data=data_seoul)
summary(model)

##매출건수 변화추이
data_seoul %>% mutate(코로나=ifelse(date <="2020-01-01","이전","이후")) %>%
  group_by(코로나) %>% summarise(매출건수=mean(매출건수)) %>%
  ggplot()+
  geom_bar(aes(x=코로나,y=매출건수,fill=코로나),width=0.5,stat='identity')

#강남구 마포구 모두 코로나 이후로 장삭가 잘되는 집의 매출이 줄어들고
#마포구는 4500을 넘어가는 식당이 아예 없어짐

#######서울 전체적인 매출건수 높은 음식점
dd <- data_seoul %>% group_by(마케팅업종세세분류명) %>%
  summarise(매출건수=mean(매출건수)) %>% arrange(desc(매출건수)) %>% 
  dplyr::slice(1:10) %>% select(1)
dd2 <- data_seoul %>% group_by(마케팅업종세세분류명) %>%
  summarise(매출건수=mean(매출건수)) %>% arrange(desc(매출건수)) %>% 
  dplyr::slice(1:10) %>% arrange(매출건수) %>% select(1)
dd3 <- tibble()
for(i in 1:10){
  dd3 <- rbind(dd3,data_seoul %>%
                 filter(마케팅업종세세분류명==dd$마케팅업종세세분류명[i]))
}
dd3 %>% ggplot()+
  geom_boxplot(aes(x=마케팅업종세세분류명,y=매출건수,color=마케팅업종세세분류명))+
  scale_x_discrete(limits=c(dd2$마케팅업종세세분류명))+
  coord_flip()


################
#서울
data_seoul %>% filter(한글시군구명.x=="강남구") %>%
  ggplot()+
  geom_line(aes(x=date,y=유동10대,color=법정동리명))
data_seoul %>% filter(한글시군구명.x=="마포구") %>%
  ggplot()+
  geom_line(aes(x=date,y=유동10대,color=법정동리명))
data_seoul %>% filter(한글시군구명.x=="강남구") %>%
  ggplot()+
  geom_line(aes(x=date,y=유동총인구수,color=법정동리명))
data_seoul %>% filter(한글시군구명.x=="마포구") %>%
  ggplot()+
  geom_line(aes(x=date,y=유동총인구수,color=법정동리명))

#코로나
data_seoul %>% group_by(date,법정동리명) %>% summarise(매출건수=mean(매출건수)) %>%
  mutate(TF=ifelse(date=="2020-01-01" & 매출건수>250,1,0)) %>%
  ggplot(aes(x=date,y=매출건수))+
  geom_line(aes(color=법정동리명,size=TF))+
  guides(size = FALSE)

##프로토타입
male_20 <- data_seoul %>% filter(성별구분=="01.남성" & 연령대=="1.20대이하") %>%
  group_by(마케팅업종세세분류명) %>% summarise(매출건수=mean(매출건수)) %>%
  arrange(desc(매출건수)) %>% dplyr::slice(1:10) %>% select(1)
male_20_b <- data_seoul %>% filter(성별구분=="01.남성" & 연령대=="1.20대이하") %>%
  group_by(마케팅업종세세분류명) %>% summarise(매출건수=mean(매출건수)) %>%
  arrange(desc(매출건수)) %>% dplyr::slice(1:10) %>% arrange(매출건수) %>% select(1)
male_20_data <- tibble()
for(i in 1:10){ male_20_data <- rbind(male_20_data,data_seoul %>% filter(성별구분=="01.남성" &
                                                                               연령대=="1.20대이하") %>%
                                        filter(마케팅업종세세분류명==male_20$마케팅업종세세분류명[i]))
}
male_20_data %>% ggplot()+
  geom_boxplot(aes(x=마케팅업종세세분류명,y=매출건수,color=마케팅업종세세분류명))+
  scale_x_discrete(limits=c(male_20_b$마케팅업종세세분류명))+
  coord_flip()

##자동화
for(gender in unique(data_seoul$성별구분)){
  for(age in unique(data_seoul$연령대)){ temp <- data_seoul %>% filter(성별구분==gender & 연령대==age) %>%
    group_by(마케팅업종세세분류명) %>% summarise(매출건수=mean(매출건수)) %>%
    arrange(desc(매출건수)) %>% dplyr::slice(1:10) %>% select(1) temp2 <- data_seoul %>% filter(성별구분==gender & 연령대==age) %>%
      group_by(마케팅업종세세분류명) %>% summarise(매출건수=mean(매출건수)) %>%
      arrange(desc(매출건수)) %>% dplyr::slice(1:10) %>% arrange(매출건수) %>% select(1) temp_data <- tibble() for(i in 1:10){ temp_data <- rbind(temp_data, data_seoul %>% filter(성별구분==gender &
                                                                                                                                                                                 연령대==age) %>%
                                                                                                                                            filter(마케팅업종세세분류명==temp$마케팅업종세세분류명[i])) } assign(paste0(ifelse(gender=="남성","male","female"),'_',age), temp_data %>% ggplot()+
                                                                                                                                                                                               geom_boxplot(aes(x=마케팅업종세세분류명,y=매출건수,color=마케팅업종세세분류명))+
                                                                                                                                                                                               scale_x_discrete(limits=c(temp2$마케팅업종세세분류명))+
                                                                                                                                                                                               coord_flip()+
                                                                                                                                                                                               ggtitle(paste0(gender," ",age))}}
male_20 
male_30
male_40
male_50
male_60
female_20
female_30
female_40
female_50
female_60

#성별 세대별 외식 회수
data_seoul %>% group_by(성별구분,연령대) %>%
  summarise(매출건수=mean(매출건수)) %>% 
  mutate(성별연령=paste0(성별구분,"_",연령대)) %>%
  ggplot()+ geom_bar(aes(x=성별연령,y=매출건수,fill=매출건수),stat="identity")

male_20 
male_30
female_20

##파일생성
for(dong in unique(data_seoul$법정동리명)){ temp <- data_seoul %>% filter(법정동리명==dong) %>%
  group_by(마케팅업종세세분류명) %>% summarise(매출건수=mean(매출건수)) %>%
  arrange(desc(매출건수)) %>% dplyr::slice(1:10) %>% select(1) temp2 <- data_seoul %>% filter(법정동리명==dong) %>%
    group_by(마케팅업종세세분류명) %>% summarise(매출건수=mean(매출건수)) %>%
    arrange(desc(매출건수)) %>% dplyr::slice(1:10) %>% arrange(매출건수) %>% select(1) temp_data <- tibble() for(i in 1:10){ temp_data <- rbind(temp_data,data_seoul %>% filter(법정동리명==dong) %>%
                                                                                                                                          filter(마케팅업종세세분류명==temp$마케팅업종세세분류명[i])) } assign(paste0(dong,"_plot"), temp_data %>% ggplot()+
                                                                                                                                                                                             geom_boxplot(aes(x=마케팅업종세세분류명,y=매출건수,color=마케팅업종세세분류명))+
                                                                                                                                                                                             scale_x_discrete(limits=c(temp2$마케팅업종세세분류명))+
                                                                                                                                                                                             coord_flip()+
                                                                                                                                                                                             ggtitle(paste0(dong," 매출건수")) )
}

#함수화
dongplot <- function(dong){ temp <- data_seoul %>% filter(법정동리명==dong) %>%
  group_by(마케팅업종세세분류명) %>% summarise(매출건수=mean(매출건수)) %>%
  arrange(desc(매출건수)) %>% dplyr::slice(1:10) %>% select(1) temp2 <- data_seoul %>% filter(법정동리명==dong) %>%
    group_by(마케팅업종세세분류명) %>% summarise(매출건수=mean(매출건수)) %>%
    arrange(desc(매출건수)) %>% dplyr::slice(1:10) %>% arrange(매출건수) %>% select(1) temp_data <- tibble() for(i in 1:10){ temp_data <- rbind(temp_data,data_seoul %>% filter(법정동리명==dong) %>%
                                                                                                                                          filter(마케팅업종세세분류명==temp$마케팅업종세세분류명[i])) } return(temp_data %>% ggplot()+
                                                                                                                                                                                             geom_boxplot(aes(x=마케팅업종세세분류명,y=매출건수,color=마케팅업종세세분류명))+
                                                                                                                                                                                             scale_x_discrete(limits=c(temp2$마케팅업종세세분류명))+
                                                                                                                                                                                             coord_flip()+
                                                                                                                                                                                             ggtitle(paste0(dong," 매출건수")) )
}
dongplot("공덕동")

#일반한식 매출이 중간값보다 많은 식당은 어느 동에 주로 위치?
seoul_hansik <- data_seoul %>% filter(마케팅업종세세분류명=="일반한식/백반" & 
                                                  매출건수 >= median(매출건수)) %>%
  group_by(한글시군구명.x,법정동리명) %>% summarise(매출건수합=sum(매출건수)) %>%
  arrange(desc(매출건수합))
seoul_hansik

#30대남성 가중치
data_seoul %>% filter(연령대=='30') %>% group_by(성별구분) %>%
  summarise(매출건수=sum(매출건수)) %>% mutate(비율=매출건수/sum(매출건수))

#0. 일반한식의 20대 비율
hansik <- tibble()
for(gender in unique(data_seoul$성별구분)){
  for(age in unique(data_seoul$연령대)){ temp <- data_seoul %>% filter(성별구분==gender & 연령대==age) %>%
    filter(마케팅업종세세분류명=="일반한식/백반") %>% summarise(매출건수합=sum(매출건수)) temp <- tibble(gender,age,temp) hansik <- rbind(hansik,assign(paste0(gender,'_',age,'_hansik'),temp)) %>%
      arrange(desc(매출건수합))
  }}
hansik

#1.일반한식 매출건수/20대남녀30대남 유동인구 가 낮은 지역
hansik_bydong <- data_seoul %>% filter(마케팅업종세세분류명=="일반한식/백반") %>% 
  group_by(한글시군구명.x,법정동리명) %>% summarise(매출건수합=sum(매출건수), 유동=mean(유동20대+유동30대*0.608)) %>%
  mutate(인구대비매출=매출건수합/유동) %>% arrange(인구대비매출)

#2.20대 인구가 어느정도 있는 곳에서 20대 인구 대비 매출이 낮은 지역
hansik_bydong %>% filter(유동 >= mean(유동))

#아현동 동교동 공덕동은 유동인구에 비해 매출건수가 낮은편

#함수화
#1.일반한식 매출건수/20대남녀30대남 유동인구 가 낮은 지역
food <- function(food){
  data_seoul %>% filter(마케팅업종세세분류명==food) %>% 
    group_by(한글시군구명.x,법정동리명) %>% summarise(매출건수합=sum(매출건수), 유동=mean(유동20대+유동30대*0.608)) %>%
    mutate(인구대비매출=매출건수합/유동) %>% arrange(인구대비매출) %>%
    filter(유동 >= mean(유동))
}
food('일식전문점')
food('종합양식')
unique(data_seoul$마케팅업종세세분류명)
food('일반한식/백반')
dongplot('동교동')

########경기도
#부스팅
library(xgboost)
head(data_gg)
train_x = data.matrix(data_gg[,c(5,9,10,11,15:21,23:29)])
train_y = data_gg[,13]
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgbc = xgboost(data = xgb_train, max.depth = 7, nrounds = 20)
xgb.importance(model = xgbc)

##소비자물가지수와 매출건수가 연관있나?
data_gg %>%
  ggplot()+
  geom_jitter(aes(x=소비자물가지수,y=매출건수,color=한글시군구명.x),width = 0.06)

#소비자물가지수에 따라서 매출건수가 다르다 가설검정?
model <- lm(매출건수~소비자물가지수,data=data_gg)
summary(model)

##지역의 집값과 매출건수가 연관있나?
data_gg %>%
  ggplot()+
  geom_jitter(aes(x=price,y=매출건수),width = 10)

#집값에 따라서 매출건수가 다르다 가설검정?
model <- lm(매출건수~price,data=data_gg)
summary(model)

##매출건수 변화추이
data_gg %>% mutate(코로나=ifelse(date <="2020-01-01","이전","이후")) %>%
  group_by(코로나) %>% summarise(매출건수=mean(매출건수)) %>%
  ggplot()+
  geom_bar(aes(x=코로나,y=매출건수,fill=코로나),width=0.5,stat='identity')+
  ggtitle("코로나 이전과 이후 매출건수 비교")

#강남구 마포구 모두 코로나 이후로 장삭가 잘되는 집의 매출이 줄어들고
#마포구는 4500을 넘어가는 식당이 아예 없어짐

#코로나
data_gg %>% group_by(date,법정동리명) %>% summarise(매출건수=mean(매출건수)) %>%
  mutate(TF=ifelse(date=="2020-01-01" & 매출건수>150,1,0)) %>%
  ggplot(aes(x=date,y=매출건수))+
  geom_line(aes(color=법정동리명,size=TF))+
  guides(size = FALSE)

#######분당구 전체적인 매출건수 높은 음식점
dd <- data_gg %>% group_by(마케팅업종세세분류명) %>%
  summarise(매출건수=mean(매출건수)) %>% arrange(desc(매출건수)) %>% 
  dplyr::slice(1:10) %>% select(1)
dd2 <- data_gg %>% group_by(마케팅업종세세분류명) %>%
  summarise(매출건수=mean(매출건수)) %>% arrange(desc(매출건수)) %>% 
  dplyr::slice(1:10) %>% arrange(매출건수) %>% select(1)
dd3 <- tibble()
for(i in 1:10){ dd3 <- rbind(dd3,data_gg %>%
                               filter(마케팅업종세세분류명==dd$마케팅업종세세분류명[i]))
}
dd3 %>% ggplot()+
  geom_boxplot(aes(x=마케팅업종세세분류명,y=매출건수,color=마케팅업종세세분류명))+
  scale_x_discrete(limits=c(dd2$마케팅업종세세분류명))+
  coord_flip()


################
##자동화
for(gender in unique(data_gg$성별구분)){ 
  for(age in unique(data_gg$연령대)){ 
    temp <- data_gg %>% filter(성별구분==gender &
                                     group_by(마케팅업종세세분류명) %>% summarise(매출건수=mean(매출건수)) %>%
                                     arrange(desc(매출건수)) %>% dplyr::slice(1:10) %>% select(1) temp2 <- data_gg %>% filter(성별구분==gender & 연령대==age) %>%
                                     group_by(마케팅업종세세분류명) %>% summarise(매출건수=mean(매출건수)) %>%
                                     arrange(desc(매출건수)) %>% dplyr::slice(1:10) %>% arrange(매출건수) %>% select(1) temp_data <- tibble() for(i in 1:10){ temp_data <- rbind(temp_data, data_gg %>% filter(성별구분==gender &
                                                                                                                                                                                                             연령대==age) %>%
                                                                                                                                                                           filter(마케팅업종세세분류명==temp$마케팅업종세세분류명[i])) } assign(paste0(ifelse(gender=="남성","male","female"),'_',age), temp_data %>% ggplot()+
                                                                                                                                                                                                                              geom_boxplot(aes(x=마케팅업종세세분류명,y=매출건수,color=마케팅업종세세분류명))+
                                                                                                                                                                                                                              scale_x_discrete(limits=c(temp2$마케팅업종세세분류명))+
                                                                                                                                                                                                                              coord_flip()+
                                                                                                                                                                                                                              ggtitle(paste0(gender," ",age)) ) }}
male_20 
male_30
male_40
male_50
male_60
female_20
female_30
female_40
female_50
female_60

#성별 세대별 외식 회수
data_gg %>% group_by(성별구분,연령대) %>%
  summarise(매출건수=mean(매출건수)) %>% 
  mutate(성별연령=paste0(성별구분,"_",연령대)) %>%
  ggplot()+ geom_bar(aes(x=성별연령,y=매출건수,fill=매출건수),stat="identity")
male_20 
male_30
male_40

#함수화
dongplot <- function(dong){ temp <- data_gg %>% filter(법정동리명==dong) %>%
  group_by(마케팅업종세세분류명) %>% summarise(매출건수=mean(매출건수)) %>%
  arrange(desc(매출건수)) %>% dplyr::slice(1:10) %>% select(1) temp2 <- data_gg %>% filter(법정동리명==dong) %>%
    group_by(마케팅업종세세분류명) %>% summarise(매출건수=mean(매출건수)) %>%
    arrange(desc(매출건수)) %>% dplyr::slice(1:10) %>% arrange(매출건수) %>% select(1) temp_data <- tibble() for(i in 1:10){ temp_data <- rbind(temp_data,data_gg %>% filter(법정동리명==dong) %>%
                                                                                                                                          filter(마케팅업종세세분류명==temp$마케팅업종세세분류명[i])) } return(temp_data %>% ggplot()+
                                                                                                                                                                                             geom_boxplot(aes(x=마케팅업종세세분류명,y=매출건수,color=마케팅업종세세분류명))+
                                                                                                                                                                                             scale_x_discrete(limits=c(temp2$마케팅업종세세분류명))+
                                                                                                                                                                                             coord_flip()+
                                                                                                                                                                                             ggtitle(paste0(dong," 매출건수")) )
}
dongplot("정자동")
data_gg$법정동리명 %>% unique()

#일반한식 매출이 중간값보다 많은 식당은 어느 동에 주로 위치?
gg_hansik <- data_gg %>% filter(마케팅업종세세분류명=="일반한식/백반" & 
                                            매출건수 >= median(매출건수)) %>%
  group_by(한글시군구명.x,법정동리명) %>% summarise(매출건수합=sum(매출건수)) %>%
  arrange(desc(매출건수합))
gg_hansik