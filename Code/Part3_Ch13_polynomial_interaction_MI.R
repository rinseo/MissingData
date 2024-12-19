#===================================================================#
#제3부 MI기반 결측데이터 분석기법 
#제13장 다차항 및 상호작용효과항 
#===================================================================#
mypath = "D:/2020_Term2/Category_A_submission/MissingData/Rcode/data"
setwd(mypath)

#데이터 불러오기 
library(tidyverse)
library(mice)
library(miceadds)
#현실 데이터를 이용한 실습 
#제2부 제4장에서 사용한 변수들만 선택 (ML과의 비교) 
mydata = read_csv("anes2008_example.csv") %>% 
  select(FT_obama,female,ageyr,race3,educ,hhinc,readyBP,economy,environ,iraqwar)
#Transform then impute approach (Just another variable) 
#사전처리  
df_MI = mydata %>%
  rename_all(~(str_remove(.,"_"))) %>% #FT_의 _를 지움 
  mutate_at(
    vars(female,race3,readyBP),
    ~(factor(.))
  ) %>% 
  mutate_at(
    vars(ageyr,educ,hhinc,economy,environ,iraqwar),
    ~(. - mean(., na.rm=TRUE))  #평균중심화 변환 
  ) %>% 
  mutate_at(
    vars(ageyr,starts_with("FT")),
    ~(0.1*(.))       #ML에서 10을 나누어 주었음. 
  ) %>% 
  mutate(
    hhinc2=hhinc^2,
    Bhhinc=ifelse(race3==1,1,0)*hhinc,
    Ohhinc=ifelse(race3==2,1,0)*hhinc,
    Bhhinc2=ifelse(race3==1,1,0)*hhinc2,
    Ohhinc2=ifelse(race3==2,1,0)*hhinc2
  )
df_MI
#Imputing 단계 
myimp = mice(data=df_MI,m=20,print=FALSE,seed=1234)
#회귀모형 추정
regress_anes=with(myimp,
                  lm(FTobama~female+ageyr+educ+race3+hhinc+hhinc2+
                       readyBP+economy+environ+iraqwar))
pool_regress_polynomial=pool(regress_anes)
summary(pool_regress_polynomial,conf.int=TRUE) %>% as_tibble()

#시각화 
#1단계: hhinc 변수의 값들을 확보
var_hhinc=unique(sort(df_MI$hhinc[!is.na(df_MI$hhinc)]))
#2단계: 연속형 변수들(ageyr,educ,economy,envrion,iraqwar) 평균(즉 0)을 가정
#(더미변수들은 0을 가정)
#절편/계수들
mycoefs=summary(pool_regress_polynomial,conf.int=TRUE)
#변수들 평균구하기 
dlist_myimp=mids2datlist(myimp)
fun_Mean_Stats = function(myimpK){
  result=myimpK %>% 
    summarise_all(
      mean
    ) %>% 
    pivot_longer(cols=names(.),names_to="vars",values_to="mean") %>%
    data.frame() 
  rownames(result)=result$vars
  result = result %>% select(-vars)
  return(result)
}
myanalysis=with(dlist_myimp,
                fun=function(data){
                  data_cont = data %>% select(where(is.numeric))  #연속형 변수들만
                  data_cate = data %>% 
                    select(where(is.factor))  %>% #범주형 변수들만 
                    mutate(
                      white=ifelse(race3==0,1,0),
                      black=ifelse(race3==1,1,0),
                      others=ifelse(race3==2,1,0),
                      female=as.numeric(ifelse(female==1,1,0)),
                      readyBP=as.numeric(ifelse(readyBP==1,1,0))
                    ) %>% select(female,white,black,others,readyBP)
                  fun_Mean_Stats(bind_cols(data_cont,data_cate))
                })
myMeans=withPool_MI(myanalysis)
#3단계: 회귀방정식을 기반으로 오바마후보에 대한 감정점수 예측 
predy=var_hhinc*mycoefs$estimate[mycoefs$term=="hhinc"]+
  var_hhinc*var_hhinc*mycoefs$estimate[mycoefs$term=="hhinc2"]+
  mycoefs$estimate[mycoefs$term=="(Intercept)"]+
  0*mycoefs$estimate[mycoefs$term=="female1"]+
  myMeans["ageyr",]*mycoefs$estimate[mycoefs$term=="ageyr"]+
  myMeans["educ",]*mycoefs$estimate[mycoefs$term=="educ"]+
  0*mycoefs$estimate[mycoefs$term=="race31"]+
  0*mycoefs$estimate[mycoefs$term=="race32"]+
  0*mycoefs$estimate[mycoefs$term=="readyBP1"]+
  myMeans["economy",]*mycoefs$estimate[mycoefs$term=="economy"]+
  myMeans["environ",]*mycoefs$estimate[mycoefs$term=="environ"]+
  myMeans["iraqwar",]*mycoefs$estimate[mycoefs$term=="iraqwar"]
#4단계: 원래 측정단위로 환산후 데이터 형태로
df_pred=tibble(FT_obama=10*predy,hhinc=(var_hhinc+mean(mydata$hhinc,na.rm=TRUE)))  
#5단계: 시각화 
df_pred %>% 
  ggplot(aes(x=hhinc,y=FT_obama))+
  geom_line()+
  geom_point(size=4)+
  labs(x="Household income level",y="Feeling thermometer for B. Obama")+
  scale_x_continuous(breaks=1:25)+
  scale_y_continuous(breaks=43:51)+
  coord_cartesian(ylim=c(43,51))+
  theme_bw()
ggsave("Curvilinear_hhinc_FTobama_meancenter_MI.png",width=22,height=17,units='cm')

#2차항까지 포함하여 소득과 인종 상호작용효과 
regress_anes2=with(myimp,
                   lm(FTobama~female+ageyr+educ+readyBP+economy+environ+iraqwar+
                        race3+hhinc+hhinc2+Bhhinc+Bhhinc2+Ohhinc+Ohhinc2))
pool_regress_interact2=pool(regress_anes2)
summary(pool_regress_interact2,conf.int=TRUE) %>% as_tibble()
#인종과 소득 상호작용효과항 없음 
regress_anes0=with(myimp,
                   lm(FTobama~female+ageyr+educ+readyBP+economy+environ+iraqwar+
                        race3+hhinc))
#인종과 소득 1차항의 상호작용효과항
regress_anes1=with(myimp,
                   lm(FTobama~female+ageyr+educ+readyBP+economy+environ+iraqwar+
                        race3+hhinc+Bhhinc+Ohhinc))
#모형비교 
D3(regress_anes1,regress_anes0) 
D3(regress_anes2,regress_anes1) 
D3(regress_anes2,regress_anes0) 
#최종모형 
pool_regress_interact1=pool(regress_anes1)
summary(pool_regress_interact1,conf.int=TRUE) %>% as_tibble()

#시각화 
#1단계: 가계소득 변수수준 
var_hhinc=unique(sort(df_MI$hhinc[!is.na(df_MI$hhinc)]))
#2단계: 인종/가계소득 외 다른 변수들 통제 
#3단계: 인종별 가계소득수준별 종속변수 예측값 계산 
#남녀 동비, 미국사회의 흑인대통령 수용가능 
#educ, economy, environ, iraqwar 평균 
mycoefs = summary(pool_regress_interact1,conf.int=TRUE) 
predy_white=var_hhinc*mycoefs$estimate[mycoefs$term=="hhinc"]+
  0*mycoefs$estimate[mycoefs$term=="race31"]+
  0*mycoefs$estimate[mycoefs$term=="race32"]+
  0*var_hhinc*mycoefs$estimate[mycoefs$term=="Bhhinc"]+
  0*var_hhinc*mycoefs$estimate[mycoefs$term=="Ohhinc"]+
  mycoefs$estimate[mycoefs$term=="(Intercept)"]+
  0.5*mycoefs$estimate[mycoefs$term=="female1"]+
  myMeans["ageyr",]*mycoefs$estimate[mycoefs$term=="ageyr"]+
  myMeans["educ",]*mycoefs$estimate[mycoefs$term=="educ"]+
  0.5*mycoefs$estimate[mycoefs$term=="readyBP1"]+
  myMeans["economy",]*mycoefs$estimate[mycoefs$term=="economy"]+
  myMeans["environ",]*mycoefs$estimate[mycoefs$term=="environ"]+
  myMeans["iraqwar",]*mycoefs$estimate[mycoefs$term=="iraqwar"]
predy_black=var_hhinc*mycoefs$estimate[mycoefs$term=="hhinc"]+
  1*mycoefs$estimate[mycoefs$term=="race31"]+
  0*mycoefs$estimate[mycoefs$term=="race32"]+
  1*var_hhinc*mycoefs$estimate[mycoefs$term=="Bhhinc"]+
  0*var_hhinc*mycoefs$estimate[mycoefs$term=="Ohhinc"]+
  mycoefs$estimate[mycoefs$term=="(Intercept)"]+
  0.5*mycoefs$estimate[mycoefs$term=="female1"]+
  myMeans["ageyr",]*mycoefs$estimate[mycoefs$term=="ageyr"]+
  myMeans["educ",]*mycoefs$estimate[mycoefs$term=="educ"]+
  0.5*mycoefs$estimate[mycoefs$term=="readyBP1"]+
  myMeans["economy",]*mycoefs$estimate[mycoefs$term=="economy"]+
  myMeans["environ",]*mycoefs$estimate[mycoefs$term=="environ"]+
  myMeans["iraqwar",]*mycoefs$estimate[mycoefs$term=="iraqwar"]
predy_others=var_hhinc*mycoefs$estimate[mycoefs$term=="hhinc"]+
  0*mycoefs$estimate[mycoefs$term=="race31"]+
  1*mycoefs$estimate[mycoefs$term=="race32"]+
  0*var_hhinc*mycoefs$estimate[mycoefs$term=="Bhhinc"]+
  1*var_hhinc*mycoefs$estimate[mycoefs$term=="Ohhinc"]+
  mycoefs$estimate[mycoefs$term=="(Intercept)"]+
  0.5*mycoefs$estimate[mycoefs$term=="female1"]+
  myMeans["ageyr",]*mycoefs$estimate[mycoefs$term=="ageyr"]+
  myMeans["educ",]*mycoefs$estimate[mycoefs$term=="educ"]+
  0.5*mycoefs$estimate[mycoefs$term=="readyBP1"]+
  myMeans["economy",]*mycoefs$estimate[mycoefs$term=="economy"]+
  myMeans["environ",]*mycoefs$estimate[mycoefs$term=="environ"]+
  myMeans["iraqwar",]*mycoefs$estimate[mycoefs$term=="iraqwar"]
#4단계: 원점수 환원 후 데이터로 
myfig = tibble(
  hhinc=(var_hhinc+mean(mydata$hhinc,na.rm=TRUE)),
  FT_white=10*predy_white,
  FT_black=10*predy_black,
  FT_others=10*predy_others
)
#5단계: 시각화
myfig=myfig %>% 
  pivot_longer(cols=FT_white:FT_others,names_to="race",values_to="FT_obama") %>%
  mutate(
    race=ifelse(str_detect(race,"wh"),"White",race),
    race=ifelse(str_detect(race,"bl"),"Black",race),
    race=ifelse(str_detect(race,"others"),"Others (Neither White nor Black)",race),
    race=fct_reorder(race,row_number())
  ) 
myfig %>% 
  ggplot(aes(x=hhinc,y=FT_obama,colour=race,shape=race))+
  geom_line(size=2)+
  geom_point(size=4)+
  labs(x="Household income level",
       y="Predicted feeling thermometer for B. Obama",
       colour="Respondents' race",
       shape="Respondents' race")+
  scale_x_continuous(breaks=1:25)+
  scale_y_continuous(breaks=round(myfig$FT_obama,0))+
  scale_colour_manual(values=c("grey","black","yellow"))+
  coord_cartesian(ylim=c(49,78))+
  theme_bw()+theme(legend.position="top")
ggsave("Interaction_MI_race_hhinc_FTobama.png",width=22,height=18,units='cm')
