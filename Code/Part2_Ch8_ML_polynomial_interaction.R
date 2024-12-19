#===================================================================#
#제2부 ML기반 결측데이터 분석기법 
#제8장 상호작용효과항 및 다차항 
#===================================================================#
mypath = "D:/2020_Term2/Category_A_submission/MissingData/Rcode/data"
setwd(mypath)

#데이터 불러오기 
library(tidyverse)
library(lavaan)

#현실 데이터를 이용한 실습 
mydata = read_csv("anes2008_example.csv")
df_ML = mydata %>% 
  mutate(
    black=ifelse(race3==1,1,0),others=ifelse(race3==2,1,0) 
  ) %>% 
  mutate_at(vars(starts_with("FT_"),ageyr,hhinc),~(0.1*(.))) %>% 
  mutate(
    hhinc2=hhinc^2  #소득변수의 2차항 
  )
#다차항 추정
mypolynomial_raw="
FT_obama~female+black+others+ageyr+educ+readyBP+economy+environ+iraqwar+hhinc+hhinc2
"
fit_polynom_raw=sem(model=mypolynomial_raw,data=df_ML,
                missing="FIML",
                fixed.x=FALSE,meanstructure=TRUE)
summary(fit_polynom_raw,
        ci=TRUE,standard=TRUE)
#시각화 과정 
#1단계: hhinc 변수의 값들을 확보
var_hhinc=unique(sort(df_ML$hhinc[!is.na(df_ML$hhinc)]))
var_hhinc
#2단계: 연속형 변수들(ageyr,educ,economy,envrion,iraqwar) 평균을 가정(더미변수들은 0을 가정)
coefficients=parameterestimates(fit_polynom_raw) %>% 
  filter(op=="~") %>% print()
Means=parameterestimates(fit_polynom_raw) %>% 
  filter(op=="~1") %>% print()
conditions_other_vars=
  Means$est[Means$lhs=="ageyr"]*coefficients$est[coefficients$rhs=="ageyr"]+
  Means$est[Means$lhs=="educ"]*coefficients$est[coefficients$rhs=="educ"]+
  Means$est[Means$lhs=="economy"]*coefficients$est[coefficients$rhs=="economy"]+
  Means$est[Means$lhs=="environ"]*coefficients$est[coefficients$rhs=="environ"]+
  Means$est[Means$lhs=="iraqwar"]*coefficients$est[coefficients$rhs=="iraqwar"]
#3단계: 회귀방정식을 기반으로 오바마후보에 대한 감정점수 예측 
pred_FT = var_hhinc*coefficients$est[coefficients$rhs=="hhinc"]+
  var_hhinc*var_hhinc*coefficients$est[coefficients$rhs=="hhinc2"]+
  Means$est[Means$lhs=="FT_obama"]+conditions_other_vars
#4단계: 원래 측정단위로 환산후 데이터 형태로
df_pred=tibble(hhinc=10*var_hhinc,FT_obama=10*pred_FT)
#5단계: 시각화 
df_pred %>% 
  ggplot(aes(x=hhinc,y=FT_obama))+
  geom_line()+
  geom_point(size=4)+
  labs(x="Household income level",y="Predicted feeling thermometer for B. Obama")+
  scale_x_continuous(breaks=1:25)+
  scale_y_continuous(breaks=round(df_pred$FT_obama,1))+
  coord_cartesian(ylim=c(43,51))+
  theme_bw()
ggsave("Curvilinear_hhinc_FTobama_raw.png",width=22,height=17,units='cm')

#평균중심화 변환 
df_ML_MC = df_ML %>% 
  select(-hhinc2) %>% #평균중심화 변환을 하지 않은 경우의 2차항 삭제
  mutate_at(
    vars(ageyr,educ,hhinc,economy,environ,iraqwar),
    ~(.-mean(.,na.rm=TRUE)
    )) %>% 
  mutate(
    hhinc2=hhinc^2  #소득변수의 2차항 
  )
#다차항 추정
mypolynomial="
FT_obama~female+black+others+ageyr+educ+readyBP+economy+environ+iraqwar+hhinc+hhinc2
"
fit_polynom=sem(model=mypolynomial,data=df_ML_MC,
                missing="FIML",
                fixed.x=FALSE,meanstructure=TRUE)
summary(fit_polynom,
        ci=TRUE,standard=TRUE)
mean(df_ML$hhinc,na.rm=TRUE)
mean(df_ML$hhinc,na.rm=TRUE)-0.590
#시각화
#1단계: hhinc 변수의 값들을 확보
var_hhinc=unique(sort(df_ML_MC$hhinc[!is.na(df_ML_MC$hhinc)]))
#2단계: 연속형 변수들(educ,economy,envrion,iraqwar) 평균(즉 0)을 가정(더미변수들은 0을 가정)
coefficients=parameterestimates(fit_polynom) %>% 
  filter(op=="~") %>% print()
Means=parameterestimates(fit_polynom) %>% 
  filter(op=="~1") %>% print()
conditions_other_vars=
  Means$est[Means$lhs=="ageyr"]*coefficients$est[coefficients$rhs=="ageyr"]+
  Means$est[Means$lhs=="educ"]*coefficients$est[coefficients$rhs=="educ"]+
  Means$est[Means$lhs=="economy"]*coefficients$est[coefficients$rhs=="economy"]+
  Means$est[Means$lhs=="environ"]*coefficients$est[coefficients$rhs=="environ"]+
  Means$est[Means$lhs=="iraqwar"]*coefficients$est[coefficients$rhs=="iraqwar"]
#3단계: 회귀방정식을 기반으로 오바마후보에 대한 감정점수 예측 
pred_FT = var_hhinc*coefficients$est[coefficients$rhs=="hhinc"]+
  var_hhinc*var_hhinc*coefficients$est[coefficients$rhs=="hhinc2"]+
  Means$est[Means$lhs=="FT_obama"]+conditions_other_vars
#4단계: 원래 측정단위로 환산후 데이터 형태로
df_pred=tibble(hhinc=10*(var_hhinc+mean(df_ML$hhinc,na.rm=TRUE)),
                         FT_obama=10*(pred_FT))
#5단계: 시각화 
df_pred %>% 
  ggplot(aes(x=hhinc,y=FT_obama))+
  geom_line()+
  geom_point(size=4)+
  labs(x="Household income level",y="Feeling thermometer for B. Obama")+
  scale_x_continuous(breaks=1:25)+
  scale_y_continuous(breaks=round(df_pred$FT_obama,1))+
  coord_cartesian(ylim=c(43,51))+
  theme_bw()
ggsave("Curvilinear_hhinc_FTobama_meancenter.png",width=22,height=17,units='cm')


#상호작용효과 추정
#새로운 변수로 생성한 후 분석모형에 투입 
df_ML_MC=df_ML_MC %>% 
  mutate(
    B_hhinc=black*hhinc,
    B_hhinc2=black*hhinc2,
    O_hhinc=others*hhinc,
    O_hhinc2=others*hhinc2,
  )
myinteraction2="
FT_obama~female+black+others+ageyr+educ+readyBP+economy+environ+iraqwar
FT_obama~hhinc+hhinc2+B_hhinc+B_hhinc2+O_hhinc+O_hhinc2
"
#아래와 같이 : 을 사용해도 됨
myinteraction2="
FT_obama~female+black+others+ageyr+educ+readyBP+economy+environ+iraqwar
FT_obama~hhinc+hhinc2+black:hhinc+black:hhinc2+others:hhinc+others:hhinc2
"
fit_interaction2=sem(model=myinteraction2,data=df_ML_MC,
                    missing="FIML",
                    fixed.x=FALSE,meanstructure=TRUE)
summary(fit_interaction2,
        standard=TRUE)
#상호작용효과항들에 대한 통계적 유의도 테스트 
#LR CHI2 test 
myinteraction1="
FT_obama~female+black+others+ageyr+educ+readyBP+economy+environ+iraqwar
FT_obama~hhinc+0*hhinc2+black:hhinc+0*black:hhinc2+others:hhinc+0*others:hhinc2
"
fit_interaction1=sem(model=myinteraction1,data=df_ML_MC,
                     missing="FIML",
                     fixed.x=FALSE,meanstructure=TRUE)
myinteraction0="
FT_obama~female+black+others+ageyr+educ+readyBP+economy+environ+iraqwar
FT_obama~hhinc+0*hhinc2+0*black:hhinc+0*black:hhinc2+0*others:hhinc+0*others:hhinc2
"
fit_interaction0=sem(model=myinteraction0,data=df_ML_MC,
                     missing="FIML",
                     fixed.x=FALSE,meanstructure=TRUE)

lavTestLRT(fit_interaction1,fit_interaction0)
lavTestLRT(fit_interaction2,fit_interaction1)
lavTestLRT(fit_interaction2,fit_interaction0)
#상호작용 효과 테스트: Wald's CHI2 test 
myinteraction_Wald1="
FT_obama~female+black+others+ageyr+educ+readyBP+economy+environ+iraqwar
FT_obama~A1*hhinc+B1*black:hhinc+C1*others:hhinc
"
fit_interaction_Wald1=sem(model=myinteraction_Wald1,data=df_ML_MC,
                          missing="FIML",
                          fixed.x=FALSE,meanstructure=TRUE)
myinteraction_Wald2="
FT_obama~female+black+others+ageyr+educ+readyBP+economy+environ+iraqwar
FT_obama~A1*hhinc+A2*hhinc2+B1*black:hhinc+B2*black:hhinc2+C1*others:hhinc+C2*others:hhinc2
"
fit_interaction_Wald2=sem(model=myinteraction_Wald2,data=df_ML_MC,
                     missing="FIML",
                     fixed.x=FALSE,meanstructure=TRUE)
#Model0 vs. Model1
lavTestWald(fit_interaction_Wald1,
            constraints=
             "B1 == 0
              C1 == 0")
#Model1 vs. Model2
lavTestWald(fit_interaction_Wald2,
            constraints=
              "A2 == 0
              B2 == 0
              C2 == 0")
#Model0 vs. Model2
lavTestWald(fit_interaction_Wald2,
            constraints=
             "A2 == 0
              B1 == 0
              B2 == 0
              C1 == 0 
              C2 == 0")
#최종모형 
myinteraction1="
FT_obama~female+black+others+ageyr+educ+readyBP+economy+environ+iraqwar
FT_obama~hhinc+black:hhinc+others:hhinc
"
fit_interaction1=sem(model=myinteraction1,data=df_ML_MC,
                     missing="FIML",
                     fixed.x=FALSE,meanstructure=TRUE)
summary(fit_interaction1,
        standard=TRUE)

#최종선택 모형 시각화 
#1단계: 가계소득 변수수준 
var_hhinc=unique(sort(df_ML_MC$hhinc[!is.na(df_ML_MC$hhinc)]))
#2단계; 인종/가계소득 외 다른 변수들 통제 
#남녀 동비, 미국사회의 흑인대통령 수용가능 
#educ, economy, environ, iraqwar 평균 
coefficients = parameterestimates(fit_interaction1) %>% 
  filter(op == "~")
Means = parameterestimates(fit_interaction1) %>% 
  filter(op == "~1")
condition_other_vars = 
  coefficients$est[coefficients$rhs=="female"]*0.5+
  coefficients$est[coefficients$rhs=="ageyr"]*Means$est[Means$lhs=="ageyr"]+
  coefficients$est[coefficients$rhs=="educ"]*Means$est[Means$lhs=="educ"]+
  coefficients$est[coefficients$rhs=="readyBP"]*0.5+
  coefficients$est[coefficients$rhs=="economy"]*Means$est[Means$lhs=="economy"]+
  coefficients$est[coefficients$rhs=="environ"]*Means$est[Means$lhs=="environ"]+
  coefficients$est[coefficients$rhs=="iraqwar"]*Means$est[Means$lhs=="iraqwar"]
#3단계: 인종별 가계소득수준별 종속변수 예측값 계산 
predy_white=var_hhinc*coefficients$est[coefficients$rhs=="hhinc"]+
  Means$est[Means$lhs=="FT_obama"]+
  condition_other_vars
predy_black=var_hhinc*(coefficients$est[coefficients$rhs=="hhinc"]+
               coefficients$est[coefficients$rhs=="black:hhinc"])+
  Means$est[Means$lhs=="FT_obama"]+
  coefficients$est[coefficients$rhs=="black"]+
  condition_other_vars
predy_others=var_hhinc*(coefficients$est[coefficients$rhs=="hhinc"]+
               coefficients$est[coefficients$rhs=="others:hhinc"])+
  Means$est[Means$lhs=="FT_obama"]+
  coefficients$est[coefficients$rhs=="others"]+
  condition_other_vars
#4단계: 원점수 환원 후 데이터로 
myfig = tibble(
  hhinc=10*(var_hhinc+mean(df_ML$hhinc,na.rm=TRUE)),
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
ggsave("Interaction_race_hhinc_FTobama.png",width=22,height=18,units='cm')
