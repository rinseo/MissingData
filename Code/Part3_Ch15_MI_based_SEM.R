#===================================================================#
#제3부 MI기반 결측데이터 분석기법 
#제15장 MI 기법과 잠재변수 포함 모형추정
#===================================================================#
mypath = "D:/2020_Term2/Category_A_submission/MissingData/Rcode/data"
setwd(mypath)

#데이터 불러오기 
library(tidyverse)
library(mice)
library(miceadds)
library(lavaan)
library(semTools)
#현실 데이터를 이용한 실습 
#제2부 제1장에서 사용한 변수들만 선택 (ML과의 비교) 
mydata = read_csv("anes2008_example.csv")
#사전처리  
df_MI = mydata %>%
  rename_all(~(str_remove(.,"_"))) %>% #FT_의 _를 지움 
  mutate(black=ifelse(race3==1,1,0),others=ifelse(race3==2,1,0)) %>%
  select(-race3) %>% 
  mutate_at(
    vars(female,black,others,readyBP,hopeBP,economy,environ,iraqwar),
    ~(factor(.))
  ) %>% 
  mutate_at(
    vars(starts_with("FT"),ageyr,hhinc),
    ~(0.1*(.))       #ML에서 10을 나누어 주었음. 
  ) %>% 
  mutate_at(
    vars(ageyr,educ,hhinc,starts_with("FT")),
    ~(. - mean(., na.rm=TRUE))  #평균중심화 변환 
  )
#Imputing 단계 
myimp = mice(data=df_MI,m=20,print=FALSE,seed=1234)
#대체투입 데이터들을 리스트 형태로 묶음 
dl_myimp = list() 
for(i in 1:20) {
  dl_myimp[[i]] = complete(myimp,action=i)   #대체투입 결측 데이터를 완전데이터 형태로 
  dl_myimp[[i]] = dl_myimp[[i]] %>% 
    mutate_at(
      vars(female,black,others,readyBP,hopeBP),
      as.numeric
    ) %>% 
    mutate_at(
      vars(economy,environ,iraqwar),
      ordered
    )
}

#CFA 
mycfa = "
FTrep =~ FTRparty+FTmccain+FTpalin
FTdem =~ FTDparty+FTobama+FTbiden
Eval_Bush =~ economy+environ+iraqwar
"
fit_cfa=runMI(model=mycfa, 
              data=dl_myimp,
              fun="sem",
              ordered=c("economy","environ","iraqwar"),
              estimator='WLSMV')
summary(fit_cfa,standard=TRUE,
        fit=TRUE)

#각주 
myresult=summary(fit_cfa,standard=TRUE,
        fit=TRUE)
#잠재변수간 공분산/상관계수
myresult %>% tibble() %>% 
  filter(op=="~~"&(lhs!=rhs)) %>% 
  mutate(
    source=str_c(lhs,op,rhs),
    unstand_est=est,
    robustSE=se,
    stand_est=std.all
  ) %>% select(source:stand_est)

#구조방정식 모형을 이용하여 매개효과 테스트하기 
mysem = "
#measurement model 
FTrep =~ FTRparty+FTmccain+FTpalin
FTdem =~ FTDparty+FTobama+FTbiden
Eval_Bush =~ economy+environ+iraqwar
#structural (causal) model 
Eval_Bush ~ female+black+others+ageyr+educ+a*hhinc
FTrep~female+black+others+ageyr+educ+cr*hhinc+br*Eval_Bush
FTdem~female+black+others+ageyr+educ+cd*hhinc+bd*Eval_Bush
#Indirect effect for Republicans 
IEr := a*br
#Total effect for Republicans  
TEr := IEr+cr
#Indirect effect for Democrats 
IEd := a*bd
#Total effect for Democrats 
TEd := IEd+cd
"
fit_sem=runMI(model=mysem, 
              data=dl_myimp,
              fun="sem",
              ordered=c("economy","environ","iraqwar"),
              estimator='WLSMV')
summary(fit_sem,ci=TRUE,
        fit=TRUE)     
