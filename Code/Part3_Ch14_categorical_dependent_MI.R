#===================================================================#
#제3부 MI기반 결측데이터 분석기법 
#제14장 범주형 변수가 종속변수인 경우 
#===================================================================#
mypath = "D:/2020_Term2/Category_A_submission/MissingData/Rcode/data"
setwd(mypath)

#데이터 불러오기 
library(tidyverse)
library(mice)
library(miceadds)
#현실 데이터를 이용한 실습 
#제2부 제1장에서 사용한 변수들만 선택 (ML과의 비교) 
mydata = read_csv("anes2008_example.csv")
#사전처리  
df_MI = mydata %>%
  rename_all(~(str_remove(.,"_"))) %>% #FT_의 _를 지움 
  mutate_at(
    vars(female,race3,readyBP,hopeBP,votechoice,voteplan,iraqwar),
    ~(factor(.))
  ) %>% 
  mutate_at(
    vars(starts_with("FT"),ageyr,hhinc),
    ~(0.1*(.))       #ML에서 10을 나누어 주었음. 
  ) %>% 
  mutate_at(
    vars(ageyr,educ,hhinc,economy,environ,starts_with("FT")),
    ~(. - mean(., na.rm=TRUE))  #평균중심화 변환 
  )
df_MI %>% count(votechoice,voteplan)
 
#Imputing 단계 
myimp = mice(data=df_MI,m=20,print=FALSE,seed=1234) #print=FALSE가 없으면 투입과정이 출력됨 
#회귀모형 추정
#이항 로지스틱 회귀모형: 투표자 대 기권자 
binary_logit_anes=with(myimp,
                       glm(ifelse(votechoice!=0,1,0)~female+race3+ageyr+educ+hhinc,
                           family=binomial(link="logit")))
pool_binary_logit_anes=pool(binary_logit_anes)
summary(pool_binary_logit_anes,conf.int=TRUE) %>% as_tibble() %>% 
  mutate(OR=exp(estimate)) #OR계산
pool_binary_logit_anes$glanced

#모형비교 
binary_logit_anes0=with(myimp,
                       glm(ifelse(votechoice!=0,1,0)~female+race3,
                           family=binomial(link="logit")))
D3(binary_logit_anes,binary_logit_anes0) %>% summary()  #LR Chi2 test 
D1(binary_logit_anes,binary_logit_anes0) %>% summary()  #Wald's Chi2 test 


#순위 로지스틱 회귀모형
dlist_myimp=mids2datlist(myimp)
ordinal_logit_anes=list()
for (i in 1:20){
  ordinal_logit_anes[[i]] = MASS::polr(iraqwar~
                                         female+race3+ageyr+educ+hhinc,
                                       data=dlist_myimp[[i]])
}
pool_ordinal_logit_anes=pool(ordinal_logit_anes)
summary(pool_ordinal_logit_anes,conf.int=TRUE) %>% as_tibble()  %>% 
  mutate(OR=exp(estimate))
pool_ordinal_logit_anes$glanced

#다항 로지스틱 회귀모형
multinom_logit_anes=with(myimp,
                       nnet::multinom(votechoice~female+race3+ageyr+educ+hhinc))
pool_multinom_logit_anes=pool(multinom_logit_anes)
summary(pool_multinom_logit_anes,conf.int=TRUE)  %>% 
  mutate(OR=exp(estimate)) %>% 
  as_tibble() %>% print(n=Inf)

#기준집단을 오바마 후보지지 유권자로 
multinom_logit_anes_base1=with(myimp,
                         nnet::multinom(fct_relevel(votechoice,"1","2","0")~female+race3+ageyr+educ+hhinc))
pool_multinom_logit_anes_base1=pool(multinom_logit_anes_base1)
summary(pool_multinom_logit_anes_base1,conf.int=TRUE)  %>% 
  mutate(OR=exp(estimate)) %>% 
  as_tibble() %>% print(n=Inf)
pool_multinom_logit_anes$glanced
