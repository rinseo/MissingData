#===================================================================#
#제3부 MI기반 결측데이터 분석기법 
#제12장 보조변수 포함 MI 기법
#===================================================================#
mypath = "D:/2020_Term2/Category_A_submission/MissingData/Rcode/data"
setwd(mypath)

#데이터 불러오기 
library(tidyverse)
library(mice)
#현실 데이터를 이용한 실습 
#제2부 제1장에서 사용한 변수들만 선택 (ML과의 비교) 
mydata = read_csv("anes2008_example.csv") %>% 
  select(FT_mccain,female,ageyr,race3,educ,hhinc,readyBP,economy,environ,iraqwar,
         starts_with("FT_"))     #보조변수들
mydata
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
  )
df_MI
#Imputing 단계 
#동일결과를 원한다면 seed 옵션을 동일하게 
myimp = mice(data=df_MI,m=20,print=FALSE,seed=1234)
myimp
plot(myimp)
#회귀모형 추정
regress_anes=with(myimp,
                  lm(FTbiden~female+ageyr+race3+educ+hhinc+readyBP+economy+environ+iraqwar))
pool_regress_anes=pool(regress_anes)
summary(pool_regress_anes,conf.int=TRUE) %>% as_tibble()

#ML 기법(포화상관모형)과의 비교 
library(lavaan)
library(semTools)
mypath_no_aux = "
FTbiden~female+ageyr+black+others+educ+hhinc+readyBP+economy+environ+iraqwar
"
aux_vars = c('FTobama','FTmccain','FTpalin','FTRparty','FTDparty')
fit_ys_auxS = sem.auxiliary(model=mypath_no_aux,aux=aux_vars,
                            data=df_MI%>% 
                              mutate(black=ifelse(race3==1,1,0),
                                     others=ifelse(race3==2,1,0),
                                     female=as.numeric(ifelse(female==1,1,0)),
                                     readyBP=as.numeric(ifelse(readyBP==1,1,0))),
                            missing="FIML",
                            fixed.x=FALSE,meanstructure=TRUE)
result_ys_auxS=parameterestimates(fit_ys_auxS) %>% 
  filter(lhs=="FTbiden") %>% 
  filter(op!="~~") %>% 
  mutate(
    rhs=ifelse(rhs=="","(Intercept)",rhs),
    source=fct_reorder(rhs,row_number()),
    source=fct_relevel(source,"(Intercept)"),
    method="ML"
  ) %>% select(source,est,ci.lower,ci.upper,method) %>% 
  arrange(source)
result_MI_ML = summary(pool_regress_anes,conf.int=TRUE) %>% 
  as_tibble() %>% 
  mutate(
    source=result_ys_auxS$source,
    method="MI"
  ) %>% 
  rename(est=estimate,ci.lower=`2.5 %`,ci.upper=`97.5 %`) %>% 
  select(source,est,ci.lower,ci.upper,method) %>% 
  bind_rows(result_ys_auxS) 
#회귀계수 비교 
result_MI_ML %>% 
  filter(source!="(Intercept)") %>% 
  ggplot(aes(x=source,y=est,colour=method))+
  geom_point(size=2,position=position_dodge(width=0.3))+
  geom_errorbar(aes(ymin=ci.lower,ymax=ci.upper),width=0.3,position=position_dodge(width=0.3))+
  labs(x="Parameter",y="Point estimates with 95% CI",colour="Method")+
  theme_bw()+theme(legend.position="top")
ggsave("Comparison_MI_ML_AuxVar_OLS.png",width=15,height=11,units='cm')
#절편 비교 
result_MI_ML %>% 
  filter(source=="(Intercept)")
