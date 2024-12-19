#===================================================================#
#제3부 MI기반 결측데이터 분석기법 
#제11장 MI기법 개요
#===================================================================#
mypath = "D:/2020_Term2/Category_A_submission/MissingData/Rcode/data"
setwd(mypath)

#데이터 불러오기 
library(tidyverse)
df_mar = read_csv("sim_mar.csv") 

#MI기법을 이용한 기술통계분석
library(mice)
library(miceadds)
#Imputing 단계 
myimp = mice(data=df_mar,m=20,seed=1234)   #mice(data=df_mar,m=20,seed=1234,print=FALSE)로 하면 계산과정 출력없음 
plot(myimp)  #결과는 별도로 제시하지 않음 
myimp
dl_myimp = mids2datlist(myimp)
dl_myimp[[1]] %>% head()   #1번째 대체투입 데이터 

#Analyzing 단계 
dl_myimp[[1]] %>% 
  summarise_all(
    .funs=c("mean","sd")
  ) %>% pivot_longer(cols=names(.)) %>% 
  separate(name,c("vars","type"),sep="_") %>% 
  pivot_wider(names_from=type,values_from=value)
#반복계산을 위한 개인함수 설정 
fun_MI_descriptive_statistics = function(myimpK){
  result=myimpK %>% 
    summarise_all(
      .funs=c("mean","sd")
    ) %>% pivot_longer(cols=names(.)) %>% 
    separate(name,c("vars","type"),sep="_") %>% 
    pivot_wider(names_from=type,values_from=value) %>% data.frame()
  rownames(result)=result$vars
  result = result %>% select(-vars)
  return(result)
}
#20회 반복 analysis  
myanalysis=with(dl_myimp,
                fun=function(data){
                  fun_MI_descriptive_statistics(data)
                })
myanalysis[[2]]  #2번째 대체투입 데이터 분석결과 
#Pooling단계 
withPool_MI(myanalysis)
mypoolMSD=withPool_MI(myanalysis) %>% 
  mutate(
    vars=rownames(.),
    M=format(round(mean,3),3),
    SD=format(round(sd,3),3)
  ) %>% select(vars:SD)
mypoolMSD

#상관계수
micombine.cor(mi.res=dl_myimp,
              variables=mypoolMSD$vars) %>% 
  as_tibble()
mypoolCOR=micombine.cor(mi.res=dl_myimp,
                    variables=mypoolMSD$vars) %>%
  as_tibble() %>% 
  mutate(
    mystar=cut(p,c(-Inf,0.001,0.01,0.05,Inf),c("***","**","*","")),
    myreport=str_c(format(round(r,3),3),mystar)
  ) %>% select(variable1,variable2,myreport) %>% 
  pivot_wider(names_from=variable2,values_from=myreport) %>% 
  select(.$variable1) %>% 
  as.matrix()
mypoolCOR
diag(mypoolCOR)="1.000"
mypoolCOR[upper.tri(mypoolCOR)]=""
mypoolCOR
#평균/표준편차 데이터와 상관계수 행렬 합치기 
mypoolMSD %>% bind_cols(mypoolCOR  %>% data.frame() )

# 부분상관계수를 구하고자 한다면
micombine.cor(mi.res=dl_myimp, 
              variables=c("Y","X1"),
              partial=c("X2","X3"))  %>% 
  as_tibble()

#회귀모형 추정
regress_mar=with(myimp,lm(Y~X1+X2+X3))  #analyzing 단계 
pool_regress_mar=pool(regress_mar)      #pooling 단계 
pool_regress_mar$pooled %>% #통합된 회귀모형 추정결과 
  as_tibble() 
pool_regress_mar$glanced %>%  #회귀모형 모형적합도 
  as_tibble() 

summary(pool_regress_mar,conf.int=TRUE) %>% 
  as_tibble() 

#모형비교 
regress_mar0=with(myimp,lm(Y~X1))
D3(regress_mar,regress_mar0)  #LR Chi2 test 
D1(regress_mar,regress_mar0)  #Wald's Chi2 test 

# 반복작업을 위한 개인함수 설정 
fun_MI_descriptive_correlation_regress=function(myseed,mydata,mydigit,myM){
  #M, SD계산
  set.seed(myseed)
  myimp=mice(data=mydata,m=myM,print=FALSE)
  dlist_myimp=mids2datlist(myimp)
  myanalysis=with(dlist_myimp,
                  fun=function(data){
                    fun_MI_descriptive_statistics(data)
                  })
  mypoolMSD=withPool_MI(myanalysis) %>% 
    mutate(
      vars=rownames(.),
      M=format(round(mean,mydigit),mydigit),
      SD=format(round(sd,mydigit),mydigit)
    ) %>% select(vars:SD)
  #상관계수
  mypoolCOR=micombine.cor(mi.res=dlist_myimp,
                          variables=mypoolMSD$vars) %>%
    as_tibble() %>% 
    mutate(
      mystar=cut(p,c(-Inf,0.001,0.01,0.05,Inf),c("***","**","*","")),
      myreport=str_c(format(round(r,mydigit),mydigit),mystar)
    ) %>% select(variable1,variable2,myreport) %>% 
    pivot_wider(names_from=variable2,values_from=myreport) %>% 
    select(.$variable1) %>% 
    as.matrix() 
  diag(mypoolCOR)=as.character(format(1,nsmall=mydigit))
  mypoolCOR[upper.tri(mypoolCOR)]=""
  #합치기 
  MSD_COR = mypoolMSD %>% bind_cols(mypoolCOR  %>% data.frame() )
  #회귀모형 추정
  myregress_pool=pool(with(myimp,lm(Y~X1+X2+X3)))
  myresult = list(MSD_COR,myregress_pool)
  myresult
}

# 네가지 시뮬레이션 데이터들에 적용 
result_comp=read_csv("sim_CD.csv") %>% 
  fun_MI_descriptive_correlation_regress(1234,.,3,20)
result_mcar=read_csv("sim_mcar.csv") %>%
  fun_MI_descriptive_correlation_regress(1234,.,3,20)
result_mar=read_csv("sim_mar.csv") %>%
  fun_MI_descriptive_correlation_regress(1234,.,3,20)
result_mnar=read_csv("sim_mnar.csv") %>% 
  fun_MI_descriptive_correlation_regress(1234,.,3,20)

#기술통계분석, 상관관계분석 추정결과 비교 
compare_MI_MSD_COR = bind_rows(
  result_comp[[1]] %>% mutate(mech="Complete"),
  result_mcar[[1]] %>% mutate(mech="MCAR"),
  result_mar[[1]] %>% mutate(mech="MAR"),
  result_mnar[[1]] %>% mutate(mech="MNAR")
)
compare_MI_MSD_COR

#시각화 : 회귀분석 추정결과 비교 
compare_MI_OLS = bind_rows(
  summary(result_comp[[2]],conf.int=T) %>% mutate(mech="Complete"),
  summary(result_mcar[[2]],conf.int=T) %>% mutate(mech="MCAR"),
  summary(result_mar[[2]],conf.int=T) %>% mutate(mech="MAR"),
  summary(result_mnar[[2]],conf.int=T) %>% mutate(mech="MNAR")
) %>% as_tibble() %>% 
  mutate(
    vars=fct_reorder(term,row_number()),
    mech=fct_reorder(mech,row_number()),
    pest=estimate,
    SE=std.error,
    ll95=`2.5 %`, 
    ul95=`97.5 %`,
  ) %>% select(mech,vars,pest,SE,df,ll95,ul95)
compare_MI_OLS %>% 
  ggplot(aes(x=vars,y=pest,colour=mech))+
  geom_point(size=2,position=position_dodge(width=0.3))+
  geom_errorbar(aes(ymin=ll95,ymax=ul95),width=0.3,position=position_dodge(width=0.3))+
  labs(x="Parameter",y="Point estimates with 95% CI",colour="Missing mechanism")+
  theme_bw()+theme(legend.position="top")
ggsave("Comparison_MI_four_sumulation.png",width=15,height=11,units='cm')


#현실 데이터를 이용한 실습 
#제2부 제1장에서 사용한 변수들만 선택 (ML과의 비교) 
mydata = read_csv("anes2008_example.csv") %>% 
  select(FT_mccain,female,ageyr,race3,educ,hhinc,readyBP,economy,environ,iraqwar)
mydata 
#사전처리 
df_MI = mydata %>%
  rename_all(~(str_remove(.,"_"))) %>% #FT_의 _를 지움 
  mutate_at(
    vars(female,race3,readyBP),
    ~(factor(.))
  ) %>% 
  mutate_at(
    vars(educ,hhinc,economy,environ,iraqwar),
    ~(. - mean(., na.rm=TRUE))  #평균중심화 변환 
  ) %>% 
  mutate(
    ageyr=0.1*ageyr,
    FTmccain=0.1*FTmccain       #ML에서 10을 나누어 주었음. 
  )
df_MI
#Imputing 단계 
#동일결과를 원한다면 seed옵션 동일하게 
myimp = mice(data=df_MI,m=20,print=FALSE,seed=1234) 
myimp

#Analyzing 단계 
#연속형 변수의 경우: 평균/표준편차
dlist_myimp=mids2datlist(myimp)
myanalysis_cont=with(dlist_myimp,
                fun=function(data){
                  data_cont = data %>% select(where(is.numeric))  #연속형 변수들만
                  fun_MI_descriptive_statistics(data_cont)
                })
withPool_MI(myanalysis_cont)

#범주형 변수의 경우: 까다로움! 
dlist_myimp[[1]] %>% count(race3)
dlist_myimp[[2]] %>% count(race3)
#더미변수들을 생성한 후 연속형 변수의 평균과 표준편차 제시 
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
                  fun_MI_descriptive_statistics(bind_cols(data_cont,data_cate))
                })
#Pooling 단계 
mypoolMSD=withPool_MI(myanalysis) %>% 
  mutate(
    vars=rownames(.),
    M=format(round(mean,3),3),
    SD=format(round(sd,3),3)
  ) %>% select(vars:SD)
mypoolMSD

#상관계수
#더미변수 접근 
dlist_myimp_dummy=list() 
for (i in 1:20){
  data_cont = dlist_myimp[[i]] %>% select(where(is.numeric))  #연속형 변수들만
  data_cate = dlist_myimp[[i]] %>% 
    select(where(is.factor))  %>% #범주형 변수들만 
    mutate(
      white=ifelse(race3==0,1,0),
      black=ifelse(race3==1,1,0),
      others=ifelse(race3==2,1,0),
      female=as.numeric(ifelse(female==1,1,0)),
      readyBP=as.numeric(ifelse(readyBP==1,1,0))
    ) %>% select(female,white,black,others,readyBP)
  dlist_myimp_dummy[[i]] = bind_cols(data_cont,data_cate)
}
mypoolCOR=micombine.cor(mi.res=dlist_myimp_dummy,
                        variables=mypoolMSD$vars) %>%
  as_tibble() %>% 
  mutate(
    mystar=cut(p,c(-Inf,0.001,0.01,0.05,Inf),c("***","**","*","")),
    myreport=str_c(format(round(r,3),3),mystar)
  ) %>% select(variable1,variable2,myreport) %>% 
  pivot_wider(names_from=variable2,values_from=myreport) %>% 
  select(.$variable1) %>% 
  as.matrix() 
diag(mypoolCOR)="1.000"
mypoolCOR[upper.tri(mypoolCOR)]=""
#평균/표준편차 데이터와 상관계수 행렬 합치기 
mypoolMSD %>% bind_cols(mypoolCOR  %>% data.frame() )

#회귀모형 추정
regress_anes=with(myimp,
                  lm(FTmccain~female+ageyr+race3+educ+hhinc+readyBP+economy+environ+iraqwar))
pool_regress_anes=pool(regress_anes)
summary(pool_regress_anes,conf.int=TRUE) %>% as_tibble()

#ML 기법과의 비교 
library(lavaan)
myregress="FTmccain~female+black+others+ageyr+educ+hhinc+readyBP+economy+environ+iraqwar"
mylavaan_object=sem(model=myregress,data=df_MI %>% 
                      mutate(black=ifelse(race3==1,1,0),
                             others=ifelse(race3==2,1,0),
                             female=as.numeric(ifelse(female==1,1,0)),
                             readyBP=as.numeric(ifelse(readyBP==1,1,0))),
                    missing="FIML",fixed.x=FALSE,meanstructure=TRUE)
result_ML = parameterestimates(mylavaan_object) %>% 
  filter(lhs=="FTmccain") %>% filter(lhs!=rhs) %>% 
  mutate(
    vars=ifelse(rhs=="","(Intercept)",rhs),
    pest=est,
    ll95=ci.lower,
    ul95=ci.upper
  ) %>% select(vars:ul95)
result_MI = summary(pool_regress_anes,conf.int=TRUE) %>% as_tibble()
#두 가지 결측기법을 합친 결과 
result_MI_ML = result_MI %>% 
  mutate(
    vars=as.character(term),
    vars=ifelse(term=="female1","female",vars),
    vars=ifelse(term=="race31","black",vars),
    vars=ifelse(term=="race32","others",vars),
    vars=ifelse(term=="readyBP1","readyBP",vars),
    method="MI"
  ) %>% 
  rename(
    ll95=`2.5 %`,ul95=`97.5 %`,
    pest=estimate 
  ) %>% 
  select(vars,pest,ll95,ul95,method) %>% 
  bind_rows(result_ML %>% mutate(method="ML")) %>% 
  mutate(vars=fct_reorder(vars,row_number(),min)) 
#절편 비교 
result_MI_ML %>% 
  filter(vars=="(Intercept)")
#회귀계수 비교 
result_MI_ML %>% 
  filter(vars!="(Intercept)") %>% 
  ggplot(aes(x=vars,y=pest,colour=method))+
  geom_point(size=2,position=position_dodge(width=0.3))+
  geom_errorbar(aes(ymin=ll95,ymax=ul95),width=0.3,position=position_dodge(width=0.3))+
  labs(x="Parameter",y="Point estimates with 95% CI",colour="Method")+
  theme_bw()+theme(legend.position="top")
ggsave("Comparison_MI_ML_OLS.png",width=15,height=11,units='cm')
