#===================================================================#
#제1부 결측데이터 분석 개요
#제4장 기존 결측데이터 분석기법과 문제점
#===================================================================#
mypath = "D:/2020_Term2/Category_A_submission/MissingData/Rcode/data"
setwd(mypath)
#데이터 불러오기
library(tidyverse)
df_comp=read_csv("sim_CD.csv")
#결측데이터가 전혀 없었을 경우(기준)
lm(Y~1, df_comp) %>% confint()
#평균계산을 위한 개인함수 
fun_95CI_M=function(myloc,mydf,mytype){
  lm(as.formula(str_c(names(df_comp)[myloc],"~1")),mydf) %>% 
    confint() %>% as_tibble() %>% 
    mutate(
      source=names(df_comp)[myloc],
      mech=mytype
    ) %>% 
    rename(ll95=`2.5 %`,ul95=`97.5 %`) %>%
    select(source,ll95,ul95,mech)
}
fun_95CI_M(1,df_comp,"Complete")
#평균값들 
M_comp=list()
for (myloc in 1:4){
  M_comp=bind_rows(M_comp,fun_95CI_M(myloc,df_comp,"Complete"))
}
M_comp
#회귀모형 모수추정결과 
myformula="Y~X1+X2+X3"
myformula=as.formula(myformula)
ols_cd=lm(myformula,df_comp)
model_comp=confint(ols_cd) %>% 
  as_tibble() %>%
  mutate(
    source=rownames(confint(ols_cd)),
    ll95=`2.5 %`,ul95=`97.5 %`,
    mech="Complete"
  ) %>% select(source:mech)
model_comp

#데이터 불러오기 
df_mcar=read_csv("sim_MCAR.csv")
df_mar=read_csv("sim_MAR.csv")
df_mnar=read_csv("sim_MNAR.csv")
#데이터별 변수의 결측값 비율(시뮬레이션으로 알고 있지만 다시 확인)
library(naniar)
miss_var_summary(df_mcar)
miss_var_summary(df_mar)
miss_var_summary(df_mnar)
#==================================================================#
#리스트단위 결측제거(listwise deletion) 적용 
#==================================================================#
#평균값들(listwise deletion)
M_lwd=list()
for (myloc in 1:4){
  M_lwd=bind_rows(M_lwd,bind_rows(
    fun_95CI_M(myloc,df_mcar%>%drop_na(),"MCAR"),
    fun_95CI_M(myloc,df_mar%>%drop_na(),"MAR"),
    fun_95CI_M(myloc,df_mnar%>%drop_na(),"MNAR")
  ))
}
M_lwd
#평균 95%CI 비교 시각화 
bind_rows(M_comp,M_lwd) %>% 
  mutate(mech=fct_reorder(mech,row_number())) %>% 
  ggplot()+
  geom_errorbar(aes(x=source,ymin=ll95,ymax=ul95,colour=mech),
                width=0.3,size=1,
                position=position_dodge(width=0.3))+
  coord_cartesian(ylim=c(-0.4,0.4))+
  labs(x="Variable",
       y="Confidence intervals for each variable's Mean",
       colour="Mechanism")+
  theme_bw()+
  ggtitle("Mean comparison (Listwise deletion applied)")
ggsave("MeanComparison_ListwiseDeletion.png",width=16,height=10,units="cm")
#회귀모형 추정결과 비교 
ols_mcar=lm(myformula,df_mcar%>%drop_na())
ols_mar=lm(myformula,df_mar%>%drop_na())
ols_mnar=lm(myformula,df_mnar%>%drop_na())
lwd_mss=rbind(confint(ols_mcar),
      confint(ols_mar),
      confint(ols_mnar))
result_lwd=lwd_mss %>% 
  as_tibble() %>%
  mutate(
    source=rownames(lwd_mss),
    ll95=`2.5 %`,ul95=`97.5 %`,
    mech=rep(c("MCAR","MAR","MNAR"),each=4)
  ) %>% select(source:mech)
result_lwd
#리스트단위 결측제거 후 분석결과와 완전데이터 모형추정결과 비교 시각화
bind_rows(model_comp,result_lwd) %>% 
  mutate(mech=fct_reorder(mech,row_number())) %>% 
  ggplot()+
  geom_errorbar(aes(x=source,ymin=ll95,ymax=ul95,colour=mech),
                width=0.3,size=1,
                position=position_dodge(width=0.3))+
  labs(x="Intercept and Coefficients",
       y="Confidence intervals for each estimate",
       colour="Mechanism")+
  theme_bw()+
  ggtitle("Parameter estimates (Listwise deletion applied)")
ggsave("ParameterEstimates_ListwiseDeletion.png",width=16,height=10,units="cm")
#==================================================================#
#쌍별 결측제거(pairwise deletion) 적용
#==================================================================#
Hmisc::rcorr(df_mcar %>% drop_na() %>% as.matrix()) #listwise deletion
Hmisc::rcorr(df_mcar %>% as.matrix())               #pairwise deletion 

library(lavaan)    #SEM 패키지라면 무엇이든 가능 
mu = colMeans(df_mcar,na.rm=TRUE)
cov = cov(df_mcar,use="pairwise")

fit1 = sem("Y~1; X1~1; X2~1; X3~1",
          sample.mean=mu,   #변수들의 평균
          sample.cov=cov,   #변수간 공분산 행렬
          sample.nobs=404)  #표본크기(가장 보수적 상황)
parameterestimates(fit1) 

fit2 = sem("Y~1+X1+X2+X3",
           sample.mean=mu,
           sample.cov=cov,
           sample.nobs=404)
parameterestimates(fit2)

fun_pwd_summary = function(data_mss,mytype){
  mu = colMeans(data_mss,na.rm=TRUE)
  cov = cov(data_mss,use="pairwise")
  fit1 = sem("Y~1; X1~1; X2~1; X3~1",
             sample.mean=mu,
             sample.cov=cov,
             sample.nobs=min(Hmisc::rcorr(data_mss %>% as.matrix())$n))
  M=parameterestimates(fit1) %>% 
    filter(op=="~1") %>% 
    mutate(ll95=ci.lower,ul95=ci.upper,source=lhs,mech=mytype) %>%
    select(source,ll95,ul95,mech)
  fit2 = sem("Y~1+X1+X2+X3",
            sample.mean=mu,
            sample.cov=cov,
            sample.nobs=min(Hmisc::rcorr(data_mss %>% as.matrix())$n))
  Coef=parameterestimates(fit2) %>% 
    filter(op!="~~"&!is.na(z)) %>%
    mutate(
      source=model_comp$source,
      ll95=ci.lower,
      ul95=ci.upper,
      mech=mytype
    ) %>% select(source:mech)
  list(M,Coef)
}
#Mean and 95% CI 
M_pwd=bind_rows(
  fun_pwd_summary(df_mcar,"MCAR")[[1]],
  fun_pwd_summary(df_mar,"MAR")[[1]],
  fun_pwd_summary(df_mnar,"MNAR")[[1]] )
bind_rows(M_comp,M_pwd) %>% 
  mutate(mech=fct_reorder(mech,row_number())) %>% 
  ggplot()+
  geom_errorbar(aes(x=source,ymin=ll95,ymax=ul95,colour=mech),
                width=0.3,size=1,
                position=position_dodge(width=0.3))+
  coord_cartesian(ylim=c(-0.4,0.4))+
  labs(x="Variable",
       y="Confidence intervals for each variable's Mean",
       colour="Mechanism")+
  theme_bw()+
  ggtitle("Mean comparison (Pairwise deletion applied)")
ggsave("MeanComparison_PairwiseDeletion.png",width=16,height=10,units="cm")
#Regression estimates 
result_pwd=bind_rows(
  fun_pwd_summary(df_mcar,"MCAR")[[2]],
  fun_pwd_summary(df_mar,"MAR")[[2]],
  fun_pwd_summary(df_mnar,"MNAR")[[2]] )

#쌍별 결측제거 후 분석결과와 완전데이터 분석결과 비교 
bind_rows(model_comp,result_pwd) %>% 
  mutate(mech=fct_reorder(mech,row_number())) %>% 
  ggplot()+
  geom_errorbar(aes(x=source,ymin=ll95,ymax=ul95,colour=mech),
                width=0.3,size=1,
                position=position_dodge(width=0.3))+
  labs(x="Intercept and Coefficients",
       y="Confidence intervals for each estimate",
       colour="Mechanism")+
  theme_bw()+
  ggtitle("Parameter estimates (Pairwise deletion applied)")
ggsave("ParameterEstimates_PairwiseDeletion.png",width=16,height=10,units="cm")
#==================================================================#
#평균대체(mean substitution) 적용
#==================================================================#
df_temp_mcar = df_mcar %>% 
  mutate_all(
    ~(ifelse(is.na(.),mean(.,na.rm=TRUE),.))  #mean substitution
  )
#평균대체 적용 이전 
df_mcar %>% 
  summarise_all(.funs=c("mean","sd"),na.rm=TRUE)
#평균대체 적용 이후  
df_temp_mcar %>% 
  summarise_all(.funs=c("mean","sd"))

#평균대체 적용 이전 
df_mcar %>% as.matrix() %>% 
  Hmisc::rcorr()
#평균대체 적용 이후  
df_temp_mcar %>% as.matrix() %>% 
  Hmisc::rcorr()

#시각화 
df_temp_mcar %>% 
  mutate(dum=!is.na(df_mcar$X1)&!is.na(df_mcar$X2)) %>% 
  ggplot(aes(x=X1,y=X2))+
  geom_point(aes(colour=dum))+
  geom_smooth(method="lm",colour="red")+
  geom_smooth(data=df_mcar,aes(x=X1,y=Y),method="lm",colour="blue")+
  labs(x="X1",y="X2",colour="Type")+
  scale_colour_manual(values=c("red","blue"),labels=c("Missing","Observed"))+
  coord_cartesian(xlim=c(-3,3),ylim=c(-3,3))+
  theme_bw()
ggsave("scatterplot_meansubstitution.png",width=14,height=9,units='cm')

#평균대체를 위한 개인함수 
fun_mean_substitute = function(data_mss){
  data_mss %>% 
    mutate_all(
      ~(ifelse(is.na(.),mean(.,na.rm=TRUE),.))
    )
}
#변수들의 평균, 표준편차 계산 및 비교 
M_ms=list()
for (myloc in 1:4){
  M_ms=bind_rows(M_ms,bind_rows(
    fun_95CI_M(myloc,df_mcar%>%fun_mean_substitute(),"MCAR"),
    fun_95CI_M(myloc,df_mar%>%fun_mean_substitute(),"MAR"),
    fun_95CI_M(myloc,df_mnar%>%fun_mean_substitute(),"MNAR")
  ))
}
bind_rows(M_comp,M_ms) %>% 
  mutate(mech=fct_reorder(mech,row_number())) %>% 
  ggplot()+
  geom_errorbar(aes(x=source,ymin=ll95,ymax=ul95,colour=mech),
                width=0.3,size=1,
                position=position_dodge(width=0.3))+
  coord_cartesian(ylim=c(-0.4,0.4))+
  labs(x="Variable",
       y="Confidence intervals for each variable's Mean",
       colour="Mechanism")+
  theme_bw()+
  ggtitle("Mean comparison (Mean substitution applied)")
ggsave("MeanComparison_MeanSubstitution.png",width=16,height=10,units="cm")
#회귀모형 추정결과 비교 
ms_mcar=lm(myformula,df_mcar%>%fun_mean_substitute())
ms_mar=lm(myformula,df_mar%>%fun_mean_substitute())
ms_mnar=lm(myformula,df_mnar%>%fun_mean_substitute())
ms_mss=rbind(confint(ms_mcar),
              confint(ms_mar),
              confint(ms_mnar))
result_ms=ms_mss %>% 
  as_tibble() %>%
  mutate(
    source=rownames(ms_mss),
    ll95=`2.5 %`,ul95=`97.5 %`,
    mech=rep(c("MCAR","MAR","MNAR"),each=4)
  ) %>% select(source:mech)
#평균대체 기법 적용후 분석결과와 완전데이터 분석결과 비교 
bind_rows(model_comp,result_ms) %>% 
  mutate(mech=fct_reorder(mech,row_number())) %>% 
  ggplot()+
  geom_errorbar(aes(x=source,ymin=ll95,ymax=ul95,colour=mech),
                width=0.3,size=1,
                position=position_dodge(width=0.3))+
  labs(x="Intercept and Coefficients",
       y="Confidence intervals for each estimate",
       colour="Mechanism")+
  theme_bw()+
  ggtitle("Parameter estimates (Mean substitution applied)")
ggsave("ParameterEstimates_MeanSubstitution.png",width=16,height=10,units="cm")
#==================================================================#
#회귀투입(regression imputation)
#==================================================================#
#회귀모형 추정 
ols_X1 = lm(X1~Y+X3,data=df_mar %>% drop_na(X1))
#결측값이 포함된 데이터의 X1 변수 예측값 계산 후 대체 
df_temp_mar1 = df_mar %>% filter(is.na(X1))
df_temp_mar1$X1 = predict(ols_X1,df_temp_mar1)
df_temp_mar_regimp = df_mar %>% 
  drop_na(X1) %>%
  bind_rows(df_temp_mar1 %>% select(names(df_mar))) 
lm(Y~X1+X3,df_temp_mar_regimp) %>% confint()
lm(Y~X1+X3,df_comp) %>% confint() 
#회귀투입 후 상관계수 
cor.test(~X1+Y,df_temp_mar_regimp)$estimate
cor.test(~X1+X3,df_temp_mar_regimp)$estimate
#완전 데이터를 대상으로 얻은 상관계수 
cor.test(~X1+Y,df_comp)$estimate
cor.test(~X1+X3,df_comp)$estimate
#mice() 함수 이용: 권장되는 방법은 아님
#위의 과정을 반복(replication)하기 위해 X2변수는 제거
library(mice)
regimp = mice(df_mar %>% select(-X2),method="norm.predict",m=1,print=FALSE)  
df_mar_ri = complete(regimp)   #결측값을 회귀예측값으로 대체하여 완전데이터처럼 변환
lm(Y~X1+X3,df_mar_ri) %>% confint() 
#개인함수 설정
fun_regimp=function(df_mss,myseed,mymethod,mytype){
  #결측값 발생 변수가 2개이기 때문에 seed옵션에 특정한 값을 넣어야 함. 
  #넣지 않으면 적용시마다 결과가 조금씩 바뀜
  df_regimp=mice(df_mss,method=mymethod,
       seed=myseed,m=1,maxit=1,print=FALSE) %>%  
    complete() 
  M_regimp=list()
  for (myloc in 1:4){
    M_regimp=bind_rows(M_regimp,bind_rows(
      fun_95CI_M(myloc,df_mss,mytype)
    ))
  }  #평균의 95%CI 
  result=lm(myformula,data=df_regimp) %>% 
    confint() 
  reg_regimp=result %>% as_tibble() %>%
    mutate(
      source=rownames(result),
      ll95=`2.5 %`,ul95=`97.5 %`,
      mech=mytype
    ) %>% select(source:mech)  #절편과 회귀계수의 95%CI 
  list(M_regimp,reg_regimp)
}

#Mean and 95% CI 
M_regimp=bind_rows(
  fun_regimp(df_mcar,123,"norm.predict","MCAR")[[1]],
  fun_regimp(df_mar,123,"norm.predict","MAR")[[1]],
  fun_regimp(df_mnar,123,"norm.predict","MNAR")[[1]] )
bind_rows(M_comp,M_regimp) %>% 
  mutate(mech=fct_reorder(mech,row_number())) %>% 
  ggplot()+
  geom_errorbar(aes(x=source,ymin=ll95,ymax=ul95,colour=mech),
                width=0.3,size=1,
                position=position_dodge(width=0.3))+
  coord_cartesian(ylim=c(-0.4,0.4))+
  labs(x="Variable",
       y="Confidence intervals for each variable's Mean",
       colour="Mechanism")+
  theme_bw()+
  ggtitle("Mean comparison (Regression imputation applied)")
ggsave("MeanComparison_RegressionImputation.png",width=16,height=10,units="cm")
#Regression estimates 
result_regimp=bind_rows(
  fun_regimp(df_mcar,123,"norm.predict","MCAR")[[2]],
  fun_regimp(df_mar,123,"norm.predict","MAR")[[2]],
  fun_regimp(df_mnar,123,"norm.predict","MNAR")[[2]] )
#회귀투입 후 분석결과와 완전데이터 분석결과 비교 
bind_rows(model_comp,result_regimp) %>% 
  mutate(mech=fct_reorder(mech,row_number())) %>% 
  ggplot()+
  geom_errorbar(aes(x=source,ymin=ll95,ymax=ul95,colour=mech),
                width=0.3,size=1,
                position=position_dodge(width=0.3))+
  labs(x="Intercept and Coefficients",
       y="Confidence intervals for each estimate",
       colour="Mechanism")+
  theme_bw()+
  ggtitle("Parameter estimates (Regression imputation applied)")
ggsave("ParameterEstimates_RegressionImputation.png",width=16,height=10,units="cm")
#==================================================================#
#확률적 회귀투입(stochastic regression imputation)
#==================================================================#
#회귀모형 추정 
ols_X1 = lm(X1~Y+X3,data=df_mar %>% drop_na(X1))
summary(ols_X1) 
#결측값이 포함된 데이터의 X1 변수 예측값 계산하고 오차를 더함 
df_temp_mar1 = df_mar %>% filter(is.na(X1))
set.seed(1234)
df_temp_mar1$X1 = predict(ols_X1,df_temp_mar1) + rnorm(length(df_temp_mar1$X1),0,0.78)
df_temp_mar_storegimp = df_mar %>% 
  drop_na(X1) %>%
  bind_rows(df_temp_mar1 %>% select(names(df_mar))) 
lm(Y~X1+X3,df_temp_mar_storegimp) %>% confint()
lm(Y~X1+X3,df_comp) %>% confint() 
#회귀투입 후 상관계수 
cor.test(~X1+Y,df_temp_mar_storegimp)$estimate
cor.test(~X1+X3,df_temp_mar_storegimp)$estimate
#완전 데이터를 대상으로 얻은 상관계수 
cor.test(~X1+Y,df_comp)$estimate
cor.test(~X1+X3,df_comp)$estimate

fun_stochastic_regimp=fun_regimp
#Mean and 95% CI 
M_stoc_regimp=bind_rows(
  fun_stochastic_regimp(df_mcar,123,"norm.nob","MCAR")[[1]],
  fun_stochastic_regimp(df_mar,123,"norm.nob","MAR")[[1]],
  fun_stochastic_regimp(df_mnar,123,"norm.nob","MNAR")[[1]] )
bind_rows(M_comp,M_stoc_regimp) %>% 
  mutate(mech=fct_reorder(mech,row_number())) %>% 
  ggplot()+
  geom_errorbar(aes(x=source,ymin=ll95,ymax=ul95,colour=mech),
                width=0.3,size=1,
                position=position_dodge(width=0.3))+
  coord_cartesian(ylim=c(-0.4,0.4))+
  labs(x="Variable",
       y="Confidence intervals for each variable's Mean",
       colour="Mechanism")+
  theme_bw()+
  ggtitle("Mean comparison (Stochastic regression imputation applied)")
ggsave("MeanComparison_StochasticRegressionImputation.png",width=16,height=10,units="cm")
#Regression estimates 
result_stoc_regimp=bind_rows(
  fun_stochastic_regimp(df_mcar,123,"norm.nob","MCAR")[[2]],
  fun_stochastic_regimp(df_mar,123,"norm.nob","MAR")[[2]],
  fun_stochastic_regimp(df_mnar,123,"norm.nob","MNAR")[[2]] )
#확률적 회귀투입 후 분석결과와 완전데이터 분석결과 비교 
bind_rows(model_comp,result_stoc_regimp) %>% 
  mutate(mech=fct_reorder(mech,row_number())) %>% 
  ggplot()+
  geom_errorbar(aes(x=source,ymin=ll95,ymax=ul95,colour=mech),
                width=0.3,size=1,
                position=position_dodge(width=0.3))+
  labs(x="Intercept and Coefficients",
       y="Confidence intervals for each estimate",
       colour="Mechanism")+
  theme_bw()+
  ggtitle("Parameter estimates (Stochastic regression imputation applied)")
ggsave("ParameterEstimates_StochasticRegressionImputation.png",width=16,height=10,units="cm")

#개인함수: 각 상황별 편차발생 계산 
fun_storegimp=function(df_mss,myseed,mytype){
  df_storegimp=mice(df_mss,method="norm.nob",
                 seed=myseed,m=1,maxit=1,print=FALSE) %>%  
    complete() 
  standard_estimate=lm(myformula, df_comp) %>% 
    coef()
  result_temp=lm(myformula,data=df_storegimp) %>% 
    coef()
  (result_temp-standard_estimate) %>% 
    as_tibble() %>% 
    mutate(
      source=c("(Intercept)",str_c("X",1:3)),
      mech=mytype,seedNo = myseed
    ) 
}
#상황별로 1000회
result_stochastic_regimp1000 = list() 
for (mytrial in 1:1000){
  result_stochastic_regimp1000 = 
    bind_rows(result_stochastic_regimp1000,
              bind_rows(
                fun_storegimp(df_mcar,mytrial,"MCAR"),
                fun_storegimp(df_mar,mytrial,"MAR"),
                fun_storegimp(df_mnar,mytrial,"MNAR") ))
}
#상황별로 1000회 반복한 결과 요약 
summary_result_stochastic_regimp1000 = result_stochastic_regimp1000 %>% 
  mutate(
    mech=fct_relevel(mech,"MCAR","MAR"),
    source=fct_relevel(source,"(Intercept)",str_c("X",1:3)),
  ) %>% 
  group_by(source,mech) %>% 
  summarise(
    loc025=quantile(value,prob=0.025),
    loc500=quantile(value,prob=0.5),
    loc975=quantile(value,prob=0.975)
  )
summary_result_stochastic_regimp1000
summary_result_stochastic_regimp1000 %>%
  ggplot(aes(x=source,y=loc500,colour=mech))+
  geom_errorbar(aes(ymin=loc025,ymax=loc975),
                width=0.3,position=position_dodge(width=0.3))+
  geom_hline(yintercept=0,color="darkviolet",lty=2)+
  labs(x="Intercept and Coefficients",
       y="Estimate obtained via stochastic reg. imp.
       - that obtained from complete data",
       colour="Mechanism")+
  theme_bw()
ggsave("EstimatesCompare1000_StochasticRegressionImputation.png",width=16,height=10,units="cm")
#==================================================================#
#최대우도(Maximum likelihood)
#==================================================================#
fun_ml_estimate=function(df_mss,mytype){
  fiml_fit = sem("Y~X1+X2+X3",df_mss,
      fixed.x=FALSE,missing="FIML",meanstructure=T)
  parameterestimates(fiml_fit) %>% 
    filter(lhs=="Y") %>% 
    filter(op!="~~") %>%
    mutate(
      source=str_c("X",1:4),
      source=ifelse(source=="X4","(Intercept)",source),
      source=fct_relevel(source,"(Intercept)",str_c("X",1:3)),
      ll95=ci.lower,ul95=ci.upper,mech=mytype
    ) %>% select(source,ll95,ul95,mech) %>% as_tibble() 
}
result_ML=bind_rows(fun_ml_estimate(df_mcar,"MCAR"),
                    fun_ml_estimate(df_mar,"MAR"),
                    fun_ml_estimate(df_mnar,"MNAR"))
#==================================================================#
#다중투입(Multiple imputation) 
#==================================================================#
fun_mi_estimate=function(df_mss,mytype){
  imp_sim = mice(df_mss,seed=1234,m=20,maxit=1,print=FALSE)
  fit_sim = with(imp_sim, lm(Y~X1+X2+X3))
  pool_sim = pool(fit_sim)
  CI95 = summary(pool_sim,conf.int=T) %>% 
    data.frame() %>% 
    mutate(ll95=X2.5..,ul95=X97.5..,
           source=as.character(term),
           mech=mytype) %>% 
    select(source,ll95,ul95,mech)
  CI95 %>% as_tibble()
}
result_MI=bind_rows(
  fun_mi_estimate(df_mcar,"MCAR"),
  fun_mi_estimate(df_mar,"MAR"),
  fun_mi_estimate(df_mnar,"MNAR")) 
#전체 비교 
bind_rows(bind_rows(model_comp,result_lwd) %>% mutate(case="Listwise deletion"),
          bind_rows(model_comp,result_pwd) %>% mutate(case="Pairwise deletion"),
          bind_rows(model_comp,result_ms) %>% mutate(case="Mean substitution"),
          bind_rows(model_comp,result_regimp) %>% mutate(case="Regression imputation"),
          bind_rows(model_comp,result_stoc_regimp) %>% mutate(case="Stochastic reg. imputation"),
          bind_rows(model_comp,result_ML) %>% mutate(case="Maximum likelihood"),
          bind_rows(model_comp,result_MI) %>% mutate(case="Multiple imputation")) %>% 
  mutate(
    mech=fct_relevel(mech,"Complete","MCAR","MAR"),
    case=fct_reorder(case,row_number())) %>% 
  ggplot()+
  geom_errorbar(aes(x=source,ymin=ll95,ymax=ul95,colour=mech),
                width=0.3,size=1,
                position=position_dodge(width=0.3))+
  labs(x="Intercept and Coefficients",
       y="Confidence intervals for each estimate",
       colour="Mechanism")+
  theme_bw()+theme(legend.position="top")+
  facet_grid(~case)
ggsave("ParameterEstimates_MIversusML_etc.png",width=33,height=9,units="cm")
