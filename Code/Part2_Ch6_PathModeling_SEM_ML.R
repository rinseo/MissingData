#===================================================================#
#제2부 ML기반 결측데이터 분석기법 
#제6장 ML기법 실습 
#===================================================================#
mypath = "D:/2020_Term2/Category_A_submission/MissingData/Rcode/data"
setwd(mypath)

#패키지 구동 & 데이터 불러오기 
library(tidyverse)
library(lavaan)
#현실 데이터를 이용한 실습 
mydata = read_csv("anes2008_example.csv")
mydata 
df_ML_MC = mydata %>% 
  mutate(
    black=ifelse(race3==1,1,0),others=ifelse(race3==2,1,0) 
  ) %>% 
  mutate_at(vars(starts_with("FT_"),ageyr,hhinc),~(0.1*(.))) %>% 
  mutate_at(
    vars(ageyr,educ,hhinc,economy,environ,iraqwar),
    ~(.-mean(.,na.rm=TRUE)
    ))
#사례1: SUR 
mypath_sur = "
#각 종속변수에 대한 독립변수들의 효과추정 
FT_mccain~female+black+others+ageyr+educ+hhinc+readyBP+economy+environ+iraqwar
FT_obama~female+black+others+ageyr+educ+hhinc+readyBP+economy+environ+iraqwar
#오차항 상관계수 추정
FT_mccain ~~ FT_obama  
"
fit_sur = sem(model=mypath_sur,data=df_ML_MC,
              missing="FIML",
              fixed.x=FALSE,meanstructure=TRUE)
summary(fit_sur,ci=TRUE,standard=TRUE)

parameterestimates(fit_sur) %>% 
  filter(lhs=="FT_mccain"|lhs=="FT_obama") %>% 
  filter(op!="~~") %>% 
  mutate(
    rhs=ifelse(rhs=="","(Intercept)",rhs),
    mystar=cut(pvalue,c(-Inf,0.001,0.01,0.05,0.10,Inf),
               labels=c("***","**","*","+","")) %>% as.character(),
    myreport=str_c(" ",format(round(est,3),nsmall=3),mystar,
                   "\n(",format(round(se,3),nsmall=3),")"),
    source=fct_reorder(rhs,row_number()),
    source=fct_relevel(source,"(Intercept)")
  ) %>% select(source,lhs,myreport) %>% 
  arrange(lhs,source) %>%  
  pivot_wider(names_from=lhs,values_from=myreport) %>%
  write_excel_csv("Summary_Pathmodeling_Case1.csv")
standardizedsolution(fit_sur) %>% 
  filter(str_detect(lhs,"FT_")&str_detect(rhs,"FT_"))

#사례2: Mediation test
#3가지 매개변수를 설정한 경로모형 
mypath_mediate="
economy~female+black+others+ageyr+educ+readyBP+hhinc
environ~female+black+others+ageyr+educ+readyBP+hhinc
iraqwar~female+black+others+ageyr+educ+readyBP+hhinc
FT_mccain~female+black+others+ageyr+educ+readyBP+hhinc+economy+environ+iraqwar
FT_obama~female+black+others+ageyr+educ+readyBP+hhinc+economy+environ+iraqwar
FT_mccain~~FT_obama
economy~~environ+iraqwar
environ~~iraqwar
"
fit_mediate=sem(model=mypath_mediate,data=df_ML_MC,
                missing="FIML",
                fixed.x=FALSE,meanstructure=TRUE)
summary(fit_mediate,ci=TRUE,standard=TRUE)

parameterestimates(fit_mediate) %>% 
  filter(lhs=="environ"|lhs=="economy"|lhs=="iraqwar"|lhs=="FT_mccain"|lhs=="FT_obama") %>% 
  filter(op!="~~") %>% 
  mutate(
    rhs=ifelse(rhs=="","(Intercept)",rhs),
    mystar=cut(pvalue,c(-Inf,0.001,0.01,0.05,0.10,Inf),
               labels=c("***","**","*","+","")) %>% as.character(),
    myreport=str_c(" ",format(round(est,3),nsmall=3),mystar,
                   "\n(",format(round(se,3),nsmall=3),")"),
    source=fct_reorder(rhs,row_number()),
    source=fct_relevel(source,"(Intercept)")
  ) %>% select(source,lhs,myreport) %>% 
  arrange(lhs,source) %>%  
  pivot_wider(names_from=lhs,values_from=myreport) %>%
  write_excel_csv("Summary_Pathmodeling_Case2.csv")
standardizedsolution(fit_mediate) %>% 
  filter(op=="~~") %>% 
  filter((lhs=="environ"|lhs=="economy"|lhs=="iraqwar"|str_detect(lhs,"FT_"))&
           (lhs=="environ"|lhs=="economy"|lhs=="iraqwar"|str_detect(rhs,"FT_"))) %>%
  mutate(vacov=ifelse(lhs==rhs,0,1)) %>% 
  arrange(vacov) %>% 
  mutate(r2=1-est.std, r2=ifelse(vacov==0,r2,NA))

#매개효과(간접효과) 테스트  
mypath_medtest="
economy~female+black+others+ageyr+educ+readyBP+a1*hhinc
environ~female+black+others+ageyr+educ+readyBP+a2*hhinc
iraqwar~female+black+others+ageyr+educ+readyBP+a3*hhinc
FT_mccain~female+black+others+ageyr+educ+readyBP+cr*hhinc+br1*economy+br2*environ+br3*iraqwar
FT_obama~female+black+others+ageyr+educ+readyBP+cd*hhinc+bd1*economy+bd2*environ+bd3*iraqwar
FT_mccain~~FT_obama
economy~~environ+iraqwar
environ~~iraqwar
#Indirect effect : Republican candidate
IEr1 := a1*br1
IEr2 := a2*br2
IEr3 := a3*br3
IEr123 := IEr1+IEr2+IEr3 
#Indirect effect : Democratic candidate
IEd1 := a1*bd1
IEd2 := a2*bd2
IEd3 := a3*bd3
IEd123 := IEd1+IEd2+IEd3 
#Total effect : Republican candidate 
TEr := IEr123+cr
#Total effect : Democratic candidate
TEd := IEd123+cd
"
fit_medtest=sem(model=mypath_medtest,data=df_ML_MC,
                    missing="FIML",
                 fixed.x=FALSE,meanstructure=TRUE)
summary(fit_medtest,ci=TRUE,standard=TRUE)

#부트스트래핑 기반 매개효과 테스트 
fun_estimate = function(object_lavaan){
  return(parameterestimates(object_lavaan)$est)
}
set.seed(1234)  #동일한 결과를 얻기 위해 
obj_boot = bootstrapLavaan(fit_medtest,R=1000,
                           type="parametric",  #다변량 정규분포 가정
                           FUN="fun_estimate")
#saveRDS(obj_boot,"object_lavaan_pathmodel_boot1000.RData")
#부트스트래핑 결과를 데이터 형식으로 
result_boot = as_tibble(obj_boot)
#모수의 이름을 부여
names(result_boot) = parameterestimates(fit_medtest)$lhs
#데이터 형태 확인
result_boot %>% select(IEr1:TEd)
#매개효과/총효과의 부트스트래핑 기반 95% 신뢰구간 추정
result_boot %>% select(IEr1:TEd) %>% 
  pivot_longer(cols=IEr1:TEd) %>% 
  group_by(name) %>% 
  summarise(
    ll95=quantile(value,probs=0.025),
    ul95=quantile(value,probs=0.975)
  )

#사례3: Nonrecursive model 
mypath_nonrecur="
FT_mccain~female+black+others+ageyr+educ+hhinc+readyBP+economy+environ+iraqwar+FT_Rparty
FT_obama~female+black+others+ageyr+educ+hhinc+readyBP+economy+environ+iraqwar+FT_Dparty
FT_obama ~~ FT_mccain
#reciprocal causal relations
FT_mccain ~ FT_obama
FT_obama ~ FT_mccain  
"
fit_nonrecur=sem(model=mypath_nonrecur,data=df_ML_MC,
                missing="FIML",
                fixed.x=FALSE,meanstructure=TRUE)
summary(fit_nonrecur,ci=TRUE,standard=TRUE)

#동등성 제약(equality constraint)
mypath_nonrecur_equal="
FT_mccain~female+black+others+ageyr+educ+hhinc+readyBP+economy+environ+iraqwar+FT_Rparty
FT_obama~female+black+others+ageyr+educ+hhinc+readyBP+economy+environ+iraqwar+FT_Dparty
FT_obama ~~ FT_mccain
#reciprocal causal relations
FT_mccain ~ b*FT_obama
FT_obama ~ b*FT_mccain  
"
fit_nonrecur_equal=sem(model=mypath_nonrecur_equal,data=df_ML_MC,
                 missing="FIML",
                 fixed.x=FALSE,meanstructure=TRUE)
fit_nonrecur_equal


#확증적 인자분석(CFA)
mycfa = "
FT_rep =~ FT_Rparty+FT_mccain+FT_palin
FT_dem =~ FT_Dparty+FT_obama+FT_biden
Eval_Bush =~ economy+environ+iraqwar
"
fit_cfa=sem(model=mycfa,data=df_ML_MC,
            missing="FIML",
            fixed.x=FALSE, #관측된 독립변수가 없기에 지정하지 않아도 무방
            meanstructure=TRUE)
summary(fit_cfa,
        standard=TRUE,  #표준화 계수들(적재치와 상관계수) 
        fit=TRUE)       #모형적합도 지수들 
#잠재변수의 신뢰도 지수들 계산
semTools::reliability(fit_cfa)

#구조방정식 모형 
mysem = "
#measurement model 
FT_rep =~ FT_Rparty+FT_mccain+FT_palin
FT_dem =~ FT_Dparty+FT_obama+FT_biden
Eval_Bush =~ economy+environ+iraqwar
#structural (causal) model 
Eval_Bush ~ female+black+others+ageyr+educ+hhinc
FT_rep~female+black+others+ageyr+educ+hhinc+readyBP+Eval_Bush
FT_dem~female+black+others+ageyr+educ+hhinc+readyBP+Eval_Bush
"
fit_sem=sem(model=mysem,data=df_ML_MC,
            missing="FIML",
            fixed.x=FALSE,meanstructure=TRUE)
summary(fit_sem,ci=TRUE,
        standard=TRUE,  #표준화 계수들(적재치와 상관계수) 
        fit=TRUE)       #모형적합도 지수들 
fitmeasures(fit_sem)

parameterestimates(fit_sem) %>% 
  filter(lhs=="FT_rep"|lhs=="FT_dem"|lhs=="Eval_Bush") %>% 
  filter(op!="~~") %>% 
  filter(op!="=~") %>% 
  mutate(
    rhs=ifelse(rhs=="","(Intercept)",rhs),
    mystar=cut(pvalue,c(-Inf,0.001,0.01,0.05,0.10,Inf),
               labels=c("***","**","*","+","")) %>% as.character(),
    myreport=str_c(" ",format(round(est,3),nsmall=3),mystar,
                   "\n(",format(round(se,3),nsmall=3),")"),
    source=fct_reorder(rhs,row_number()),
    source=fct_relevel(source,"(Intercept)")
  ) %>% select(source,lhs,myreport) %>% 
  arrange(lhs,source) %>%  
  pivot_wider(names_from=lhs,values_from=myreport) %>%
  write_excel_csv("Summary_SEM.csv")
# 매개효과 테스트 
mysem_medtest = "
#measurement model 
FT_rep =~ FT_Rparty+FT_mccain+FT_palin
FT_dem =~ FT_Dparty+FT_obama+FT_biden
Eval_Bush =~ economy+environ+iraqwar
#structural (causal) model 
Eval_Bush ~ female+black+others+ageyr+educ+a*hhinc
FT_rep~female+black+others+ageyr+educ+cr*hhinc+readyBP+br*Eval_Bush
FT_dem~female+black+others+ageyr+educ+cd*hhinc+readyBP+bd*Eval_Bush
#Indirect effect for Republicans 
IEr := a*br
#Total effect for Republicans  
TEr := IEr+cr
#Indirect effect for Democrats 
IEd := a*bd
#Total effect for Democrats 
TEd := IEd+cd
"
fit_sem_medtest=sem(model=mysem_medtest,data=df_ML_MC,
                    missing="FIML",
                    fixed.x=FALSE,meanstructure=TRUE)
#Defined Parameters: 부분만 
parameterestimates(fit_sem_medtest) %>% 
  filter(op==":=")  

#부트스트래핑 기반 매개효과 테스트 
set.seed(1234)  #동일한 결과를 얻기 위해 
obj_boot = bootstrapLavaan(fit_sem_medtest,R=1000,
                           type="parametric",  #다변량 정규분포 가정
                           FUN="fun_estimate")
#saveRDS(obj_boot,"object_lavaan_SEM_boot1000.RData")
#obj_boot = readRDS("object_lavaan_SEM_boot1000.RData")
#부트스트래핑 결과를 데이터 형식으로 
result_boot = as_tibble(obj_boot)
#모수의 이름을 부여
names(result_boot) = parameterestimates(fit_sem_medtest)$lhs
#매개효과/총효과의 부트스트래핑 기반 95% 신뢰구간 추정
result_boot %>% select(IEr:TEd) %>% 
  pivot_longer(cols=IEr:TEd) %>% 
  group_by(name) %>% 
  summarise(
    ll95=quantile(value,probs=0.025),
    ul95=quantile(value,probs=0.975)
  )
