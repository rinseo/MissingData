#===================================================================#
#제2부 ML기반 결측데이터 분석기법 
#제7장 보조변수 포함 ML기법 실습 
#===================================================================#
mypath = "D:/2020_Term2/Category_A_submission/MissingData/Rcode/data"
setwd(mypath)

#데이터 불러오기 
library(tidyverse)
library(lavaan)
#시뮬레이션 데이터를 이용한 실습 
df_mar = read_csv("sim_mar.csv")
#포화상관모형
mymar_scm = "
Y~X1+X2
X3~~X1+X2
X3~~Y
X1~~X2
"
fit_mar_scm=sem(model=mymar_scm,data=df_mar,
                missing="FIML",
                fixed.x=FALSE,meanstructure=TRUE)
summary(fit_mar_scm,
        ci=TRUE,standard=TRUE)

#현실 데이터를 이용한 실습 
mydata = read_csv("anes2008_example.csv")
df_ML_MC = mydata %>% 
  mutate(
    black=ifelse(race3==1,1,0),others=ifelse(race3==2,1,0) 
  ) %>% 
  mutate_at(vars(starts_with("FT_"),ageyr,hhinc),~(0.1*(.))) %>% 
  mutate_at(
    vars(ageyr,educ,hhinc,economy,environ,iraqwar),
    ~(.-mean(.,na.rm=TRUE)
    ))

#보조변수 투입없이 추정(보조변수 불포함 분석방법)
mypath_no_aux = "
FT_biden~female+black+others+ageyr+educ+hhinc+readyBP+economy+environ+iraqwar
"
fit_no_aux=sem(model=mypath_no_aux,data=df_ML_MC,
               missing="FIML",
               fixed.x=FALSE,meanstructure=TRUE)
summary(fit_no_aux,ci=TRUE,standard=TRUE)

#보조변수(FT_obama) 활용: 보조변수 포함 분석방법, 포화상관모형 
mypath_ys_aux = "
FT_biden~female+black+others+ageyr+educ+hhinc+readyBP+economy+environ+iraqwar
# Correlations between auxiliary variable and variables of interest 
FT_obama~~FT_biden+female+black+others+ageyr+educ+hhinc+readyBP+economy+environ+iraqwar
# Correlations between predictors 
female ~~ black+others+ageyr+educ+hhinc+readyBP+economy+environ+iraqwar
black ~~ others+ageyr+educ+hhinc+readyBP+economy+environ+iraqwar
others ~~ ageyr+educ+hhinc+readyBP+economy+environ+iraqwar
ageyr ~~ educ+hhinc+readyBP+economy+environ+iraqwar
educ ~~ hhinc+readyBP+economy+environ+iraqwar
hhinc ~~ readyBP+economy+environ+iraqwar
readyBP ~~ economy+environ+iraqwar
economy ~~ environ+iraqwar
environ ~~ iraqwar
"
fit_ys_aux=sem(model=mypath_ys_aux,data=df_ML_MC,
               missing="FIML",
               fixed.x=FALSE,meanstructure=TRUE)
summary(fit_ys_aux,ci=TRUE,standard=TRUE)

#여러 보조변수들이 투입되는 경우 프로그래밍이 매우 번잡. 
#semTools 패키지의 sem.auxiliary() 함수를 이용하면 편리  
library(semTools)
aux_vars = c('FT_obama','FT_mccain','FT_palin','FT_Rparty','FT_Dparty')
fit_ys_auxS = sem.auxiliary(model=mypath_no_aux,aux=aux_vars,
                            data=df_ML_MC,missing="FIML",
                            fixed.x=FALSE,meanstructure=TRUE)
summary(fit_ys_auxS,
        ci=TRUE,standard=TRUE)

result_no_aux=parameterestimates(fit_no_aux) %>% 
  filter(lhs=="FT_biden") %>% 
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
  pivot_wider(names_from=lhs,values_from=myreport)
result_ys_aux=parameterestimates(fit_ys_aux) %>% 
  filter(lhs=="FT_biden") %>% 
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
  pivot_wider(names_from=lhs,values_from=myreport)
result_ys_auxS=parameterestimates(fit_ys_auxS) %>% 
  filter(lhs=="FT_biden") %>% 
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
  pivot_wider(names_from=lhs,values_from=myreport)
results3=bind_cols(result_no_aux,result_ys_aux[2],result_ys_auxS[2])
names(results3)[2:4]=c("No","AuxS","AuxP")
results3 %>%
  write_excel_csv("Summary_Regress_auxiliary.csv")

#보조변수 불포함 SEM 분석모형
mysem_no_aux = "
#Measurement model 
FT_reps =~ FT_mccain+FT_palin
FT_dems =~ FT_obama+FT_biden
Eval_Bush =~ economy+environ+iraqwar 
#Structural model 
Eval_Bush ~ female
FT_reps ~ Eval_Bush+female
FT_dems ~ Eval_Bush+female
"
fit_sem_no_aux=sem(model=mysem_no_aux,data=df_ML_MC,
                   missing="FIML",
                   fixed.x=FALSE,meanstructure=TRUE)
summary(fit_sem_no_aux,ci=TRUE,standard=TRUE,fit=TRUE)
#보조변수 포함 SEM 
fit_sem_ys_aux1=sem.auxiliary(model=mysem_no_aux,
                             aux=c('hhinc'),
                             data=df_ML_MC,missing="FIML",
                             fixed.x=FALSE,meanstructure=TRUE)
summary(fit_sem_ys_aux1,ci=TRUE,standard=TRUE,fit=TRUE)
#여러 보조변수인 경우 
fit_sem_ys_aux2=sem.auxiliary(model=mysem_no_aux,
                             aux=c('hhinc','educ'),
                             data=df_ML_MC,missing="FIML",
                             fixed.x=FALSE,meanstructure=TRUE)
summary(fit_sem_ys_aux2,ci=TRUE,standard=TRUE,fit=TRUE)
#보조변수 포함 SEM 
fit_sem_ys_aux_problem=sem.auxiliary(model=mysem_no_aux,
                             aux=c('FT_Dparty'),
                             data=df_ML_MC,missing="FIML",
                             fixed.x=FALSE,meanstructure=TRUE)
summary(fit_sem_ys_aux_problem,standard=TRUE)
