#===================================================================#
#제2부 ML기반 결측데이터 분석기법 
#제5장 ML기법 개요
#===================================================================#
mypath = "D:/2020_Term2/Category_A_submission/MissingData/Rcode/data"
setwd(mypath)

#ML추정 예시 
library(tidyverse)
set.seed(123)
y = rnorm(200,3,1)
y
mean(y); sd(y)
#무작위로 50개의 결측값 생성
MCAR_NA = sample(1:200,size=200,replace=FALSE)<51
mean(y[MCAR_NA]); sd(y[MCAR_NA])  #결측값인 경우 
mean(y[!MCAR_NA]); sd(y[!MCAR_NA])  #실측값인 경우 
# 실측값의 평균과 표준편차를 이용하여 50개 결측값의 평균과 표준편차를 갖는 데이터를 생성할 경우 
tibble(y2=rnorm(50,mean(y[MCAR_NA]),sd(y[MCAR_NA]))) %>% 
  summarise(mean(y2),sd(y2))

#데이터 불러오기 
df_comp = read_csv("sim_CD.csv")
df_mcar = read_csv("sim_mcar.csv")
df_mar = read_csv("sim_mar.csv") 
df_mnar = read_csv("sim_mnar.csv") 

#ML기법을 이용한 기술통계분석
#시뮬레이션 데이터를 이용한 실습 
#평균과 표준편차 추정 
library(lavaan)
mynull="
Y~1
X1~1
X2~1
X3~1
"
fit_mar=sem(model=mynull,
            data=df_mar,missing="FIML",
            fixed.x=FALSE,  #독립변수가 없기 때문에 평균/표준편차를 구할 때라면 지정하지 않아도 무방
            meanstructure=TRUE)
summary(fit_mar,ci=TRUE)
parameterestimates(fit_mar) 

parameterestimates(fit_mar) %>% 
  filter(op=="~~")  %>% #분산부분만 선택 
  mutate(SD=sqrt(est)) 

#위의 과정을 개인함수로 
fun_ML_descriptive_statistics = function(mymodel, mydata){
  mylavaan_object=sem(model=mymodel,data=mydata,missing="FIML",
                      fixed.x=FALSE,meanstructure=TRUE)
  result_M = parameterestimates(mylavaan_object) %>% 
    filter(op=="~1") %>% 
    mutate(
      vars=lhs,
      type="Mean",
      pest=est,
      ll95=ci.lower,
      ul95=ci.upper
    ) %>% select(vars:ul95)
  result_SD = parameterestimates(mylavaan_object) %>% 
    filter(op=="~~") %>% 
    mutate(
      vars=lhs,
      type="SD",
      pest=sqrt(est),
      ll95=sqrt(ci.lower),
      ul95=sqrt(ci.upper)
    ) %>% select(vars:ul95)
  bind_rows(result_M,result_SD)
}
#네 가지 시뮬레이션 데이터에 적용 
mymodel = "Y~1;X1~1;X2~1;X3~1"
Descriptives_ML=bind_rows(
  fun_ML_descriptive_statistics(mymodel,df_comp) %>% mutate(mech="Complete"),
  fun_ML_descriptive_statistics(mymodel,df_mcar) %>% mutate(mech="MCAR"),
  fun_ML_descriptive_statistics(mymodel,df_mar) %>% mutate(mech="MAR"),
  fun_ML_descriptive_statistics(mymodel,df_mnar) %>% mutate(mech="MNAR")
  ) %>% 
  mutate_at(
    vars(vars,type,mech),
    ~(fct_reorder(., row_number() ))
  ) %>% print()
#시각화   
fig_ML_M=Descriptives_ML %>% 
  filter(type=="Mean") %>% 
  ggplot(aes(x=vars,y=pest,colour=mech))+
  geom_point(size=2,position=position_dodge(width=0.3))+
  geom_errorbar(aes(ymin=ll95,ymax=ul95),width=0.3,position=position_dodge(width=0.3))+
  labs(x="Variables",y="Mean estimates with 95% CI",colour="Missing mechanism")+
  theme_bw()+theme(legend.position="top")
fig_ML_SD=Descriptives_ML %>% 
  filter(type=="SD") %>% 
  ggplot(aes(x=vars,y=pest,colour=mech))+
  geom_point(size=2,position=position_dodge(width=0.3))+
  geom_errorbar(aes(ymin=ll95,ymax=ul95),width=0.3,position=position_dodge(width=0.3))+
  labs(x="Variables",y="SD estimates with 95% CI",colour="Missing mechanism")+
  theme_bw()+theme(legend.position="top")
gridExtra::grid.arrange(fig_ML_M,fig_ML_SD,ncol=1)
png("Comparison_ML_MSD.png",width=16,height=18,units="cm",res=600)
gridExtra::grid.arrange(fig_ML_M,fig_ML_SD,ncol=1)
dev.off()

#상관관계 추정 
mycorr = "
Y ~~ X1+X2+X3
X1 ~~ X2+X3
X2 ~~ X3
"
fit_mar=sem(model=mycorr,data=df_mar,missing="FIML",
             fixed.x=FALSE,  #독립변수가 없기 때문에 평균/표준편차를 구할 때라면 지정하지 않아도 무방
             meanstructure=TRUE)
summary(fit_mar,standard=TRUE) #표준화된 결과도 같이 출력 
standardizedsolution(fit_mar)

standardizedsolution(fit_mar) %>% 
  filter(op=="~~") %>%   #상관계수 부분만 선별 
  select(lhs,rhs,est.std) %>% 
  pivot_wider(names_from=rhs,values_from=est.std) %>% 
  select(lhs,Y,X1,X2,X3)

#위의 과정을 개인함수로 
fun_ML_correlation_statistics = function(mymodel,mydata,mydigit){
  mylavaan_object=sem(model=mymodel,data=mydata,missing="FIML",meanstructure=TRUE)
  cor_mat = standardizedsolution(mylavaan_object) %>% 
    filter(op=="~~") %>% 
    mutate(
      mystar=cut(pvalue,c(-Inf,0.001,0.01,0.05,0.10,Inf),
                 labels=c("***","**","*","+","")) %>% as.character(),
      mystar=ifelse(est.std==1,"",mystar),
      Cor=str_c(format(round(est.std,mydigit),nsmall=mydigit),mystar)) %>% 
    select(lhs,rhs,Cor) %>% 
    pivot_wider(names_from=rhs,values_from=Cor) %>% 
    rename(vars=lhs)
  cor_mat %>% select(vars,cor_mat$vars)}
#네 가지 시뮬레이션 데이터에 적용 
mymodel="Y ~~ X1+X2+X3;X1 ~~ X2+X3;X2 ~~ X3"
Correlations_ML=bind_rows(
  fun_ML_correlation_statistics(mymodel,df_comp,3) %>% mutate(mech="Complete"),
  fun_ML_correlation_statistics(mymodel,df_mcar,3) %>% mutate(mech="MCAR"),
  fun_ML_correlation_statistics(mymodel,df_mar,3) %>% mutate(mech="MAR"),
  fun_ML_correlation_statistics(mymodel,df_mnar,3) %>% mutate(mech="MNAR")
) %>% 
  mutate_at(
    vars(vars,mech),
    ~(fct_reorder(., row_number() ))
  ) %>% print() 

#ML OLS 회귀모형 추정 
myregress="
Y~X1+X2+X3
"
fit_mar=sem(model=myregress, data=df_mar,missing="FIML",
            fixed.x=FALSE, #여기서는 반드시 fixed.x=FALSE옵션 지정 
            meanstructure=TRUE)
summary(fit_mar,ci=TRUE,standard=TRUE)
parameterestimates(fit_mar)   #절편+비표준화 회귀계수
standardizedsolution(fit_mar)   #표준화 회귀계수

#위의 과정을 개인함수로 
fun_ML_regress_statistics = function(mymodel,mydependent,mydata){
  mylavaan_object=sem(model=mymodel,data=mydata,fixed.x=FALSE,
                      missing="FIML", meanstructure=TRUE)
  parameterestimates(mylavaan_object) %>% 
    filter(lhs==mydependent) %>% filter(lhs!=rhs) %>% 
    mutate(
      vars=ifelse(rhs=="","(Intercept)",rhs),
      pest=est,
      ll95=ci.lower,
      ul95=ci.upper
    ) %>% select(vars:ul95)
}
#네 가지 시뮬레이션 데이터에 적용 
mymodel = "Y~X1+X2+X3"
Regression_ML=bind_rows(
  fun_ML_regress_statistics(mymodel,"Y",df_comp) %>% mutate(mech="Complete"),
  fun_ML_regress_statistics(mymodel,"Y",df_mcar) %>% mutate(mech="MCAR"),
  fun_ML_regress_statistics(mymodel,"Y",df_mar) %>% mutate(mech="MAR"),
  fun_ML_regress_statistics(mymodel,"Y",df_mnar) %>% mutate(mech="MNAR")
  ) %>% 
  mutate_at(
    vars(vars,mech),
    ~(fct_reorder(., row_number() ))
  ) %>% print() 

#시각화 
Regression_ML %>% 
  ggplot(aes(x=vars,y=pest,colour=mech))+
  geom_point(size=2,position=position_dodge(width=0.3))+
  geom_errorbar(aes(ymin=ll95,ymax=ul95),width=0.3,position=position_dodge(width=0.3))+
  labs(x="Parameter",y="Point estimates with 95% CI",colour="Missing mechanism")+
  theme_bw()+theme(legend.position="top")
ggsave("Comparison_ML_parameters_regression.png",width=14,height=10,units='cm')


#현실 데이터를 이용한 실습 
mydata = read_csv("anes2008_example.csv")
#인종변수 더미변수로 
df_ML = mydata %>% 
  mutate(
    black=ifelse(race3==1,1,0),others=ifelse(race3==2,1,0) 
  )   #race3 더미변수 2개생성 
#평균/표준편차
mynull="FT_mccain~1;
female~1;black~1;others~1;ageyr~1;educ~1;hhinc~1;readyBP~1
economy~1;environ~1;iraqwar~1"
fun_ML_descriptive_statistics(mynull,df_ML)
#변수의 측정단위가 너무 다르지 않도록 조정 
df_ML = df_ML %>% 
  mutate_at(vars(FT_mccain,ageyr,hhinc),~(0.1*(.)))
#아래의 경우 경고메시지가 나타나지 않음 
fun_ML_descriptive_statistics(mynull,df_ML)
#상관계수 행렬
mycorr="FT_mccain~~female+black+others+ageyr+educ+hhinc+readyBP+economy+environ+iraqwar
female~~black+others+ageyr+educ+hhinc+readyBP+economy+environ+iraqwar
black~~others+ageyr+educ+hhinc+readyBP+economy+environ+iraqwar
others~~ageyr+educ+hhinc+readyBP+economy+environ+iraqwar
ageyr~~educ+hhinc+readyBP+economy+environ+iraqwar
educ~~hhinc+readyBP+economy+environ+iraqwar
hhinc~~readyBP+economy+environ+iraqwar
readyBP~~economy+environ+iraqwar
economy~~environ+iraqwar
environ~~iraqwar
"
fun_ML_correlation_statistics(mycorr,df_ML,3)

MSD_COR=fun_ML_descriptive_statistics(mynull,df_ML) %>% 
  mutate(pest=format(round(pest,3),nsmall=3)) %>% 
  select(vars:pest) %>% 
  pivot_wider(names_from=type,values_from=pest) %>% 
  full_join(fun_ML_correlation_statistics(mycorr,df_ML,3),by="vars") %>% 
  mutate_all(~(ifelse(is.na(.),"",.)))
MSD_COR
MSD_COR %>% 
  write_excel_csv("ML_M_SD_Correlation.csv")

myregress="FT_mccain~female+black+others+ageyr+educ+hhinc+readyBP+economy+environ+iraqwar"
fun_ML_regress_statistics(myregress,"FT_mccain",df_ML)

df_ML_MC = df_ML %>% 
  mutate_at(
    vars(ageyr,educ,hhinc,economy,environ,iraqwar),
    ~(.-mean(.,na.rm=TRUE)
  )) #평균중심화변환(mean-centering)
fun_ML_regress_statistics(myregress,"FT_mccain",df_ML_MC) %>% 
  mutate(
    significance=ifelse(ll95*ul95<0,"Insig","Sig")
  ) 
