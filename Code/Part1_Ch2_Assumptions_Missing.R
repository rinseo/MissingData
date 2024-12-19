#===================================================================#
#제1부 결측데이터 분석 개요
#제2장 결측데이터 메커니즘 가정
#===================================================================#
mypath = "D:/2020_Term2/Category_A_submission/MissingData/Rcode/data"
setwd(mypath)
#======================================================#
#완전데이터 생성 
#======================================================#
library(tidyverse)  #data management 
library(MASS)    #data generation 
select = dplyr::select 
mysigma=matrix(data=c(1,0.4,0.4,0.4,0.4,0.4,0.4,0.4, 
                      0.4,1,0.4,0.4,0.4,0.4,0.4,0.4, 
                      0.4,0.4,1,0.4,0.4,0.4,0.4,0.4,
                      0.4,0.4,0.4,1,0.4,0.4,0.4,0.4,
                      0.4,0.4,0.4,0.4,1,0.4,0.4,0.4,
                      0.4,0.4,0.4,0.4,0.4,1,0.4,0.4,
                      0.4,0.4,0.4,0.4,0.4,0.4,1,0.7,
                      0.4,0.4,0.4,0.4,0.4,0.4,0.7,1),nrow=8)
set.seed(1234)
complete_data=mvrnorm(n=400,mu=rep(4,8),Sigma=mysigma) %>% 
  data.frame() %>% as_tibble() 
names(complete_data)=c(str_c("X",1:7),"Y")
write_excel_csv(complete_data,"complete_data_part2_ch2.csv")
#======================================================#
#MCAR 메커니즘 
#======================================================#
library(tidyverse)  #data management 
library(Hmisc)  #correlation matrix 
summarize = Hmisc::summarize
complete_data=read_csv("complete_data_part2_ch2.csv")
rcorr(as.matrix(complete_data))
lm(Y~X1+X2+X3+X4+X5+X6+X7,data=complete_data) %>% summary()
COM_OLS_CI95 = lm(Y~X1+X2+X3+X4+X5+X6+X7,data=complete_data) %>% 
  confint() %>%  # 추정치의 95% 신뢰구간
  data.frame() %>% mutate(source=rownames(.)) %>% 
  rename(ll95=X2.5..,ul95=X97.5..) %>% 
  mutate(N_MCAR_variable=0) %>% as_tibble() 
COM_OLS_CI95
#MCAR 가정상황에서 효율성 저하문제
#만약 변수별로 20%의 결측값이 MCAR 가정하에서 발생할 경우 
MCAR_mechanism20=function(myvar){
  temp_mcar = sample(c(rep(1,320),rep(0,80)),size=400,replace=FALSE)
  myvar[temp_mcar==0] = NA 
  myvar
}
fun_MCAR_mech=function(myrange){
  set.seed(123)
  MSS_OLS_CI95=complete_data %>% mutate_at(vars(1:myrange),MCAR_mechanism20) %>% 
    lm(Y~X1+X2+X3+X4+X5+X6+X7,.) %>% 
    confint() %>%  # 추정치의 95% 신뢰구간
    data.frame() %>% mutate(source=rownames(.)) %>% 
    rename(ll95=X2.5..,ul95=X97.5..) %>% 
    mutate(N_MCAR_variable=length(1:myrange)) %>% as_tibble() 
  MSS_OLS_CI95
}
myresult=COM_OLS_CI95 
for (i in 1:7){
  myresult=bind_rows(myresult,fun_MCAR_mech(i))  #1개 변수부터 7개 변수까지 
}
myfig=myresult %>% filter(source=="X7") %>% 
  mutate(pest=0.5*(ll95+ul95)) #점추정치 
myfig %>% ggplot(aes(x=N_MCAR_variable,y=pest))+
  geom_point(size=2,color="red")+
  geom_errorbar(aes(ymin=ll95,ymax=ul95),width=0.3)+
  scale_x_continuous(breaks=0:7)+
  scale_y_continuous(breaks=round(myfig$pest,2))+
  geom_hline(yintercept=c(myfig$ll95[1],myfig$ul95[1]),lty=2,color="blue")+
  labs(x="Number of variables in which MCAR-assumed missing occurs (20%)",
       y="Point estimates with 95% CI")+
  theme_bw()
ggsave("Efficiency_change_MCAR20.png",width=14,height=9,units='cm')

#만약 변수별로 40%의 결측값이 MCAR 가정하에서 발생할 경우 
MCAR_mechanism40=function(myvar){
  temp_mcar = sample(c(rep(1,240),rep(0,160)),size=400,replace=FALSE)
  myvar[temp_mcar==0] = NA 
  myvar
}
fun_MCAR_mech=function(myrange){
  set.seed(123)
  MSS_OLS_CI95=complete_data %>% mutate_at(vars(1:myrange),MCAR_mechanism40) %>% 
    lm(Y~X1+X2+X3+X4+X5+X6+X7,.) %>% 
    confint() %>%  # 추정치의 95% 신뢰구간
    data.frame() %>% mutate(source=rownames(.)) %>% 
    rename(ll95=X2.5..,ul95=X97.5..) %>% 
    mutate(N_MCAR_variable=length(1:myrange)) %>% as_tibble() 
  MSS_OLS_CI95
}
myresult=COM_OLS_CI95 
for (i in 1:7){
  myresult=bind_rows(myresult,fun_MCAR_mech(i))  #1개 변수부터 7개 변수까지 
}
myfig=myresult %>% filter(source=="X7") %>% 
  mutate(pest=0.5*(ll95+ul95)) #점추정치 
myfig %>% ggplot(aes(x=N_MCAR_variable,y=pest))+
  geom_point(size=2,color="red")+
  geom_errorbar(aes(ymin=ll95,ymax=ul95),width=0.3)+
  scale_x_continuous(breaks=0:7)+
  scale_y_continuous(breaks=round(myfig$pest,1))+
  geom_hline(yintercept=c(myfig$ll95[1],myfig$ul95[1]),lty=2,color="blue")+
  labs(x="Number of variables in which MCAR-assumed missing occurs (40%)",
       y="Point estimates with 95% CI")+
  theme_bw()
ggsave("Efficiency_change_MCAR40.png",width=14,height=9,units='cm')

#======================================================#
#분산 동질성과 이질성 사례 
#======================================================#
library(tidyverse)  #data management 
set.seed(1234)  #동일한 결과를 얻고자 하면 
y1=rnorm(100,3,1)
y2=rnorm(100,5,1)
sd(y1); sd(y2)  #두 집단의 분산은 거의 동일 
sd(c(y1,y2))    #전체집단의 분산은 이질적 
mysim=tibble(Y=c(y1,y2),X=rep(1:2,each=100))
lm(Y~1, mysim) %>% summary()   #Residual에 주목: 이질성 존재 
lm(Y~factor(X), mysim) %>% summary()  #Residual에 주목: 이질성 해소 

#MAR 메커니즘 
set.seed(1234)  #동일한 결과를 얻고자 하면 
miss1=rbinom(n=100,size=1,prob=0.8)
miss2=rbinom(n=100,size=1,prob=0.5)
mysim=tibble(
  Y_com=c(y1,y2),miss=c(miss1,miss2),
  Y_obs=ifelse(miss==1,Y_com,NA),
  X=rep(1:2,each=100)
  )
mysim %>% group_by(X) %>%
  summarise(sum(miss))
lm(Y_com~1, mysim) %>% summary()
lm(Y_obs~1, mysim) %>% summary() 
lm(Y_com~factor(X), mysim) %>% summary()
lm(Y_obs~factor(X), mysim) %>% summary()

#MNAR 메커니즘 
library(tidyverse)  #data management 
library(magrittr)   #%$% operator 
set.seed(1234)
mnar_known=round(mvrnorm(n=200,mu=c(115,70),
                         Sigma=matrix(data=c(100,15,15,100),nrow=2))) %>% 
  data.frame() %>% as_tibble() %>% 
  mutate(
    toeic_com = 5*X1, 
    jobperf_com = X2, 
    toeic_obs = ifelse(toeic_com<600,NA,toeic_com),
    jobperf_obs_realize=ifelse(toeic_com<600,NA,jobperf_com),
    selected=ifelse(is.na(jobperf_obs_realize),"Missing","Observed")) %>% 
  arrange(desc(jobperf_obs_realize)) %>% 
  select(-X1,-X2) 
mnar_known %>% write_excel_csv("MNAR_example.csv")

ggplot() + 
  geom_point(data=mnar_known, 
             aes(x=toeic_com,y=jobperf_com,color=selected), 
             alpha=0.5,size=3)+
  geom_smooth(data=mnar_known,
              aes(x=toeic_com, y=jobperf_com),
              method='lm',color="purple",se=F)+
  geom_smooth(data=mnar_known%>%filter(selected=="Observed"),
              aes(x=toeic_obs, y=jobperf_obs_realize),
              method='lm',color="blue",se=F)+
  labs(x="TOEIC score",y="Job performance",color="Data type")+
  theme_bw()
ggsave("MNAR_example.png",width=14,height=9,units='cm')
cor.test(~jobperf_com+toeic_com, mnar_known) %$% estimate %>% round(2)
cor.test(~jobperf_obs_realize+toeic_obs, mnar_known) %$% estimate %>% round(2)
