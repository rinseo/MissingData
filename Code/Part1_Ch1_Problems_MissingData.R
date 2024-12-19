#===================================================================#
#제1부 결측데이터 분석 개요
#제1장 결측데이터 이해를 위한 간단한 시뮬레이션
#===================================================================#
library(tidyverse)  # 데이터 관리를 위해 실시 
library(magrittr)   # %$% operator 
# N = 1000 (집단별 500)
set.seed(1234)  # 동일한 결과를 원한다면 반드시 실행 
my_n=500
myY=c(rnorm(my_n,5,1),
      rnorm(my_n,4,1))
myX=c(rep(0,my_n),rep(1,my_n))
mydata=tibble(Y=myY,X=myX)
summary(mydata)
mydata %>% 
  group_by(X) %>% 
  summarise(M=mean(Y),SD=sd(Y),N=length(Y))

#=== 결측값 발생비율 50% 
set.seed(1234) #동일한 결과를 얻고자 한다면 반드시 실행 
mydata1=mydata %>% 
  mutate(
    miss_mech1=sample(c(rep(1,1),rep(0,1)),size=2*my_n,replace=TRUE),
    Y1=ifelse(miss_mech1==0,NA,Y),
    Y2=ifelse(Y<quantile(Y,prob=0.5),NA,Y),
    miss_mech3=c(sample(c(rep(1,75),rep(0,15)),size=my_n,replace=TRUE),
                 sample(c(rep(1,25),rep(0,85)),size=my_n,replace=TRUE)),
    Y3=ifelse(miss_mech3==0,NA,Y)
  ) %>% select(-miss_mech1,-miss_mech3)  #결측값 발생 메커니즘 변수는 삭제 
summary(mydata1)
mydata1 %>% 
  group_by(X) %>% 
  summarise(M=mean(Y),SD=sd(Y),N=length(Y)/my_n,
            M1=mean(Y1,na.rm=TRUE),SD1=sd(Y1,na.rm=TRUE),N1=sum(!is.na(Y1))/my_n,
            M2=mean(Y2,na.rm=TRUE),SD2=sd(Y2,na.rm=TRUE),N2=sum(!is.na(Y2))/my_n,
            M3=mean(Y3,na.rm=TRUE),SD3=sd(Y3,na.rm=TRUE),N3=sum(!is.na(Y3))/my_n)

bind_rows(
  mydata1 %>% drop_na(Y) %>% summarise(mean(X),sd(X)) %>% mutate(cond="Y"),
  mydata1 %>% drop_na(Y1) %>% summarise(mean(X),sd(X)) %>% mutate(cond="Y1"),
  mydata1 %>% drop_na(Y2) %>% summarise(mean(X),sd(X)) %>% mutate(cond="Y2"),
  mydata1 %>% drop_na(Y3) %>% summarise(mean(X),sd(X)) %>% mutate(cond="Y3")
)

missing50=c(t.test(Y~X,data=mydata1,var.equal=TRUE) %$% statistic,
         t.test(Y1~X,data=mydata1,var.equal=TRUE) %$% statistic,
         t.test(Y2~X,data=mydata1,var.equal=TRUE) %$% statistic,
         t.test(Y3~X,data=mydata1,var.equal=TRUE) %$% statistic)
tibble(cond=c("Y","Y1","Y2","Y3"),tvalue=missing50)

#=== 결측값 발생비율 20% 
set.seed(1234)
mydata2=mydata %>% 
  mutate(
    miss_mech1=sample(c(rep(1,2*2),rep(0,1)),size=2*my_n,replace=TRUE),
    Y1=ifelse(miss_mech1==0,NA,Y),
    Y2=ifelse(Y<quantile(Y,prob=0.2),NA,Y),
    miss_mech3=c(sample(c(rep(1,2*85),rep(0,5)),size=my_n,replace=TRUE),
                 sample(c(rep(1,2*50),rep(0,65)),size=my_n,replace=TRUE)),
    Y3=ifelse(miss_mech3==0,NA,Y)
  ) %>% select(-miss_mech1,-miss_mech3)
summary(mydata2)

mydata2 %>% 
  group_by(X) %>% 
  summarise(M=mean(Y),SD=sd(Y),N=length(Y)/my_n,
            M1=mean(Y1,na.rm=TRUE),SD1=sd(Y1,na.rm=TRUE),N1=sum(!is.na(Y1))/my_n,
            M2=mean(Y2,na.rm=TRUE),SD2=sd(Y2,na.rm=TRUE),N2=sum(!is.na(Y2))/my_n,
            M3=mean(Y3,na.rm=TRUE),SD3=sd(Y3,na.rm=TRUE),N3=sum(!is.na(Y3))/my_n)

bind_rows(
  mydata2 %>% drop_na(Y) %>% summarise(mean(X),sd(X)) %>% mutate(cond="Y"),
  mydata2 %>% drop_na(Y1) %>% summarise(mean(X),sd(X)) %>% mutate(cond="Y1"),
  mydata2 %>% drop_na(Y2) %>% summarise(mean(X),sd(X)) %>% mutate(cond="Y2"),
  mydata2 %>% drop_na(Y3) %>% summarise(mean(X),sd(X)) %>% mutate(cond="Y3")
)

missing25=c(t.test(Y~X,data=mydata2,var.equal=TRUE) %$% statistic,
          t.test(Y1~X,data=mydata2,var.equal=TRUE) %$% statistic,
          t.test(Y2~X,data=mydata2,var.equal=TRUE) %$% statistic,
          t.test(Y3~X,data=mydata2,var.equal=TRUE) %$% statistic)
tibble(cond=c("Y","Y1","Y2","Y3"),tvalue=missing25)
