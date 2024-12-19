#===================================================================#
#제1부 결측데이터 분석 개요
#제3장 예시데이터를 이용한 결측데이터 점검
#===================================================================#
mypath = "D:/2020_Term2/Category_A_submission/MissingData/Rcode/data"
setwd(mypath)
library(tidyverse)
read_csv("sim_CD.csv") %>% summary()
read_csv("sim_MCAR.csv") %>% summary()
read_csv("sim_MAR.csv") %>% summary()
read_csv("sim_MNAR.csv") %>% summary()

mydata = read_csv("anes2008_example.csv") 
mydata 
mydata = mydata %>% 
  mutate_at(
    vars(female,race3,voteplan,votechoice,hopeBP,readyBP),
    ~(factor(.))
  )  #범주형 변수는 factor 변수로 
mydata

library(naniar)   #결측데이터 분석 
library(visdat)   #결측데이터 시각화 
library(finalfit)   #결측데이터 시각화 및 분석 

# 결측값의 칸수, 실측값의 칸수
n_miss(mydata); n_complete(mydata)
# 결측값의 칸수, 실측값의 칸수 비율 
prop_miss(mydata); prop_complete(mydata)

# 변수별 결측데이터 요약통계치 
miss_var_summary(mydata)
# 사례별 결측데이터 요약통계치 
miss_case_summary(mydata)
miss_case_summary(mydata) %>% 
  ggplot(aes(x=n_miss))+
  geom_histogram()+
  scale_x_continuous(breaks=0:16)+
  scale_y_continuous(breaks=100*(0:10))+
  labs(x="Number of missing values per case",y="Frequency")+
  theme_bw()
ggsave("histogram_missing_per_case1.png",width=12,height=9,units='cm')

# 한장의 그림으로 요약 
vis_miss(mydata)+
  theme(axis.text.x.top=element_text(angle=90)) #X축의 앵글을 조정
ggsave("missingdata_pattern_overall.png",width=12,height=11,units='cm')
# 그림을 좀더 보기 좋게 다듬으면 
mydata %>% 
  add_prop_miss() %>%  #사례별 결측값 빈도 계산 
  arrange(prop_miss_all) %>%   #사례별 결측값 빈도별로 정렬 
  select(-prop_miss_all) %>% 
  vis_miss(sort_miss=T)+  #변수별 결측값 발생 빈도별로 정렬 
  theme(axis.text.x.top=element_text(angle=90))
ggsave("missingdata_pattern_overall_sort.png",width=12,height=11,units='cm')
#변수의 속성구분 
mydata %>% 
  add_prop_miss() %>%  #사례별 결측값 빈도 계산 
  arrange(prop_miss_all) %>%   #사례별 결측값 빈도별로 정렬 
  select(miss_var_summary(mydata)$variable) %>% #변수별 결측값 발생 빈도별로 정렬 
  vis_dat()+
  theme(axis.text.x.top=element_text(angle=90),
        legend.position="bottom")
ggsave("missingdata_pattern_overall_type.png",width=12,height=11,units='cm')

#결측데이터 탐색 
mydata %>% 
  group_by(female) %>%
  miss_var_summary() 
#성별 비교 표
mydata %>% 
  group_by(female) %>%
  miss_var_summary() %>% 
  select(variable,pct_miss,female) %>% 
  arrange(female) %>% 
  pivot_wider(names_from="female",values_from="pct_miss")
#성별 비교 그래프 
mydata %>% 
  group_by(female) %>%
  miss_var_summary() %>% 
  mutate(variable=fct_reorder(variable, n_miss)) %>% 
  ggplot(aes(x=variable,y=pct_miss,fill=female))+
  geom_bar(stat='identity',position='dodge')+
  coord_flip(ylim=c(0,40))+
  scale_y_continuous(breaks=5*(0:8),expand=expansion(add=0))+
  scale_fill_discrete(labels=c("Male","Female"))+
  labs(x="variable",y="Percent of missing values per case",fill="Gender")+
  theme_bw()+
  theme(legend.position="bottom")
ggsave("missingdata_by_gender.png",width=14,height=11,units='cm')

#변수를 실측값(!NA)/결측값(NA)으로 이분화
mydata %>% bind_shadow() 
#두 변수의 결측값 발생여부 교차표 
mydata %>% bind_shadow() %>% 
  count(FT_obama_NA,hopeBP_NA)
#확률밀도분포를 통해 시각화 
mydata %>% bind_shadow() %>% 
  ggplot(aes(x=FT_obama,fill=hopeBP_NA))+
  geom_density(alpha=0.5)+
  scale_x_continuous(breaks=10*(0:100),expand=expansion(mult=0,add=0))+
  scale_y_continuous(breaks=0.002*(0:10),expand=expansion(mult=0,add=0))+
  scale_fill_discrete(labels=c("Observed","Missing"))+
  coord_cartesian(ylim=c(0,0.020))+
  labs(x="Feeling thermometer for B. Obama",y="Density",
       fill="Hoping black president question")+
  theme_bw()+
  theme(legend.position="top")
ggsave("Distribution_FT_obama_hopeBP.png",width=12,height=9,units='cm')

#여러 변수들을 동시에 비교
var_dep = "FT_obama"
var_exp = c("educ","race3","hopeBP")
mydata %>% 
  missing_pairs(dependent=var_dep,
                explanatory=var_exp)
ggsave("Comparison_missing.png",width=20,height=17,units='cm')

mydata %>% 
  missing_pairs(dependent=var_dep,
                explanatory=var_exp,
                position="fill")
ggsave("Comparison_missing_fill.png",width=20,height=17,units='cm')
#수치제시 
mydata %>% 
  missing_compare(dependent=var_dep,
                     explanatory=var_exp)
#결측값 패턴
png("Comparison_missing_pattern.png",width=18,height=30,units="cm",pointsize=9,res=600)
mydata %>% 
  missing_pattern(dependent=names(mydata %>% select(starts_with("FT_"))))
dev.off()

#FT_obama 변수의 결측값 발생과 관련된 다른 변수들
bivariate_stat=mydata %>% 
  missing_compare(dependent="FT_obama",
                  explanatory=names(mydata %>% select(-FT_obama)))
bivariate_stat %>% 
  as_tibble() %>% 
  mutate(
    pvalue=ifelse(p=="<0.001","0.000",p),
    pvalue=as.numeric(pvalue)
  ) %>% 
  fill(pvalue) %>% 
  arrange(pvalue) %>% print(n=40)

#mvnmle, BaylorEdPsych 두 패키지를 먼저 수동으로 설치
#mvnmle: https://cran.r-project.org/src/contrib/Archive/mvnmle/
#BaylorEdPsych:  https://cran.r-project.org/src/contrib/Archive/BaylorEdPsych/

#아래는 실행불가 
BaylorEdPsych::LittleMCAR(mydata %>% select(starts_with("FT_")))
#아래는 실행가능 
BaylorEdPsych::LittleMCAR(mydata %>% select(female,starts_with("FT_")))

#시뮬레이션 데이터를 이용하여 테스트 
Little_MCAR_test_function=function(testdata){
  test_mcar = BaylorEdPsych::LittleMCAR(testdata)
  str_c(
    "X2=",
    format(round(test_mcar$chi.square,3),nsmall=3),
    ", df=",
    test_mcar$df,
    ", p=",
    format(round(test_mcar$p.value,3),nsmall=3)
  )
}
setwd("D:/2020_Term2/Category_A_submission/MissingData/Rcode/data")
Little_MCAR_test_function( read.csv("sim_MCAR.csv") )
Little_MCAR_test_function( read.csv("sim_MAR.csv") )
Little_MCAR_test_function( read.csv("sim_MNAR.csv") )

set.seed(1234)
Little_MCAR_test_function( 
  read.csv("sim_MAR.csv")[sample(1:1000,size=100,replace=F),]  )
Little_MCAR_test_function( 
  read.csv("sim_MAR.csv")[sample(1:1000,size=100,replace=F),]  )
Little_MCAR_test_function( 
  read.csv("sim_MAR.csv")[sample(1:1000,size=100,replace=F),]  )
Little_MCAR_test_function( 
  read.csv("sim_MAR.csv")[sample(1:1000,size=100,replace=F),]  )
Little_MCAR_test_function( 
  read.csv("sim_MAR.csv")[sample(1:1000,size=100,replace=F),]  )
