# Replication: DeBell (2010) How to Analyze ANES Survey Data 
mypath = "D:/2020_Term2/Category_A_submission/MissingData/Rcode/data"
setwd(mypath)
library(tidyverse)
mydata = haven::read_dta("anes_timeseries_2008.dta") 
mydata

#====== Socio-demographics ======# 
# Gender/Sex
mydata %>% count(V081101)
# Race
mydata %>% count(V081102)
# Age year 
count(mydata, V083215x)
# Education 
count(mydata,V083218x)
# HH income 
count(mydata,V083248x)
d1 = mydata %>% 
  mutate(
    female = ifelse(V081101==2,1,0),
    race3 = ifelse(V081102==1,0,NA),
    race3 = ifelse(V081102==2,1,race3),
    race3 = ifelse(V081102>2,2,race3) %>% as.factor(),
    ageyr = ifelse(V083215x<1,NA,V083215x),
    educ = ifelse(V083218x<1,NA,V083218x),
    hhinc = ifelse(V083248x<1,NA,V083248x)
  ) %>% select(female:hhinc)
summary(d1)
#====== Vote Choice ======# 
# Pre survey 
count(mydata,V083169a)
# Post survey
count(mydata,V085044a) 
count(mydata,V083169a,V085044a) %>% print(n=Inf)
d2=mydata %>% 
  mutate(
    votechoice = ifelse(V085044a==1,1,NA),
    votechoice = ifelse(V085044a==3|V085044a==7,2,votechoice),
    votechoice = ifelse(V085044a==-1,0,votechoice) %>% as.factor(),
    voteobama = ifelse(V085044a==1,1,NA),
    voteobama = ifelse(V085044a==3|V085044a==7,0,voteobama),
    voteplan = ifelse(V083169a==1,1,NA),  #Obama
    voteplan = ifelse(V083169a==2,2,voteplan),  #McCain 
    voteplan = ifelse(V083169a==7|V083169a==5,3,voteplan), #Not for mainstream 
    voteplan = ifelse(V083169a==-8,0,voteplan) #DK 
  ) %>% select(voteplan,votechoice,voteobama)
summary(d2)
#====== Feeling thermometer / Issue-attitude ======# 
# Obama FT
count(mydata, V083037a)
# McCain  FT
count(mydata, V083037b)
# Biden FT
count(mydata, V083039a)
# Palin FT
count(mydata, V083039b)
# Democratic party FT  
count(mydata, V083044a)
# Republican party FT
count(mydata, V083044b)
# Issue evaluation 
# Economy
count(mydata,V083029x)
# Envrionment 
count(mydata,V083031x)
# Iraqwar 
count(mydata,V083033x)
d3 = mydata %>% 
  mutate(
    FT_obama=ifelse(V083037a<0,NA,V083037a),
    FT_mccain=ifelse(V083037b<0,NA,V083037b),
    FT_biden=ifelse(V083039a<0,NA,V083039a),
    FT_palin=ifelse(V083039b<0,NA,V083039b),
    FT_Dparty=ifelse(V083044a<0,NA,V083044a),
    FT_Rparty=ifelse(V083044b<0,NA,V083044b),
    economy=ifelse(V083029x<0,NA,ifelse(V083029x>3,V083029x-1,V083029x)),
    environ=ifelse(V083031x<0,NA,ifelse(V083031x>3,V083031x-1,V083031x)),
    iraqwar=ifelse(V083033x<0,NA,ifelse(V083033x>3,V083033x-1,V083033x)) 
  ) %>% select(FT_obama:iraqwar)
summary(d3)
#======== Others ========#
# black president 
count(mydata,V083172,V083173)
d4 = mydata %>% 
  mutate(
    hopeBP = ifelse(V083172<0,NA,ifelse(V083172==1,1,0)),
    readyBP = ifelse(V083173<0,NA,ifelse(V083173==1,1,0))
  ) %>% select(hopeBP:readyBP)
summary(d4)
# Combining four sets of data #
mydata=bind_cols(d1,d2,d3,d4)
summary(mydata)
# write_excel_csv(mydata, "anes2008_example.csv") 
#======= MAR, some examples =====# 
myfig1=mydata %>% 
  group_by(race3) %>% drop_na(race3) %>% 
  mutate_at(
    vars(hopeBP,readyBP),
    ~(ifelse(is.na(.),1,0))
  ) %>% 
  summarise_at(
    vars(hopeBP,readyBP),
    mean
  ) %>% print(n=Inf)
myfig1 %>% 
  pivot_longer(cols=hopeBP:readyBP) %>% 
  mutate(name=ifelse(name=="hopeBP","Hope that US has African-American President",
                "US ready for African-American President"),
         race3=ifelse(race3==0,"White",ifelse(race3==1,"Black","Others")),
         race3=fct_reorder(race3,row_number())
         ) %>% 
  ggplot(aes(x=race3,y=value))+
  geom_bar(stat='identity')+
  labs(x="Race of respondents",y="Proportion of missing values\n")+
  scale_y_continuous(expand=expand_scale(mult=c(0, .1)))+
  theme_bw()+theme(panel.grid=element_blank())+
  facet_grid(~name)
myfig2=mydata %>% 
  group_by(educ) %>% drop_na(educ) %>% 
  mutate_at(
    vars(FT_obama,FT_mccain,FT_biden,FT_palin,economy,environ,iraqwar),
    ~(ifelse(is.na(.),1,0))
  ) %>% 
  summarise_at(
    vars(FT_obama,FT_mccain,FT_biden,FT_palin,economy,environ,iraqwar),
    mean
  ) %>% print(n=Inf)
myfig2 %>% 
  pivot_longer(cols=FT_obama:iraqwar) %>% 
  mutate(name=ifelse(name=="FT_obama","FT for Obama",name),
         name=ifelse(name=="FT_mccain","FT for McCain",name),
         name=ifelse(name=="FT_biden","FT for Biden",name),
         name=ifelse(name=="FT_palin","FT for Palin",name),
         name=ifelse(name=="economy","Pres. Bush's economy handling",name),
         name=ifelse(name=="environ","Pres. Bush's envrionment handling",name),
         name=ifelse(name=="iraqwar","Pres. Bush's Iraq War handling",name),
         name=fct_reorder(name,row_number()),
         educ=factor(educ,labels=c("1. 0-8 grades","2. 9-12 grades",
                                   "3. 0-12 grades","4. 13+ grades",
                                   "5. Junior or community college",
                                   "6. BA level degree",
                                   "7. Advanced degree"))) %>% 
  ggplot(aes(x=educ,y=value))+
  geom_bar(stat='identity')+
  labs(x="Educational achivements",y="Proportion of missing values\n")+
  scale_y_continuous(expand=expand_scale(mult=c(0, .1)))+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    panel.grid=element_blank()
    )+
  facet_wrap(~name,ncol=4)
# 유권자만 대상으로 연구를 진행한다면 
mydata #N=2322
mydata %>% 
  select(-voteobama,-votechoice) %>% 
  drop_na() #n=1047 (49%)
# 투표자만 대상으로 연구를 진행한다면 
mydata %>% drop_na() #n=876 (38%)

# write_excel_csv(mydata, "anes2008_example.csv") 
