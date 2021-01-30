library(readxl)
data = read.csv("/Users/DELL/Documents/GH Intern/Obesity&Mortality/wt_change_surv.csv", encoding = "UTF-8")
#wt_change_cat<-cut(data$weight_change_rate, breaks = c(-Inf,-0.1, -0.5,0.5,0.1,Inf), labels = c(0,1,2,3,4))
bmidata = read.csv("/Users/DELL/Documents/GH Intern/Obesity&Mortality/bmi_surv_adjust.csv", encoding = "UTF-8")
GDP_raw = read_xlsx("/Users/DELL/Documents/GH Intern/Obesity&Mortality/prov_GDP.xlsx")

province<-NA
for(i in 1:length(GDP_raw$province)){
  if (GDP_raw$province[i]=="安徽"){
    province[i]="anhui"
  }else if (GDP_raw$province[i]=="北京"){
    province[i]="beijing"
  }else if (GDP_raw$province[i]=="重庆"){
    province[i]="chongqing"
  }else if (GDP_raw$province[i]=="福建"){
    province[i]="fujian"
  }else if (GDP_raw$province[i]=="广东"){
    province[i]="guangdong"
  }else if (GDP_raw$province[i]=="广西"){
    province[i]="guangxi"
  }else if (GDP_raw$province[i]=="河北"){
    province[i]="hebei"
  }else if (GDP_raw$province[i]=="黑龙江"){
    province[i]="heilongjiang"
  }else if (GDP_raw$province[i]=="河南"){
    province[i]="henan"
  }else if (GDP_raw$province[i]=="湖北"){
    province[i]="hubei"
  }else if (GDP_raw$province[i]=="湖南"){
    province[i]="jiangsu"
  }else if (GDP_raw$province[i]=="江苏"){
    province[i]="jiangsu"
  }else if (GDP_raw$province[i]=="江西"){
    province[i]="jiangxi"
  }else if (GDP_raw$province[i]=="吉林"){
    province[i]="jilin"
  }else if (GDP_raw$province[i]=="辽宁"){
    province[i]="liaoning"
  }else if (GDP_raw$province[i]=="陕西"){
    province[i]="shaanxi"
  }else if (GDP_raw$province[i]=="山东"){
    province[i]="shandong"
  }else if (GDP_raw$province[i]=="上海"){
    province[i]="shanghai"
  }else if (GDP_raw$province[i]=="山西"){
    province[i]="shanxi"
  }else if (GDP_raw$province[i]=="江西"){
    province[i]="jiangxi"
  }else if (GDP_raw$province[i]=="四川"){
    province[i]="sichuan"
  }else if (GDP_raw$province[i]=="天津"){
    province[i]="tianjin"
  }else if (GDP_raw$province[i]=="浙江"){
    province[i]="zhejiang"
  }else{province[i]=NA}
}
GDP_raw$prov<-province

for(i in 1:length(region$prov)){
  if (region$prov[i]=="helongjiang"){
    region$prov[i]="heilongjiang"
  }else if (region$prov[i]=="shangdong"){
    region$prov[i]="shandong"
  }
}
region_gdp<-left_join(region,GDP_raw,by="prov")

dropdata$GDPpc_cat<-cut(dropdata$GDP,
                        breaks = c(-Inf,5937.45,12501.16,Inf),
                        labels = c(0,1,2))


#income data
income = read.csv("/Users/DELL/Documents/GH Intern/Obesity&Mortality/Region_income.csv", encoding = "UTF-8")
library(survival)
library(dplyr)
income_bmi = left_join(bmidata,income,by = "id")
predictor<-data.frame(cbind(bmidata$gender,bmidata$trueage,bmidata$residence,bmidata$occupation))
predictor<-data.frame(cbind(c("income",income_bmi$income),c("gender",income_bmi$gender),c("age",income_bmi$trueage),c("residence",income_bmi$residence),c("occupation",income_bmi$occupation)))
impute_income<-mice(predictor,m = 4,method = "pmm")
write.csv(impute_income$imp$X1,file="impute_income.csv", fileEncoding = "UTF-8")
imputedata = read.csv("/Users/DELL/Documents/GH Intern/Obesity&Mortality/all_impute_income.csv", encoding = "UTF-8")

count = 1
for (i in 1:nrow(income_bmi)){
  if(is.na(income_bmi$income[i])){
    income_bmi$income[i]=imputedata$X4[count]
    count=count+1
  }
}
quantile(income_bmi$income,1/3)
quantile(income_bmi$income,2/3)

income_bmi$income_tri<-cut(income_bmi$income,
                        breaks = c(-Inf,1999,5000,Inf),
                        labels = c(0,1,2))


#bmi
newdata = bmidata%>%filter(bmidata$trueage>=80)
newdata = income_bmi%>%filter(income_bmi$trueage>=80)
newdata = gdp_income%>%filter(gdp_income$trueage>=80)

newdata = dropdisea_data%>%filter(dropdisea_data$trueage>=80)
newdata$id<-newdata$X.U.FEFF..id
newdata = dropci_data%>%filter(dropci_data$trueage>=80)
newdata = dropadl_data%>%filter(dropadl_data$trueage>=80)
temp_df<-data.frame(id=c(bmidata$X.U.FEFF..id),
                    wcr_adjust=c(bmidata$wcr_adjust),
                    survivalbas_adjust=c(bmidata$survivalbas_adjust),
                    impute_height=c(bmidata$impute_height))
newdata = inner_join(newdata,temp_df,by='id')


dropdata=newdata%>%filter(newdata$wcr_adjust>-0.3492183&newdata$wcr_adjust<0.4650866 )
dropdata=dropdata%>%filter(dropdata$weight>=30&dropdata$weight<=71 )

dropdata$wcr_new_cat<-cut(dropdata$wcr_adjust, 
                          breaks = c(-Inf,-0.10, -0.05,0.05,0.10,Inf), 
                          labels = c(0,1,2,3,4))
#data$wcr_cat = factor(data$wcr_cat, levels=c(2,0,1,3,4))
dropdata$wcr_new_cat = factor(dropdata$wcr_new_cat, levels=c(2,0,1,3,4))
dropdata$wcr_new2_cat<-cut(dropdata$wcr_adjust, 
                           breaks = c(-Inf,-0.05,0.05,Inf), 
                           labels = c(0,1,2))
dropdata$wcr_new2_cat = factor(dropdata$wcr_new2_cat, levels=c(1,0,2))
setwd("/Users/DELL/Documents/GH Intern/Obesity&Mortality/")
write.csv(dropdata,file="weightchange_data.csv", fileEncoding = "UTF-8")
modelwt<-coxph(Surv(survivalbas_adjust,censor)~wcr_new_cat+trueage
               +factor(gender)+factor(ethnicity)+weight+impute_height+factor(edug)
               +factor(occupation)+factor(coresidence)+factor(marital)
               +factor(residence)+factor(smkl_bi)+factor(dril_bi)
               +factor(pa_bi)+factor(meat1)+factor(fish1)
               +factor(egg1)+factor(veg1)+factor(fruit1)+factor(sugar1)
               +adl+factor(ci_cat)+factor(leisure_cat)+diabetes+heartdisea+ulcer                    
               +hypertension+strokecvd+cancer+parkinson+wave,data = dropdata)
summary(modelwt)

lowincome= dropdata%>%filter(dropdata$income_tri==0)
midincome= dropdata%>%filter(dropdata$income_tri==1)
highincome= dropdata%>%filter(dropdata$income_tri==2)

modelwt<-coxph(Surv(survivalbas_adjust,censor)~wcr_new_cat+trueage
               +factor(gender)+factor(ethnicity)+weight+impute_height+factor(edug)
               +factor(occupation)+factor(coresidence)+factor(marital)
               +factor(residence)+factor(smkl_bi)+factor(dril_bi)
               +factor(pa_bi)+factor(meat1)+factor(fish1)
               +factor(egg1)+factor(veg1)+factor(fruit1)+factor(sugar1)
               +adl+factor(ci_cat)+factor(leisure_cat)+diabetes+heartdisea+ulcer                    
               +hypertension+strokecvd+cancer+parkinson+wave,data = highincome)
summary(modelwt)

underweight=dropdata%>%filter(dropdata$impute_bmi<18.5)
normalweight=dropdata%>%filter(dropdata$impute_bmi>=18.5&dropdata$impute_bmi<24)
overweight=dropdata%>%filter(dropdata$impute_bmi>24)

modelwt<-coxph(Surv(survivalbas_adjust,censor)~wcr_new2_cat+trueage
               +factor(gender)+factor(ethnicity)+weight+impute_height+factor(edug)
               +factor(occupation)+factor(coresidence)+factor(marital)
               +factor(residence)+factor(smkl_bi)+factor(dril_bi)
               +factor(pa_bi)+factor(meat1)+factor(fish1)
               +factor(egg1)+factor(veg1)+factor(fruit1)+factor(sugar1)
               +adl+factor(ci_cat)+factor(leisure_cat)+diabetes+heartdisea+ulcer                    
               +hypertension+strokecvd+cancer+parkinson+wave,data = normalweight)
summary(modelwt)

modelwt<-coxph(Surv(survivalbas_adjust,censor)~wcr_new_cat+trueage
               +factor(gender)+factor(ethnicity)+weight+impute_height+factor(edug)
               +factor(occupation)+factor(coresidence)+factor(marital)
               +factor(residence)+factor(smkl_bi)+factor(dril_bi)
               +factor(pa_bi)+factor(meat1)+factor(fish1)
               +factor(egg1)+factor(veg1)+factor(fruit1)+factor(sugar1)
               +adl+factor(ci_cat)+factor(leisure_cat)+diabetes+heartdisea+ulcer                    
               +hypertension+strokecvd+cancer+parkinson+wave,data = overweight)
summary(modelwt)

modelwt<-coxph(Surv(survivalbas_adjust,censor)~wcr_new_cat+trueage
               +factor(gender)+factor(ethnicity)+weight+impute_height+factor(edug)
               +factor(occupation)+factor(coresidence)+factor(marital)
               +factor(residence)+factor(smkl_bi)+factor(dril_bi)
               +factor(pa_bi)+factor(meat1)+factor(fish1)
               +factor(egg1)+factor(veg1)+factor(fruit1)+factor(sugar1)
               +adl+factor(ci_cat)+factor(leisure_cat)+diabetes+heartdisea+ulcer                    
               +hypertension+strokecvd+cancer+parkinson+wave,data = underweight)
summary(modelwt)


#gender
men=dropdata%>%filter(dropdata$gender==1)
women=dropdata%>%filter(dropdata$gender==0)

modelwt<-coxph(Surv(survivalbas_adjust,censor)~wcr_new_cat+trueage
               +factor(ethnicity)+weight+impute_height+factor(edug)
               +factor(occupation)+factor(coresidence)+factor(marital)
               +factor(residence)+factor(smkl_bi)+factor(dril_bi)
               +factor(pa_bi)+factor(meat1)+factor(fish1)
               +factor(egg1)+factor(veg1)+factor(fruit1)+factor(sugar1)
               +adl+factor(ci_cat)+factor(leisure_cat)+diabetes+heartdisea+ulcer                    
               +hypertension+strokecvd+cancer+parkinson+wave,data = men)
summary(modelwt)

modelwt<-coxph(Surv(survivalbas_adjust,censor)~wcr_new_cat+trueage
               +factor(ethnicity)+weight+impute_height+factor(edug)
               +factor(occupation)+factor(coresidence)+factor(marital)
               +factor(residence)+factor(smkl_bi)+factor(dril_bi)
               +factor(pa_bi)+factor(meat1)+factor(fish1)
               +factor(egg1)+factor(veg1)+factor(fruit1)+factor(sugar1)
               +adl+factor(ci_cat)+factor(leisure_cat)+diabetes+heartdisea+ulcer                    
               +hypertension+strokecvd+cancer+parkinson+wave,data = women)
summary(modelwt)


modelwt<-coxph(Surv(survival_bas,censor)~data_wt$wcr_cat
               +trueage+gender+weight+ethnicity+edug
               +occupation+coresidence+marital+residence
               +smkl+dril+diet+hypertension+diabetes+heartdisea
               +strokecvd+copd+tb+cancer+adl+ci+leisure,data = data_wt)
modelwt<-coxph(Surv(survival_bas,censor)~data$wcr_cat+trueage
               +factor(gender)+factor(ethnicity)+weight+factor(edug)
               +factor(occupation)+factor(coresidence)+factor(marital)
               +factor(residence)+factor(smkl_bi)+factor(dril_bi)
               +factor(pa_bi)+factor(lifesty_bi)+factor(she_bi)
               +adl+factor(ci_cat)+diabetes+heartdisea+hypertension
               +strokecvd+copd+cancer+factor(leisure_cat),data = data)
modelwt<-coxph(Surv(survival_bas,censor)~wcr_cat+trueage
               +factor(gender)+factor(ethnicity)+weight+factor(edug)
               +factor(occupation)+factor(coresidence)+factor(marital)
               +factor(residence)+factor(smkl_bi)+factor(dril_bi)
               +factor(pa_bi)+factor(meat1)+factor(fish1)                                     
               +factor(egg1)+factor(veg1)+factor(fruit1)+factor(sugar1)
               +adl+factor(ci_cat)+factor(leisure_cat)+diabetes+heartdisea                    
               +hypertension+strokecvd+copd+cancer+parkinson+wave,data = data)

newdata$smkl_bi<-ifelse(newdata$smkl==3|newdata$smkl==4,1,0)
newdata$dril_bi<-ifelse(newdata$dril==3|newdata$dril==4,1,0)
newdata$pa_bi<-ifelse(newdata$pa==1|newdata$pa==2,1,0)
lifestyle<-NA
for (j in 1:19957){
if (is.na(newdata$smkl_bi)[j]==FALSE&is.na(newdata$dril_bi)[j]==FALSE&is.na(newdata$pa_bi)[j]==FALSE){
if(newdata$smkl_bi[j]==0&newdata$dril_bi[j]==0&newdata$pa_bi[j]==1){
  lifestyle[j]<-3
} else if (newdata$smkl_bi[j]==1&newdata$dril_bi[j]==1&newdata$pa_bi[j]==0){
  lifestyle[j]=0
} else if (newdata$smkl_bi[j]==1&newdata$dril_bi[j]==0&newdata$pa_bi[j]==1){
  lifestyle[j]=1
} else if (newdata$smkl_bi[j]==0&newdata$dril_bi[j]==1&newdata$pa_bi[j]==1){
  lifestyle[j]=1
} else if (newdata$smkl_bi[j]==0&newdata$dril_bi[j]==0&newdata$pa_bi[j]==0){
  lifestyle[j]=1
} else{
  lifestyle[j]=2
}
}
}



#BMI subgroup
joint<-NA
for (j in 1:14928){
  if (is.na(dropdata$impute_bmi)[j]==FALSE&is.na(dropdata$wcr_new_cat)[j]==FALSE){
    if(dropdata$impute_bmi[j]<18.5){
      if(dropdata$wcr_new_cat[j]==0){joint[j]=0}
      else if(dropdata$wcr_new_cat[j]==1){joint[j]=1}
      else if(dropdata$wcr_new_cat[j]==2){joint[j]=2}
      else if(dropdata$wcr_new_cat[j]==3){joint[j]=3}
      else if(dropdata$wcr_new_cat[j]==4){joint[j]=4}
    } 
    else if(dropdata$impute_bmi[j]>=18.5&dropdata$impute_bmi[j]<24){
      if(dropdata$wcr_new_cat[j]==0){joint[j]=5}
      else if(dropdata$wcr_new_cat[j]==1){joint[j]=6}
      else if(dropdata$wcr_new_cat[j]==2){joint[j]=7}
      else if(dropdata$wcr_new_cat[j]==3){joint[j]=8}
      else if(dropdata$wcr_new_cat[j]==4){joint[j]=9}
    } 
    else  if(dropdata$impute_bmi[j]>=24){
      if(dropdata$wcr_new_cat[j]==0){joint[j]=10}
      else if(dropdata$wcr_new_cat[j]==1){joint[j]=11}
      else if(dropdata$wcr_new_cat[j]==2){joint[j]=12}
      else if(dropdata$wcr_new_cat[j]==3){joint[j]=13}
      else if(dropdata$wcr_new_cat[j]==4){joint[j]=14}
    } 
  }
}

#lifestyle subgroups
dropdata$smkl_tri<-ifelse(dropdata$smkl==3|dropdata$smkl==4,0,1)
dropdata$smkl_tri<-ifelse(dropdata$smkl==1,2,dropdata$smkl_tri)

dropdata$dril_tri<-ifelse(dropdata$dril==1|dropdata$dril==2,2,1)
dropdata$dril_tri<-ifelse(dropdata$dril==4,0,dropdata$dril_tri)

dropdata$pa_tri<-ifelse(dropdata$pa==1|dropdata$pa==2,2,1)
dropdata$pa_tri<-ifelse(dropdata$pa==4,0,dropdata$pa_tri)

dropdata$she_tri<-ifelse(dropdata$she==3|dropdata$she==2,1,0)
dropdata$she_tri<-ifelse(dropdata$she==4,2,dropdata$she_tri)

dropdata$lifestyle<- dropdata$smkl_tri+ dropdata$dril_tri+ dropdata$pa_tri+ dropdata$she_tri
dropdata$lifestyle_tri<-as.numeric(cut(dropdata$lifestyle,breaks = c(-Inf,4,5,Inf),labels = c(0,1,2)))
summary(factor(dropdata$lifestyle_tri))

unhealthy=dropdata%>%filter(dropdata$lifestyle_tri==1)
normal=dropdata%>%filter(dropdata$lifestyle_tri==2)
healthy=dropdata%>%filter(dropdata$lifestyle_tri==3)

#记得删掉含有NA的行！！！
#new weight change subgroup
dropdata$wcr_new2_cat<-cut(dropdata$wcr_adjust, 
                          breaks = c(-Inf,-0.05,0.05,Inf), 
                          labels = c(0,1,2))

joint<-NA
for (j in 1:14588){
  if (is.na(dropdata$lifestyle_tri)[j]==FALSE&is.na(dropdata$wcr_new2_cat)[j]==FALSE){
    if(dropdata$lifestyle_tri[j]==1){
      if(dropdata$wcr_new2_cat[j]==0){joint[j]=0}
      else if(dropdata$wcr_new2_cat[j]==1){joint[j]=1}
      else if(dropdata$wcr_new2_cat[j]==2){joint[j]=2}
    } 
    else if(dropdata$lifestyle_tri[j]==2){
      if(dropdata$wcr_new2_cat[j]==0){joint[j]=3}
      else if(dropdata$wcr_new2_cat[j]==1){joint[j]=4}
      else if(dropdata$wcr_new2_cat[j]==2){joint[j]=5}
    } 
    else  if(dropdata$lifestyle_tri[j]==3){
      if(dropdata$wcr_new2_cat[j]==0){joint[j]=6}
      else if(dropdata$wcr_new2_cat[j]==1){joint[j]=7}
      else if(dropdata$wcr_new2_cat[j]==2){joint[j]=8}
    } 
    else{print("no")}
  }
}
dropdata$lifestyle_weight<-joint
dropdata$lifestyle_weight = factor(dropdata$lifestyle_weight,
                                   levels=c(4,0,1,2,3,5,6,7,8))

#lifestyleXweight change subgroup
joint<-NA
for (j in 1:14588){
  if (is.na(dropdata$lifestyle_tri)[j]==FALSE&is.na(dropdata$wcr_new_cat)[j]==FALSE){
    if(dropdata$lifestyle_tri[j]==1){
      if(dropdata$wcr_new_cat[j]==0){joint[j]=0}
      else if(dropdata$wcr_new_cat[j]==1){joint[j]=1}
      else if(dropdata$wcr_new_cat[j]==2){joint[j]=2}
      else if(dropdata$wcr_new_cat[j]==3){joint[j]=3}
      else if(dropdata$wcr_new_cat[j]==4){joint[j]=4}
    } 
    else if(dropdata$lifestyle_tri[j]==2){
      if(dropdata$wcr_new_cat[j]==0){joint[j]=5}
      else if(dropdata$wcr_new_cat[j]==1){joint[j]=6}
      else if(dropdata$wcr_new_cat[j]==2){joint[j]=7}
      else if(dropdata$wcr_new_cat[j]==3){joint[j]=8}
      else if(dropdata$wcr_new_cat[j]==4){joint[j]=9}
    } 
    else  if(dropdata$lifestyle_tri[j]==3){
      if(dropdata$wcr_new_cat[j]==0){joint[j]=10}
      else if(dropdata$wcr_new_cat[j]==1){joint[j]=11}
      else if(dropdata$wcr_new_cat[j]==2){joint[j]=12}
      else if(dropdata$wcr_new_cat[j]==3){joint[j]=13}
      else if(dropdata$wcr_new_cat[j]==4){joint[j]=14}
    } 
  }
}

GDP<-NA
for (j in 1:length(dropdata$X.U.FEFF..id)){
  if(dropdata$GDP_cat[j]==0){
    if(dropdata$wcr_new2_cat[j]==0){GDP[j]=0}
    else if(dropdata$wcr_new2_cat[j]==1){GDP[j]=1}
    else if(dropdata$wcr_new2_cat[j]==2){GDP[j]=2}
  } 
  else if(dropdata$GDP_cat[j]==1){
    if(dropdata$wcr_new2_cat[j]==0){GDP[j]=3}
    else if(dropdata$wcr_new2_cat[j]==1){GDP[j]=4}
    else if(dropdata$wcr_new2_cat[j]==2){GDP[j]=5}
  } 
  else  if(dropdata$GDP_cat[j]==2){
    if(dropdata$wcr_new2_cat[j]==0){GDP[j]=6}
    else if(dropdata$wcr_new2_cat[j]==1){GDP[j]=7}
    else if(dropdata$wcr_new2_cat[j]==2){GDP[j]=8}
  }
}
GDPpc<-NA
for (j in 1:length(dropdata$X.U.FEFF..id)){
  if(dropdata$GDPpc_cat[j]==0){
    if(dropdata$wcr_new2_cat[j]==0){GDPpc[j]=0}
    else if(dropdata$wcr_new2_cat[j]==1){GDPpc[j]=1}
    else if(dropdata$wcr_new2_cat[j]==2){GDPpc[j]=2}
  } 
  else if(dropdata$GDPpc_cat[j]==1){
    if(dropdata$wcr_new2_cat[j]==0){GDPpc[j]=3}
    else if(dropdata$wcr_new2_cat[j]==1){GDPpc[j]=4}
    else if(dropdata$wcr_new2_cat[j]==2){GDPpc[j]=5}
  } 
  else  if(dropdata$GDPpc_cat[j]==2){
    if(dropdata$wcr_new2_cat[j]==0){GDPpc[j]=6}
    else if(dropdata$wcr_new2_cat[j]==1){GDPpc[j]=7}
    else if(dropdata$wcr_new2_cat[j]==2){GDPpc[j]=8}
  }
}
dropdata$wcr_joint_gdppc<-factor(GDPpc)

lowgdp=dropdata%>%filter(dropdata$wcr_joint_gdppc==0)
middlegdp=dropdata%>%filter(dropdata$wcr_joint_gdppc==1)
highgdp=dropdata%>%filter(dropdata$wcr_joint_gdppc==2)


data = read.csv("/Users/DELL/Documents/GH Intern/Obesity&Mortality/wt_change_surv.csv", encoding = "UTF-8")
