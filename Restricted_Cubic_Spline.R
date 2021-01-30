library(segmented)
library(splines)
library(ggplot2)
library(Hmisc)
library(rms)
library(survival)
library(ggpubr)
library(openxlsx)
data = read.csv("/Users/DELL/Documents/GH Intern/Obesity&Mortality/bmi_surv_adjust.csv", encoding = "UTF-8")
data<-sapply(normal,as.numeric)
biomarker=data.frame(underweight)

#第一种
dd <- datadist(biomarker)
options(datadist='dd')
fit<-cph(Surv(survivalbas_adjust,censor)~rcs(wcr_adjust,3) +trueage
         +gender+ethnicity+weight+edug
         +occupation+coresidence+marital
         +residence+meat1+fish1                                     
         +egg1+veg1+fruit1+sugar1
         +adl+ci_cat+leisure_cat+diabetes+heartdisea                    
         +hypertension+strokecvd+copd+cancer+parkinson+wave,data = biomarker)
dd$limits$wcr_adjust[2]=0
summary
HR1<-Predict(fit, wcr_adjust,fun=exp,ref.zero =TRUE)

P1<-ggplot()+geom_line(data=HR1, aes(wcr_adjust,yhat),linetype="solid",size=1,alpha = 0.7,colour="black")+
  geom_ribbon(data=HR1, aes(wcr_adjust,ymin = lower, ymax = upper),alpha = 0.1,fill="black")+
  labs(title = "Underweight", x="Weight Change Ratio", y="HR (95%CI)") 

P1+theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())


#第二种

fit.coxph <- coxph(Surv(survivalbas_adjust,censor)~rcs(wcr_adjust,3) +trueage
                   +factor(gender)+factor(ethnicity)+weight+impute_height+factor(edug)
                   +factor(occupation)+factor(coresidence)+factor(marital)
                   +factor(residence)+factor(smkl_bi)+factor(dril_bi)
                   +factor(pa_bi)+factor(meat1)+factor(fish1)                                     
                   +factor(egg1)+factor(veg1)+factor(fruit1)+factor(sugar1)
                   +adl+factor(ci_cat)+factor(leisure_cat)+diabetes+heartdisea                    
                   +hypertension+strokecvd+copd+cancer+parkinson+wave,data = biomarker)

# all
tp <- termplot(fit.coxph , se = TRUE, plot = FALSE)
ref <- 0
se <- 0.0012

p=ggplot() +
  
  geom_line(data = tp$wcr_adjust, aes(x = x, y = exp(y - ref))) + 
  geom_line(data = tp$wcr_adjust, aes(x = x, y = exp(y + 1.96 * se - ref)), linetype = 2) + 
  geom_line(data = tp$wcr_adjust, aes(x = x, y = exp(y - 1.96 * se - ref)), linetype = 2) +
  geom_hline(aes(yintercept = 1), linetype = 2) +
  ylim(0.9,1.3)+xlim(-0.4,0.5)+theme(panel.grid.major=element_line(colour=NA))+
  geom_histogram(data=biomarker, fill="blue",aes(x=wcr_adjust,y= 0.1*..count../sum(..count..)),binwidth = 0.1,alpha=I(0.3)) +
  
  labs(x = "Weight Change Rate",
       y = "Hazard Ratio (95% CI)",
       title = "All participants")

p
