 
install.packages("segmented")
install.packages("splines")
install.packages("ggplot2")
install.packages("Hmisc")
install.packages("rms")
install.packages("survival")
install.packages("ggpubr")
install.packages("openxlsx")

## install packages
library(segmented)
library(splines)
library(ggplot2)
library(Hmisc)
library(rms)
library(survival)
library(ggpubr)
library(openxlsx)
setwd("E:/Desktop/Code/spline")
data=read.csv(file="wt_change_surv.csv",encoding = "UTF-8")
data=read.csv(file="ndvi.csv",header=T)
 
data<-sapply(old_newdata,as.numeric)
biomarker=data.frame(data)

venlow= subset(biomarker,vent5<=2)
venmedian= subset(biomarker,vent5==3)
venhigh= subset(biomarker,vent5==4)

#weight change&mortality
data<-sapply(data,as.numeric)
biomarker=data.frame(data)
dd <- datadist(biomarker)
options(datadist='dd')
fit<-cph(Surv(survival_bas,censor)~rcs(wcr_adjust,3) +trueage
                     +gender+ethnicity+weight+edug
                     +occupation+coresidence+marital
                     +residence+smkl_bi+dril_bi
                     +pa_bi+meat1+fish1                                     
                     +egg1+veg1+fruit1+sugar1
                     +adl+ci_cat+leisure_cat+diabetes+heartdisea                    
                     +hypertension+strokecvd+copd+cancer+parkinson+wave,data = biomarker)
dd$limits$wcr_adjust[2]=0
fit=update(fit)
HR1<-Predict(fit, wcr_adjust,fun=exp,ref.zero =TRUE)

library(splines)
fit.coxph <- coxph(Surv(survivalbas_adjust,censor)~rcs(wcr_adjust,3) +trueage
                   +factor(gender)+factor(ethnicity)+weight+factor(edug)
                   +factor(occupation)+factor(coresidence)+factor(marital)
                   +factor(residence)+factor(smkl_bi)+factor(dril_bi)
                   +factor(pa_bi)+factor(meat1)+factor(fish1)                                     
                   +factor(egg1)+factor(veg1)+factor(fruit1)+factor(sugar1)
                   +adl+factor(ci_cat)+factor(leisure_cat)+diabetes+heartdisea                    
                   +hypertension+strokecvd+copd+cancer+parkinson+wave,data = biomarker)

org_par <- par(xaxs = "i", ask = TRUE)
plotHR(fit.coxph, term = "wcr_90", plot.bty = "o", xlim = c(30, 70), xlab = "weight change")


fit <- lrm(dep_bi ~ rcs(contndvi10,3) + trueage + gender + edug + mari2 + bmi1 + income_bi + socleis + smoke + drink + tea3+ srh+ DietDiver9 +ci + adl_sum + pm254+ season + geo ,
                         data = biomarker)
dd$limits$contndvi10[2]=5
fit=update(fit)
HR1<-predict(fit, contndvi10,fun=exp,ref.zero =T)
 

fit <- lrm(censor ~ rcs(wcr_90,3) + trueage + gender + edug + marital + occupation + weight + smkl + dril + pa+ci.1 + adl,
           data = biomarker)
fit <- lrm(dep_bi ~ rcs(contndvi10,3) + trueage + gender + edug + mari2 + bmi1 + income_bi + socleis + smoke + drink + tea3+ srh+ DietDiver9 +ci + adl_sum + pm254+ season + geo ,
           data = venlow)
dd$limits$contndvi10[2]=5
fit=update(fit)
HR2<-Predict(fit, contndvi10,fun=exp,ref.zero =T)
 

fit <- lrm(dep_bi ~ rcs(contndvi10,3) + trueage + gender + edug + mari2 + bmi1 + income_bi + socleis + smoke + drink + tea3+ srh+ DietDiver9 +ci + adl_sum + pm254+ season + geo ,
           data = venmedian)
dd$limits$contndvi10[2]=5
fit=update(fit)
HR3<-Predict(fit, contndvi10,fun=exp,ref.zero =T)
 

fit <- lrm(dep_bi ~ rcs(contndvi10,3) + trueage + gender + edug + mari2 + bmi1 + income_bi + socleis + smoke + drink + tea3+ srh+ DietDiver9 +ci + adl_sum + pm254+ season + geo ,
           data = venhigh)
dd$limits$contndvi10[2]=5
fit=update(fit)
HR4<-Predict(fit, contndvi10,fun=exp,ref.zero =T)
 
P1<-ggplot()+geom_line(data=HR1, aes(wcr_adjust,yhat),linetype="solid",size=1,alpha = 0.7,colour="black")+
  geom_ribbon(data=HR1, aes(wcr_adjust,ymin = lower, ymax = upper),alpha = 0.1,fill="black")

P1<-ggplot()+geom_line(data=HR1, aes(contndvi10,yhat),linetype="solid",size=1,alpha = 0.7,colour="black")+
  geom_ribbon(data=HR1, aes(contndvi10,ymin = lower, ymax = upper),alpha = 0.1,fill="black")

P2<-ggplot()+geom_line(data=HR2, aes(contndvi10,yhat),linetype="solid",size=1,alpha = 0.7,colour="red")+
  geom_ribbon(data=HR2, aes(contndvi10,ymin = lower, ymax = upper),alpha = 0.1,fill="red")

P3<-ggplot()+geom_line(data=HR3, aes(contndvi10,yhat),linetype="solid",size=1,alpha = 0.7,colour="red")+
  geom_ribbon(data=HR3, aes(contndvi10,ymin = lower, ymax = upper),alpha = 0.1,fill="red")

P4<-ggplot()+geom_line(data=HR4, aes(contndvi10,yhat),linetype="solid",size=1,alpha = 0.7,colour="red")+
  geom_ribbon(data=HR4, aes(contndvi10,ymin = lower, ymax = upper),alpha = 0.1,fill="red")

  P5=ggplot()  + geom_line(data=HR2, aes(contndvi10,yhat),linetype="solid",size=1,alpha = 0.5,colour="red")+
    geom_ribbon(data=HR2, aes(contndvi10,ymin = lower, ymax = upper),alpha = 0.1,fill="red")  +geom_line(data=HR3, aes(contndvi10,yhat),linetype="solid",size=1,alpha = 0.5,colour="blue")+
    geom_ribbon(data=HR3, aes(contndvi10,ymin = lower, ymax = upper),alpha = 0.1,fill="blue") +geom_line(data=HR4, aes(contndvi10,yhat),linetype="solid",size=1,alpha = 0.9,colour="darkseagreen4")+
    geom_ribbon(data=HR4, aes(contndvi10,ymin = lower, ymax = upper),alpha = 0.1,fill="darkseagreen4")
  
  P5<-P5+theme_classic()+geom_hline(yintercept=1, linetype=2,size=1,,alpha = 0.1)+ 
    labs(title = "RCS", x="NDVI", y="Odds Ratio (95% CI)") 
  P5
  
  
  P6<-P1+theme_classic()+geom_hline(yintercept=1, linetype=2,size=1,,alpha = 0.1)+ 
    labs(title = "RCS", x="NDVI", y="Odds Ratio (95% CI)") 
  P6
  
  
dd$limits$age[2] <- 50

fit=update(y_model.spline1)
HR<-Predict(y_model.spline1, contndvi10,fun=exp,ref.zero = TRUE)


P1<-ggplot(y_model.spline1)

# restricted cubic spline

model.spline1 <- lm(TestC ~ rcs(Age, 3))
model.spline2 <- lm(TestC ~ rcs(Age, 5))

# compare models (to determine number of knots)
AIC(model.spline1,model.spline2)

# make predictions
y_model.spline2 <- predict(model.spline2)




# or use ggplot

# 3 knots
ggplot(data=data.wide, aes(x=Age, y=TestC))  +
  geom_point()+geom_smooth(aes(x=Age, y=TestC), method="lm", formula=y ~ rcs(x, 3), se=FALSE, colour="blue")

# 5 knots
ggplot(data=data.wide, aes(x=Age, y=TestC))  +
  geom_point()+geom_smooth(aes(x=Age, y=TestC), method="lm", formula=y ~ rcs(x, 5), se=FALSE, colour="red")
















# all
tp <- termplot(model.alb  , se = TRUE, plot = FALSE)
ref <- -0.01270235
se <- 0.02433033

p=ggplot() +
  
  geom_line(data = tp$contndvi10, aes(x = x, y = exp(y - ref))) + 
  geom_line(data = tp$contndvi10, aes(x = x, y = exp(y + 1.96 * se - ref)), linetype = 2) + 
  geom_line(data = tp$contndvi10, aes(x = x, y = exp(y - 1.96 * se - ref)), linetype = 2) +
  geom_hline(aes(yintercept = 1), linetype = 2) +
  ylim(0.75,1.5)+xlim(-1,8)+theme(panel.grid.major=element_line(colour=NA))+
  geom_histogram(data=biomarker, fill="blue",aes(x=contndvi10,y= 0.1*..count../sum(..count..)),binwidth = 0.1,alpha=I(0.3)) +
  
  labs(x = "Open window ventilation index, Kg",
       y = "Odds Ratio (95% CI)",
       title = "All participants")

p

q= p+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))
q




model.alb1 <- glm(dep_bi ~ trueage + gender + edug + mari2 + bmi1 + income_bi + socleis + smoke + drink + tea3+ srh+ DietDiver9 +ci + adl_sum + pm254+ season+ contndvi10 + geo ,
                 family = binomial(link = logit), data = venlow)
 
# venlow
tp <- termplot(model.alb1  , se = TRUE, plot = FALSE)
ref <- -0.01270235
se <- 0.02433033

p=ggplot() +
  
  geom_line(data = tp$contndvi10, aes(x = x, y = exp(y - ref))) + 
  geom_line(data = tp$contndvi10, aes(x = x, y = exp(y + 1.96 * se - ref)), linetype = 2) + 
  geom_line(data = tp$contndvi10, aes(x = x, y = exp(y - 1.96 * se - ref)), linetype = 2) +
  geom_hline(aes(yintercept = 1), linetype = 2) +
  ylim(0.75,1.5)+xlim(-1,8)+theme(panel.grid.major=element_line(colour=NA))+
  geom_histogram(data=biomarker, fill="blue",aes(x=contndvi10,y= 0.1*..count../sum(..count..)),binwidth = 0.1,alpha=I(0.3)) +
  
  labs(x = "NDVI",
       y = "Odds Ratio (95% CI)",
       title = "Low ventilation")

p

q= p+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))
q




model.alb2 <- glm(dep_bi ~ trueage + gender + edug + mari2 + bmi1 + income_bi + socleis + smoke + drink + tea3+ srh+ DietDiver9 +ci + adl_sum + pm254+ season+ contndvi10 + geo ,
                  family = binomial(link = logit), data = venmedian)

 
# venmedian
tp <- termplot(model.alb2  , se = TRUE, plot = FALSE)
ref <- 0.005130925

se <- 0.05410999 

p=ggplot() +
  
  geom_line(data = tp$contndvi10, aes(x = x, y = exp(y - ref))) + 
  geom_line(data = tp$contndvi10, aes(x = x, y = exp(y + 1.96 * se - ref)), linetype = 2) + 
  geom_line(data = tp$contndvi10, aes(x = x, y = exp(y - 1.96 * se - ref)), linetype = 2) +
  geom_hline(aes(yintercept = 1), linetype = 2) +
  ylim(0.75,1.5)+xlim(-1,8)+theme(panel.grid.major=element_line(colour=NA))+
  geom_histogram(data=biomarker, fill="blue",aes(x=contndvi10,y= 0.1*..count../sum(..count..)),binwidth = 0.1,alpha=I(0.3)) +
  
  labs(x = "NDVI",
       y = "Odds Ratio (95% CI)",
       title = "median ventilation")

p

q= p+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))
q



model.alb3 <- glm(dep_bi ~ trueage + gender + edug + mari2 + bmi1 + income_bi + socleis + smoke + drink + tea3+ srh+ DietDiver9 +ci + adl_sum + pm254+ season+ contndvi10 + geo ,
                  family = binomial(link = logit), data = venhigh)


 

 

#venlow

 

 

# all
tp <- termplot(model.alb3  , se = TRUE, plot = FALSE)
ref <- -0.0008758431

se <- 0.03621587

p=ggplot() +
  
  geom_line(data = tp$contndvi10, aes(x = x, y = exp(y - ref))) + 
  geom_line(data = tp$contndvi10, aes(x = x, y = exp(y + 1.96 * se - ref)), linetype = 2) + 
  geom_line(data = tp$contndvi10, aes(x = x, y = exp(y - 1.96 * se - ref)), linetype = 2) +
  geom_hline(aes(yintercept = 1), linetype = 2) +
  ylim(0.75,1.5)+xlim(-1,8)+theme(panel.grid.major=element_line(colour=NA))+
  geom_histogram(data=biomarker, fill="blue",aes(x=contndvi10,y= 0.1*..count../sum(..count..)),binwidth = 0.1,alpha=I(0.3)) +
  
  labs(x = "NDVI ",
       y = "Odds Ratio (95% CI)",
       title = "ventilation high")

p

q= p+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))
q

# all
tp <- termplot(model.all  , se = TRUE, plot = FALSE)
ref <- tp$vd3$y[tp$vd3$x == 48.32]

ggplot() +
  
  geom_line(data = tp$vd3, aes(x = x, y = exp(y - ref))) + 
  geom_line(data = tp$vd3, aes(x = x, y = exp(y - 1.96 * se - ref)), linetype = 2) + 
  geom_line(data = tp$vd3, aes(x = x, y = exp(y + 1.96 * se - ref)), linetype = 2) +
  geom_hline(aes(yintercept = 1), linetype = 2) +

  labs(x = "Vitamin D, nmol/L",
       y = "Hazard Ratio (95% CI)",
       title = "All participants")
# alb high
tp <- termplot(model.all , se = TRUE, plot = FALSE)
ref <- tp$vd3$y[tp$vd3$x == 0.30372898]

 ggplot() +
  
  geom_line(data = tp$vd3, aes(x = x, y = exp(y - ref))) + 
  geom_line(data = tp$vd3, aes(x = x, y = exp(y - 1.96 * se - ref)), linetype = 2) + 
  geom_line(data = tp$vd3, aes(x = x, y = exp(y + 1.96 * se - ref)), linetype = 2) +
  
  geom_hline(aes(yintercept = 1), linetype = 3) +
  geom_rug(data = albhigh, aes(x = vd3), sides = "b") +
  
  labs(x = "Vitamin D, nmol/L",
       y = "Hazard Ratio (95% CI)",
       title = "Mortality hazard ratio as a function of age",
       subtitle = "Alb high")

library(ggpubr)



# by gender
tp <- termplot(model.male , se = TRUE, plot = F)
ref <- tp$vd3$y[tp$vd3$x == 50.00]
tp1 <- termplot(model.female, se = TRUE, plot = F)
ref1 <- tp1$vd3$y[tp1$vd3$x == 48.32]

ggplot() +
  
  geom_line(data = tp$vd3, aes(x = x, y = exp(y - ref)), colour = 'red') + 
  geom_line(data = tp$vd3, aes(x = x, y = exp(y - 1.96 * se - ref)), linetype = 3, colour = 'red') + 
  geom_line(data = tp$vd3, aes(x = x, y = exp(y + 1.96 * se - ref)), linetype = 3, colour = 'red') +
  geom_line(data = tp1$vd3, aes(x = x, y = exp(y - ref1)), colour = 'blue') + 
  geom_line(data = tp1$vd3, aes(x = x, y = exp(y - 1.96 * se - ref1)), linetype = 3, colour = 'blue') + 
  geom_line(data = tp1$vd3, aes(x = x, y = exp(y + 1.96 * se - ref1)), linetype = 3, colour = 'blue') +
  geom_hline(aes(yintercept = 1), linetype = 2) +
  geom_histogram(data=male, fill="red",aes(x=vd3,y=5*..count../sum(..count..)),binwidth = 5,alpha=I(0.3)) +
  geom_histogram(data=female, fill="blue",aes(x=vd3,y=5*..count../sum(..count..)),binwidth = 5,alpha=I(0.3)) +
  
  labs(x = "Vitamin D, nmol/L",
       y = "Hazard Ratio (95% CI)",
       title = "")



















#ML RSF

setwd("C:/Users/18581/Desktop/HABCDEMENTIA")
library("randomForestSRC")
library("caret")
library("survival")
library("pec")
library("prodlim")
library("ggplot2")
library("pec")
library("CoxBoost")
library("survival")
library("riskRegression")
library("prodlim")
library("Matrix")
library("survminer")

set.seed(100)
all_data=read.csv("all_data.csv",header=T)
prediction=data.frame(all_data)

 #imputaiton 
f <- as.formula(Surv(days, status) ~ .)
prediction.i <- impute( f,data = prediction )
#Test and training
inTrain <- createDataPartition(y=prediction.i$status, p=0.666, list=F)  
train_dementia<- prediction.i[inTrain, ] #trainin set
test_dementia<- prediction.i[-inTrain,] #test set
prediction.good= prediction.i[c("days","status","mmmscore",	"dss",	"epeseppb",	"y1uwratio",	"apoehap",	"p4tap",	"pm50cur",	"adipon1",	"bqshhrs",	"diabp",	"y1rxtot",	"creatin1",	"fsbtime"	,"albumin1"	,"f1hstat",	"pr",	"ces_d",	"qrs"	,"hg_a1c1",	"fphospx"	,"sysbp"	,"fast8glu1",	"qt"	,"hqssfc1m"	,"b2corn",	"hqsshapy",	"b2hdog",	"glucose1",	"bqwt50",	"b2apa",	"b2crab",	"b2burg",	"hr",	"b2cnpc",	"b2frys",	"insulin1",	"axis",	"pai1_1", "resistin1",	"shbg1",	"b2yams",	"b2peas",	"hqssrclo",	"b2frch",	"wtotbmd"	,"p4ltr1",	"b2saus",	"hdl1"	,"cyst_uncal1",	"p4rtr1",	"hqssfclo"	,"crp_calib1"	,"f1appet"	,"p4rtr2"	,"y1wtk"	,"fast8ins1",	"totkkwk"	,"hr_gluc1"	,"crp_lcbr1"	,"il6sr1"	,"srage1"	,"p2sgi",	"fast8hdl1",	"triglyc1",	"b2pork",	"b3busos1"	,"wtotbmc")]
prediction.20= prediction.i[c("days","status","mmmscore",	"dss",	"epeseppb",	"y1uwratio",	"apoehap",	"p4tap",	"pm50cur",	"adipon1",	"bqshhrs",	"diabp",	"y1rxtot",	"creatin1",	"fsbtime"	,"albumin1"	,"f1hstat",	"pr",	"ces_d",	"qrs"	,"hg_a1c1",	"fphospx")]
prediction.10= prediction.i[c("days","status","mmmscore",	"dss",	"epeseppb",	"y1uwratio",	"apoehap",	"p4tap",	"pm50cur",	"adipon1",	"bqshhrs",	"diabp")]
inTrain <- createDataPartition(y=prediction.10$status, p=0.666, list=F)  


#rsf model training and prediction
Model.train.rsf <- rfsrc(Surv(days, status) ~ ., prediction.i,nodesize=10,importance = T,   ntree = 1000)
vs.dementia <- var.select(object = Model.train.rsf)
topvars <- vs.dementia$topvars
print (topvars)
find.interaction(Model.train.rsf, method = "vimp", nvar = 8)

pbc.obj <- rfsrc(Surv(days, status) ~ ., pbc, nodesize = 20, importance = TRUE)

#coxm model training and prediciton
Model.train.cox.good <- coxph(Surv(days, status) ~ ., prediction.good,x=T)
Model.train.rsfc.good <- rfsrc(Surv(days, status) ~ ., prediction.good,   ntree = 1000)

Model.train.cox.10 <- coxph(Surv(days, status) ~ ., prediction.10,x=T)
Model.train.rsfc.10 <- rfsrc(Surv(days, status) ~ ., prediction.10,    ntree = 1000)

Model.train.cox.20 <-coxph(Surv(days, status) ~ ., prediction.20,x=T)
Model.train.rsfc.20 <- rfsrc(Surv(days, status) ~ ., prediction.20, ntree = 1000)

## C-index for all 
bcvCindex.good <- pec::cindex(list("Cox X2"=Model.train.cox.good,
"RSF"=Model.train.rsfc.good),
formula=Surv(days,status)~ .,
data=prediction.good,
splitMethod="bootcv",
B=5,
eval.times=seq(1,15,1))
print(bcvCindex)
plot(bcvCindex)

bcvCindex.10 <- pec::cindex(list("Cox X2"=Model.train.cox.10,
"RSF"=Model.train.rsfc.10),
formula=Surv(days,status)~ .,
data=prediction.10,
splitMethod="bootcv",
B=5,
eval.times=seq(1,15,1))
print(bcvCindex)
plot(bcvCindex)

bcvCindex <- pec::cindex(list("Cox X2"=Model.train.cox.20,
"RSF"=Model.train.rsfc.20),
formula=Surv(days,status)~ .,
data=prediction.20,
splitMethod="bootcv",
B=5,
eval.times=seq(1,15,1))
print(bcvCindex)
plot(bcvCindex)





if (library("survival", logical.return = TRUE)
    & library("pec", logical.return = TRUE)
    & library("prodlim", logical.return = TRUE))
{
  ##prediction function required for pec
  predictSurvProb.rfsrc <- function(object, newdata, times, ...){
    ptemp <- predict(object,newdata=newdata,...)$survival
    pos <- sindex(jump.times = object$time.interest, eval.times = times)
    p <- cbind(1,ptemp)[, pos + 1]
    if (NROW(p) != NROW(newdata) || NCOL(p) != length(times))
      stop("Prediction failed")
    p
  }
  ## data, formula specifications
 
  surv.f <- as.formula(Surv(days, status) ~ .)
  pec.f <- as.formula(Hist(days,status) ~ 1)
  ## run cox/rfsrc models
  ## for illustration we use a small number of trees
  cox.obj <- coxph(surv.f, data=prediction.10,x=T)
  rfsrc.obj <- rfsrc(surv.f, prediction.10, ntree = 150)
  set.seed(999)
  prederror <- pec(list(cox.obj,rfsrc.obj), data = prediction.10??? formula = pec.f,
                       splitMethod = "cv5")
  print(prederror)
  plot(prederror)
  ## compute out-of-bag C-index for cox regression and compare to rfsrc
  rfsrc.obj <- rfsrc(surv.f, prediction.10)
  cat("out-of-bag Cox Analysis ...", "\n")
  cox.err <- sapply(1:100, function(b) {
    if (b%%10 == 0) cat("cox bootstrap:", b, "\n")
    train <- sample(1:nrow(prediction.10), nrow(prediction.10), replace = TRUE)
    cox.obj <- tryCatch({coxph(surv.f, prediction.10[inTrain, ])}, error=function(ex){NULL})
    if (!is.null(cox.obj)) {
    get.cindex(prediction.10$days[-inTrain,], prediction.10$status[-inTrain,], predict(cox.obj, prediction.10[-inTrain, ]))
    } else NA
  })
  cat("\n\tOOB error rates\n\n")
  cat("\tRSF : ", rfsrc.obj$err.rate[rfsrc.obj$ntree], "\n")
  cat("\tCox regression : ", mean(cox.err, na.rm = TRUE), "\n")
}










if (library("survival", logical.return = TRUE)
    & library("pec", logical.return = TRUE)
    & library("prodlim", logical.return = TRUE))
{
  ##prediction function required for pec
  predictSurvProb.rfsrc <- function(object, newdata, times, ...){
    ptemp <- predict(object,newdata=newdata,...)$survival
    pos <- sindex(jump.times = object$time.interest, eval.times = times)
    p <- cbind(1,ptemp)[, pos + 1]
    if (NROW(p) != NROW(newdata) || NCOL(p) != length(times))
      stop("Prediction failed")
    p
  }
  ## data, formula specifications
  data(pbc, package = "randomForestSRC")
  pbc.na <- na.omit(pbc) ##remove NA's
  surv.f <- as.formula(Surv(days, status) ~ .)
  pec.f <- as.formula(Hist(days,status) ~ 1)
  ## run cox/rfsrc models
  ## for illustration we use a small number of trees
  cox.obj <- coxph(surv.f, data = pbc.na, x = TRUE)
  rfsrc.obj <- rfsrc(surv.f, pbc.na, ntree = 150)
  ## compute bootstrap cross-validation estimate of expected Brier score
## see Mogensen, Ishwaran and Gerds (2012) Journal of Statistical Software
set.seed(17743)
prederror.pbc <- pec(list(cox.obj,rfsrc.obj), data = pbc.na, formula = pec.f,
                     splitMethod = "bootcv", B = 50)
print(prederror.pbc)
plot(prederror.pbc)
## compute out-of-bag C-index for cox regression and compare to rfsrc
rfsrc.obj <- rfsrc(surv.f, pbc.na)
cat("out-of-bag Cox Analysis ...", "\n")
cox.err <- sapply(1:100, function(b) {
  if (b%%10 == 0) cat("cox bootstrap:", b, "\n")
  train <- sample(1:nrow(pbc.na), nrow(pbc.na), replace = TRUE)
  cox.obj <- tryCatch({coxph(surv.f, pbc.na[train, ])}, error=function(ex){NULL})
  if (!is.null(cox.obj)) {
    get.cindex(pbc.na$days[-train], pbc.na$status[-train], predict(cox.obj, pbc.na[-train, ]))
  } else NA
})
cat("\n\tOOB error rates\n\n")
cat("\tRSF : ", rfsrc.obj$err.rate[rfsrc.obj$ntree], "\n")
cat("\tCox regression : ", mean(cox.err, na.rm = TRUE), "\n")
}

##ventilation

install.packages("segmented")
install.packages("splines")
install.packages("ggplot2")
install.packages("Hmisc")
install.packages("rms")
install.packages("survival")
install.packages("ggpubr")
install.packages("openxlsx")

## install packages
library(segmented)
library(splines)
library(ggplot2)
library(Hmisc)
library(rms)
library(survival)
library(ggpubr)
library(openxlsx)
setwd("E:/Desktop/Code/spline")
data=read.csv(file="venti.csv",header=T)

data<-sapply(data,as.numeric)
biomarker=data.frame(data)


dd <- datadist(biomarker)
options(datadist='dd')
venmedian=subset(biomarker,ndvitep2==1)
venhigh=subset(biomarker,ndvitep2==2)
venlow=subset(biomarker,ndvicum2==1)
venlow1=subset(biomarker,ndvicum2==2)


fit <- lrm(dep_bi ~ rcs(ven_index,3) + trueage + gender + residenc ,
           data = biomarker)
dd$limits$ven_index[2]=12
fit=update(fit)
HR1<-Predict(fit, ven_index,fun=exp,ref.zero =T)


fit <- lrm(dep_bi ~ rcs(contndvi10,3) + trueage + gender + edug + mari2 + bmi1 + income_bi + socleis + smoke + drink + tea3+ srh+ DietDiver9 +ci + adl_sum + pm254+ season + geo ,
           data = venlow)
dd$limits$contndvi10[2]=5
fit=update(fit)
HR2<-Predict(fit, contndvi10,fun=exp,ref.zero =T)


fit <- lrm(dep_bi ~ rcs(contndvi10,3) + trueage + gender + edug + mari2 + bmi1 + income_bi + socleis + smoke + drink + tea3+ srh+ DietDiver9 +ci + adl_sum + pm254+ season + geo ,
           data = venmedian)
dd$limits$contndvi10[2]=5
fit=update(fit)
HR3<-Predict(fit, contndvi10,fun=exp,ref.zero =T)


fit <- lrm(dep_bi ~ rcs(contndvi10,3) + trueage + gender + edug + mari2 + bmi1 + income_bi + socleis + smoke + drink + tea3+ srh+ DietDiver9 +ci + adl_sum + pm254+ season + geo ,
           data = venhigh)
dd$limits$contndvi10[2]=5
fit=update(fit)
HR4<-Predict(fit, contndvi10,fun=exp,ref.zero =T)


fit <- lrm(dep_bi ~ rcs(contndvi10,3) + trueage + gender + edug + mari2 + bmi1 + income_bi + socleis + smoke + drink + tea3+ srh+ DietDiver9 +ci + adl_sum + pm254+ season + geo ,
           data = venhigh)
dd$limits$contndvi10[2]=5
fit=update(fit)
HR5<-Predict(fit, contndvi10,fun=exp,ref.zero =T)



P1<-ggplot()+geom_line(data=HR1, aes(ven_index,yhat),linetype="solid",size=1,alpha = 0.7,colour="red")+
  geom_ribbon(data=HR1, aes(ven_index,ymin = lower, ymax = upper),alpha = 0.1,fill="red")
P1

P2<-ggplot()+geom_line(data=HR2, aes(contndvi10,yhat),linetype="solid",size=1,alpha = 0.7,colour="red")+
  geom_ribbon(data=HR2, aes(contndvi10,ymin = lower, ymax = upper),alpha = 0.1,fill="red")

P3<-ggplot()+geom_line(data=HR3, aes(contndvi10,yhat),linetype="solid",size=1,alpha = 0.7,colour="red")+
  geom_ribbon(data=HR3, aes(contndvi10,ymin = lower, ymax = upper),alpha = 0.1,fill="red")

P4<-ggplot()+geom_line(data=HR4, aes(contndvi10,yhat),linetype="solid",size=1,alpha = 0.7,colour="red")+
  geom_ribbon(data=HR4, aes(contndvi10,ymin = lower, ymax = upper),alpha = 0.1,fill="red")

P5=ggplot()  + geom_line(data=HR2, aes(contndvi10,yhat),linetype="solid",size=1,alpha = 0.5,colour="red")+
  geom_ribbon(data=HR2, aes(contndvi10,ymin = lower, ymax = upper),alpha = 0.1,fill="red")  +geom_line(data=HR3, aes(contndvi10,yhat),linetype="solid",size=1,alpha = 0.5,colour="blue")+
  geom_ribbon(data=HR3, aes(contndvi10,ymin = lower, ymax = upper),alpha = 0.1,fill="blue") +geom_line(data=HR4, aes(contndvi10,yhat),linetype="solid",size=1,alpha = 0.9,colour="darkseagreen4")+
  geom_ribbon(data=HR4, aes(contndvi10,ymin = lower, ymax = upper),alpha = 0.1,fill="darkseagreen4")

P5<-P1+theme_classic()+geom_hline(yintercept=1, linetype=2,size=1,,alpha = 0.5)+ 
  labs(title = "RCS", x="NDVI", y="Odds Ratio (95% CI)") 
P5


P6<-P1+theme_classic()+geom_hline(yintercept=1, linetype=2,size=1,,alpha = 0.1)+ 
  labs(title = "RCS", x="NDVI", y="Odds Ratio (95% CI)") 
P6

##?Ŵ?

## install packages
library(segmented)
library(splines)
library(ggplot2)
library(Hmisc)
library(rms)
library(survival)
library(ggpubr)
library(openxlsx)
setwd("E:/Desktop/Code/spline")
data=read.csv(file="ndvi_spine.csv",header=T)

data<-sapply(data,as.numeric)
biomarker=data.frame(data)

venmedian=subset(biomarker,ADRISK==1)
venhigh=subset(biomarker,ADRISK==2)
 
 dd <- datadist(biomarker)
options(datadist='dd')


fit <- lrm(ci_bi ~ rcs(ADprs,3) + trueage + gender + residence + ethnicity +edug  +occupation +marital +smkl + dril + pa + dd_bi   , data = biomarker)
dd$limits$ADprs[2]=0
fit=update(fit)
HR1<-Predict(fit, ADprs,fun=exp,ref.zero =T)


fit <- lrm(ci_bi ~ rcs(ndvitep,3) + trueage + gender + residence + ethnicity +edug  +occupation +marital +smkl + dril + pa + dd_bi   , data = biomarker)
dd$limits$ndvitep[2]=0.403156
fit=update(fit)
HR2<-Predict(fit, ndvitep,fun=exp,ref.zero =T)

fit <- lrm(ci_bi ~ rcs(average,3) + trueage + gender + residence + ethnicity +edug  +occupation +marital +smkl + dril + pa + dd_bi   , data = biomarker)
dd$limits$average[2]=0.4510278
fit=update(fit)
HR3<-Predict(fit, average,fun=exp,ref.zero =T)


fit <- lrm(ci_bi ~ rcs(ndvitep,3) + trueage + gender + residence + ethnicity +edug  +occupation +marital +smkl + dril + pa + dd_bi +adl + leisure   , data = venmedian)
dd$limits$ndvitep[2]=0.403156
fit=update(fit)
HR4<-Predict(fit, ndvitep,fun=exp,ref.zero =T)

fit <- lrm(ci_bi ~ rcs(ndvitep,3) + trueage + gender + residence + ethnicity +edug  +occupation +marital +smkl + dril + pa + dd_bi +adl + leisure   , data = venhigh)
dd$limits$ndvitep[2]=0.403156
fit=update(fit)
HR5<-Predict(fit, ndvitep,fun=exp,ref.zero =T)

fit <- lrm(ci_bi ~ rcs(average,3) + trueage + gender + residence + ethnicity +edug  +occupation +marital +smkl + dril + pa + dd_bi +adl + leisure   , data = venmedian)
dd$limits$average[2]=0.4510278
fit=update(fit)
HR6<-Predict(fit, average,fun=exp,ref.zero =T)

fit <- lrm(ci_bi ~ rcs(average,3) + trueage + gender + residence + ethnicity +edug  +occupation +marital +smkl + dril + pa + dd_bi +adl + leisure   , data = venhigh)
dd$limits$average[2]=0.4510278
fit=update(fit)
HR7<-Predict(fit, average,fun=exp,ref.zero =T)
 
  

P1<-ggplot()+geom_line(data=HR1, aes(ADprs,yhat),linetype="solid",size=1,alpha = 0.7,colour="black")+
  geom_ribbon(data=HR1, aes(ADprs,ymin = lower, ymax = upper),alpha = 0.1,fill="black")
P1

P2<-ggplot()+geom_line(data=HR2, aes(ndvitep,yhat),linetype="solid",size=1,alpha = 0.7,colour="black")+
  geom_ribbon(data=HR2, aes(ndvitep,ymin = lower, ymax = upper),alpha = 0.1,fill="black")
P2
P3<-ggplot()+geom_line(data=HR3, aes(average,yhat),linetype="solid",size=1,alpha = 0.7,colour="black")+
  geom_ribbon(data=HR3, aes(average,ymin = lower, ymax = upper),alpha = 0.1,fill="black")
P3


P4<-ggplot()+geom_line(data=HR4, aes(ndvitep,yhat),linetype="solid",size=1,alpha = 0.7,colour="red")+
  geom_ribbon(data=HR4, aes(ndvitep,ymin = lower, ymax = upper),alpha = 0.1,fill="red")
P4
P5<-ggplot()+geom_line(data=HR5, aes(ndvitep,yhat),linetype="solid",size=1,alpha = 0.7,colour="red")+
  geom_ribbon(data=HR5, aes(ndvitep,ymin = lower, ymax = upper),alpha = 0.1,fill="red")
P5

P4<-ggplot()+geom_line(data=HR6, aes(average,yhat),linetype="solid",size=1,alpha = 0.7,colour="red")+
  geom_ribbon(data=HR6, aes(average,ymin = lower, ymax = upper),alpha = 0.1,fill="red")
P4
P5<-ggplot()+geom_line(data=HR7, aes(average,yhat),linetype="solid",size=1,alpha = 0.7,colour="red")+
  geom_ribbon(data=HR7, aes(average,ymin = lower, ymax = upper),alpha = 0.1,fill="red")
P5




P6=ggplot()  + geom_line(data=HR4, aes(ndvitep,yhat),linetype="solid",size=1,alpha = 0.5,colour="red")+
  geom_ribbon(data=HR4, aes(ndvitep,ymin = lower, ymax = upper),alpha = 0.1,fill="red")  +geom_line(data=HR5, aes(ndvitep,yhat),linetype="solid",size=1,alpha = 0.5,colour="blue")+
  geom_ribbon(data=HR5, aes(ndvitep,ymin = lower, ymax = upper),alpha = 0.1,fill="blue")   
P6

P7=ggplot()  + geom_line(data=HR6, aes(average,yhat),linetype="solid",size=1,alpha = 0.5,colour="red")+
  geom_ribbon(data=HR6, aes(average,ymin = lower, ymax = upper),alpha = 0.1,fill="red")  +geom_line(data=HR7, aes(average,yhat),linetype="solid",size=1,alpha = 0.5,colour="blue")+
  geom_ribbon(data=HR7, aes(average,ymin = lower, ymax = upper),alpha = 0.1,fill="blue")   
P7

P1<-P1+theme_classic()+geom_hline(yintercept=1, linetype=2,size=1,,alpha = 0.5)+ 
  labs(title = "RCS", x="NDVI", y="Odds Ratio (95% CI)")  + ylim(0,2.5) 
P1

P2<-P2+theme_classic()+geom_hline(yintercept=1, linetype=2,size=1,,alpha = 0.5)+ 
  labs(title = "RCS", x="NDVI", y="Odds Ratio (95% CI)")  + ylim(0,2.5) 
P2

P3<-P3+theme_classic()+geom_hline(yintercept=1, linetype=2,size=1,,alpha = 0.5)+ 
  labs(title = "RCS", x="NDVI", y="Odds Ratio (95% CI)")  + ylim(0,2.5) 
P3

P6<-P6+theme_classic()+geom_hline(yintercept=1, linetype=2,size=1,,alpha = 0.5)+ 
  labs(title = "RCS", x="NDVI", y="Odds Ratio (95% CI)")   
P6
 

P7<-P7+theme_classic()+geom_hline(yintercept=1, linetype=2,size=1,,alpha = 0.1)+ 
  labs(title = "RCS", x="NDVI", y="Odds Ratio (95% CI)")   
P7


