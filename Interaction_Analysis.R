data_used<-read.csv(file = "/Users/DELL/Documents/GH Intern/Distance2Park&Depression/Lengyu_location_park.csv", fileEncoding = "utf-8")
list_trybi<-c("gender","residenc","edug_bi","marital","smkl","dril","pa","ven_bi","income_bi","location_bi","community_score_bi","region","adl","leisure_bi")
gender<-data_used$gender
residenc<-data_used$residenc
edug<-data_used$edug
marital<-data_used$marital
smkl<-data_used$smkl
dril<-data_used$dril
pa<-data_used$pa
ven_bi<-data_used$ven_bi
income_bi<-data_used$income_bi
location_bi<-data_used$location_bi
community_score_tri<-data_used$community_score_tri
region<-data_used$region
adl<-data_used$adl
leisure_bi<-data_used$leisure_bi
edug_bi<-ifelse(edug==0,0,1)
community_score_bi<-ifelse(community_score_tri==0|community_score_tri==1,0,1)
for (i in list_trybi) {
  model.dd <- glm(dep_bi ~ trueage + factor(distance_bi)
                  +factor(distance_bi)*factor(get(i))+factor(data_used$gender)
                  +factor(data_used$residenc)+factor(data_used$income_bi)
                  +factor(data_used$edug_bi)+factor(data_used$marital)+factor(data_used$smkl)
                  +factor(data_used$dril)+factor(data_used$adl)+factor(data_used$leisure_bi)
                  +factor(data_used$urban_medical_insurance)
                  +factor(data_used$rural_cooperative_insurance)
                  +factor(data_used$region)+factor(data_used$location_bi)
                  +factor(data_used$ven_bi)+factor(data_used$hypertension)
                  +factor(data_used$cancer)+factor(data_used$diabetes)
                  +data_used$strokecvd+data_used$heartdisea+data_used$tb
                  +data_used$parkinson, family = binomial(link = logit), data = data_used)
  print(summary(model.dd))
  print(exp(coef(model.dd)))
  print(exp(confint(model.dd)))
}
