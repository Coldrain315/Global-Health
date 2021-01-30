data_used<-read.csv(file = "/Users/DELL/Documents/GH Intern/Distance2Park&Depression/01 Lengyu_distance_to_park_depression.csv", fileEncoding = "utf-8")
distance_cat<-cut(data_used$distance_park, breaks = c(-Inf,0.9, 5, 10,Inf), labels = c(0,1,2,3))
summary(distance_cat)
model1.dd <- glm(dep_bi ~ trueage + factor(distance_cat)+  
                   factor(data_used$gender), family = binomial(link = logit), 
                   data = data_used)
summary(model1.dd)
exp(coef(model1.dd))
exp(confint(model1.dd))

model2.dd <- glm(dep_bi ~ data_used$trueage + factor(distance_cat)+  
                   factor(data_used$gender) + factor(data_used$residenc) + 
                   factor(data_used$edug) + factor(data_used$marital) +
                   factor(data_used$smkl) + factor(data_used$dril) + 
                   factor(data_used$pa)+ data_used$bmi+ factor(data_used$meat) , 
                   family = binomial(link = logit))
summary(model2.dd)
exp(coef(model2.dd))
exp(confint(model2.dd))

model3.dd <- glm(dep_bi ~ data_used$trueage + factor(distance_cat)+  factor(data_used$gender) + 
                   factor(data_used$residenc) + factor(data_used$edug) + 
                   factor(data_used$marital) +factor(data_used$smkl) + 
                   factor(data_used$dril) + factor(data_used$pa)+ data_used$bmi+ 
                   factor(data_used$meat)+factor(data_used$income_bi)+
                   factor(data_used$citylevel)+factor(data_used$urban_medical_insurance)+
                   factor(data_used$commercial_medical_insurance)+factor(data_used$f151), 
                   family = binomial(link = logit))
summary(model3.dd)
exp(coef(model3.dd))
exp(confint(model3.dd))

library(MatchIt) 


lalonde$treat <- factor(lalonde$treat, levels=c(0, 1, 2), labels=c("Control", "Treatment", "P-value"))
lalonde$black    <- factor(lalonde$black)
lalonde$hispan   <- factor(lalonde$hispan)
lalonde$married  <- factor(lalonde$married)
lalonde$nodegree <- factor(lalonde$nodegree)
lalonde$black    <- as.logical(lalonde$black == 1)
lalonde$hispan   <- as.logical(lalonde$hispan == 1)
lalonde$married  <- as.logical(lalonde$married == 1)
lalonde$nodegree <- as.logical(lalonde$nodegree == 1)

label(lalonde$black)    <- "Black"
label(lalonde$hispan)   <- "Hispanic"
label(lalonde$married)  <- "Married"
label(lalonde$nodegree) <- "No high school diploma"
label(lalonde$age)      <- "Age"
label(lalonde$re74)     <- "1974 Income"
label(lalonde$re75)     <- "1975 Income"
label(lalonde$re78)     <- "1978 Income"
units(lalonde$age)      <- "years"

rndr <- function(x, name, ...) {
  if (length(x) == 0) {
    y <- lalonde[[name]]
    s <- rep("", length(render.default(x=y, name=name, ...)))
    if (is.numeric(y)) {
      p <- t.test(y ~ lalonde$treat)$p.value
    } else {
      p <- chisq.test(table(y, droplevels(lalonde$treat)))$p.value
    }
    s[2] <- sub("<", "&lt;", format.pval(p, digits=3, eps=0.001))
    s
  } else {
    render.default(x=x, name=name, ...)
  }
}

rndr.strat <- function(label, n, ...) {
  ifelse(n==0, label, render.strat.default(label, n, ...))
}

table1(~ age + black + hispan + married + nodegree + re74 + re75 + re78 | treat,
       data=lalonde, droplevels=F, render=rndr, render.strat=rndr.strat, overall=F)