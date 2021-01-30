##install.packages("table1")
##install.packages("boot")
setwd("C:/Users/Administrator/Desktop/XR/CI")
library(boot)
library(table1)
library(MatchIt)
table1=read.csv(file="fortable1venti.csv",header=T)

table1$gender <- 
  factor(table1$gender, levels=c(1,2),
         labels=c("Male", 
                  "Female"))
label(table1$gender)       <- "Sex"
 
table1$residenc <- 
  factor(table1$residenc, levels=c(0,1),
         labels=c("Rural", 
                  "City"))
label(table1$residenc)       <- "Residency"
 
table1$ventilation_c  <- 
  factor(table1$ventilation_c , levels=c(2,3,4),
         labels=c("0-3", 
                  "3-5",">5"))
label(table1$ventilation_c)       <- "Spring"



table1$ventilation_x  <- 
  factor(table1$ventilation_x , levels=c(2,3,4),
         labels=c("0-3", 
                  "3-5",">5"))
label(table1$ventilation_x)       <- "Summer"

table1$ventilation_q  <- 
  factor(table1$ventilation_q , levels=c(2,3,4),
         labels=c("0-3", 
                  "3-5",">5"))
label(table1$ventilation_q)       <- "Autumn"

table1$ventilation_d  <- 
  factor(table1$ventilation_d , levels=c(2,3,4),
         labels=c("0-3", 
                  "3-5",">5"))
label(table1$ventilation_d)       <- "Winter"

table1$ci  <- 
  factor(table1$ci , levels=c(0,1),
         labels=c("Without", 
                  "With"))
label(table1$ci)       <- "Cognitive impairment"

table1$dep_bi  <- 
  factor(table1$dep_bi , levels=c(0,1),
         labels=c("Without", 
                  "With"))
label(table1$dep_bi)       <- "Depressive syptem"

table1$adl  <- 
  factor(table1$adl , levels=c(0,1),
         labels=c("Without", 
                  "With"))
label(table1$adl)       <- "Activity of daily living"

table1$edug  <- 
  factor(table1$edug , levels=c(1,2),
         labels=c("<6 years", 
                  ">6 years"))
label(table1$edug)       <- "Education years"


table1$marital   <- 
  factor(table1$marital  , levels=c(0,1),
         labels=c("Currently married and living with spouse", 
                  "Others"))
label(table1$marital)       <- "Marital status"


table1$smkl   <- 
  factor(table1$smkl  , levels=c(0,1),
         labels=c("Non-current", 
                  "Current"))
label(table1$smkl)       <- "Tobacco smoking"

table1$dril   <- 
  factor(table1$dril  , levels=c(0,1),
         labels=c("Non-current", 
                  "Current"))
label(table1$dril)       <- "Alcohol consumption"

table1$pa     <- 
  factor(table1$pa    , levels=c(0,1),
         labels=c("Non-current", 
                  "Current"))
label(table1$pa)       <- "Physical activity"




table1$income_bi    <- 
  factor(table1$income_bi  , levels=c(0,1),
         labels=c("<30000", 
                  ">=30000"))
label(table1$income_bi)       <- "family income"




table1$ven_tri   <- 
  factor(table1$ven_tri  , levels=c(0,1,2),
         labels=c("0", 
                  "1","2"))
label(table1$ven_tri)       <- "Cooking ventilation"

table1$pm_4    <- 
  factor(table1$pm_4   , levels=c(0,1),
         labels=c("< 55", 
                  "?? 55"))
label(table1$pm_4)       <- "PM2.5"

table1$region    <- 
  factor(table1$region   , levels=c(1,2,3,4),
         labels=c("Northern China",
"Eastern China","Southern China",
"Western China"))
label(table1$region)       <- "Geographical Region"

table1$city_bi   <- 
  factor(table1$city_bi  , levels=c(0,1),
         labels=c("< 5 million", 
                  "?? 5 million"))
label(table1$city_bi)       <- "City Population"

table1$insu   <- 
  factor(table1$insu  , levels=c(0,1),
         labels=c("< 5 million", 
                  "?? 5 million"))
label(table1$insu)       <- "City Population"


table1$remover_bi   <- 
  factor(table1$remover_bi  , levels=c(0,1),
         labels=c("Never", 
                  "Sometimes or oftern"))
label(table1$remover_bi)       <- "Oli remover"


#T-TEST or CHi-test
library(MatchIt) 
table1$venindex_4    <- factor(table1$venindex_4, levels=c( 1, 2,3,4,5), labels=c("0", "1", "2","3","P-value"))

rndr <- function(x, name, ...) {
  if (length(x) == 0) {
    y <- table1[[name]]
    s <- rep("", length(render.default(x=y, name=name, ...)))
    if (is.numeric(y)) {
      p <- t.test(y ~ table1$venindex_4)$p.value
    } else {
      p <- chisq.test(table(y, droplevels(table1$venindex_4)))$p.value
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


table1(~ gender + residenc+ trueage+bmi +ventilation_c+ventilation_x+ventilation_q+ventilation_d+insu+pm_4+ci+ven_tri+city_bi+region+dep_bi+adl+smkl+dril+pa+ edug+remover_bi +marital+income_bi| venindex_4, data=table1, overall="Total")
