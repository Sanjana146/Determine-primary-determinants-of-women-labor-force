library(ggplot2)
library(useful)
library(car)
library(lmtest)
require(coefplot)
library(car)
install.packages("readxl")
library("readxl")
mwlab<-read_excel("C:\\Users\\sanjana\\OneDrive\\Desktop\\SNU MONSOON 2022 COURSES\\DOM 207\\DOM207 MPSS\\DOM207MINIPROJECT4-SANJANA ADITYA\\MWLabPartmod.xlsx",sheet=1)
head(mwlab)
shapiro.test(mwlab$hours)
shapiro.test(log(mwlab$hours+1))
shapiro.test(sqrt(mwlab$hours+1))
shapiro.test(1/(mwlab$hours+1))

shapiro.test(mwlab$age)
shapiro.test(log(mwlab$age+1))
shapiro.test(sqrt(mwlab$age+1))
shapiro.test(1/(mwlab$age+1))

shapiro.test(mwlab$kidslt6)
shapiro.test(log(mwlab$kidslt6+1))
shapiro.test(sqrt(mwlab$kidslt6+1))
shapiro.test(1/(mwlab$kidslt6+1))

shapiro.test(mwlab$kidsge6)
shapiro.test(log(mwlab$kidsge6+1))
shapiro.test(sqrt(mwlab$kidsge6+1))
shapiro.test(1/(mwlab$kidsge6+1))


shapiro.test(mwlab$edu)
shapiro.test(log(mwlab$edu+1))
shapiro.test(sqrt(mwlab$edu+1))
shapiro.test(1/(mwlab$edu+1))

shapiro.test(mwlab$wage)
shapiro.test(log(mwlab$wage+1))
shapiro.test(sqrt(mwlab$wage+1))
shapiro.test(1/(mwlab$wage+1))

shapiro.test(mwlab$repwage)
shapiro.test(log(mwlab$repwage+1))
shapiro.test(sqrt(mwlab$repwage+1))
shapiro.test(1/(mwlab$repwage+1))


shapiro.test(mwlab$hushrs)
shapiro.test(log(mwlab$hushrs+1))
shapiro.test(sqrt(mwlab$hushrs+1))
shapiro.test(1/(mwlab$hushrs+1))

shapiro.test(mwlab$husage)
shapiro.test(log(mwlab$husage+1))
shapiro.test(sqrt(mwlab$husage+1))
shapiro.test(1/(mwlab$husage+1))

shapiro.test(mwlab$husedu)
shapiro.test(log(mwlab$husedu+1))
shapiro.test(sqrt(mwlab$husedu+1))
shapiro.test(1/(mwlab$husedu+1))

shapiro.test(mwlab$huswage)
shapiro.test(log(mwlab$huswage+1))
shapiro.test(sqrt(mwlab$huswage+1))
shapiro.test(1/(mwlab$huswage+1))

shapiro.test(mwlab$faminc)
shapiro.test(log(mwlab$faminc+1))
shapiro.test(sqrt(mwlab$faminc+1))
shapiro.test(1/(mwlab$faminc+1))

shapiro.test(mwlab$mtr)
shapiro.test(log(mwlab$mtr+1))
shapiro.test(sqrt(mwlab$mtr+1))
shapiro.test(1/(mwlab$mtr+1))

shapiro.test(mwlab$momedu)
shapiro.test(log(mwlab$momedu+1))
shapiro.test(sqrt(mwlab$momedu+1))
shapiro.test(1/(mwlab$momedu+1))

shapiro.test(mwlab$dadedu)
shapiro.test(log(mwlab$dadedu+1))
shapiro.test(sqrt(mwlab$dadedu+1))
shapiro.test(1/(mwlab$dadedu+1))

shapiro.test(mwlab$unem)
shapiro.test(log(mwlab$unem+1))
shapiro.test(sqrt(mwlab$unem+1))
shapiro.test(1/(mwlab$unem+1))

shapiro.test(mwlab$exper)
shapiro.test(log(mwlab$exper+1))
shapiro.test(sqrt(mwlab$exper+1))
shapiro.test(1/(mwlab$exper+1))

shapiro.test(mwlab$nwifeinc)
shapiro.test(sqrt(mwlab$nwifeinc+1))
shapiro.test(1/(mwlab$nwifeinc+1))


mwlab<-read_excel("C:\\Users\\sanjana\\OneDrive\\Desktop\\SNU MONSOON 2022 COURSES\\DOM 207\\DOM207 MPSS\\DOM207MINIPROJECT4-SANJANA ADITYA\\MWLabPartmod.xlsx",sheet=2)
head(mwlab)
shapiro.test(mwlab$hours)
shapiro.test(mwlab$kidslt6)
shapiro.test(mwlab$kidsge6)
shapiro.test(mwlab$age)
shapiro.test(mwlab$edu)
shapiro.test(mwlab$husage)
shapiro.test(mwlab$husedu)
shapiro.test(mwlab$huswage)
shapiro.test(mwlab$faminc)
shapiro.test(mwlab$mtr)
shapiro.test(mwlab$momedu)
shapiro.test(mwlab$dadedu)
shapiro.test(mwlab$unem)
shapiro.test(mwlab$exper)
shapiro.test(mwlab$nwifeinc)
shapiro.test(mwlab$repwage)
shapiro.test(mwlab$hushrs)
shapiro.test(mwlab$wage)

model<- glm(inlf ~ hours + kidslt6 + kidsge6+ age + edu+wage+repwage+hushrs+husage+husedu+huswage+faminc+mtr+momedu+dadedu+unem+city+exper+nwifeinc,data=mwlab, family=binomial(link="logit"))
summary(model)
bptest(model,data=mwlab)
vif(model)

model<- glm(inlf ~ hours + kidslt6 + kidsge6+ age + edu+wage+repwage+hushrs+husage+husedu+huswage+mtr+momedu+dadedu+unem+city+exper,data=mwlab, family=binomial(link="logit"))
summary(model)
bptest(model,data=mwlab)
vif(model)


model<- glm(inlf ~kidslt6+kidsge6+age +edu+repwage+hushrs+husage+husedu+huswage+momedu+city+exper+unem+dadedu+mtr,data=mwlab, family=binomial(link="logit"))
summary(model)
bptest(model,data=mwlab)
vif(model)

invlogit<-function(x)
{
  1/(1+exp(-x))
}
invlogit(model$coefficients)

library(dplyr)
library(explore)
install.packages("explore")
library(explore)
mwlab%>%explore_tbl()
mwlab %>% describe()
attach(mwlab)
plot(age,inlf)
abline(lm(inlf~age))
title("Regression of inlf on age")

plot(kidslt6,inlf)
abline(lm(inlf~kidslt6))
title("Regression of inlf on kidslt6")

plot(kidsge6,inlf)
abline(lm(inlf~kidsge6))
title("Regression of inlf on kidsge6")

plot(edu,inlf)
abline(lm(inlf~edu))
title("Regression of inlf on edu")

plot(repwage,inlf)
abline(lm(inlf~repwage))
title("Regression of inlf on repwage")

plot(hushrs,inlf)
abline(lm(inlf~hushrs))
title("Regression of inlf on hushrs")

plot(husage,inlf)
abline(lm(inlf~husage))
title("Regression of inlf on husage")

plot(husedu,inlf)
abline(lm(inlf~husedu))
title("Regression of inlf on husedu")

plot(huswage,inlf)
abline(lm(inlf~huswage))
title("Regression of inlf on huswage")

plot(mtr,inlf)
abline(lm(inlf~mtr))
title("Regression of inlf on mtr")

plot(momedu,inlf)
abline(lm(inlf~momedu))
title("Regression of inlf on momedu")

plot(dadedu,inlf)
abline(lm(inlf~dadedu))
title("Regression of inlf on dadedu")
detach(mwlab)
