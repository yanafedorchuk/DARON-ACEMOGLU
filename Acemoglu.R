library(foreign)
library(ggplot2)
library(sandwich)
library(gmm)
library(AER)
library(latex2exp)
library(xtable)

data=read.dta("/home/xenakas/Desktop/hw1/Acemoglu.dta")

#########################
ggplot(data, aes(x=prot, y=lgdp)) + geom_point() + geom_smooth(method='lm',formula=y~x) + xlab("Мера защиты прав собственности в данной стране") +
  ylab("Логарифм ВВП на душу населения")

model_a <- lm(data=data, lgdp ~ prot ) 

summary(model_a)

#########################
model_b <- lm(data=data, lgdp ~ prot + latitude)  
summary(model_b)

#########################
model_c <- lm(data=data, lgdp ~ logmort)  
summary(model_c) 

ggplot(data, aes(x =logmort, y=lgdp)) + geom_point() + geom_smooth(method='lm',formula=y~x) + xlab("Логарифм уровня смертности колонистов") +
  ylab("Логарифм ВВП на душу населения")

#########################
#tslsmodel <- tsls(lgdp ~ prot, ~ logmort + prot, data=data)

tslsmodel_d <- ivreg(lgdp ~ prot | logmort , data=data )
summary(tslsmodel_d)
#table 5 base sample 7

firststep_e <- lm(prot ~ logmort, data=data)
summary(firststep_e)
#table 3 (9)

ggplot(data, aes(x=logmort, y=prot)) + geom_point() + geom_smooth(method='lm',formula=y~x)  + xlab("Логарифм уровня смертности колонистов") +
  ylab("Мера защиты прав собственности в данной стране")

prot_est <- firststep_e$fitted.values
prot <- data$prot

secondstep_f <- lm(lgdp ~ prot_est , data=data)
summary(secondstep_f)

#########################
tslsmodel_g <- ivreg(lgdp ~ prot  + latitude   | logmort + latitude , data=data )
summary(tslsmodel_g)
#table 4 base sample 2

firststep_g <- lm(prot ~ logmort + latitude , data=data)
summary(firststep_g)

prot_est_g <- firststep_g$fitted.values

secondstep_g <- lm(lgdp ~ prot_est_g + latitude , data=data)
summary(secondstep_g)

#########################
newdata <- subset(data, africa==0)

tslsmodel_h <- ivreg(lgdp ~ prot | logmort , data=newdata )
summary(tslsmodel_h)
#table 5 base sample 7

tslsmodel_i <- ivreg(lgdp ~ prot  + latitude   | logmort + latitude , data=newdata )
summary(tslsmodel_i)


##########################
tslsmodel_j <- ivreg(lgdp ~ prot + euro | logmort + euro , data=data )
summary(tslsmodel_j)

tslsmodel_k <- ivreg(lgdp ~ prot + euro + latitude | logmort + euro + latitude , data=data )
summary(tslsmodel_k)


print(xtable(model_a), include.rownames = TRUE, include.colnames = TRUE, sanitize.text.function = I)

