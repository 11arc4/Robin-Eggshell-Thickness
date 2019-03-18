library(tidyverse)
library(lme4)
library(lmerTest)

setwd("C:/Users/11arc/Dropbox/AmeliaBob/Robin")

shell <- read.csv( "Thickness Dataset (for RMarkdown).csv", na.strings = "", as.is=T)
#remove oddly shaped eggs and calculate elongation. 
shell3 <- shell %>% filter(Length_egg>2.6) %>% mutate(Elongation=Length_egg/Width_egg)

#Hatching Success and Offspring Growth Rate:

#If we have yolk/albumen mass, then the egg was destroyed to collect. That looks
#to be almost all eggs-- eggs where we don't have that data appear to have
#broken or were unseperateable.


#Do albumen and yolk mass change with age of egg (amount incubated), clutch size, day laid, etc

#check what influences albumen mass
ggplot(shell3, aes(y=Mass_albumen, x=IncStage ))+
  geom_point()+
  geom_smooth(method="lm")

cmod_albumen1 <- lmer(Mass_albumen ~IncStage + (1|NestID), data=shell3)
plot(cmod_albumen1)
qqnorm(resid(cmod_albumen1))
qqline(resid(cmod_albumen1))
#Not a great fit at the low end, but OK at the high end.
anova(cmod_albumen1)
#suggests that inc stage should be controlled for (albumen mass decreases with inc stage)


ggplot(shell3, aes(y=Mass_albumen, x=LayOrder_est ))+
  geom_point()+
  geom_smooth(method="lm")

cmod_albumen2 <- lmer(Mass_albumen ~LayOrder_est + (1|NestID), data=shell3)
plot(cmod_albumen2)
qqnorm(resid(cmod_albumen2))
qqline(resid(cmod_albumen2))
#Not a great fit at the low end, but OK at the high end.
anova(cmod_albumen2)
summary(cmod_albumen2)
#suggests that Lay order should be controlled for (albumen mass decreases with inc stage)


ggplot(shell3, aes(y=Mass_albumen, x=factor(ClutchSize) ))+
  geom_boxplot()
cmod_albumen3 <- lmer(Mass_albumen ~factor(ClutchSize) + (1|NestID), data=shell3 %>% filter(ClutchSize<5))
plot(cmod_albumen3)
qqnorm(resid(cmod_albumen3))
qqline(resid(cmod_albumen3))
anova(cmod_albumen3)
summary(cmod_albumen3)
#No need to controll for clutch size


ggplot(shell3, aes(y=Mass_albumen, x=LayDOY ))+
  geom_point()+
  geom_smooth(method="lm")

cmod_albumen4 <- lmer(Mass_albumen ~LayDOY + (1|NestID), data=shell3)
plot(cmod_albumen4)
qqnorm(resid(cmod_albumen4))
qqline(resid(cmod_albumen4))
#Not a great fit at the low end, but OK at the high end.
anova(cmod_albumen4)
summary(cmod_albumen4)
#no need to control for lay date

ggplot(shell3, aes(y=Mass_albumen, x=factor(Year )))+
  geom_boxplot()
cmod_albumen5 <- lmer(Mass_albumen ~factor(Year) + (1|NestID), data=shell3 )
plot(cmod_albumen5)
qqnorm(resid(cmod_albumen5))
qqline(resid(cmod_albumen5))
anova(cmod_albumen5)
summary(cmod_albumen5)
#No need to controll for year


#Conclusion: Should controll for lay order and incubation stage in all models of Albumen mass. 


#Check what influences yolk mass. 
ggplot(shell3, aes(y=Mass_yolk, x=IncStage ))+
  geom_point()+
  geom_smooth(method="lm")

cmod_yolk1 <- lmer(Mass_yolk ~IncStage + (1|NestID), data=shell3)
plot(cmod_yolk1)
qqnorm(resid(cmod_yolk1))
qqline(resid(cmod_yolk1))
#pretty OK. 
anova(cmod_yolk1)
#suggests that inc stage should be controlled for (yolk mass increases with inc stage)


ggplot(shell3, aes(y=Mass_yolk, x=LayOrder_est ))+
  geom_point()+
  geom_smooth(method="lm")

cmod_yolk2 <- lmer(Mass_yolk ~LayOrder_est + (1|NestID), data=shell3)
cmod_yolk2 <- lmer(Mass_yolk ~LayOrder_est + (1|NestID), data=shell3 %>% filter(Mass_yolk<2))

plot(cmod_yolk2)
qqnorm(resid(cmod_yolk2))
qqline(resid(cmod_yolk2))
#Not a great fit at the high end-- looks to be some pretty strong outliers--
#checked, we get the same results, so should definitely be controlled
anova(cmod_yolk2)
summary(cmod_yolk2)
#suggests that Lay order should be controlled for (yolk mass decreases with lay order)


ggplot(shell3, aes(y=Mass_yolk, x=factor(ClutchSize) ))+
  geom_boxplot()
cmod_yolk3 <- lmer(Mass_yolk ~factor(ClutchSize) + (1|NestID), data=shell3 %>% filter(ClutchSize<5))
plot(cmod_yolk3)
qqnorm(resid(cmod_yolk3))
qqline(resid(cmod_yolk3))
anova(cmod_yolk3)
summary(cmod_yolk3)
#No need to controll for clutch size


ggplot(shell3, aes(y=Mass_yolk, x=LayDOY ))+
  geom_point()+
  geom_smooth(method="lm")

cmod_yolk4 <- lmer(Mass_yolk ~LayDOY + (1|NestID), data=shell3)
plot(cmod_yolk4)
qqnorm(resid(cmod_yolk4))
qqline(resid(cmod_yolk4))
#Not a great fit at the low end, but OK at the high end.
anova(cmod_yolk4)
summary(cmod_yolk4)
#no need to control for lay date

ggplot(shell3, aes(y=Mass_yolk, x=factor(Year )))+
  geom_boxplot()
cmod_yolk5 <- lmer(Mass_yolk ~factor(Year) + (1|NestID), data=shell3 )
cmod_yolk5 <- lmer(Mass_yolk ~factor(Year) + (1|NestID), data=shell3  %>% filter(Mass_yolk<2))

plot(cmod_yolk5)
qqnorm(resid(cmod_yolk5))
qqline(resid(cmod_yolk5))
anova(cmod_yolk5)
summary(cmod_yolk5)
#SHould control for year with yolk mess-- 2008 was higher. 


#What is the relationship between yolk and albumen mass controlling for whatever is necessary?
ggplot(shell3, aes(y=Mass_yolk, x=Mass_albumen ))+
  geom_point()

mod_ratio <- lmer(Mass_yolk ~ Mass_albumen + IncStage + factor(Year) + LayOrder_est + (1|NestID), data=shell3)
plot(mod_ratio)
qqnorm(resid(mod_ratio))
qqline(resid(mod_ratio))
anova(mod_ratio)
summary(mod_ratio)

#No relationship between albumen mass and yolk mass-- This suggests that you
#could have a larger egg because either the yolk or the albumen was larger. Of
#course, IncStage and Albumen mass are related so this isn't a prefect analysis.

#Is the degree of elongation is related to albumen mass?
ggplot(shell3, aes(y=Elongation, x=Mass_albumen ))+
  geom_point()

mod_elongation <- lmer(Elongation ~  Mass_albumen + (1|NestID), data=shell4)
plot(mod_elongation)
qqnorm(resid(mod_elongation))
qqline(resid(mod_elongation))
anova(mod_elongation)
summary(mod_elongation)
#No relationship between egg shape (elongation) and Albumen mass. 


#DO we have dry weights of yolk and albumen and if so analyze those too, and water content (wet-dry mass)? 
#Nope. Just one measurement of weight. Think this is dry but not sure. 
