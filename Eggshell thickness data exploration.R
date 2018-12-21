
library(tidyverse)
library(corrplot)
library(lme4)
library(lmerTest)
library(stats)


shell <- read.csv("file:///C:/Users/11arc/Documents/Montogomerie Work/Robins/Thickness Dataset (reduced).csv")

shell2 <- shell %>% 
  select(-"NOTESLP", -"NOTESPE", -"KIT", -"SampleID", -"Yolk_sample_mass", -Tng_yolkg,-Tng_yolkmg, -YolkTug, -CONDIT) 


colnames(shell2) <- c("NestID", "EggLetter", "Year", "Treatment", 
                      "FemaleID", "MaleID", "ClutchSize", "SampleDOY", 
                      "IncStage", "LayDOY", "EggAge", "LayOrder_est", "LayOrder_known",
                      "Mass_egg", "Length_egg", "Width_egg", "Volume_egg",
                      "Mass_yolk", "Mass_albumen", "Mass_shell", "MeanStrength", 
                      "Carotenoid_ugml", "Carotenoid_ugg", "Carot_ugyolk", "T_concentration", 
                      "FcaughtDOY", "FemaleAge", "Ectoparasites", "Mass_female", 
                      "Tarsus_female", "YellowAreaScore_female", "HTheta_female", "HPhi_female",
                      "RVec_female", "Lum_female", "ScaledBodyMass_female", "HTheta_egg", 
                      "HPhi_egg", "RVec_egg", "RMax_egg", "RAchieved_egg", 
                      "Lum_egg", "Thickness_shell"
)

shell2 <- shell2 %>%
  group_by(NestID) %>% 
  mutate(ClutchMass_shell=sum(Mass_shell), 
         Ectoparasites2 = ifelse(Ectoparasites>0, "YES", "NO"), 
         EggAge = SampleDOY-LayDOY, 
         Mass_yolk= ifelse(Mass_yolk>3, NA, Mass_yolk), 
         ClutchInitiationDate=floor(mean(LayDOY[LayOrder_est<2]))) 

###Calculate female body condition as residual body mass in Mass ~ Tarsus
Female <- shell2 %>% 
  group_by(FemaleID, FcaughtDOY, FemaleAge, Mass_female, Tarsus_female) %>%
  summarise(n=length(unique(Mass_female))) %>% select(-n) %>% filter(!is.na(Mass_female))
#no female was caught more than once

residmod <- lm(Mass_female ~ Tarsus_female , data=Female)
plot(residmod) #looks flawless
Female$ResidMass_female <- resid(residmod, "pearson")
shell2 <- merge(shell2, Female, all.x = T, all.y=T)

rm(residmod, Female)


####Are shells thinner when there are more eggs in the clutch? 
hist(shell2$ClutchSize) 
# we should consider dropping all clutches that don't have 3-4 eggs since there are so few. 
shell3 <- shell2 %>% group_by(NestID) %>% filter(ClutchSize %in% c(3,4) )





#########################################################
#Which variables should we see if we need to control for them? E.g., things that we hope don't matter. 

#Year, Treatment, IncStage (SampleDOY-LayDOY) 
mod_year <- lmer(Thickness_shell~factor(Year)+ (1|NestID), data=shell2)
plot(mod_year)
summary(mod_year)
anova(mod_year)
#No year differences

mod_treatment <- lm(Thickness_shell~Treatment, data=shell2)
plot(mod_treatment)
summary(mod_treatment)
anova(mod_treatment)
#No treatment differences in shell thickness

mod_incstage <- lm(Thickness_shell~EggAge, data=shell2)
plot(mod_incstage)
summary(mod_incstage)
anova(mod_incstage)
#No age of egg at sampling differences, somewhat surprised because there is a
#long tail of egg ages. 
hist(shell2$EggAge)


#Should still check to see if Inc Stage influenced  egg qualities (mass, carotenoids, testosterone)
cmod_yolk <- lm(Mass_yolk~EggAge, data=shell2 %>% filter(Mass_yolk<2.2 & EggAge<6))
plot(cmod_yolk) #saw some outliers on the histogram, now removed those above and it's much better
summary(cmod_yolk)
anova(cmod_yolk)
#Yes it really does. We will need to control for this....Do need to remove the
#outliers from this analysis as well probably.
ggplot(shell2, aes(x=EggAge, y=Mass_yolk))+
  geom_point(color=ifelse(shell2$Mass_yolk < 2.2, "black", "red"))+
  geom_smooth(method="lm", data=shell2 %>% filter(Mass_yolk<2.2 & EggAge<6), color="black")+
  geom_smooth(method="lm", color="red")+
  theme_classic()
ggplot(shell2, aes(x=IncStage, y=Mass_yolk))+
  geom_point(color=ifelse(shell2$Mass_yolk < 2.2, "black", "red"), shape=1)+
  geom_smooth(method="lm", data=shell2 %>% filter(Mass_yolk<2.2), color="black")+
  geom_smooth(method="lm", color="red")+
  geom_smooth(method="lm", data=)
  theme_classic()



cmod_eggmass <- lm(Mass_egg~EggAge, data=shell2)
plot(cmod_eggmass) #fits perfectly
summary(cmod_eggmass)
anova(cmod_eggmass)
#Nope no need to control for how old the eggs where when looking at egg mass



cmod_alb <- lm(Mass_albumen~EggAge, data=shell2)
plot(cmod_alb) #not really normal even a little bit
summary(cmod_eggmass)
anova(cmod_eggmass)
#Don't need to control for age of eggs in albumen mass (ignoring normality issues)
ggplot(shell2, aes(x=EggAge, y=Mass_albumen))+
  geom_point()+
  geom_smooth(method="lm", color="red")+
  theme_classic()

cmod_carot <- lm(Carot_ugyolk~EggAge, data=shell2[-c(6,80,114),]) #removed leverage points
plot(cmod_carot) #Pretty good fit
summary(cmod_carot)
anova(cmod_carot)
#Must control for age of egg when analysing carotenoids in yolk (even after removing leverage points)

ggplot(shell2, aes(x=EggAge, y=Carot_ugyolk))+
  geom_point( shape=1)+
  geom_smooth(method="lm")+
  geom_smooth(method="lm", data=shell2[-c(6,80,114),])+
  theme_classic()

cmod_T <- lm(T_concentration~EggAge, data=shell2)
plot(cmod_T) #Pretty good fit
summary(cmod_T)
anova(cmod_T)
#Don't need to control for eggAge in testosterone concentration

ggplot(shell2, aes(x=EggAge, y=T_concentration))+
  geom_point( shape=1)+
  geom_smooth(method="lm")+
  theme_classic()

ggplot(shell2%>% filter(T_concentration<20), aes(x=IncStage, y=T_concentration))+
  geom_point( shape=1)+
  geom_smooth(method="lm")+
  xlim(c(0,2))+
  theme_classic()


###########################################################################################
#Do clutch level traits affect eggshell thickness?
mod_clutch <- lmer(Thickness_shell ~ ClutchSize + (1|NestID), data=shell2)
plot(mod_clutch)
summary(mod_clutch)
anova(mod_clutch)

mod_date <- lmer(Thickness_shell ~ ClutchInitiationDate + (1|NestID), data=shell2) #Don't need random effect
#mod_date <- lm(Thickness_shell ~ ClutchInitiationDate, data=shell2)
plot(mod_date)
summary(mod_date)
anova(mod_date)
ggplot(shell2, aes(y=Thickness_shell, x=ClutchInitiationDate))+
  geom_count()+
  geom_smooth(method="lm")
#Possible very slight indication that eggs are thinner when laid later, but it's
#not significant.

mod_order <- lmer(Thickness_shell ~ LayOrder_est + (1|NestID), data=shell2) #Don't need random effect
mod_order <- lm(Thickness_shell ~ LayOrder_est , data=shell2)
plot(mod_order)
summary(mod_order)
anova(mod_order)
#Laying order does not correlate with thickness

###########################################################################################
#####Do higher quality females lay eggs with thicker eggshells?

mod_femage <- lmer(Thickness_shell ~ factor(FemaleAge) + (1|NestID), data=shell2) #No need for random
#mod_femage <- lm(Thickness_shell ~ FemaleAge, data=shell2)
plot(mod_femage)
summary(mod_femage)
anova(mod_femage)
#Female age doesn't correlate with shell thickness. 

mod_femcondition <- lmer(Thickness_shell ~ ScaledBodyMass_female+ (1|NestID), data=shell2) #No random needed
#mod_femcondition <- lm(Thickness_shell ~ ScaledBodyMass_female, data=shell2)
plot(mod_femcondition)
summary(mod_femcondition)
anova(mod_femcondition)
#Female residual body mass doesn't correlate with shell thickness

#mod_ecto <- lmer(Thickness_shell ~ log(Ectoparasites+1)+(1|NestID), data=shell2) #No need for random effect
#Based on AICc log is the better model but it's not as good of a visual so I
#don't really trust it very well.
mod_ecto <- lmer(Thickness_shell ~ Ectoparasites2+(1|NestID), data=shell2) #No need for random effect
#mod_ecto <- lm(Thickness_shell ~ Ectoparasites2, data=shell2)
plot(mod_ecto)
summary(mod_ecto)
anova(mod_ecto)
#When there are more ectoparasites, shells are thinner!
ggplot(shell2 %>% filter(!is.na(Ectoparasites2)), aes(x=(Ectoparasites), y=Thickness_shell))+
  geom_count()+
  geom_smooth(method="lm", formula=y~log(x+1))+
  labs(x="Ectoparasites on crown", y="Eggshell thickness (mm)" )+
  theme_classic()+
  scale_x_log10()

ggplot(shell2 %>% filter(!is.na(Ectoparasites2)), aes(x=Ectoparasites2, y=Thickness_shell))+
  geom_boxplot()+
  geom_count()+
  labs(x="Ectoparasites on crown", y="Eggshell thickness (mm)" )+
  theme_classic()

mod_bill <- lmer(Thickness_shell ~ YellowAreaScore_female + (1|NestID) , data=shell2) #No need to include nestID
#mod_bill <- lmer(Thickness_shell ~ YellowAreaScore_female + (1|NestID) , data=shell2 %>% filter(YellowAreaScore_female>1 & Thickness_shell<0.14 & Thickness_shell>0.08)) #No need to include nestID
#Those couple of points do drive the slight trend. We won't make too much of it then. 

#mod_bill <- lm(Thickness_shell ~ YellowAreaScore_female , data=shell2)
plot(mod_bill)
summary(mod_bill)
anova(mod_bill)

#There is a non-significant correlation between Bill color and shell thickness
#(more yellow= slightly thinner)
ggplot(shell2, aes(x=YellowAreaScore_female, y=Thickness_shell))+
  geom_count()+
  geom_smooth(method="lm")+
  labs(x="Proportion of yellow", y="Eggshell thickness (mm)" )+
  theme_classic()



#########################################################################################################  
#Do higher quality eggs have thicker shells?
#Egg volume, strength, mass of yolk, albumen, carotenoid in yolk, Testosterone

mod_volume <- lmer(Thickness_shell~Volume_egg + (1|NestID), data=shell2) #No random effect necessary
#mod_volume <- lm(Thickness_shell~Volume_egg, data=shell2)
plot(mod_volume)
summary(mod_volume)
anova(mod_volume)
# larger eggs (by volume) have thicker shells. 
ggplot(shell2, aes(x=Thickness_shell, y=Volume_egg))+
  geom_point(shape=1)+
  geom_smooth(method="lm")+
  labs(x="Volume (ml?)", y="Eggshell thickness (mm)" )+
  theme_classic()


#Yolk mass needs to have inc day controlled. 
shell_yolk <- shell2 %>% filter(!is.na(Mass_yolk) & !is.na(IncStage))
cmod_yolk <- lm(Mass_yolk~IncStage, data=shell_yolk)
shell_yolk$ResidMass_yolk <- resid(cmod_yolk, "pearson")

mod_yolk <- lmer(Thickness_shell~ ResidMass_yolk + (1|NestID), data=shell_yolk) #Don't need random effect
#mod_yolk <- lm(Thickness_shell~ ResidMass_yolk, data=shell_yolk)
plot(mod_yolk)
summary(mod_yolk)
anova(mod_yolk)
#Yolk mass doesn't correlate with thickness.

mod_alb <- lmer(Thickness_shell~Mass_albumen + (1|NestID), data=shell2) #Don't need random effect
#mod_alb <- lm(Thickness_shell~Mass_albumen, data=shell2)
plot(mod_alb)
summary(mod_alb)
anova(mod_alb)
#No real indication thickness correlates with albumen mass. 



#Testosterone requires corrections for incubation stage. 
shell_T <- shell2 %>% filter(!is.na(T_concentration) & !is.na(IncStage))
cmod_T <- lm(log(T_concentration)~IncStage, data=shell_T)
shell_T$ResidT <- resid(cmod_T, "pearson")

mod_T <- lmer(Thickness_shell ~ ResidT+(1|NestID), data=shell_T)#Don't need random effect
#mod_T <- lm(Thickness_shell ~ ResidT, data=shell_T)
plot(mod_T)
summary(mod_T)
anova(mod_T)
#Shells are much thicker when the yolk has high testosterone for the age of the egg. 

ggplot(shell_T, aes(x=ResidT, y=Thickness_shell))+
  geom_point(shape=1)+
  geom_smooth(method="lm")+
  theme_classic()+
  labs(x="Residual log(yolk testosterone)", y="Eggshell thickness (mm)")

#Carotenoids require correcting for incubation stage
shell_carot <- shell2 %>% filter(!is.na(Carot_ugyolk) & !is.na(IncStage))
cmod_carot <- lm(Carot_ugyolk~IncStage, data=shell_carot)
shell_carot$ResidCarot_ugyolk <- resid(cmod_carot, "pearson")

mod_carot <- lmer(Thickness_shell ~ ResidCarot_ugyolk + (1|NestID), data=shell_carot) #Don't need random effect
#mod_carot <- lm(Thickness_shell ~ ResidCarot_ugyolk, data=shell_carot)
plot(mod_carot)
summary(mod_carot)
anova(mod_carot)
#Once you control for how old the egg is, carotenoids don't predict shell thickness. 


#PCA for egg color. 
#https://stats.stackexchange.com/questions/59213/how-to-compute-varimax-rotated-principal-components-in-r

PCA_color <- prcomp( shell2[,c(37:39, 41:42)], 
               center=T, 
               scale=T, 
               retx=T)

plot(PCA_color, type="lines")
ncomp<-2
rawLoadings     <- PCA_color$rotation[,1:ncomp] %*% diag(PCA_color$sdev, ncomp, ncomp)
rotatedLoadings <- varimax(rawLoadings)$loadings
invLoadings     <- t(pracma::pinv(rotatedLoadings))
rownames(invLoadings) <- colnames(shell2[,c(37:39, 41:42)])

scores <- scale(shell2[,c(37:39, 41:42)]) %*% invLoadings

####Calculate PCs for Top of Egg
shell2$PC1 <-  scores[,1] #50% of variance
shell2$PC2 <-  scores[,2] #ANother 47% variance


mod_colorpc <- lmer(Thickness_shell ~ PC1 + (1|NestID), data=shell2) #Don't need random effect
plot(mod_colorpc)#Doesn't fit well
hist(resid(mod_colorpc))
anova(mod_colorpc)
summary(mod_colorpc)


mod_pc <- lm(PC1 ~ EggAge, data=shell2) 
plot(mod_pc)#Doesn't fit well
anova(mod_pc)
ggplot(shell2, aes(x=EggAge, y=PC1))+
  geom_point()
  geom_smooth(method="lm")
#PC1 should be controlled for egg age

mod_pc <- lm(PC2 ~ EggAge, data=shell2[-c(142,114),])
plot(mod_pc)#Doesn't fit well
anova(mod_pc)
ggplot(shell2[-c(142,114),], aes(x=EggAge, y=PC2))+
  geom_point()+
  geom_smooth(method="lm")

ggplot(shell2, aes(x=PC1, y=Thickness_shell))+
  geom_point(shape=1)+
  geom_smooth(method="lm")


mod_colorpc2 <- lmer(Thickness_shell ~ PC2 + (1|NestID), data=shell2) #Don't need random effect
plot(mod_colorpc2)#Doesn't fit well
hist(resid(mod_colorpc2))
anova(mod_colorpc2)
summary(mod_colorpc2)

ggplot(shell2, aes(x=PC2, y=Thickness_shell))+
  geom_point(shape=1)+
  geom_smooth(method="lm")
