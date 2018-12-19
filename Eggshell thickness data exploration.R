
library(tidyverse)
library(corrplot)
library(lme4)

shell <- read.csv("file:///C:/Users/11arc/Documents/Montogomerie Work/Robins/GoodThickness.csv")

shell2 <- shell %>% 
  select(-"NOTESLP", -"NOTESPE", -"KIT", -"SampleID",-AGEATSAM, -"yolkmass", -"Yolk_sample_mass", -Tng_yolkg, -Yolk_mass, -YolkTug, -fem.scaled.mass, -ECTO.s, -CONDIT) 


colnames(shell2) <- c("NestID", "EggLetter", "Year", "Treatment", 
                      "FemaleID", "MaleID", "ClutchSize", "SampleDOY", 
                      "IncStage", "LayDOY", "LayOrder_est", "LayOrder_known",
                      "Mass_egg", "Length_egg", "Width_egg", "Volume_egg",
                      "Mass_yolk", "Mass_albumin", "Mass_shell", "MeanStrength", 
                      "Carotenoid_ugml", "Carotenoid_ugg", "Carot_ugyolk", "T_concentration", 
                      "FcaughtDOY", "FemaleAge", "Ectoparasites", "Mass_female", 
                      "Tarsus_female", "YellowAreaScore_female", "ColorRAchieved_female", 
                      "Lum_egg_s", "HPhi_egg_s", "RVec_egg_s", "Thickness_shell"
                      )

shell3 <- shell2 %>%
  group_by(NestID) %>% 
  mutate(ClutchMass_shell=sum(Mass_shell), 
         Ectoparasites2 = ifelse(Ectoparasites>0, "YES", "NO"), 
         EggAge = SampleDOY-LayDOY, 
         Mass_yolk= ifelse(Mass_yolk>3, NA, Mass_yolk)) 

###Calculate female body condition as residual body mass in Mass ~ Tarsus
Female <- shell3 %>% 
  group_by(FemaleID, FcaughtDOY, FemaleAge, Mass_female, Tarsus_female) %>%
  summarise(n=length(unique(Mass_female))) %>% select(-n) %>% filter(!is.na(Mass_female))
#no female was caught more than once

residmod <- lm(Mass_female ~ Tarsus_female , data=Female)
plot(residmod) #looks flawless
Female$ResidMass_female <- resid(residmod, "pearson")
shell3 <- merge(shell3, Female, all.x = T)

rm(residmod, Female)



m <- cor(shell2[,c(7:35, 37)], use="pairwise.complete.obs")
res1 <- cor.mtest(shell2[,7:37],  conf.level = .95)
corrplot(m, p.mat=res1$p, sig.level = 0.05, insig="blank")
 


####Are shells thinner when there are more eggs in the clutch? 
hist(shell2$ClutchSize, breaks=4) 
# we should consider dropping all clutches that don't have 3-4 eggs since there are so few. 
shell4 <- shell3 %>% group_by(NestID) %>% filter(ClutchSize %in% c(3,4) )





#########################################################
#Which variables should we see if we need to control for them? E.g., things that we hope don't matter. 

#Year, Treatment, IncStage (SampleDOY-LayDOY) 
mod_year <- lm(Thickness_shell~factor(Year), data=shell4)
plot(mod_year)
summary(mod_year)
anova(mod_year)
#No year differences

mod_treatment <- lm(Thickness_shell~Treatment, data=shell4)
plot(mod_treatment)
summary(mod_treatment)
anova(mod_treatment)
#No treatment differences in shell thickness

mod_incstage <- lm(Thickness_shell~IncStage, data=shell4)
plot(mod_incstage)
summary(mod_incstage)
anova(mod_incstage)
#No age of egg at sampling differences, somewhat surprised because there is a
#long tail of egg ages. 
hist(shell4$IncStage)


#Should still check to see if Inc Stage influenced  egg qualities (mass, carotenoids, testosterone)
cmod_yolk <- lm(Mass_yolk~IncStage, data=shell4)
plot(cmod_yolk) #saw some outliers on the histogram, now removed those above and it's much better
summary(cmod_yolk)
anova(cmod_yolk)
#Yes it really does. We will need to control for this....


cmod_eggmass <- lm(Mass_egg~IncStage, data=shell4)
plot(cmod_eggmass) #fits perfectly
summary(cmod_eggmass)
anova(cmod_eggmass)
#Nope no need to control for how old the eggs where when looking at egg mass



cmod_alb <- lm(Mass_albumin~IncStage, data=shell4)
plot(cmod_alb) #not really normal even a little bit
summary(cmod_eggmass)
anova(cmod_eggmass)
#Don't need to control for age of eggs in albumin mass (ignoring normality issues)


cmod_carot <- lm(Carot_ugyolk~IncStage, data=shell4)
plot(cmod_carot) #Pretty good fit
summary(cmod_carot)
anova(cmod_carot)
#Must control for age of egg when analysing carotenoids in yolk

cmod_T <- lm(log(T_concentration)~IncStage, data=shell4)
plot(cmod_T) #Pretty good fit
summary(cmod_T)
anova(cmod_T)
#Should control for age of egg when analysing Testosterone concentration in yolk



#####Do higher quality females have higher quality eggs?
#Indicators of Female quality: female body mass, yellowAreaScore,
#colorRAchieved, FAge, Ectoparasites (Y/N)
      ##Maybe test against the total shell mass as well
mod_femage <- lmer(Thickness_shell ~ FemaleAge + (1|NestID), data=shell4) #No need for random
mod_femage <- lm(Thickness_shell ~ FemaleAge, data=shell4)
plot(mod_femage)
summary(mod_femage)
#Female age doesn't correlate with shell thickness. 

mod_femcolor <- lmer(Thickness_shell ~ ColorRAchieved_female + (1|NestID), data=shell4) #No random needed
mod_femcolor <- lm(Thickness_shell ~ ColorRAchieved_female, data=shell4)
plot(mod_femcolor)
summary(mod_femcolor)
#Female color doesn't correlate with shell thickness

mod_femmass <- lmer(Thickness_shell ~ ResidMass_female+ (1|NestID), data=shell4) #No random needed
mod_femmass <- lm(Thickness_shell ~ ResidMass_female, data=shell4)
plot(mod_femmass)
summary(mod_femmass)
#Female residual body mass doesn't correlate with shell thickness

mod_bill <- lmer(Thickness_shell ~ YellowAreaScore_female + (1|NestID) , data=shell4) #No need to include nestID
mod_bill <- lm(Thickness_shell ~ YellowAreaScore_female , data=shell4)
plot(mod_bill)
summary(mod_bill)
#There is a non-significant correlation between Bill color and shell thickness
#(more yellow= slightly thinner)
ggplot(shell4, aes(x=YellowAreaScore_female, y=Thickness_shell))+
  geom_count()+
  geom_smooth(method="lm")

mod_ecto <- lmer(Thickness_shell ~ Ectoparasites2+(1|NestID), data=shell4) #No need for random effect
mod_ecto <- lm(Thickness_shell ~ Ectoparasites2, data=shell4)
plot(mod_ecto)
summary(mod_ecto)
#When there are more ectoparasites, shells are thinner!
ggplot(shell4 %>% filter(!is.na(Ectoparasites2)), aes(x=Ectoparasites2, y=Thickness_shell, fill=Ectoparasites2))+
  geom_boxplot()+
  geom_count()
  
#Indicators of higher quality eggs (presumably laid by higher quality females):
#Egg volume, strength, mass of yolk, albumen, carotenoid in yolk, Testosterone
#(?)
mod_volume <- lmer(Thickness_shell~Volume_egg + (1|NestID), data=shell4) #No random effect necessary
mod_volume <- lm(Thickness_shell~Volume_egg, data=shell4)
plot(mod_volume)
summary(mod_volume)
#Just barely nonsignificant, but larger eggs (by volume) have thicker shells. 
ggplot(shell4, aes(y=Thickness_shell, x=Volume_egg))+
  geom_point(shape=1)+
  geom_smooth(method="lm")

mod_strength <- lmer(Thickness_shell ~MeanStrength + (1|NestID), data=shell4 ) #Random effect doesn't add anything
mod_strength <- lm(Thickness_shell ~MeanStrength, data=shell4 )
plot(mod_strength)
summary(mod_strength)
#Thicker shells are stronger
ggplot(shell4, aes(x=Thickness_shell, y=MeanStrength))+
  geom_point(shape=1)+
  geom_smooth(method="lm")


#Yolk mass needs to have inc day controlled. 
shell_yolk <- shell4 %>% filter(!is.na(Mass_yolk) & !is.na(IncStage))
cmod_yolk <- lm(Mass_yolk~IncStage, data=shell_yolk)

shell_yolk$ResidMass_yolk <- resid(cmod_yolk, "pearson")

mod_yolk <- lmer(Thickness_shell~ ResidMass_yolk + (1|NestID), data=shell_yolk) #Don't need random effect
mod_yolk <- lm(Thickness_shell~ ResidMass_yolk, data=shell_yolk)
plot(mod_yolk)
summary(mod_yolk)
#Yolk mass doesn't correlate with thickness.

mod_alb <- lmer(Thickness_shell~Mass_albumin + (1|NestID), data=shell4) #Don't need random effect
mod_alb <- lm(Thickness_shell~Mass_albumin, data=shell4)
plot(mod_alb)
summary(mod_alb)
#No real indication thickness correlates with albumin mass. 

#Carotenoids require correcting for incubation stage
shell_carot <- shell4 %>% filter(!is.na(Carot_ugyolk) & !is.na(IncStage))
cmod_carot <- lm(Carot_ugyolk~IncStage, data=shell_carot)
shell_carot$ResidCarot_ugyolk <- resid(cmod_carot, "pearson")

mod_carot <- lmer(Thickness_shell ~ ResidCarot_ugyolk + (1|NestID), data=shell_carot) #Don't need random effect
mod_carot <- lm(Thickness_shell ~ ResidCarot_ugyolk, data=shell_carot)
plot(mod_carot)
summary(mod_carot)
#Once you control for how old the egg is, carotenoids don't predict shell thickness. 


#Testosterone requires corrections for incubation stage. 
shell_T <- shell4 %>% filter(!is.na(T_concentration) & !is.na(IncStage))
cmod_T <- lm(log(T_concentration)~IncStage, data=shell_T)
shell_T$ResidT <- resid(cmod_T, "pearson")

mod_T <- lmer(Thickness_shell ~ ResidT+(1|NestID), data=shell_T)#Don't need random effect
mod_T <- lm(Thickness_shell ~ ResidT, data=shell_T)
plot(mod_T)
summary(mod_T)
#Shells are much thicker when the yolk has high testosterone for the age of the egg. 

ggplot(shell_T, aes(x=ResidT, y=Thickness_shell))+
  geom_point(shape=1)+
  geom_smooth(method="lm")


#Do eggs that need to hatch faster have thinner eggs?
#Lay order (presumed or otherwise), layDOY

mod_date <- lmer(Thickness_shell ~ LayDOY + (1|NestID), data=shell4) #Don't need random effect
mod_date <- lm(Thickness_shell ~ LayDOY, data=shell4)

plot(mod_date)
summary(mod_date)
ggplot(shell4, aes(y=Thickness_shell, x=LayDOY))+
  geom_point()+
  geom_smooth(method="lm")
#Possible very slight indication that eggs are thinner when laid later, but it's
#not significant.

mod_order <- lmer(Thickness_shell ~ LayOrder_est + (1|NestID), data=shell4) #Don't need random effect
mod_order <- lm(Thickness_shell ~ LayOrder_est , data=shell4)
plot(mod_order)
summary(mod_order)
#Laying order does not correlate with thickness




#Egg color 
mod_eggcolor <- lmer(Thickness_shell ~ Lum_egg_s + HPhi_egg_s + RVec_egg_s + (1|NestID), data=shell4)
summary(mod_eggcolor)
anova(mod_eggcolor)
plot(mod_eggcolor)#Doesn't fit great but not so bad
#RVec might correlate weakly with thickness but that's about it. 

ggplot(shell4, aes(x=Lum_egg_s, y=Thickness_shell))+
  geom_point()+
  geom_smooth(method="lm")

ggplot(shell4, aes(x=HPhi_egg_s, y=Thickness_shell))+
  geom_point()+
  geom_smooth(method="lm")


ggplot(shell4, aes(x=RVec_egg_s, y=Thickness_shell))+
  geom_point()+
  geom_smooth(method="lm")


