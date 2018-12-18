
library(tidyverse)
library(corrplot)
library(summarytools)
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

#Indicators of higher quality eggs (presumably laid by higher quality females):
#Egg volume, strength, mass of yolk, albumen, carotenoid in yolk, Testosterone
#(?)

#Do eggs that need to hatch faster have thinner eggs?
#Lay order (presumed or otherwise), layDOY
