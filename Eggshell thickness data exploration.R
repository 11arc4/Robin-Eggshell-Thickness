
library(tidyverse)
library(corrplot)
library(summarytools)
library(lme4)

shell <- read.csv("file:///C:/Users/11arc/Documents/Montogomerie Work/Robins/GoodThickness.csv")
summarytools(shell)
dfsummary(shell2)

names(shell)

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
                      "Lum_egg_s", "HPhi_egg_s", "RVec_egg_s", "Thickness_egg"
                      )

shell3 <- shell2 %>%
  group_by(NestID) %>% 
  mutate(ClutchMass_shell=sum(Mass_shell), 
         Ectoparasites2 = ifelse(Ectoparasites>0, "YES", "NO")) 

###Calculate female body condition as residual body mass in Mass ~ Tarsus
Female <- shell3 %>% 
  group_by(FemaleID, FcaughtDOY, FemaleAge, Mass_female, Tarsus_female) %>%
  summarise(n=length(unique(Mass_female))) %>% select(-n) %>% filter(!is.na(Mass_female))
#no female was caught more than once

residmod <- lm(Mass_female ~ Tarsus_female , data=Female)
plot(residmod) #looks flawless
Female$ResidMass_female <- resid(residmod, "pearson")
shell2 <- merge(shell3, Female, all.x = T)

rm(residmod, Female)



m <- cor(shell2[,c(7:35, 37)], use="pairwise.complete.obs")
res1 <- cor.mtest(shell2[,7:37],  conf.level = .95)
corrplot(m, p.mat=res1$p, sig.level = 0.05, insig="blank")
 


####Are shells thinner when there are more eggs in the clutch? 
hist(shell2$ClutchSize, breaks=4) 
# we should consider dropping all clutches that don't have 3-4 eggs since there are so few. 
shell3 <- shell2 %>% group_by(NestID) %>% filter(ClutchSize %in% c(3,4)& n()>1) 





#########################################################
#Which variables should we control for?
shell3 <- shell2 %>% group_by(NestID) %>% filter(CLTCHSIZ %in% c(3,4)& n()>1) 

#Clutch size ()




#Female body conditio

