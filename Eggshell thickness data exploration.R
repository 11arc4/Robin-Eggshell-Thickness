
library(tidyverse)
library(corrplot)

Spec <- read.csv("file:///C:/Users/11arc/Dropbox/AmeliaBob/Robin/GoodSpec.csv")
shell <- read.csv("file:///C:/Users/11arc/Dropbox/AmeliaBob/Robin/GoodShell.csv")

summary(shell)

names(shell)


shell2 <- shell %>% filter(!is.na(THICKNESS)) %>%
  select(EggID, NestID, EggNum, FEMID, MALEID, YEAR.x, TYPE, 
         CLTCHSIZ, SAMDOY,LAYDOY, 
         MASS, LENGTH, WIDTH, VOLUME,YOLKMAS, ALBMAS, SHELMAS, MEANSTR, Carot_ug_mg,  THICKNESS,
         FDOY, AGE, BMASS, TARSUS, fem.scaled.mass, fem.resid.mass, r.achieved.fem,  ECTO) %>%
  mutate(DAYSAFTERLAY_EGGSAMPL =SAMDOY-LAYDOY,
         DAYSAFTERLAY_FCATCH=FDOY-LAYDOY,
         THICKNESS= ifelse(THICKNESS>0.13, NA,THICKNESS), 
         ECTO2 = ifelse(ECTO>0, "ECTOPARASITES", "NONE"))

shell3 <- shell2 %>% filter(!is.na(BMASS))

fmass_mod <- lm(BMASS~TARSUS+DAYSAFTERLAY_FCATCH, data=shell3)
plot(fmass_mod)
anova(fmass_mod)
summary(fmass_mod)

shell3$FRESIDMASS <- resid(fmass_mod)


m <- cor(shell2[,8:ncol(shell2)], use="pairwise.complete.obs")
res1 <- cor.mtest(shell2[,8:ncol(shell2)], conf.level = .95)
corrplot(m, p.mat=res1$p, sig.level = 0.05, insig="blank")
 



ggplot(shell2 , aes(x=THICKNESS, y=SHELMAS))+
  geom_point()+
  labs(x="Thickness (mm)", y="Shell mass (g?)")+
  geom_smooth(method='glm')


ggplot(shell2 , aes(x=THICKNESS, y=MEANSTR))+
  geom_point()+
  labs(x="Thickness (mm)", y="Strength")+
  geom_smooth(method='glm')

ggplot(shell2 , aes(y=THICKNESS*100, x=VOLUME))+
  geom_point()+
  labs(y="Thickness", x="Volume")+
  geom_smooth(method='glm', method.args= list(family="poisson"))

ggplot(shell2 , aes(y=THICKNESS, x=ECTO))+
  geom_jitter()+
  labs(y="Thickness", x="Ectoparasites")+
  geom_smooth(method='glm')
#Not that many points with Ectoparasites compared to without. Lets 

ggplot(shell2 %>% filter(!is.na(ECTO2)) , aes(y=THICKNESS, x=ECTO2, fill=ECTO2))+
  geom_boxplot(show.legend = F)+
  labs(y="Thickness", x="")



ggplot(shell3 , aes(y=THICKNESS, x=FRESIDMASS))+
  geom_point()+
  labs(y="Thickness (mm)", x="Resid. body mass")+
  geom_smooth(method='glm')
