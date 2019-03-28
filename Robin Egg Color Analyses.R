#Why do robin eggshells contain different levels of bilvardin/are different colors?


library(tidyverse)
library(lmerTest)
library(lme4)
library(MuMIn)


#Load data

fullegg <- read.csv("file:///C:/Users/11arc/OneDrive/Documents/Montogomerie Work/Robins/Philena's Robin Project/Egg_col_avguv_contents_fel_col2.csv", na.strings = "NA") 
#There are over 200 columns. THis is insane and needs to be cut back. 
names(fullegg)

egg <-  fullegg  %>% mutate(YEAR=ifelse(!is.na(YEAR.x), YEAR.x, YEAR.y))  %>% select(c(NestID, EggID, YEAR, TYPE, FEMID, MALEID, CLTCHSIZ, SAMDOY, 
                               INCSTAGE, LAYDOY, AGEATSAM, ESTORD, MASS, LENGTH, WIDTH, 
                               VOLUME, YOLKMAS, ALBMAS, SHELMAS, MEANSTR, KIT, NOTESLP, 
                               NOTESPE, Carot_conc_ugml, Carot_ug_yolk, exclude_carot, exclude_notes, T_concentration,  r.vec.egg, FDOY, AGE,
                               ECTO, BMASS, TARSUS, MEANYAS, r.vec.fem)) %>% 
  filter(!is.na(r.vec.egg)) %>%  #Remove all eggs where their color wasn't measured since this is a color analysis
  mutate(ELONGATION= LENGTH/WIDTH) %>%
  filter(ELONGATION>1.1 & VOLUME<12.5) #Remove oddly shaped PE24 eggs and all SA16 eggs which are oddly large. 
names(egg) #Much more manageable

#r.vec.fem is Philena's chosen measure of female bill color
#r.vec.egg seems to be Philena's measure of egg shell color, not totally sure on that though
#Unclear what measure Philena likes best for head color

#############################################################
#OK. Now we need to see if some basic things like year, lay date, clutch size,
#and egg age need to be controlled for in analyses of egg color.

#ALso will probably want to remove oddly shaped eggs. 

#Any difference in color by clutch size?
hist(egg$CLTCHSIZ)

#Remove all clutches that did not have 3-4 eggs. just too rare (removed 16 nests)
#egg %>% group_by (NestID)%>% filter(!(CLTCHSIZ %in% c(3, 4, NA))) %>% summarise(n())
egg2 <- egg %>% filter(CLTCHSIZ %in% c(3,4, NA) )


ggplot(egg, aes(x=factor(CLTCHSIZ), y=r.vec.egg))+
  geom_dotplot(binaxis = "y", stackdir = "center")

mod <- lmer(r.vec.egg ~ factor(CLTCHSIZ) + (1|NestID), data=egg2)
summary(mod) #No difference in color based on clutch size



#Any difference in color by year?
ggplot(egg2, aes(x=factor(YEAR), y=r.vec.egg))+
  geom_dotplot(binaxis = "y", stackdir = "center")

mod <- lmer(r.vec.egg ~ factor(YEAR) + (1|NestID), data=egg2)
summary(mod)
#Nothing going on

#Any difference in color by laydate?
ggplot(egg %>% filter(LAYDOY<165), aes(x=LAYDOY, y=r.vec.egg))+
  geom_point()+
  #geom_smooth()+
  geom_smooth(method="lm")

ggplot(egg, aes(x=LAYDOY, y=r.vec.egg))+
  geom_point()+
  geom_vline(xintercept = 165)

  
  
  #REmove all the late layed eggs-- second nests I would bet
  
firstnests <- egg2 %>% filter(LAYDOY<165)

mod <- lmer(r.vec.egg ~ LAYDOY + (1|NestID), data=firstnests)
summary(mod)
plot(mod)
#Clear differences based on LAYDOY-- need to include this parameter in all models. 

#ANy differences in color by laying order?

ggplot(egg, aes(x=INCSTAGE, y=r.vec.egg))+
  geom_point()+
  geom_smooth(method="lm")

mod <- lmer(r.vec.egg ~ INCSTAGE + (1|NestID), data=egg)
summary(mod)
plot(mod)
#Don't need to control for incubation sated

#Any difference in color by incubation stage (egg age)?

ggplot(egg, aes(x=INCSTAGE, y=r.vec.egg))+
  geom_point()+
  geom_smooth()
#OH there are some very old eggs included here. We will need to make sure to take this into account if we ever look at masses. 
#DOesn't look like there's anythign there though

mod <- lmer(r.vec.egg ~ INCSTAGE + (1|NestID), data=egg)
summary(mod)
plot(mod)
#Nope don't need to control for incubation stage. 



#What does the blue green color of the egg signal?
#Make a dataset of the fully reduced data (i.e clutch size of 3-4 eggs, and probably a first nest based on the date laid)
firstnests <- egg %>% 
  filter(CLTCHSIZ %in% c(3,4, NA) )%>% 
  filter(LAYDOY<165) 


#Is egg quality predictive of egg color?

#Carotenoids?
#Need to make sure you have a dataset with GOOD carotenoid data. A lot of it is super suspect. 
carot <- firstnests %>% filter(!is.na(Carot_ug_yolk))%>% filter(Carot_ug_yolk>50 & INCSTAGE<2)

ggplot(carot, aes(y=Carot_ug_yolk, x=INCSTAGE, color=exclude_carot))+
  geom_point()


ggplot(carot, aes(x=Carot_ug_yolk, y=r.vec.egg, color=exclude_carot))+
  geom_point()+
  geom_smooth(method="lm")

mod <- lmer(r.vec.egg ~Carot_ug_yolk + LAYDOY + (1|NestID), data=carot)
plot(mod)
summary(mod)
#no difference in color based on carotenoid content of egg yolk

####Egg size (volume will probably be a better measure than mass)
hist(firstnests$VOLUME)
hist(firstnests$LENGTH)
hist(firstnests$WIDTH)

ggplot(firstnests, aes(x=VOLUME, y=r.vec.egg))+
  geom_point()+
  geom_smooth(method="lm")


mod <- lmer(r.vec.egg ~VOLUME + LAYDOY + (1|NestID), data=firstnests)
plot(mod)
summary(mod)
#Larger eggs have higher R.vec.egg

#Eggshell color is correlated to volume (after controlling for laying date)



####Testosterone Concentration
ggplot(firstnests, aes(x=T_concentration, y=r.vec.egg))+
  geom_point()+
  geom_smooth(method="lm", formula=y~log(x))


mod <- lmer(r.vec.egg ~log(T_concentration) + LAYDOY + (1|NestID), data=firstnests)
plot(mod)
summary(mod)
#Testosterone concentration does not correlate with egg color


####Yolk Mass 

#Make a better dataset with only eggs that seperated properly for use with the yolk and albumen mass
sep <- firstnests %>% 
  filter(!grepl("too advanced", NOTESLP) & !is.na(YOLKMAS) & !grepl("mold", NOTESLP) & !grepl("crack", NOTESLP)) %>%
  mutate(CalcMassEgg = ALBMAS+ SHELMAS + YOLKMAS) %>%
  mutate(CalctoWeigh = CalcMassEgg/MASS, 
         YOlktoALB = YOLKMAS/ALBMAS) %>% 
  filter(CalctoWeigh>0.86)

hist(sep$YOLKMAS)
hist(sep$ALBMAS)

hist(sep$CalcMassEgg/sep$MASS)


ggplot(firstnests %>% filter(YOLKMAS<2), aes(x=YOLKMAS, y=r.vec.egg))+
  geom_point()+
  geom_smooth(method="lm")

ggplot(sep, aes(x=YOLKMAS, y=r.vec.egg))+
  geom_point()+
  geom_smooth(method="lm")


mod <- lmer(r.vec.egg ~YOLKMAS + LAYDOY + (1|NestID), data=sep)
plot(mod)
summary(mod)
#No relationship between egg color and yolk mass

###ALbumen Mass
ggplot(firstnests, aes(x=ALBMAS, y=r.vec.egg))+
  geom_point()+
  geom_smooth(method="lm")


ggplot(sep, aes(x=ALBMAS, y=r.vec.egg))+
  geom_point()+
  geom_smooth(method="lm")


mod <- lmer(r.vec.egg ~ALBMAS + LAYDOY + (1|NestID), data=sep)
plot(mod)
summary(mod)
#NO relationship with albumen mass



#####Eggshell strength/thickness

ggplot(firstnests, aes(x=MEANSTR, y=r.vec.egg))+
  geom_point()+
  geom_smooth(method="lm")


mod <- lmer(r.vec.egg ~MEANSTR + LAYDOY + (1|NestID), data=firstnests)
plot(mod)
summary(mod)
#strength of the egg is correlated to the eggshell color. 