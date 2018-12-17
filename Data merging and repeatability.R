

#Read in GoodSpec and Good Yolk data

library(tidyverse)


spec <- read.csv("file:///C:/Users/11arc/Dropbox/AmeliaBob/Robin/GoodSpec.csv", as.is=T)
yolk <- read.csv("file:///C:/Users/11arc/Dropbox/AmeliaBob/Robin/GoodYolk.csv", as.is=T)


#make sure that they have the same strength data

for (i in 1:nrow(yolk)){
  n <- which(spec$EggSpecLetter==yolk$EggSpecLetter[i])
  
  
  if(!anyNA(c(spec$MEANSTR[n],yolk$MEANSTR[i]))){
    if(spec$MEANSTR[n]!=yolk$MEANSTR[i]){
      message ( paste(spec$EggSpecLetter[n], "has two different hardnesses listed", sep=" "))
    }
  } else {
    if(!is.na(spec$MEANSTR[n]) | !is.na(yolk$MEANSTR[i])){
      if(which(is.na(c(spec$MEANSTR[n],yolk$MEANSTR[i])))==1){
        message("Spec file missing strength data for egg", spec$EggSpecLetter[n])
      } else {
        message("Yolk file missing strength data for egg", spec$EggSpecLetter[n])
      }
    }
  }
}


#We are all good. Both files have the same MEANSTR strength data. 




#Add the thickness data to the goodspec dataset (the more complete dataset)
shell <- read.csv("file:///C:/Users/11arc/Documents/Montogomerie Work/Robins/AMRO_Eggshell thickness.csv", as.is=T)


spec$THICKNESS <- NA


for (i in 1:nrow(spec)){
  spec$THICKNESS[i] <- shell$THICKNESS[spec$EggSpecLetter[i]==shell$EggSpecLetter]
}



spec2 <- spec %>% filter(!is.na(THICKNESS))
spec3 <- spec %>% filter(!is.na(THICKNESS) & !is.na(MEANSTR))

which(spec3$THICKNESS>0.14)
spec3$EggID[48] #Remove this point--- there is note that this egg was infected by a fungus so it makes a lot of sense that it was wonky!!

spec4 <- spec3 %>% filter(THICKNESS<0.14)


ggplot(spec4, aes(y=MEANSTR, x=THICKNESS))+
  geom_smooth(method="lm")+
  geom_point(shape=1)+
    labs(x="Shell thickness (mm)", y="Mean shell strenth")+
  theme_classic(base_size = 16, base_family = "serif")

ggsave("~/Montogomerie Work/Robins/Shell thickness to strength.jpeg", width=5, height=4 )




mod <- lm(MEANSTR ~THICKNESS, data=spec4 )
plot(mod)
hist(resid(mod))
plot(resid(mod)~spec4$THICKNESS)

summary(mod)
#R^2=0.15 so they're correlated but not super strongly. The mean strength might
#actually be the better measure (it seems to be more variable)



write.csv(spec, "file:///C:/Users/11arc/Dropbox/AmeliaBob/GoodShell.csv", row.names=F, na="")




#Are we missing any thicknesses for shells where we know mean strength?

missing <- spec %>% filter(!is.na(MEANSTR) & is.na(THICKNESS))
missing$EggID





########### Determine the measurement error/ variablity based on shell piece (ie
#do we need to take more than one measurement of each shell?)


val <- read.csv("file:///C:/Users/11arc/Dropbox/AmeliaBob/Shell thickness validation.csv") %>% arrange(Egg)
ggplot(val, aes(x=Egg, y=Thickness))+
  geom_boxplot(aes(fill=Egg))+
  geom_count()

val$MovingAverage <- NA
for (egg in unique(val$Egg)){
  val$MovingAverage[val$Egg==egg & val$Measurement==1] <- val$Thickness[val$Egg==egg & val$Measurement==1]
  val$MovingAverage[val$Egg==egg & val$Measurement==2] <- mean(val$Thickness[val$Egg==egg & val$Measurement<=2])
  val$MovingAverage[val$Egg==egg & val$Measurement==3] <- mean(val$Thickness[val$Egg==egg & val$Measurement<=3])
  val$MovingAverage[val$Egg==egg & val$Measurement==4] <- mean(val$Thickness[val$Egg==egg & val$Measurement<=4])
  val$MovingAverage[val$Egg==egg & val$Measurement==5] <- mean(val$Thickness[val$Egg==egg & val$Measurement<=5])
  
  
  
}


ggplot(val, aes(x=Measurement, y=MovingAverage, color=Egg))+
  geom_point()+
  geom_line()+
  labs(x="Measurements taken", y="Average thickness (mm)")+
  theme_classic(base_size = 16, base_family = "serif")




#Calculate repeatability
#https://cran.r-project.org/web/packages/rptR/vignettes/rptR.html
library(rptR)

rpt1 <- rpt(Thickness ~ (1 | Egg), grname = "Egg", data = val, datatype="Gaussian",
    nboot = 1000, npermut = 0)
