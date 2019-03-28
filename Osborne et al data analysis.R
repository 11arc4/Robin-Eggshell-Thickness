#Comparisong across MANY species using the Osborne et al. 1968 dataset. 

library(tidyverse)

#Should be noted that eggshell thicknesses are unusually thin in this dataset. Could be that these eggs are a museum collection


osborne <- read.csv("file:///C:/Users/11arc/OneDrive/Documents/Montogomerie Work/Robins/Eggshell Thickness Table from Osborne et al. 1968.csv")


#Their dataset is grouped as either OH or other. I will recalculate the numbers
#(weighting averaged by the number of eggs) to not distinguish between locations

osborne2 <- osborne %>% 
  group_by(Species, Latin.Name) %>% 
  mutate(Eggs=sum(c(n.eggs,  neggs_OH), na.rm=T), 
         Clutches=sum(c(n.clutches,  nclutches_OH), na.rm=T)) %>%
  mutate(Length=round(sum(n.eggs*L..mm., neggs_OH*L..mm._OH, na.rm=T)/Eggs, 1), #1 decimal place
         Width=round(sum(n.eggs*B..mm., neggs_OH*B..mm._OH, na.rm=T)/Eggs, 1), 
         Mass=round(ifelse(is.na(Wt..g.) & is.na(Wt..g._OH), NA, sum(n.eggs*Wt..g., neggs_OH*Wt..g._OH, na.rm=T)/Eggs), 1), 
         Thickness=round(ifelse(is.na(Thickness) & is.na(Thickness_OH), NA , sum(n.eggs*Thickness, neggs_OH*Thickness_OH, na.rm=T)/Eggs), 2) ) %>%
  select(Species, Latin.Name, Eggs, Clutches, Length, Width, Mass, Thickness) %>%
  mutate(Elongation=Length/Width, 
         )
  
                                                                    
 ggplot(osborne2, aes(x=Length, y=Width))+
   geom_point()
 
 
 ggplot(osborne2, aes(x=Elongation, y=Thickness))+
   geom_point()+
   ylim(0, 3.5)
 
 
 
 
 hist(osborne2$Thickness)
 
 hist(osborne2$Elongation)
 
 ggplot(osborne2, aes(x=Length, y=Thickness))+
   geom_point()
 
 #Who is particularly high?
 osborne2 %>% filter(Length<50 & Thickness>2.5)
 #Gray Partidge is that weird high one. Let's just quickly check that. Yup-- that's really in the table. Very unusual
 
 
 #Who is unusually low?
 osborne2 %>% filter(Length>62 & Thickness<1.5)
#None of those are wrong based on the table. 
 
 
 ggplot(osborne2, aes(x=Width, y=Thickness))+
   geom_point()
 
 write.csv(osborne2, "file:///C:/Users/11arc/Dropbox/AmeliaBob/Robin/Osborne et al 1968 Table 1.csv", na="", row.names = F)
 
 
 
 
 
 
