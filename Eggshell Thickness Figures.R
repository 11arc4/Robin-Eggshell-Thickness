


ggplot(shell3, aes(x=Volume_egg, y=Thickness_shell))+
  geom_point()+
  theme_classic()+
  labs(x="Volume (ml)", y="Eggshell Thickness (nm)")



ggplot(shell3, aes(x=Length_egg, y=Thickness_shell))+
  geom_point()+
  theme_classic()+
  labs(x="Length Egg (cm)", y="Eggshell Thickness (nm)")



newdata <- data.frame(Volume_egg= seq(min(shell3$Volume_egg), max(shell3$Volume_egg), length.out=30), 
                      Length_egg=seq(min(shell3$Length_egg), max(shell3$Length_egg), length.out=30))


mod_volume <- lmer(Thickness_shell~Volume_egg + (1|NestID), data=shell3) 
mod_length2 <- lmer(Thickness_shell ~ Length_egg + (1|NestID), data=shell3)


library(boot)

sfun2 <- function(x) {
  simulate(x,newdata=newdata,re.form=~0,
           allow.new.levels=TRUE,cond.sim=FALSE)[[1]]
}

## param only
b_volume <- bootMer(mod_volume,FUN=function(x) predict(x,newdata=newdata,re.form=~0),
              ## re.form=~0 is equivalent to use.u=FALSE
              nsim=100,seed=101)

b_length <- bootMer(mod_length2,FUN=function(x) predict(x,newdata=newdata,re.form=~0),
                    ## re.form=~0 is equivalent to use.u=FALSE
                    nsim=100,seed=101)
#### Confidence and prediction intervals for *unobserved* levels

bootsum <- function(x,ext="_1") {
  d <- data.frame(apply(x$t,2,
                        function(x) c(mean(x),quantile(x,c(0.025,0.975)))))
  d <- setNames(d,paste0(c("bpred","lwr","upr"),ext))
  return(d)
}


dd <- data.frame(newdata,
                 t(bootsum(b_volume,"_3")), 
                 t(bootsum(b_length,"_3")))

names(dd)[3:8] <-c("Pred_v", "LCL_v", "UCL_v","Pred_l", "LCL_l", "UCL_l" ) 







PanelB <- ggplot()+
  geom_point(data=shell3, aes(x=Volume_egg, y=Thickness_shell), shape=1)+
  theme_classic()+
  geom_ribbon(data=dd, aes(x=Volume_egg, ymin=LCL_v, ymax=UCL_v), alpha=0.3)+
  geom_line(data=dd, aes(x=Volume_egg, y=Pred_v))+
  labs(x="Volume (ml)", y="")



PanelA <- ggplot()+
  theme_classic()+
  geom_ribbon(data=dd, aes(x=Length_egg, ymin=LCL_l, ymax=UCL_l), alpha=0.3)+
  geom_line(data=dd, aes(x=Length_egg, y=Pred_l))+
  geom_point(data=shell3, aes(x=Length_egg, y=Thickness_shell), shape=1)+
  labs(x="Length Egg (cm)", y="Eggshell Thickness (nm)")


cowplot::plot_grid(PanelA, PanelB, labels="AUTO")

ggsave("C:/Users/11arc/OneDrive/Documents/Montogomerie Work/Robins/Thickness Plots/Physics of thickness.png",
       units="in", 
       width=7, 
       height=3.5, 
       device="png")






ggplot(shell3 %>% filter(!is.na(Ectoparasites2)), aes(x=Length_egg, y=Thickness_shell, color=Ectoparasites2))+
  geom_smooth(method="lm")+
  scale_color_grey(start=0.2, end=0.7)+
  geom_point(shape=1)+
  theme_classic()+
  labs(x="Length Egg (cm)", y="Eggshell Thickness (nm)", color="Ectoparasites")



mod_ecto <- lmer(Thickness_shell ~ Ectoparasites2+ Length_egg+(1|NestID), data=shell3)



newdata_e <- data.frame(Ectoparasites2= c(rep("NO", 30), rep("YES", 30)), 
                        Length_egg=seq(min(shell3$Length_egg), max(shell3$Length_egg), length.out=30))


b_ecto <- bootMer(mod_ecto,FUN=function(x) predict(x,newdata=newdata_e,re.form=~0),
                    ## re.form=~0 is equivalent to use.u=FALSE
                    nsim=100,seed=101)



dd_ecto <- data.frame(newdata_e,
                 t(bootsum(b_ecto,"_3")))

names(dd_ecto)[3:5] <-c("Pred", "LCL", "UCL") 



ggplot()+
  scale_shape_manual(values=c(1,2))+
  scale_fill_grey()+
  geom_point(data=shell3 %>% filter(!is.na(Ectoparasites2)), aes(x=Length_egg, y=Thickness_shell, shape=Ectoparasites2))+
  theme_classic()+
  geom_ribbon(data=dd_ecto, aes(x=Length_egg, ymin=LCL, ymax=UCL, fill=Ectoparasites2), alpha=0.3)+
  geom_line(data=dd_ecto, aes(x=Length_egg, y=Pred, linetype=Ectoparasites2))+
  labs(x="Length Egg (cm)", y="Eggshell Thickness (nm)", shape="Ectoparasites", fill="Ectoparasites", linetype="Ectoparasites")



ggsave("C:/Users/11arc/OneDrive/Documents/Montogomerie Work/Robins/Thickness Plots/Ectoparasites and thickness.png",
       units="in", 
       width=4.5, 
       height=3.5, 
       device="png")


