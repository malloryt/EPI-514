################################################################################
# Script purpose: To produce figures for key results to show in presentation
# Author: Austin Hammermeister Suger 
# Last Updated: 5/26/2023
# Required dependencies:
# tidyverse
################################################################################

library(tidyverse)
setwd("C:/Users/Austin Hammermeister/Desktop/PhD/Year 1 Coursework/SPR_2023/EPI 514/Group Project")

tab2_data = data.frame(PR = c(0.9,0.892,0.901,0.64,0.670,0.555),
                       PR_lower = c(0.86,0.852,0.829,0.60,0.631,0.5),
                       PR_upper = c(0.93,0.935,0.978,0.67,0.711,0.616),
                       exposure = c(rep("Financial barrier to healthcare access",3),
                                    rep("Lack of healthcare coverage",3)),
                       PR_type = rep(c("All years (2017-2021)","pre-COVID-19 period (2017-2019)",
                                   "COVID-19 period (2020-2021)"),2))
tab2_data$PR_type = factor(tab2_data$PR_type, 
                              levels = c("pre-COVID-19 period (2017-2019)","COVID-19 period (2020-2021)",
                                           "All years (2017-2021)"))

## Exposure 
p1=ggplot(data=tab2_data, aes(y=exposure,x=PR,color=PR_type))+
  geom_errorbarh(aes(xmin=PR_lower, xmax=PR_upper),height=0.35,
                position=position_dodge(width=0.4), stat="identity")+
  geom_point(aes(color=PR_type),size=3, 
             position=position_dodge(width=0.4), stat="identity")+
  scale_color_manual(values=c("purple","blue","red"),
                     guide_legend(title = "period"))+
  scale_x_continuous(trans = "log",limits = c(0.4,1.1),
                     breaks = seq(0.4,1.1,by=0.1))+
  geom_vline(xintercept=1.0,linetype=2)+
  ylab("Exposure")+
  xlab("prevalence ratio")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

## Exposure facet ##

p2=ggplot(data = tab2_data, aes(x=PR_type,y=PR))+
  geom_errorbar(aes(ymin=PR_lower, ymax=PR_upper),width=0.25)+
  geom_point(aes(color=PR_type),size=3)+
  scale_color_manual(values=c("purple","blue","red"),
                     guide_legend(title = "period"))+
  facet_grid(~exposure)+
  scale_y_continuous(trans = "log",limits = c(0.4,1.1),
                     breaks = seq(0.4,1.1,by=0.1))+
  geom_hline(yintercept=1.0,linetype=2)+
  ylab("prevalence ratio")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("fig1_nofacet.jpeg",dpi=500,plot=p1,height = 6, width = 10)
ggsave("fig1_exposurefacet.jpeg",dpi=500,plot=p2,height = 8, width = 8)

