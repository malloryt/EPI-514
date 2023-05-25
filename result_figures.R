################################################################################
# Script purpose: To produce figures for key results to show in presentation
# Author: Austin Hammermeister Suger 
# Last Updated: 5/25/2023
# Required dependencies:
# tidyverse
################################################################################

library(tidyverse)
setwd("C:/Users/Austin Hammermeister/Desktop/PhD/Year 1 Coursework/SPR_2023/EPI 514/Group Project")

tab2_data = data.frame(PR = c(1.81,1.78,1.78,1.79,1.35,1.34,1.42,1.15),
                       PR_lower = c(1.77,1.75,1.74,1.73,1.32,1.31,1.37,1.10),
                       PR_upper = c(1.84,1.82,1.82,1.86,1.38,1.37,1.46,1.21),
                       exposure = c(rep("Financial barrier to healthcare access",4),
                                    rep("Lack of healthcare coverage",4)),
                       PR_type = rep(c("crude","COVID-19 period MH adjusted","pre-COVID-19 period (2017-2019)",
                                   "COVID-19 period (2020-2021)"),2),
                       PR_stratum = c(rep("N",2),rep("Y",2),rep("N",2),rep("Y",2)))
tab2_data$PR_type = factor(tab2_data$PR_type, 
                              levels = c("crude","COVID-19 period MH adjusted","pre-COVID-19 period (2017-2019)",
                                           "COVID-19 period (2020-2021)"))

## Exposure 
p1=ggplot(data=tab2_data, aes(y=exposure,x=PR,color=PR_type))+
  geom_errorbarh(aes(xmin=PR_lower, xmax=PR_upper),height=0.35,
                position=position_dodge(width=0.4), stat="identity")+
  geom_point(aes(color=PR_type),size=3, 
             position=position_dodge(width=0.4), stat="identity")+
  scale_color_manual(values=c("black","purple","blue","red","green","orange"),
                     guide_legend(title = "estimate type"))+
  scale_x_continuous(trans = "log",limits = c(0.98,1.9),
                     breaks = c(1,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9))+
  geom_vline(xintercept=1.0,linetype=2)+
  ylab("Exposure")+
  xlab("prevalence ratio")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

## Exposure facet ##

p2=ggplot(data = tab2_data, aes(x=PR_type,y=PR))+
  geom_errorbar(aes(ymin=PR_lower, ymax=PR_upper),width=0.25)+
  geom_point(aes(color=PR_type),size=4)+
  scale_color_manual(values=c("black","purple","blue","red"),
                     guide_legend(title = "estimate type"))+
  facet_grid(~exposure)+
  scale_y_continuous(trans = "log",limits = c(0.98,1.9),
                     breaks = c(1,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9))+
  geom_hline(yintercept=1.0,linetype=2)+
  ylab("prevalence ratio")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("fig1_nofacet.jpeg",dpi=500,plot=p1,height = 4, width = 10)
ggsave("fig1_exposurefacet.jpeg",dpi=500,plot=p2,height = 8, width = 8)

