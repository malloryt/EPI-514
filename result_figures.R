################################################################################
# Script purpose: To produce figures for key results to show in presentation
# Author: Austin Hammermeister Suger 
# Last Updated: 6/1/2023
# Required dependencies:
# tidyverse
################################################################################

library(tidyverse)
setwd("C:/Users/Austin Hammermeister/Desktop/PhD/Year 1 Coursework/SPR_2023/EPI 514/Group Project")

tab2_prev_data = data.frame(Prev = c(6.6,3.8,6.4,4.0),
                            exposure=c("Financial barrier to healthcare",
                                           "Financial barrier to healthcare",
                                       "Lack of healthcare coverage",
                                       "Lack of healthcare coverage"),
                            exposed = c(c("Yes",
                                               "No"),
                                         c("Yes","No")),
                            period = rep(c("All years (2017-2021)"),2))

p1_prev=ggplot(data=tab2_prev_data, aes(y=Prev,x=exposure,
                                        fill=exposed))+
  geom_bar(position = "dodge",stat="identity")+
  scale_fill_manual(values=c("olivedrab","indianred"),
                     guide_legend(title = "Exposed to access barrier"))+
  
  ylab("prevalence of HIV testing (%)")+
  xlab("healthcare access barrier")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


ggsave("fig1_prev.jpeg",dpi=500,plot=p1_prev,height = 6, width = 10)

tab2_data = data.frame(PR = c(1.12,1.13,1.10,0.65,0.68,0.56),
                       PR_lower = c(1.07,1.07,1.01,0.63,0.64,0.51),
                       PR_upper = c(1.17,1.18,1.20,0.68,0.73,0.63),
                       exposure = c(rep("Financial barrier to healthcare vs. no barrier",3),
                                    rep("Lack of healthcare coverage vs. coverage",3)),
                       PR_type = rep(c("All years (2017-2021)","pre-COVID-19 period (2017-2019)",
                                       "COVID-19 period (2020-2021)"),2))
tab2_data$PR_type = factor(tab2_data$PR_type, 
                           levels = c("All years (2017-2021)","COVID-19 period (2020-2021)",
                                      "pre-COVID-19 period (2017-2019)"))

p1=ggplot(data=tab2_data, aes(y=exposure,x=PR,color=PR_type))+
  geom_errorbarh(aes(xmin=PR_lower, xmax=PR_upper),height=0.35,
                 position=position_dodge(width=0.4), stat="identity")+
  geom_point(aes(color=PR_type),size=3, 
             position=position_dodge(width=0.4), stat="identity")+
  scale_color_manual(values=c("mediumorchid4","royalblue","indianred"),
                     guide_legend(title = " "))+
  scale_x_continuous(trans = "log",limits = c(0.5,1.3),
                     breaks = seq(0.5,1.3,by=0.1))+
  geom_vline(xintercept=1.0,linetype=2)+
  ylab("Exposure")+
  xlab("adjusted prevalence ratio")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("fig1_nofacet.jpeg",dpi=500,plot=p1,height = 4, width = 10)
