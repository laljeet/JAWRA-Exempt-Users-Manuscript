library(grid)

library(gridExtra)
library(tidyverse)

base <- read.csv("~/withdrawal_data/ccdata/PEC_base.csv")
d2040 <- read.csv("~/withdrawal_data/ccdata/PEC_2040.csv")
plotexempt <- read.csv("~/withdrawal_data/ccdata/PEC_exempt.csv")
plot10percc <- read.csv("~/withdrawal_data/ccdata/PEC_CC.csv")


cn11 <- 'Base Scenario' #labels for the plot
cn18 <- 'Exempt Users'
cn15 <- 'Climate Change'
# cn14 <- 'CC 50_50'
# cn16 <- 'CC 90_90'
cn13 <- '2040 Demand'

options(scipen=10000)






POE <- function(x){
  x %>%
    mutate(rank = rank(-Flow)) %>%
    mutate(Prob_whole =  100*(rank / (length(Flow) + 1))) %>% 
    mutate(Flow_cms = Flow*0.028)
}


base <- POE(base)
d2040 <- POE(d2040)
plotexempt <- POE(plotexempt)
plot10percc <- POE(plot10percc)



pct_cal <- function(dat, x,y){
  dat %>% 
  filter( between(Prob_whole,x,y)) %>% 
  summarise(mean =round(mean(Flow_cms)),0)
}

pct_cal(plotexempt,95,99)




max <- max(base$Flow_cms)
min <- min(base$Flow_cms)


if (max > 10000){
  max <- 100000
}else if (max > 1000){
  max <- 10000
}else if (max > 100){
  max <- 1000
}else if (max > 10){
  max <- 100
}else {
  max <- 10
}

if (min>100){
  min<-100
}else if (min>10){
  min<-10
}else{
  min<-1
}

if (min==100){
  fixtheyscale<- scale_y_continuous(trans = log_trans(),
                                    breaks = c(100, 1000, 10000, 100000),
                                    limits=c(min,max))
}else if (min==10){
  fixtheyscale<- scale_y_continuous(trans = log_trans(),
                                    breaks = c(10, 100, 1000, 10000),
                                    limits=c(min,max))
}else if (min==1){
  fixtheyscale<- scale_y_continuous(trans = log_trans(),
                                    breaks = c(0.1, 1, 10, 100, 1000, 10000),
                                    limits=c(0.1,1000))
}else{
  fixtheyscale<- scale_y_continuous(trans = log_trans(), breaks = base_breaks(),
                                    labels=scaleFUN, limits=c(min,max))
}

p1<-ggplot()+
  geom_line(data = base[,c(2,6,7)], aes(x= Prob_whole, y = Flow_cms, group =Year,col= cn11),size=0.5)+
  geom_line(data = d2040[,c(2,6,7)], aes(x= Prob_whole, y = Flow_cms, group =Year,col=cn13),size=0.5)+
  scale_y_continuous(trans = log_trans(),
                     breaks = c(0.1, 1, 10, 100, 1000, 10000),
                     limits=c(0.1,1000))+
  theme_bw()+
  theme(legend.position="top",
        legend.title=element_blank(),
        legend.box = "horizontal",
        legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid",
                                         colour ="white"),
        legend.text=element_text(size=14),
        axis.text=element_text(size=14, colour="black"),
        axis.title=element_text(size=14, colour="black"),
        axis.line = element_line(colour = "black",
                                 size = 0.5, linetype = "solid"),
        axis.ticks = element_line(colour="black"),
        panel.grid.major=element_line(colour = "light grey"),
        panel.grid.minor=element_blank())+
  scale_colour_manual(values=c("black","darkgoldenrod"), 
                      breaks=c(cn11, cn13),
                      labels=c(cn11, cn13))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  labs(x= element_blank(), y = bquote("Flow "~(m^3/s)))
p1

p5<-ggplot()+
  geom_line(data = base[,c(2,6,7)], aes(x= Prob_whole, y = Flow_cms, group =Year,col= cn11),size=0.5)+
  geom_line(data = plotexempt[,c(2,6,7)], aes(x= Prob_whole, y = Flow_cms, group =Year,col=cn18),size=0.5)+
  fixtheyscale+
  theme_bw()+
  theme(legend.position="top",
        legend.title=element_blank(),
        legend.box = "horizontal",
        legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid",
                                         colour ="white"),
        legend.text=element_text(size=14),
        axis.text=element_text(size=14, colour="black"),
        axis.title=element_text(size=14, colour="black"),
        axis.line = element_line(colour = "black",
                                 size = 0.5, linetype = "solid"),
        axis.ticks = element_line(colour="black"),
        panel.grid.major=element_line(colour = "light grey"),
        panel.grid.minor=element_blank())+
  scale_colour_manual(values=c("black","darkgoldenrod"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  labs(x=element_blank() , y = element_blank())

p3<-ggplot()+
  geom_line(data = base[,c(2,6,7)], aes(x= Prob_whole, y = Flow_cms, group =Year,col= cn11),size=0.5)+
  geom_line(data = plot10percc[,c(2,6,7)], aes(x= Prob_whole, y = Flow_cms, group =Year,col=cn15),size=0.5)+
  fixtheyscale+
  theme_bw()+
  theme(legend.position="top",
        legend.title=element_blank(),
        legend.box = "horizontal",
        legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid",
                                         colour ="white"),
        legend.text=element_text(size=14),
        axis.text=element_text(size=14, colour="black"),
        axis.title=element_text(size=14, colour="black"),
        axis.line = element_line(colour = "black",
                                 size = 0.5, linetype = "solid"),
        axis.ticks = element_line(colour="black"),
        panel.grid.major=element_line(colour = "light grey"),
        panel.grid.minor=element_blank())+
  scale_colour_manual(values=c("black","darkgoldenrod"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  labs(x="Probability of Exceedance (%)" , y = element_blank())
p3

Name <- "Dry River"
# riv.seg <- "PS3_5990_6161"
p<-grid.arrange(p1,p3,p5, nrow = 1)
p

p<- annotate_figure(p,
                top = text_grob("River segment: Dry River", size = 16),
               )

p
file_name = paste("Aprikl2",Name, "_flow_exceedance_compsingle.png")
ggsave(plot=p, device = "png", file=file_name,width = 13, height = 6.5, units = 'in')
