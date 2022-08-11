#This code generates the summary table for exempt user data

# load the data below or run the code 



####

library(tidyverse)
library(sqldf)
library(openxlsx)
library(tmap)
library(tmaptools)
library(rgdal)
library(raster)
library(purrr)
library(scales)

setwd("F:/My Drive/Exemptusers_Data_analysis/Code")
options(scipen=999) #Disable scientific notation
options(digits = 3)



wdata_original <- read.csv('https://raw.githubusercontent.com/laljeet/JAWRA-Exempt-Users-Manuscript/main/Data/Exempt_Users_Foundational_dataset.csv',header = T, sep = ",", 
                           na.strings=c("","NA"))



wdata = wdata_original[order(wdata_original[,'mp_hydroid'],-wdata_original[,'mp_hydroid']),]
wdata = wdata_original[!duplicated(wdata_original$mp_hydroid),]

duplicate<-wdata_original[duplicated(wdata_original$mp_hydroid),]
d_fac<-filter(wdata, Facility_Status == 'duplicate')
wdata<-filter(wdata, Facility_Status != "duplicate")
dupli_facilities<-rbind.data.frame(duplicate,d_fac)



#### Remove abandoned intakes

wdata <- filter(wdata, Facility_Status != "abandoned")



wdata$mp_latitude<-ifelse(wdata$mp_latitude <"34"| wdata$mp_latitude>"40"| is.na(wdata$mp_latitude),wdata$facility_latitude,wdata$mp_latitude)

wdata$mp_longitude<-ifelse(wdata$mp_longitude <"-74"| wdata$mp_longitude>"-84"| is.na(wdata$mp_longitude),wdata$facility_longitude,wdata$mp_longitude)

badlatlong<-filter(wdata,mp_latitude <"34"| mp_latitude>"40"  | mp_longitude>"-84" | mp_longitude <"-74" | 
                     is.na(mp_latitude)|is.na(mp_longitude))

plot_data<- anti_join(wdata, badlatlong) # use this for plotting

###Create adatfram ewith atleast one value for exempt data sources
non_zero <- wdata[,c(1,2,5, 11, 13:19,25:27,20,21)]

non_zero <- non_zero %>% 
  mutate(Max_Pre.89_.MGY.MG = Max_Pre.89_.MGY./365,
         Max_Pre.89_.MGM.MG = Max_Pre.89_.MGM./31,
         rfi_wd_capacity_mgy.mg = rfi_wd_capacity_mgy/365)

non_zero <- non_zero[,-c(7,8,11,12)]
non_zero <- non_zero[,c(1:8,13:15,9:12)]

# _______________________________________________________________
# Specify the number of data sources
 Number.of.datsources <- 2

data_source_comparision <- function(Number.of.datsources){
df <- non_zero[ rowSums(non_zero[,4:11] > 0) >=Number.of.datsources, ]
df$Intake_Capacity_.MGD. <- as.numeric(df$Intake_Capacity_.MGD.)
Long_dat <- pivot_longer(df[,c(4:11)], cols = c(1:8))
# Long_dat <- Long_dat[,c(8,9)]

summary<-Long_dat%>%
  dplyr::group_by(name)%>%
  dplyr::summarise(Sum=sum(value, na.rm = T),
                   max=max(value, na.rm = T),
                   min=min(value, na.rm = T),
                   mean=mean(value, na.rm = T))

#Add numbers before names to get them in right order
Long_dat["name"][Long_dat["name"] == "Intake_Capacity_.MGD."] <- "3Intake Capacity "
Long_dat["name"][Long_dat["name"] == "X2005_Safe_Yield_.MGD."] <- "5Safe Yield 2005"
Long_dat["name"][Long_dat["name"] == "Max_Pre.89_.MGM.MG"] <- "1Withdrawals before 1990 monthly"
Long_dat["name"][Long_dat["name"] == "VDH_Total_Pumping_Capacity_.MGD."] <- "4VDH pumping capacity"
Long_dat["name"][Long_dat["name"] == "rfi_wd_capacity_mgy.mg"] <- "7wRFI withdrawls"
Long_dat["name"][Long_dat["name"] == "X1985_Safe_Yield_.MGD."] <- "6Safe Yield 1985"
Long_dat["name"][Long_dat["name"] == "Max_Pre.89_.MGY.MG"] <- "2Withdrawals before 1990 yearly"

Long_dat$value <- Long_dat$value * 0.0438

# summary <- merge.data.frame(summary, count_df, by = "name")


# _________________________________________________________________________
dat_plot <- Long_dat%>%
  dplyr::filter(value > 0)

dat_plot <- dat_plot%>%
  dplyr::filter(name != "X401_Certification_Limit_.MGD.")

count_df<-dat_plot %>% 
  dplyr::group_by(name)%>%
  dplyr::summarise(count = n())

# ggpairs(dat_plot,  ggplot2::aes(colour=name),
#         upper = list(continuous = "density", combo = "box_no_facet"),
#         lower = list(continuous = "points", combo = "dot_no_facet")) 

# p <- dat_plot %>% 
# ggplot() +
#   geom_boxplot(aes(x = name, y = log(value)))+xlab("Data Sources") +
#   ylab("Exempt Amount (log) ") +
#   labs(title = paste0("More than ",Number.of.datsources," values for avaliable data sources" ))
#                
# p <- p + theme_classic()
# 
# p <- p+theme(axis.text.x=element_text(angle = -90, hjust = 0),
#              legend.position="top", 
#                    legend.title=element_blank(),
#                    legend.box = "horizontal", 
#                    legend.background = element_rect(fill="white",
#                                                     size=0.5, linetype="solid", 
#                                                     colour ="white"),
#                    legend.text=element_text(size=12),
#                    axis.text=element_text(size=12, colour="black"),
#                    axis.title=element_text(size=14, colour="black"),
#                    axis.line = element_line(colour = "black", 
#                                             size = 0.5, linetype = "solid"),
#                    axis.ticks = element_line(colour="black"),
#                    panel.grid.major=element_line(colour = "light grey"), 
#                    panel.grid.minor=element_blank())
# p
# 
# ggsave(paste(Number.of.datsources, "data sources.png" ), plot = p,width = 8, height=8 , units = "in" )




# C Density plot
min_plot <- min(dat_plot$value)
max_plot <- max(dat_plot$value)
p2<-ggplot(dat_plot) +
  geom_density(aes(x=value, group=name, fill=name))+
  labs(
    x = " Exempt Amount cms (log)",
    y = "Probability Density",
    # title = paste0("Number of intakes with atleast ",Number.of.datsources," permit exempt data categories" )
    ) +
  facet_wrap(~name,  ncol=1 ,labeller = labeller(name = c("3Intake Capacity "=  paste0("Intake Capacity (Number of intakes: ",    count_df[3,2],")") ,
                                                                            "6Safe Yield 1985"= paste0("Safe Yield 1985 (Number of intakes: ",    count_df[6,2],")") ,
                                                                            "5Safe Yield 2005"= paste0("Safe Yield 2005 (Number of intakes: ",    count_df[5,2],")") ,
                                                                           "1Withdrawals before 1990 monthly"= paste0("Withdrawals before 1990 monthly (Number of intakes: ",  count_df[1,2],")") ,
                                                                           "2Withdrawals before 1990 yearly"= paste0("Withdrawals before 1990 yearly (Number of intakes: ",    count_df[2,2],")") ,
                                                                           "4VDH pumping capacity"= paste0("VDH pumping capacity (Number of intakes: ",    count_df[4,2],")") ,
                                                                           "7wRFI withdrawls"= paste0("RFI withdrawls (Number of intakes: ",    count_df[7,2],")"))))+
 # Add scales = "free_y" in facet wrap for free y axis
  scale_x_continuous(trans = log_trans(), 
                     breaks = c(0.0001,0.001,0.01,0.1,1, 10, 100, 1000), 
                     labels =  ~ifelse(.x <= 1, label_number(accuracy = .0001)(.x), label_number(accuracy = 1)(.x)),
                     limits=c(min_plot,max_plot))+
   theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="name"))
p2<-p2+   theme_bw()+ 
  theme(plot.title = element_text(size=24),
        legend.position="none", 
        legend.text=element_text(size=18),
        axis.text=element_text(size=18, colour="black"),
        axis.title=element_text(size=18, colour="black"),
        axis.line = element_line(colour = "black", 
                                 size = 0.5, linetype = "solid"),
        axis.ticks = element_line(colour="black"),
        panel.grid.major=element_line(colour = "light grey"), 
        panel.grid.minor=element_blank(),
        strip.text = element_text(size = 18, color = "black" ),
        strip.background = element_rect(
          color="black", fill ="#FFE4C4", size=1.5, linetype="solid"))
p2
ggsave(paste(Number.of.datsources, "March_densityplot2.png" ), plot = p2,width = 11.5, height=12 , units = "in" )
}
data_source_comparision(2)



################Filter 4000 value out

data_source_comparision_filtered <- function(Number.of.datsources){
  df <- non_zero[ rowSums(non_zero[,4:11] > 0) >=Number.of.datsources, ]
  df$Intake_Capacity_.MGD. <- as.numeric(df$Intake_Capacity_.MGD.)
  Long_dat <- pivot_longer(df[,c(4:11)], cols = c(1:8))
  # Long_dat <- Long_dat[,c(8,9)]
  
  summary<-Long_dat%>%
    dplyr::group_by(name)%>%
    dplyr::summarise(Sum=sum(value, na.rm = T),
                     max=max(value, na.rm = T),
                     min=min(value, na.rm = T),
                     mean=mean(value, na.rm = T))
  
  
  count_df<-Long_dat%>%
    dplyr::filter(value != 0) %>% 
    dplyr::group_by(name)%>%
    dplyr::summarise(count = n())
  
  summary <- merge.data.frame(summary, count_df, by = "name")
  
  
  # _________________________________________________________________________
  dat_plot <- Long_dat%>%
    dplyr::filter(value != 0 & value < 2500)
  
  # ggpairs(dat_plot,  ggplot2::aes(colour=name),
  #         upper = list(continuous = "density", combo = "box_no_facet"),
  #         lower = list(continuous = "points", combo = "dot_no_facet")) 
  
  p <- dat_plot %>% 
    ggplot() +
    geom_boxplot(aes(x = name, y = value))+xlab("Data Sources") +
    ylab("Exempt Amount") +
    labs(title = paste0("More than ",Number.of.datsources," values for avaliable data sources" ))
  
  p <- p + theme_classic()
  
  p <- p+theme(axis.text.x=element_text(angle = -90, hjust = 0),
               legend.position="top", 
               legend.title=element_blank(),
               legend.box = "horizontal", 
               legend.background = element_rect(fill="white",
                                                size=0.5, linetype="solid", 
                                                colour ="white"),
               legend.text=element_text(size=12),
               axis.text=element_text(size=12, colour="black"),
               axis.title=element_text(size=14, colour="black"),
               axis.line = element_line(colour = "black", 
                                        size = 0.5, linetype = "solid"),
               axis.ticks = element_line(colour="black"),
               panel.grid.major=element_line(colour = "light grey"), 
               panel.grid.minor=element_blank())
  p
  
  ggsave(paste(Number.of.datsources, "filtered_valuedata sources.png" ), plot = p,width = 8, height=8 , units = "in" )
  
  # C Density plot
  
  p2<-ggplot(dat_plot) +
    geom_density(aes(x=value, group=name, fill=name))+
    labs(
      x = " Exempt Amount",
      y = "Probability Density",
      title = paste0("More than ",Number.of.datsources," values for avaliable data sources" )) +
    facet_wrap(~name,  ncol=1, scales = "free_y")+
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    guides(fill=guide_legend(title="name"))
  p2<-p2+   theme_bw()+ 
    theme(plot.title = element_text(size=24),
          legend.position="none", 
          legend.text=element_text(size=18),
          axis.text=element_text(size=18, colour="black"),
          axis.title=element_text(size=18, colour="black"),
          axis.line = element_line(colour = "black", 
                                   size = 0.5, linetype = "solid"),
          axis.ticks = element_line(colour="black"),
          panel.grid.major=element_line(colour = "light grey"), 
          panel.grid.minor=element_blank(),
          strip.text = element_text(size = 18, color = "black" ),
          strip.background = element_rect(
            color="black", fill ="#FFE4C4", size=1.5, linetype="solid"))
  p2
  ggsave(paste(Number.of.datsources, "data_source_comparision_filtered_densityplot.png" ), plot = p2,width = 11.5, height=11 , units = "in" )
}
data_source_comparision_filtered(6)
# ____________________________________________________________________________
summary <- t(summary)
colnames(summary) <- summary[1,]
summary <- (summary[-c(1),])


quantile <- Long_dat %>% 
  group_by(name) %>%  
  summarise(quantile = scales::percent(c(0.25, 0.5, 0.75)),
            mpg = quantile(value, c(0.25, 0.5, 0.75), na.rm =T))

quantile2 <- pivot_wider(quantile, names_from = name, values_from = mpg)


rownames(quantile2) <- as.data.frame(quantile2[,1])






