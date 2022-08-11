library(tidyverse)
library(sqldf)
library(openxlsx)
library(tmap)
library(tmaptools)
library(rgdal)
library(raster)
library(purrr)
library(SciViews)

setwd("F:/My Drive/Exemptusers_Data_analysis")
options(scipen=999) #Disable scientific notation
options(digits = 9)



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


non_exemptions<-c("wsp2020_2020","vwp_mgd", "vwp_mgm", "vwp_mgy",  "401_certification", "unknown")

exemptions_amounts<-filter(wdata, !final_exempt_propcode %in% non_exemptions)


exemptions_amounts<- subset(exemptions_amounts, !is.na(exemptions_amounts$final_exempt_propcode))



# load("F:/My Drive/Exemptusers_Data_analysis/exempt users data.RData")  #Load data from locale 

#Add numbers before names to get them in right order
exemptions_amounts["final_exempt_propcode"][exemptions_amounts["final_exempt_propcode"] == "intake_capacity_mgd"] <- "2Intake Capacity "
exemptions_amounts["final_exempt_propcode"][exemptions_amounts["final_exempt_propcode"] == "safe_yield_2005"] <- "4Safe Yield"
exemptions_amounts["final_exempt_propcode"][exemptions_amounts["final_exempt_propcode"] == "pre_89_mgm"] <- "1Pre 1990 withdrawals"
exemptions_amounts["final_exempt_propcode"][exemptions_amounts["final_exempt_propcode"] == "vdh_pump_capacity_mgd"] <- "3VDH pumping capacity"
exemptions_amounts["final_exempt_propcode"][exemptions_amounts["final_exempt_propcode"] == "rfi_exempt_wd"] <- "5wRFI withdrawls"
exemptions_amounts["final_exempt_propcode"][exemptions_amounts["final_exempt_propcode"] == "safe_yield_1985"] <- "4Safe Yield"
exemptions_amounts["final_exempt_propcode"][exemptions_amounts["final_exempt_propcode"] == "wd_mgy_max_pre1990"] <- "1Pre 1990 withdrawals"

Count_exemptions_amounts<- exemptions_amounts %>% 
  group_by(final_exempt_propcode) %>% 
  summarise(count = n())

p<-ggplot(exemptions_amounts) +
  geom_density(aes(x=log(final_exempt_propvalue_mgd), group=final_exempt_propcode, fill=final_exempt_propcode))+
  labs(
    x = "Data source volume (log)",
    y = "Probability Density",
    title = paste0("Density plot for exempt withdrawals used in model \nfrom each data source")) +
  facet_wrap(~final_exempt_propcode,  ncol=1,
             labeller = labeller(final_exempt_propcode = c("2Intake Capacity "=  paste0("Intake Capacity \n   Number of intakes: ",    Count_exemptions_amounts[2,2]) ,
                                                                                            "4Safe Yield"= paste0("Safe Yield  \n   Number of intakes: ",    Count_exemptions_amounts[4,2]) ,
                                                                                            "1Pre 1990 withdrawals"= paste0("Pre 1990 withdrawals \n   Number of intakes: ",    Count_exemptions_amounts[1,2]) ,
                                                                                            "3VDH pumping capacity"= paste0("VDH pumping capacity \n   Number of intakes: ",    Count_exemptions_amounts[3,2]) ,
                                                                                            "5wRFI withdrawls"= paste0("RFI withdrawls \n   Number of intakes: ",    Count_exemptions_amounts[5,2])
                                                                                            )))+
             # ,scales = "free_y")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Scenarios"))
p<-p+   theme_bw()+ 
  theme(legend.position="none", 
        plot.title = element_text(color = "black", size = 22, face = "bold"),
        legend.text=element_text(size=22),
        axis.text=element_text(size=14, colour="black"),
        axis.title=element_text(size=16, colour="black"),
        axis.line = element_line(colour = "black", 
                                 size = 0.5, linetype = "solid"),
        axis.ticks = element_line(colour="black"),
        panel.grid.major=element_line(colour = "light grey"), 
        panel.grid.minor=element_blank(),
        strip.text = element_text(size = 14, color = "black" ),
        strip.background = element_rect(
          color="black", fill ="#FFE4C4", size=1, linetype="solid"))
p


# ggsave('datsources.png', plot = p, width =8, height = 10, units = "in")

###########################################################################################
# Compare RFI values 

rfi <- filter(wdata, final_exempt_propcode == "rfi_exempt_wd")

rfi <- rfi[,c(1,2,5, 11, 13:19,25:27)]
rfi <- rfi %>% 
  mutate(Max_Pre.89_.MGY.MG = Max_Pre.89_.MGY./365,
         Max_Pre.89_.MGM.MG = Max_Pre.89_.MGM./31,
         rfi_wd_capacity_mgy.mg = rfi_wd_capacity_mgy/365)

rfi <- rfi[,c(1:6,9, 10,15:17,13,14 )]

rfi2 <- as.data.frame(rfi[,c(4:11)])

wdata$Intake_Capacity_.MGD. <- as.numeric(rfi2$Intake_Capacity_.MGD.)
  
col = 8

#Grab location of nearest

x <- cbind(1:nrow(rfi2),max.col(-abs(rfi2[, col] - rfi2[, -col])))

rfi$near_rfi <- rfi2[, -col][x]

rfi$near_dc_name <- names(rfi2[-col])[x[,2]]



#another way
# rfi$nearest <- rfi2[, -col][cbind(1:nrow(rfi2),
#                                   max.col(-abs(rfi2[, col] - rfi2[, -col])))]
 rfi_nearest <- filter(rfi, near_rfi > 0)


#23 values had non-zero nearest values.
#None of these are exact same but all are either Max pre89 mgm/mgy or VDH Pumping Cap are the values closet to RFI

rfi_datsources_count <- rfi_nearest %>% 
  group_by(near_dc_name) %>% 
  summarise(count = n()) 



#############################################################################################
###For intakes that have multiple permit exempt values (eg one safe yield value, one pumping capacity, and one pre-1989), 
 # can we do a comparison to get a sense of the differences between these values at the facility level? Need to think about how best to do this.
 
non_zero <- wdata[,c(1,2,5, 11, 13:19,25:27,20,21)]

 non_zero <- non_zero %>% 
   mutate(Max_Pre.89_.MGY.MG = Max_Pre.89_.MGY./365,
          Max_Pre.89_.MGM.MG = Max_Pre.89_.MGM./31,
          rfi_wd_capacity_mgy.mg = rfi_wd_capacity_mgy/365)
 
non_zero <- non_zero[,-c(7,8,11,12)]
non_zero <- non_zero[,c(1:8,13:15,9:12)]

df <- non_zero[ rowSums(non_zero[,4:11] > 0) >= 1, ]

Long_dat <- pivot_longer(df, cols = c(4:11))


ggplot(Long_dat, aes(y = value))+
  geom_histogram()+
  ggtitle("Change in flow from Ivanhoe to Radford")+
  theme_classic()



################################################################################

safe_yield <- filter(wdata, final_exempt_propcode == "safe_yield_1985" |final_exempt_propcode == "safe_yield_2005" )

# write.csv(safe_yield, "safe_yield.csv")

