#This code generates the summary table for exempt user data

# load the data below or run the code 

## For manuscript analysis we choose non power intakes. Make sure wdata is non_power.

## The abandoned intakes are not removed in this summary (State plan had removed those)

####
library(tidyverse)
library(sqldf)
library(openxlsx)
library(tmap)
library(tmaptools)
library(rgdal)
library(raster)
library(purrr)

setwd("F:/My Drive/Exemptusers_Data_analysis")
options(scipen=999) #Disable scientific notation
options(digits = 4)


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

#separate power and non power
# target <- c("fossilpower", "nuclearpower","hydropower")
# facilities_power<-filter(wdata, Facility.Type %in% target)
# facilities_nonpower<-filter(wdata, !Facility.Type %in% target)
# summary(facilities_nonpower$Facility.Type)
# summary(facilities_power$Facility.Type)

# #read the required file
# wdata<-facilities_power

# wdata<-facilities_nonpower

# low_withdrawal <- c( "wsp2020_2020", "unknown")
# 

# wdata <- filter(wdata, final_exempt_propvalue_mgd >0  )
 permit<-c("vwp_mgd", "vwp_mgm", "vwp_mgy",  "401_certification")



non_exemptions<-c("wsp2020_2020","vwp_mgd", "vwp_mgm", "vwp_mgy",  "401_certification", "unknown")

exemptions_amounts<-filter(wdata, !final_exempt_propcode %in% non_exemptions)


exemptions_amounts<- subset(exemptions_amounts, !is.na(exemptions_amounts$final_exempt_propcode))

 permit_amounts<-filter(wdata, final_exempt_propcode %in% permit)
# 
# wsp_amount<-filter(wdata, final_exempt_propcode %in% low_withdrawal)

nonexemptions_amounts<-filter(wdata, final_exempt_propcode %in% non_exemptions)

Pre_89 <- filter(wdata, final_exempt_propcode == "pre_89_mgm" |final_exempt_propcode == "wd_mgy_max_pre1990" )
rfi <- filter(wdata, final_exempt_propcode == "rfi_exempt_wd"  )

SY <- filter(wdata, final_exempt_propcode == "safe_yield_2005" |final_exempt_propcode == "safe_yield_1985" )
# 
IC <- filter(wdata, final_exempt_propcode == "intake_capacity_mgd"  )

wsp <- filter(wdata, final_exempt_propcode == "wsp2020_2020"  )
wsp <- filter(wsp, final_exempt_propvalue_mgd >0  )

dat <- wdata
# 
# Avaliable_RFI <- filter(wdata, rfi_wd_capacity_mgy > 0)


fn_summary<-function(dat){
  # dat<-filter(dat, Facility_Status != "abandoned")  #  if we plan to use abandoned intakes. 
  summary<-dat%>%
    dplyr::group_by(final_exempt_propcode)%>%
    dplyr::summarise(`Exempt Amount`=sum(final_exempt_propvalue_mgd),
                     Max=max(final_exempt_propvalue_mgd),
                     Min=min(final_exempt_propvalue_mgd),
                     Count=n(),
                     Mean=mean(final_exempt_propvalue_mgd))
  
  quantiles<-as.data.frame(do.call("rbind", tapply(dat$final_exempt_propvalue_mgd, dat$final_exempt_propcode, quantile, c(0.25, 0.5, 0.75))))
  quantiles$final_exempt_propcode<-rownames(quantiles)
  
  
  data_summary<-merge(summary,quantiles, by.x = "final_exempt_propcode")
  
  rownames(data_summary)<-data_summary[,1]
  col_summary<-c("Total volume of exemptions", "Max", "Min", "Number of exemptions","Mean", "1st Quantile", "Median", "3rd Quantile")
  
   data_summary<-data_summary[,2:9]
  colnames(data_summary)<-col_summary
  
  data_summary<-data_summary[,c(4,3,5,6,7,8,2,1)]
  sum(data_summary$`Total volume of exemptions`)
  sum(data_summary$`Number of exemptions`)
  
  data_summary<-t(data_summary) #to global Env
  
  data_summary <-  rbind(data_summary[1,], data_summary[2:8,])
  # convert to cms
  data_summary <-  rbind(data_summary[1,], data_summary[2:8,]*0.0438)
  # list2env(data_summary, envir=.GlobalEnv)
   
    data_summary <- data_summary[,c(4,5,3,2,7,1,6,11)]

   Total_count <- colSums(dat[,c(25,13,19,11,17,18,14,16)] != 0)
    data_summary <- rbind.data.frame(data_summary,Total_count)
    data_summary <<-data_summary[c(9,1:8),c(2,7,1,3,8,4,5)]
   
   }

all_dat_summary<-fn_summary(wdata)
 # exempt_summary<-fn_summary(exemptions_amounts) #comment lines 130 from function
# permit_summary<-fn_summary(permit_amounts)
# wsp_summary<-fn_summary(wsp_amount)
# nonexempt_summary<-fn_summary(nonexemptions_amounts)

# write.csv(all_dat_summary, "F:/My Drive/Exemptusers_Data_analysis/Tables/Table1_March_2022-CMS.csv", row.names = T)
# 
# write.csv(exempt_summary, "F:/My Drive/Exemptusers_Data_analysis/Tables/Non-Power_Exempt_withdrawalSummary2-CMS.csv", row.names = T)
# write.csv(permit_summary, "F:/My Drive/Exemptusers_Data_analysis/Tables/Non-Power_permit_withdrawalSummary2-CMS.csv", row.names = T)
# write.csv(wsp_summary, "F:/My Drive/Exemptusers_Data_analysis/Tables/Non-Power_wsp_withdrawalSummary2.csv-CMS", row.names = T)
# write.csv(nonexempt_summary, "F:/My Drive/Exemptusers_Data_analysis/Tables/Non-Power_nonexempt_summary2-CMS.csv", row.names = T)


# write.csv(exempt_summary, "F:/My Drive/Exemptusers_Data_analysis/Tables/Power_Exempt_withdrawalSummary.csv", row.names = T)
# write.csv(permit_summary, "F:/My Drive/Exemptusers_Data_analysis/Tables/Power_permit_withdrawalSummary.csv", row.names = T)
# write.csv(wsp_summary, "F:/My Drive/Exemptusers_Data_analysis/Tables/Power_wsp_withdrawalSummary.csv", row.names = T)
# 

# Table 3 Based on facility type
############################################################
dat <- exemptions_amounts

fn_ftype_summary<-function(dat){
  summary<-dat%>%
    dplyr::group_by(Facility.Type)%>%
    dplyr::summarise(ExemptValue=sum(final_exempt_propvalue_mgd),
                     max=max(final_exempt_propvalue_mgd),
                     min=min(final_exempt_propvalue_mgd),
                     Count=n(),
                     mean=mean(final_exempt_propvalue_mgd))
  
  quantiles<-as.data.frame(do.call("rbind", tapply(dat$final_exempt_propvalue_mgd, dat$Facility.Type, quantile, c(0.25, 0.5, 0.75)))
  )
  quantiles$Facility.Type<-rownames(quantiles)
  
  
  data_summary<-merge(summary,quantiles, by.x = "Facility.Type")
  
  rownames(data_summary)<-data_summary[,1]
  col_summary<-c("Total volume of exemptions", "Max", "Min", "Number of exemptions","Mean", "1st Quantile", "Median", "3rd Quantile")
  
  data_summary<-data_summary[,2:9]
  colnames(data_summary)<-col_summary
  
  data_summary<-data_summary[,c(4,3,5,6,7,8,2,1)]
  sum(data_summary$`Total volume of exemptions`)
  sum(data_summary$`Number of exemptions`)
  
  data_summary<-t(data_summary) #to global Env
  data_summary <<-  rbind(data_summary[1,], data_summary[2:8,]*0.0438)
  #list2env(data_summary, envir=.GlobalEnv)
}

 # all_dat_summary<-fn_ftype_summary(wdata)
 exempt_F_Level_summary<-fn_ftype_summary(exemptions_amounts)
rowSums(exempt_F_Level_summary)

 
 # permit_summary<-fn_ftype_summary(permit_amounts)
# wsp_summary<-fn_ftype_summary(wsp_amount)
# write.csv(exempt_F_Level_summary, "F:/My Drive/Exemptusers_Data_analysis/Tables/Table3March_CMS.csv", row.names = T)

####Plotting Exempt Amounts

# manu <- filter(exemptions_amounts, Facility.Type == "manufacturing")

#Figure 1

# We used Locality to get the county.

Va_counties2<-readOGR("F:/My Drive/VA_shapefile_updated" , layer = "VA_counties_new")

Localities <- as.data.frame(unique(wdata$Locality))
colnames(Localities) <- "NAME"

### Get the Localities that don't match County name in the shapefile

Va_counties2@data <- merge.data.frame(Localities, Va_counties2@data, by = "NAME", all.x = TRUE, all.y = TRUE)

Va_counties2@data <- Va_counties2@data[complete.cases(Va_counties2@data), ]

#The names of unmatched counties
new_DF <- Va_counties2@data[is.na(Va_counties2@data$GEOID),]

# Repalce names
Va_counties2@data$NAME[Va_counties2@data$NAME=="Bedford"]<-"Bedford County"

Va_counties2@data$NAME[Va_counties2@data$GEOID=="51600"]<-"Fairfax City"

Va_counties2@data$NAME[Va_counties2@data$GEOID=="51059"]<-"Fairfax County"

Va_counties2@data$NAME[Va_counties2@data$GEOID=="51067"]<-"Franklin County"

Va_counties2@data$NAME[Va_counties2@data$GEOID=="51760"]<-"Richmond City"

Va_counties2@data$NAME[Va_counties2@data$GEOID=="51159"]<-"Richmond County"

Va_counties2@data$NAME[Va_counties2@data$GEOID=="51770"]<-"Roanoke City"

Va_counties2@data$NAME[Va_counties2@data$GEOID=="51161"]<-"Roanoke County"

# Calculate the exemption summary all 710 is accounted here

data_summary2<-wdata%>%
  dplyr::filter(!final_exempt_propcode %in% non_exemptions)%>%  #remove permitted
  dplyr::group_by(Locality)%>%
  dplyr::summarise(ExemptValue=sum(final_exempt_propvalue_mgd*0.043812))%>%
  arrange(desc(ExemptValue)) %>% 
  filter(ExemptValue !=0)

data_summary2 <- left_join(data_summary2,Va_counties2@data[,c(1,2)], by = c("Locality"= "NAME"))

# Reload shape file for plotting

VA_counties<-readOGR("F:/My Drive/VA_shapefile_updated" , layer = "VA_counties_new")
Plot_dat2 <- sp::merge(VA_counties,data_summary2, by.x="GEOID", by.y="GEOID")
sum(Plot_dat2@data$ExemptValue, na.rm =TRUE)

tmap_mode("plot")
tm_map2<-tm_shape(Plot_dat2)+
  tm_polygons("ExemptValue", title = expression("Exempt Volume " (m^3/s)),
              # n=5,style="jenks",
               breaks = c(0,1,4,10,30,50,100,200),
              textNA = "No Exemptions",
              colorNA="grey",
              id="NAME")+
  tm_layout(
    # main.title = "Exempt water withdrawals in Virginia",
            legend.title.size = 1.5,
            legend.text.size = 1,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)
# tm_credits("Source: VDEQ", size=0.7, position = c("right","top"))
tm_map2

 tmap_save(tm_map2, "F:/My Drive/Exemptusers_Data_analysis/Figures/March Exempt withdrawals cms.png",  width = 10, height = 6, units = 'in')

####

# Non-Exempt

data_summary<-wdata%>%
  dplyr::filter(final_exempt_propcode %in% non_exemptions)%>%  #remove permitted
  dplyr::group_by(Locality)%>%
  dplyr::summarise(ExemptValue=sum(final_exempt_propvalue_mgd*0.043812))%>%
  arrange(desc(ExemptValue)) %>% 
  filter(ExemptValue !=0)


non_exempt_summary<-left_join(data_summary,Va_counties2@data[,c(1,2)], by = c("Locality"= "NAME"))
# non_exempt_summary[is.na(non_exempt_summary)] <- 0



plot_dat<-sp::merge(VA_counties,non_exempt_summary,by.x="GEOID", by.y="GEOID")

tmap_mode("view")
tm_map<-tm_shape(plot_dat)+
  tm_polygons("ExemptValue", title = "Non-exempt Amount (mgd)",
              # breaks = c(0,25,50,100,200,500,1000,2500, Inf),
              n=5,style="jenks",
              id="NAME",
              textNA = "No data",
              colorNA="grey",
              legend.hist = TRUE)+
  tm_layout(main.title = "Non-exempt water withdrawals in Virginia",
            legend.outside = TRUE,
            legend.title.size = 1.5,
            legend.text.size = 1,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)
  # tm_credits("Source: VDEQ", size=0.7, position = c("right","top"))
tm_map

# tmap_save(tm_map, "F:/My Drive/Exemptusers_Data_analysis/Figures/March non-exempt_withdrawals.png",  width = 10, height = 6, units = 'in')

##
percent_dat<-merge(data_summary2,non_exempt_summary[,c("GEOID","ExemptValue")], by.x = "GEOID", by.y = "GEOID", all.x=TRUE, all.y=TRUE)
colnames(percent_dat)<-c("GEOID","NAME", "Exempt Withdrawals", "Non Exempt Withdrawals")

percent_dat<-percent_dat %>% 
  rowwise() %>% 
  mutate(sum = sum(`Exempt Withdrawals`,`Non Exempt Withdrawals`, na.rm=TRUE))


percent_dat$percet<-100*percent_dat$`Exempt Withdrawals`/percent_dat$sum

# percent_dat$percet[percent_dat$percet=="NaN"]<-"0"
percent_dat$percet<-(round(as.numeric(percent_dat$percet),0))
sum(percent_dat$percet, na.rm=TRUE)/133

plot_dat<-sp::merge(VA_counties,percent_dat,by.x = "GEOID", by.y = "GEOID")

tmap_mode("plot")
tm_map<-tm_shape(plot_dat)+
  tm_polygons("percet", title = "Exempt withdrawals (%)",
              breaks = c(0,25,50,75,90,100),
              # n=5,style="jenks",
              id="CountyName",
              textNA = "No Exemptions",
              colorNA="grey"
              # legend.hist = TRUE
  )+
  tm_layout(
    # main.title = "Percentage of exempt amounts in Virginia",
            # legend.outside = TRUE,
            legend.title.size = 1.5,
            legend.text.size = 1.2,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)
  # tm_credits("Source: VDEQ", size=0.7, position = c("right","top"))
tm_map

tmap_save(tm_map, "F:/My Drive/Exemptusers_Data_analysis/Figures/2March percent-exempt.png",  width = 10, height = 6, units = 'in')




# ##
# percent_dat<-merge(data_summary[,c("NAMELSAD","ExemptValue")],non_exempt_summary[,c("NAMELSAD","ExemptValue")], by.x = "NAMELSAD", by.y = "NAMELSAD")
# colnames(percent_dat)<-c("NAMELSAD", "Exempt Withdrawals", "Non Exempt Withdrawals")
# 
# percent_dat<-percent_dat %>% 
#   rowwise() %>% 
#   mutate(sum = sum(`Exempt Withdrawals`,`Non Exempt Withdrawals`, na.rm=TRUE))
# 
# 
# percent_dat$percet<-100*percent_dat$`Non Exempt Withdrawals`/percent_dat$sum
# 
# percent_dat$percet[percent_dat$percet=="NaN"]<-"0"
# percent_dat$percet<-(round(as.numeric(percent_dat$percet),0))
# 
# plot_dat<-sp::merge(VA_counties,percent_dat,by.x = "NAMELSAD", by.y = "NAMELSAD")
# 
# tmap_mode("plot")
# tm_map<-tm_shape(plot_dat)+
#   tm_polygons("percet", title = "Non-exempt withdrawals (%)",
#                # breaks = c(0,10,25,50,100, Inf),
#               n=5,style="jenks",
#               id="CountyName",
#               textNA = "No permit data",
#               colorNA="grey"
#               # legend.hist = TRUE
#               )+
#   tm_layout(main.title = "Percentage of non-exempt amount in Virginia",
#              # legend.outside = TRUE,
#             legend.title.size = 1.2,
#             legend.text.size = 0.8,
#             legend.position = c("left","top"),
#             legend.bg.alpha = 1)+
#   tm_credits("Source: VDEQ", size=0.7, position = c("right","top"))
# tm_map
# 
# tmap_save(tm_map, "F:/My Drive/Exemptusers_Data_analysis/Figures/percent-nonexempt.png",  width = 10, height = 8, units = 'in')



