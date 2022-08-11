library(rgdal)
library(raster)
library(sf)
library(dplyr)
library(rgeos)
library(tmap)
library(tmaptools)
library(htmlwidgets)
library(openxlsx)
library(grid)



rsegs <- readOGR("C:/var/www/R/P6_RSegs_VA.csv")
metric.table<-read.csv("~/withdrawal_data/ccdata/all_l90_Qout.csv")
new<-sp::merge(rsegs,metric.table, by.x = "RiverSeg", by.y = "river.seg")
map.name<-"90-day Low Flows"
prop.name <- "l90_Qout"

  tm_map1<-tm_shape(new)+
    tm_polygons("Pct.Diff.11_13",title = "Percentage Difference",
                #n=10,style="jenks",midpoint = NA,
                palette="-YlOrBr",
                #breaks = c(-5,0,5, 10, 20, 50,Inf),midpoint = NA,
                breaks=c(-Inf, -50,-30,-20,-1,1,5, Inf),midpoint = NA)+
    tm_layout(main.title = paste0(map.name, ": Base Scenario vs Future 2040 Demands Scenario"),
              # frame = FALSE,
              # legend.outside = TRUE,
              legend.title.size = 1.5,
              legend.text.size = 1,
              legend.position = c("left","top"),
              legend.bg.alpha = 1)+
    # legend.hist.width = .4)
    tm_credits("A", size=1, position =c("right","top") )
  
  tm_map1
  #tmap_save(tm_map1, paste0("~/withdrawal_data/ccplot/new/", prop.name,'_runid_', "Pct.Diff.11_13", '_map.png'),  width = 10, height = 5, units = 'in')
  
  tm_map2<-tm_shape(new)+
    tm_polygons("Pct.Diff.11_15",title = "Percentage Difference",
                #n=10,style="jenks",midpoint = NA,
                palette="-YlOrBr",
                #breaks = c(-5,0,5, 10, 20, 50,Inf),midpoint = NA,
                breaks=c(-Inf, -50,-30,-20,-1,1,5, Inf),midpoint = NA)+
    tm_layout(main.title = paste0(map.name, ": Base Scenario vs Climate Change Scenario"),
              # frame = FALSE,
              # legend.outside = TRUE,
              legend.title.size = 1.5,
              legend.text.size = 1,
              legend.position = c("left","top"),
              legend.bg.alpha = 1)+
    # legend.hist.width = .4)
    tm_credits("B", size=1, position =c("right","top") )
  tm_map2
  #tmap_save(tm_map2, paste0("~/withdrawal_data/ccplot/new/", prop.name,'_runid_', PD[i], '_map.png'),  width = 10, height = 5, units = 'in')
  
  tm_map3<-tm_shape(new)+
    tm_polygons("Pct.Diff.11_18",title = "Percentage Difference",
                #n=10,style="jenks",midpoint = NA,
                palette="-YlOrBr",
                #breaks = c(-5,0,5, 10, 20, 50,Inf),midpoint = NA,
                breaks=c(-Inf, -50,-30,-20,-1,1,5, Inf),midpoint = NA)+
    tm_layout(main.title = paste0(map.name, ": Base Scenario vs Exempt Users Scenario"),
              # frame = FALSE,
              # legend.outside = TRUE,
              legend.title.size = 1.5,
              legend.text.size = 1,
              legend.position = c("left","top"),
              legend.bg.alpha = 1)+
    # legend.hist.width = .4)
    tm_credits("C", size=1, position =c("right","top") )
  tm_map3
  
  #tmap_save(tm_map3, paste0("~/withdrawal_data/ccplot/new/", prop.name,'_runid_', PD[i], '_map.png'),  width = 10, height = 5, units = 'in')
  
  # grid.newpage()
  # pushViewport(viewport(layout = grid.layout(
  #   nrow = 4,
  #   ncol = 1,
  #   heights = c(0.1, 0.3, 0.3, 0.3))))
  # grid.text(map.name, vp = viewport(layout.pos.row = 1), gp=gpar(fontsize=20))
  # print(tm_map1, vp = viewport(layout.pos.row = 2))
  # print(tm_map2, vp = viewport(layout.pos.row = 3))
  # print(tm_map3, vp = viewport(layout.pos.row = 4))
  
  tmap<-tmap_arrange(tm_map1,tm_map2,tm_map3, ncol= 1)
  tmap
  tmap_save(tmap,  paste0("F:/My Drive/Exemptusers_Data_analysis/Figures/", prop.name, '_map_March_feb.png'), dpi = 300, outer.margins = NA, width = 9, height =12, units = 'in')
   