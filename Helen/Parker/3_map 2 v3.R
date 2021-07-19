#Author: Parker Snipes
#Date: 5/19/2021
#Purpose: Adapted from Helen Burkhardt's 3_map v2.R. Adds gradient map of year spread, for comparison. 

#load packages
library(tidyverse)
library(sp)
library(rgdal)
library(sf)
library(st)

#import columbian municipality shapefiles
muni_pol = st_read("raw_data/col muni polygons/col_admbnda_adm2_mgn_20200416.shp",
                   stringsAsFactors=FALSE)

#make municipality code match codes from flows dataset
muni_pol = muni_pol %>% 
  mutate(municipality = as.numeric(str_replace(ADM2_PCODE,"CO",""))) %>% 
  select(c("municipality"))

#load flows dataset
muni_flows = read.csv("output/COL_muni_flows.csv",stringsAsFactors=FALSE)

#generate when first episode of violence occurred
muni_flows = muni_flows %>% 
  group_by(municipality) %>%
  mutate(violence = ifelse(victims__UR>0,year,99999)) %>% 
  summarise(first_violence = min(violence)) %>% 
  mutate(first_violence2 = ifelse(first_violence == 99999, NA, first_violence)) %>% 
  select(c("municipality","first_violence2")) %>% 
  rename(first_violence = first_violence2)

(muni_flows)

#connect with polygons
muni_map = left_join(muni_pol,muni_flows,by=c("municipality")) %>% 
  st_as_sf()
# seems to be no problem connecting

#output data to excel file
muni_data = muni_map %>% st_set_geometry(NULL)
muni_data = muni_data[order(muni_data$first_violence),]
muni_data = muni_data %>% rename(year_first_violence = first_violence)
write.csv(muni_data,"output/muni_year_first_violence.csv",row.names=FALSE)

#map
#muni_map$first_violence[is.na(muni_map$first_violence)] = 0

#map that highlights when violence first appeared by region
for (y in seq(1996,2012)) {
  
  muni_map_yr = muni_map %>%  mutate(first_violence_yr = ifelse(first_violence==y,"Yes","No/NA"))
  
  filenome = paste(paste("figures/first_violence_map_",y,sep=""),".png",sep="")
  title = paste(paste("First violence in ",y,sep=""),sep="")
  
  ggplot() +
    #geom_sf(data = muni_map, fill='white', color = 'grey34',lwd=.05) +
    geom_sf(data = muni_map_yr, aes(fill=first_violence_yr),color = 'grey34',lwd=.05) +
    ggtitle(title) +
    scale_fill_manual(values=c("white","royalblue1"),aesthetics = c("fill"),name= paste("First Year of Violence in ",sep="")) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  ggsave(filenome,width=11,height=8)
  
}

# Creates gradient map showing spread of first violence across 1996-2012.  . 
  
  muni_map_yr = muni_map %>%  mutate(first_violence_yr = ifelse(first_violence<=y & first_violence!=0,"Yes","No/NA"))
  
  filenome = paste(paste("figures/violence_spread_map",sep=""),".png",sep="")
  title = paste(paste("Spread of violence 1996-2012",sep=""),sep="")
  
  ggplot() + 
    #geom_sf(data = muni_map, fill='white', color = 'grey34',lwd=.05) +
    geom_sf(data = muni_map_yr, aes(fill=first_violence),color = 'grey34',lwd=.05) +
    ggtitle(title) +
    scale_fill_gradient(low = "red",high= "blue",na.value = "grey50",aesthetics = "fill")+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  
  ggsave(filenome,width=11,height=8)
  








