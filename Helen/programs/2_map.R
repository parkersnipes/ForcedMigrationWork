#Author: Helen Burkhardt
#Date: 5/18/2020
#Purpose: read in data and create summary statistics/figures

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

#generate when first episode of violence occured
muni_flows = muni_flows %>% 
  group_by(municipality) %>%
  mutate(violence = ifelse(victims__UR>0,year,99999)) %>% 
  summarise(first_violence = min(violence)) %>% 
  mutate(first_violence2 = ifelse(first_violence == 99999, NA, first_violence)) %>% 
  select(c("municipality","first_violence2")) %>% 
  rename(first_violence = first_violence2)
  
#connect with polygons
muni_map = left_join(muni_pol,muni_flows,by=c("municipality")) %>% 
  st_as_sf()
# seems to be no problem connecting

#map
png("figures/first_violence_map.png", width = 800, height = 600)
ggplot() + 
  geom_sf(data = muni_map, aes(fill=first_violence), color = 'grey34',lwd=.05) +
  ggtitle("First year of violence by Columbian municipality") +
  scale_fill_continuous(name="first year of violence",na.value = "white") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
dev.off()





