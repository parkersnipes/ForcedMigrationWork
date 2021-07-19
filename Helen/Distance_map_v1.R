#Author: Parker Snipes
#Date: 7/15/2021
#Purpose: Adapted from Helen Burkhardt's 3_map 2 v2. Create map that highlights 
#municipalities betweeen 200 and 250 kilometers from nearest source. 

#load packages
library(tidyverse)
library(sp)
library(rgdal)
library(sf)
library(st)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(naniar)
library(purrr)
library(sp)
library(rgdal)
library(sf)
library(st)
library(forcedMigration)
library(geosphere)


# Save latitude and longitude for municipalities 23807 and 50251, respectively. 
cross_section_geo = select(cross_section,municipality,lat_mean,lon_mean,popn1993,V_cum)
mun23807 <- dplyr::filter(cross_section,municipality == 23807,)
mun50251 <- dplyr::filter(cross_section,municipality == 50251,)

lat23807 <- mun23807[["lat_mean"]]
lon23807 <- mun23807[["lon_mean"]]
lat50251 <- mun50251[["lat_mean"]]
lon50251 <- mun50251[["lon_mean"]]


# Create new variable delta_1 representing distance from municipality 23807, delta_2 representing 
# distance from municipality 50251, and delta_min representing min(delta_1,delta_2). 

# First, define a function to calculate distance from each source to 
# a given latitude and longitude, in kilometers, for use inside map2().  
calc_dist_23807 <- function(lat,lon){
  return(distGeo(c(lat,lon),c(lat23807,lon23807))/1000)
}

calc_dist_50251 <- function(lat,lon){
  return(distGeo(c(lat,lon),c(lat50251,lon50251))/1000)
}

# Now, using our helper functions, create new column for each delta we want to calculate. 
# Use map2_dbl() to create the new columns (since distGeo must work with individual entries) 

cross_section_geo <- 
  mutate(cross_section_geo,
         delta_1 = purrr::map2_dbl(lat_mean,lon_mean,calc_dist_23807),
         delta_2 = purrr::map2_dbl(lat_mean,lon_mean,calc_dist_50251),
         delta_min = purrr::map2_dbl(delta_1,delta_2,min)
  )


#import columbian municipality shapefiles
muni_pol = st_read("raw_data/col muni polygons/col_admbnda_adm2_mgn_20200416.shp",
                   stringsAsFactors=FALSE)

#make municipality code match codes from flows dataset
muni_pol = muni_pol %>% 
  mutate(municipality = as.numeric(str_replace(ADM2_PCODE,"CO",""))) %>% 
  select(c("municipality"))

(muni_pol)
(cross_section_geo)

#connect with polygons
muni_map = left_join(muni_pol,cross_section_geo,by=c("municipality"),copy = TRUE) %>% 
  st_as_sf()
# seems to be no problem connecting



#map that highlights only those within 200-250 kilometers of nearest source. 

# Replace anomalous delta_min's (missing values) with NA's. 
muni_map$delta_min[muni_map$delta_min > 2000] <- NA

muni_map_dist = muni_map %>%  mutate(in_target_distance = ifelse((!is.na(delta_min)&delta_min<=250&delta_min>200),"Yes","No/NA"))

filenome = paste(paste("figures/distance_map_200-250",sep=""),".png",sep="")
title = paste(paste("Colombian municipalities by distance from closest violence source ",sep=""),sep="")
ggplot() +
  #geom_sf(data = muni_map, fill='white', color = 'grey34',lwd=.05) +
  geom_sf(data = muni_map_dist, aes(fill=in_target_distance),color = 'grey34',lwd=.05) +
  ggtitle(title) +
  scale_fill_manual(values=c("white","royalblue1"),aesthetics = c("fill"),name= paste("Between 200-250 km from nearest source ", sep="")) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
ggsave(filenome,width=11,height=8) 


filenome = paste(paste("figures/distance_map_gradient",sep=""),".png",sep="")
title = paste(paste("Colombian municipalities by distance from closest violence source ",sep=""),sep="")
ggplot() + 
  #geom_sf(data = muni_map, fill='white', color = 'grey34',lwd=.05) +
  geom_sf(data = muni_map_dist, aes(fill=V_cum),color = 'grey34',lwd=.05) +
  ggtitle(title) +
  scale_fill_gradient(low = "red", high = "blue",aesthetics = c("fill"),name= paste("Distance from source (km)", sep="")) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
ggsave(filenome,width=11,height=8)

summarize(muni_map$delta_min)


