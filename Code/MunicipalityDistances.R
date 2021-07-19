#Author: Parker Snipes
#Date: 7/13/2021
#Purpose: Create new variables to track distance of each municipality from the two epicenters of violence,
# and from the closest epicenter of violence. 

#load packages
library(tidyverse)
library(dplyr)
library(ggplot2)
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
# distance from municipality 50251, and delta_3 representing min(delta_1,delta_2). 

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
         delta_3 = purrr::map2_dbl(delta_1,delta_2,min)
)

# Find outliers, which appear to be 23 missing latitude/longitude values (0's). 
good_munis <- dplyr::filter(cross_section_geo,delta_3<2000)


# Generate histogram of corresponding values
ggplot(good_munis, aes(x = delta_3))+geom_histogram(binwidth = 10,fill = "darkblue")



