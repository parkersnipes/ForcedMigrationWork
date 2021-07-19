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
library(Matrix)
library(igraph)
library(shp2graph)
library(maptools)
library(rgeos)
library(spdep)
library(rvest)
library(rlist)
library(readr)
library(xlsx)

# Save latitude and longitude for municipalities 23807 and 50251, respectively. 
cross_section_geo = select(cross_section,municipality,lat_mean,lon_mean,popn1993,ruggedness,slope,V_cum)
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

# Merge scraped files
admin = read_csv("output/col-administrative-division-names-and-p-codes.csv")
AttributeTableFinal <- read_csv("output/AttributeTableFinal.csv")

#cross_section_merged <- merge(AttributeTableFinal,cross_section_geo,by="ADM2_PCODE")
#cross_section_merged <- transform(cross_section_merged,
#                                  latnum = as.numeric(as.character(cross_section_merged$lat)),
#                                  lonnum = as.numeric(as.character(cross_section_merged$lon)))

AttributeTableFinal <- transform(AttributeTableFinal,
                                  latnum = as.numeric(as.character(AttributeTableFinal$lat)),
                                  lonnum = as.numeric(as.character(AttributeTableFinal$lon)))

# Now, using our helper functions, create new column for each delta we want to calculate. 
# Use map2_dbl() to create the new columns (since distGeo must work with individual entries) 



cross_section_merged <- 
  mutate(cross_section_merged,
         delta_1 = purrr::map2_dbl(latnum,lonnum,calc_dist_23807),
         delta_2 = purrr::map2_dbl(latnum,lonnum,calc_dist_50251),
         delta_min = purrr::map2_dbl(delta_1,delta_2,min)
)

# Calculate adjusted distance metric. 
cross_section_merged <- 
  mutate(cross_section_merged,
         topographical_delta_min = (delta_min * (1+ruggedness))^(1+slope/90))

summary(cross_section_merged$topographical_delta_min)
summary(cross_section_merged$delta_min)

# Generate histogram of corresponding values
filenome = paste(paste("Histogram of distance values (missing removed)",sep=""),".png",sep="")
qplot(good_munis$delta_min, geom = "histogram",binwidth = 10,fill = I("darkblue"),main="Distribution of Municipality Distances from Violence Source",xlab="Distance from Source (km)")
ggsave(filenome,width=11,height=8)


# Important: This ONLY gives a sense of what the metric does. It's taking the municipal characteristics and 
# calculating as if they held across the entire crow-flies distance to source, which they do not. 


#import columbian municipality shapefiles
muni_pol <- st_read("raw_data/col muni polygons/col_admbnda_adm2_mgn_20200416.shp",
                   stringsAsFactors=FALSE)

muni_pol <- subset(muni_pol,ADM2_PCODE != "CO88001")
muni_pol <- subset(muni_pol,ADM2_PCODE!="CO88564")

# Prepare for cleaning by merging into the shapefile. 

# First, create merge column to match shapefile merge column

addCO <- function(muni){
  paste("CO",muni,sep = "")
}

cross_section_geo <- mutate(cross_section_geo,
                            ADM2_PCODE = purrr::map_chr(municipality,addCO))

# Next, join on the newly created merge column. 
#latlong_map <- left_join(muni_pol,cross_section_geo,by=c("ADM2_PCODE"))

# Drop all entries with NA for latitude and longitude, to create our "overlay" map. 
overlay_map <- filter(latlong_map,!is.na(latlong_map$lat_mean))
#st_write(overlay_map,dsn = "output",layer = "overlaymap", driver = "ESRI Shapefile")


(admin)
(latlon)

# Make new shapefile to display
ll_map <- left_join(muni_pol,AttributeTableFinal,by=c("ADM2_PCODE"))
class(ll_map)
#sf::st_write(ll_map,dsn = "output",layer = "LatMap",driver = "ESRI Shapefile")

# Create adjacency matrix of shapefile. 
polygon <- ll_map
nb <- poly2nb(polygon)
(nb)
listw <- nb2listw(nb,zero.policy = TRUE)
mat2 <- listw2mat(listw)

print(nb)

# Create igraph from adjacency matrix, without initializing weights yet. 
munigraph <- graph_from_adjacency_matrix(mat2,mode ="undirected",weighted = TRUE)
plot(munigraph)

# Create matrix of weights - this is unnecessarily intensive, but works. 

# Helper
calcdist <- function(lat1,lon1,lat2,lon2){
  return (distGeo(c(lat1,lon1),c(lat2,lon2))/1000)
}


weight_matrix <- matrix(,nrow = 1120,ncol = 1120)
  for(i in 1:1120){
    for(j in 1:1120){
      total_dist<-1000000
      if(!is.na(mat2[i,j]) && mat2[i,j]!=0){
        (AttributeTableFinal$latnum[i])
        (AttributeTableFinal$lonnum[i])
        (AttributeTableFinal$latnum[j])
        (AttributeTableFinal$lonnum[j])
        d <- calcdist(AttributeTableFinal$latnum[i],AttributeTableFinal$lonnum[i],AttributeTableFinal$latnum[j],AttributeTableFinal$lonnum[j])
        (total_dist)
        total_dist <- d
        if(!is.na(cross_section_merged$ruggedness[i])){
        total_dist<- d/2 * (1+cross_section_merged$ruggedness[i])^(1+cross_section_merged$slope[i]/90)+
          d/2 * (1+cross_section_merged$ruggedness[j])^(1+cross_section_merged$slope[j]/90)
        }
      }
      (total_dist)
      (i)
      weight_matrix[i,j]<- total_dist
      
    }
    (i)
  }


