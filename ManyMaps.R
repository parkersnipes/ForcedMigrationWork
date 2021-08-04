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


cross_section_geo = select(cross_section,municipality,lat_mean,lon_mean,popn1993,ruggedness,slope,V_cum)
muni_pol <- st_read("Data/col muni polygons/col_admbnda_adm2_mgn_20200416.shp",
                    stringsAsFactors=FALSE)

muni_pol <- subset(muni_pol,ADM2_PCODE != "CO88001")
muni_pol <- subset(muni_pol,ADM2_PCODE!="CO88564")

addCO <- function(muni){
  paste("CO",muni,sep = "")
}


AttributeTableFinal <- read.csv("Data/AttributeTableFinal.csv")

AttributeTableFinal <- mutate(AttributeTableFinal,latnum = as.numeric(lat),
                              lonnum = as.numeric(lon))


cross_section_geo <- mutate(cross_section_geo,
                            ADM2_PCODE = purrr::map_chr(municipality,addCO))

ll_map <- left_join(muni_pol,AttributeTableFinal,by=c("ADM2_PCODE"))

cross_section_merged <- merge(cross_section_geo,ll_map,by = c("ADM2_PCODE"))

# Create adjacency matrix of shapefile. 
polygon <- ll_map
nb <- poly2nb(polygon)
(nb)
listw <- nb2listw(nb,zero.policy = TRUE)
mat2 <- listw2mat(listw)

munigraph <- graph_from_adjacency_matrix(mat2,mode ="undirected",weighted = TRUE)

calcdist <- function(lat1,lon1,lat2,lon2){
  return (distGeo(c(lat1,lon1),c(lat2,lon2))/1000)
}


roads = read.csv("Data/roads.csv")
road_code_list <- roads$ADM2_PCODE
AttributeTableFinal <- mutate(AttributeTableFinal, has_road = (ADM2_PCODE %in% road_code_list))
weight_matrix <- matrix(,nrow = 1120,ncol = 1120)
for(i in 1:1120){
  for(j in 1:1120){
    total_dist<-1000000
    road_factor = 1
    if(AttributeTableFinal$has_road[i] == 1 && (AttributeTableFinal$has_road[j] == 1)){
      road_factor = 0.25
    }
    if(are.connected(munigraph,i,j)){
      d <- calcdist(AttributeTableFinal$latnum[i],AttributeTableFinal$lonnum[i],AttributeTableFinal$latnum[j],AttributeTableFinal$lonnum[j])
      total_dist <- d
      temp <- road_factor*d/2 * (1+cross_section_merged$ruggedness[i])^(1+cross_section_merged$slope[i]/90)+
        road_factor*d/2 * (1+cross_section_merged$ruggedness[j])^(1+cross_section_merged$slope[j]/90)
      edge <- get.edge.ids(munigraph,c(i,j))
      munigraph <- set_edge_attr(munigraph,"weight",edge,total_dist) 
    }
    weight_matrix[i,j]<-total_dist
  }
}

delta_1 = distances(munigraph,v = 259,to = V(munigraph),algorithm = "dijkstra")
delta_2 = distances(munigraph,v = 1009,to = V(munigraph),algorithm = "dijkstra")

deltas <- data.frame(matrix(c(delta_1,delta_2),ncol = 2))
colnames(deltas) <- c("delta_1","delta_2")
deltas <- mutate(deltas, 
                 delta_min = purrr::map2(delta_1,delta_2,min))

var = as.numeric(deltas$delta_min)
summary(var)
sd(var)

delta_min_vector = vector(length = 1120)
vertex_ids = vector(length = 1120)
for(i in 1:1120){
  vertex_ids[i]<- AttributeTableFinal$ADM2_PCODE[i]
  delta_min_vector[i] <- var[i]
}

merge_deltamins <- data.frame(delta_min = delta_min_vector,ADM2_PCODE = vertex_ids) 
FinalWithDeltamins <- merge(ll_map,merge_deltamins,by = "ADM2_PCODE")

# Make the graphs. 

k = 120

title = paste(paste("Colorscaled Colombian municipalities by distance from closest violence source ",sep=""),sep="")
ggplot() +
  #geom_sf(data = muni_map, fill='white', color = 'grey34',lwd=.05) +
  geom_sf(data = FinalWithDeltamins, aes(fill=delta_min),color = 'grey34',lwd=.05) +
  ggtitle(title) +
  #scale_fill_manual(values=c("white","royalblue1"),aesthetics = c("fill"),name= paste("Between 720-630 km from nearest source ", sep="")) +
  #binned_scale(aesthetics = c("fill"),scale_name = "bin",palette = scale,breaks = c(0,10,20,180,240,300),labels = c("km","180km","270km","360km","450km"))
  scale_fill_stepsn(colors = c("red","gold","darkgreen","blue","magenta","violet"),values = NULL,space = "Lab",na.value = "grey50",guide = "coloursteps",aesthetics = "fill",n.breaks = 7,breaks = c(1,k,2*k,3*k,4*k,5*k,6*k),labels = c(paste(k," km"),paste(2*k," km"),paste(3*k," km"),paste(4*k," km"),paste(5*k," km"),paste(6*k," km"),paste(7*k," km")),limits=c(0, 1000))
theme(axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank())
ggsave(filenome,width=11,height=8) 

for(rng in 1:13){
  m <- as.matrix(yearshare_df[rng,])
  col = t(m)
  frame <- data.frame(col,sapply(c(1:17),add1995))
  colnames(frame) <- c("share","year")
  ggplot(frame,aes(x = year,y = share)) + geom_line()
  filename = paste(paste("figures/violence_timeseries_ring",rng,sep=""),".png",sep="")
  ggsave(filename,width=11,height=8)
}

