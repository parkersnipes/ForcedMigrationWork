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
library(reshape2)
library(plotrix)


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
      #road_factor = 0.25
    }
    if(are.connected(munigraph,i,j)){
      d <- calcdist(AttributeTableFinal$latnum[i],AttributeTableFinal$lonnum[i],AttributeTableFinal$latnum[j],AttributeTableFinal$lonnum[j])
      total_dist <- d
      temp <- road_factor*d/2 * (1+cross_section_merged$ruggedness[i])^(1+cross_section_merged$slope[i]/90)+
        road_factor*d/2 * (1+cross_section_merged$ruggedness[j])^(1+cross_section_merged$slope[j]/90)
      #total_dist <= temp
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

#FinalWithDeltamins$delta_min[FinalWithDeltamins$delta_min > 1000] <- NA

filenome = paste(paste("Histogram of updated delta-min",sep=""),".png",sep="")
qplot(as.numeric(deltas$delta_min), geom = "histogram",binwidth = 10,fill = I("darkblue"),main="Cumulative, Terrain-weighted Distance from Violence Source",xlab="Distance from Source (km)")
ggsave(filenome,width=11,height=8)

filenome = paste(paste("figures/Updated_distance_map_gradient",sep=""),".png",sep="")
title = paste(paste("Updated Colombian municipalities by distance from closest violence source ",sep=""),sep="")
ggplot() +
  #geom_sf(data = muni_map, fill='white', color = 'grey34',lwd=.05) +
  geom_sf(data = FinalWithDeltamins, aes(fill=delta_min),color = 'grey34',lwd=.05) +
  ggtitle(title) +
  scale_fill_gradient(low = "red", high = "blue",aesthetics = c("fill"),name= paste("Distance from source (km)", sep="")) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
ggsave(filenome,width=11,height=8)
counter = 0
  FinalWithDeltamins = FinalWithDeltamins %>%  mutate(in_target_distance = ifelse((delta_min<=720&delta_min>630),"Yes","No/NA"))
  filenome = paste(paste("figures/colorscaled_distance_ring",sep=""),".png",sep="")
  title = paste(paste("Colorscaled Colombian municipalities by distance from closest violence source ",sep=""),sep="")
  ggplot() +
    #geom_sf(data = muni_map, fill='white', color = 'grey34',lwd=.05) +
    geom_sf(data = FinalWithDeltamins, aes(fill=delta_min),color = 'grey34',lwd=.05) +
    ggtitle(title) +
    #scale_fill_manual(values=c("white","royalblue1"),aesthetics = c("fill"),name= paste("Between 720-630 km from nearest source ", sep="")) +
    #binned_scale(aesthetics = c("fill"),scale_name = "bin",palette = scale,breaks = c(0,10,20,180,240,300),labels = c("km","180km","270km","360km","450km"))
    scale_fill_stepsn(colors = c("red","gold","darkgreen","blue","magenta","violet"),values = NULL,space = "Lab",na.value = "grey50",guide = "coloursteps",aesthetics = "fill",n.breaks = 7,breaks = c(1,30,60,90,120,150,180),labels = c("0km","90km","180km","270km","360km","450km","540km"),limits=c(0, 1000))
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  ggsave(filenome,width=11,height=8) 
  
  addCO <- function(muni){
    paste("CO",muni,sep = "")
  }
  
  
  
  violence_data <- read.csv("Data/Book1.csv")
  violence_data <- mutate(violence_data, ADM2_PCODE = addCO(municipality))
  violence_set <- merge(violence_data,FinalWithDeltamins,by="ADM2_PCODE")
 
  
  violence_set <- mutate(violence_set, violence = ifelse(victims__UR>0,year,99999))
  violence_set <- mutate(violence_set, total_violence = ifelse(year==2012,cum_victims_UR,0))
  violence_set <- mutate(violence_set, total_violence_num = as.numeric(total_violence))
  violence_set <- mutate(violence_set, ring_num = as.integer(delta_min/30)+1)
  
  vtot_set <- violence_set
  vtot_set <- select(violence_set,ADM2_PCODE,year,cum_victims_UR,total_violence_num,victims__UR,ring_num)
  
  for(i in 1:nrow(vtot_set)){
      if(vtot_set$year[i] == 2012){
        code = vtot_set$ADM2_PCODE[i]
        for(j in 1:nrow(vtot_set)){
          if(vtot_set$ADM2_PCODE[j] == code){
            vtot_set$total_violence_num[j] <- vtot_set$cum_victims_UR[i]
          }
        }
      }
        
  }
  
  # Part 3 here is just graphic year:share for each ring. Do this for all ring sizes.   
  Vtot_cumulative <- select(vtot_set,year,victims__UR,cum_victims_UR,total_violence_num,ADM2_PCODE)
  Vtot_final <- merge(FinalWithDeltamins,vtot_set,by = "ADM2_PCODE")
  Vtot_final <- mutate(Vtot_final, share = victims__UR / total_violence_num)
  Vtot_final <- filter(Vtot_final,total_violence_num>0)
  
  stderrs <- matrix(nrow = 21,ncol= 17)
  
  
  for(width_count in 1:4){
    Vtot_final <- mutate(Vtot_final, ring_num = as.integer(delta_min/(30*width_count)+1))
    yearshare_matrix <- matrix(0,nrow = 17,ncol = 21)
    stderr_matrix <- matrix(0,nrow = 17,ncol = 21)
    ring_count_list <- matrix(0,nrow= 1,ncol = 21)
    for(year_index in 1996:2012){
      for(ring_index in 1:21){
        temp_Vtot_Final <- filter(Vtot_final,year == year_index & ring_num == ring_index)
        ring_count <- nrow(temp_Vtot_Final)
        yearshare_list <- temp_Vtot_Final$share
        stderr_matrix[year_index-1995,ring_index] <- std.error(yearshare_list)
        yearshare_matrix[year_index-1995,ring_index] <- sum(yearshare_list)/ring_count
        ring_count_list[ring_index] <- ring_count
      }
      
    }
    print(sum(ring_count_list))
    pd = position_dodge2(width = NULL,
                         preserve = c("total", "single"),
                         padding = 0.1,
                         reverse = FALSE)
    
    yearshare_df <- data.frame(data = t(yearshare_matrix))
    stderr_df <- data.frame(data = t(stderr_matrix))
    cnames = c("1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012")
    colnames(yearshare_df) <- cnames
    colnames(stderr_df) <- cnames
    yearshare_df <- cbind("ring" = as.numeric(rownames(yearshare_df)),yearshare_df)
    yearshare_df <- pivot_longer(yearshare_df,cnames,names_to = "year",values_to = "share")
    stderr_df <- cbind("ring" = as.numeric(rownames(stderr_df)),stderr_df)
    stderr_df <- pivot_longer(stderr_df,cnames,names_to = "year",values_to = "se")
    yearshare_df <- merge(yearshare_df,stderr_df,by = c("ring","year"))
    

    yearshare_df_1 <- filter(yearshare_df,ring<7)
    yearshare_df_2 <- filter(yearshare_df,ring>=7 & ring<14)
    yearshare_df_3 <- filter(yearshare_df,ring>=14 & ring<21)
    
    ggplot(yearshare_df_1,aes(x = year,y = share,group = factor(ring)))+geom_point(aes(colour = factor(ring)))+ scale_x_discrete(limits=c("1996","1998","2000","2002","2004","2006","2008","2010","2012"))+geom_errorbar( aes(ymin = share-se, ymax = share+se,colour = factor(ring)),width = 0.2)
    filename = paste(paste("figures/violence_ringseries_1-6 width = ",toString(30*width_count),sep = ""),".png",sep = "")
    ggsave(filename,width=11,height=8)
    ggplot(yearshare_df_2,aes(x = year,y = share,group = factor(ring)))+geom_point(aes(colour = factor(ring)))+ scale_x_discrete(limits=c("1996","1998","2000","2002","2004","2006","2008","2010","2012"))+geom_errorbar( aes(ymin = share-se, ymax = share+se,colour = factor(ring)),width = 0.2)
    filename = paste(paste("figures/violence_ringseries_7-13-width = ",toString(30*width_count),sep = ""),".png",sep = "")
    ggsave(filename,width=11,height=8)
    ggplot(yearshare_df_3,aes(x = year,y = share,group = factor(ring)))+geom_point(aes(colour = factor(ring)))+ scale_x_discrete(limits=c("1996","1998","2000","2002","2004","2006","2008","2010","2012"))+geom_errorbar( aes(ymin = share-se, ymax = share+se,colour = factor(ring)),width = 0.2)
    filename = paste(paste("figures/violence_ringseries_14-20, width = ",toString(30*width_count),sep = ""),".png",sep = "")
    ggsave(filename,width=11,height=8)
    
    yearshare_df_1 <- filter(yearshare_df,year<2002)
    yearshare_df_2 <- filter(yearshare_df,year>=2002 & year<2008)
    yearshare_df_3 <- filter(yearshare_df,year>=2008)
    
    ggplot(yearshare_df_1,aes(x = ring,y = share,group = factor(year)))+geom_point(aes(colour = factor(year)))+ scale_x_discrete(limits=c("2","4","6","8","10","12","14","16","18","20"))+geom_errorbar( aes(ymin = share-se, ymax = share+se,colour = factor(year)),width = 0.2)
    filename = paste(paste("figures/violence_yearseries_1996-2001, width = ",toString(30*width_count),sep = ""),".png",sep = "")
    ggsave(filename,width=11,height=8)
    ggplot(yearshare_df_2,aes(x = ring,y = share,group = factor(year)))+geom_point(aes(colour = factor(year)))+ scale_x_discrete(limits=c("2","4","6","8","10","12","14","16","18","20"))+geom_errorbar( aes(ymin = share-se, ymax = share+se,colour = factor(year)),width = 0.2)
    filename = paste(paste("figures/violence_yearseries_2001-2007, width = ",toString(30*width_count),sep = ""),".png",sep = "")
    ggsave(filename,width=11,height=8)
    ggplot(yearshare_df_3,aes(x = ring,y = share,group = factor(year)))+geom_point(aes(colour = factor(year)))+ scale_x_discrete(limits=c("2","4","6","8","10","12","14","16","18","20"))+geom_errorbar( aes(ymin = share-se, ymax = share+se,colour = factor(year)),width = 0.2)
    filename = paste(paste("figures/violence_yearseries_2008-2012, width = ",toString(30*width_count),sep = ""),".png",sep = "")
    ggsave(filename,width=11,height=8)
    
    yearshare_heatmap = na.omit(yearshare_df)
    short_cnames <- c("'96","'97","'98","'99","'00","'01","'02","'03","'04","'05","'06","'07","'08","'09","'10","'11","'12")
    ggplot(data=yearshare_heatmap,mapping=aes(x=year,y=ring,fill=share))+
      geom_tile()+theme_minimal()+scale_fill_gradient(name="Violence Share",low="darkblue",high="red")+
      scale_x_discrete(breaks = c(1996:2012),label = short_cnames)
    filename = paste(paste("figures/Violence_Share_Heatmap, width = ",toString(30*width_count),sep = ""),".png",sep = "")
    ggsave(filename,width=11,height=8)
    #ggplot(yearshare_heatmap,aes(x = year,y = ring,fill = share)) + geom_bin2d(binwidth = c(20,20))+scale_fill_gradient(low = "royalblue",high = "red")
}
  
  for(rng in 1:21){
    m_30 <- as.matrix(yearshare_30[rng,])
    m_60 <- as.matrix(yearshare_60[rng,])
    m_90 <- as.matrix(yearshare_90[rng,])
    m_120 <- as.matrix(yearshare_120[rng,])
    
    col_30 <- t(m_30)
    col_60 <- t(m_60)
    col_90 <- t(m_90)
    col_120 <- t(m_120)
    
    frame_30 <- data.frame(col_30,sapply(c(1:17),add1995))
    frame_60 <- data.frame(col_60,sapply(c(1:17),add1995))
    frame_90 <- data.frame(col_90,sapply(c(1:17),add1995))
    frame_120 <- data.frame(col_120,sapply(c(1:17),add1995))
    
    colnames(frame_30) <- c("width = 30","year")
    colnames(frame_60) <- c("width = 60","year")
    colnames(frame_90) <- c("width = 90","year")
    colnames(frame_120) <- c("width = 120","year")
    
    plotting <- merge(frame_30,frame_60,by = "year")
    plotting <- merge(plotting,frame_90,by = "year")
    plotting <- merge(plotting,frame_120,by = "year")
    
    plotting %>% pivot_longer(cols = -year) %>%
    ggplot(aes(x = year, y = value, col = name)) + geom_line()    
    #geom_line(y =plotting$share_120.x,color = "violet")
    filename = paste(paste("figures/violence_timeseries_ring",rng,sep=""),".png",sep="")
    ggsave(filename,width=11,height=8)
  }
  
  for(year in 1996:2012){
    m_1996 <- as.matrix(yearshare_2012$year)
    
    col_30 <- t(m_30)
    col_60 <- t(m_60)
    col_90 <- t(m_90)
    col_120 <- t(m_120)
    
    frame_30 <- data.frame(col_30,sapply(c(1:17),add1995))
    frame_60 <- data.frame(col_60,sapply(c(1:17),add1995))
    frame_90 <- data.frame(col_90,sapply(c(1:17),add1995))
    frame_120 <- data.frame(col_120,sapply(c(1:17),add1995))
  }
  
  for (y in seq(1996,2012)) {
    
    muni_map_yr = muni_map %>%  mutate(first_violence_yr = ifelse(first_violence==y,"Yes","No/NA"))
    
    filenome = paste(paste("../figures/share_violence_map_",y,sep=""),".png",sep="")
    title = paste(paste("Share of violence in violence in ",y,sep="")," by Columbian municipality",sep="")
    
    ggplot() +
      #geom_sf(data = muni_map, fill='white', color = 'grey34',lwd=.05) +
      geom_sf(data = Vtot_final, aes(fill=first_violence_yr),color = 'grey34',lwd=.05) +
      ggtitle(title) + 
      scale_fill_manual(values=c("white","royalblue1"),aesthetics = c("fill"),name= paste("Share by Year of Violence in ", y, sep="")) +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    ggsave(filenome,width=11,height=8)
    
  }
  
 
  