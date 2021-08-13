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
library(shiny)
library(shinythemes)
library(shiny)

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

roads = read.csv("Data/real_roads.csv")
road_code_list <- roads$ADM2_PCODE
AttributeTableFinal <- mutate(AttributeTableFinal, has_road = (ADM2_PCODE %in% road_code_list))
weight_matrix <- matrix(,nrow = 1120,ncol = 1120)


server <- function(input, output, session)({
  
  width <-reactive(input$width)
  k <- reactive({input$k})
  
  
  output$thresholdSlider <- renderUI({
    sliderInput("width", 'Width Slider', 
                min = 30, max = 120, value = 60, step= 30)
    sliderInput("k", 'k Slider', 
                min = 0, max = 1, value = 0.5, step= 0.1)
    
    output$selected_width <- renderText(paste("k: ",input$k))
    output$selected_k <- renderText(paste("Width: ",input$width))
  })

    
  output$Plot <- renderPlot(ggplot(yearshare_df_1,aes(x = year,y = share,group = factor(ring)))+geom_point(aes(colour = factor(ring)))+ scale_x_discrete(limits=c("1996","1998","2000","2002","2004","2006","2008","2010","2012"))+geom_errorbar( aes(ymin = share-se, ymax = share+se,colour = factor(ring)),width = 0.2)+facet_wrap(~ ring))
  })  
  

shinyApp(ui = ui, server = server)