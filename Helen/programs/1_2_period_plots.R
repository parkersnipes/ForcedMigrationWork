#Author: Helen Burkhardt
#Date: 5/5/2021
#Purpose: read in data and create pairwise plots by period

#load packages
library(tidyverse)
library(ggplot2)
library(GGally)
library(gridExtra)
library(WVPlots)
library(knitr)
library(gdata)

#import data
muni = read.csv(file= "raw_data/DANE municipality codes and regions .csv") %>% 
  rename(municipality = Muni_code)

#import data
data = read.csv(file = "raw_data/Book1.csv")

#append province and region to data
data = left_join(data,muni,by=c("municipality"))

#Make province id
data$province_code = data %>% group_indices(number_dept,province)

#remove outliers
#I define 3 standard deviations from the mean as the outlier
data_out = data %>% filter(displaced_AS > mean(data$displaced_AS) - sd(data$displaced_AS)
                           & displaced_AS < mean(data$displaced_AS) + sd(data$displaced_AS)) %>% 
  filter(displaced_CODHES > mean(data$displaced_CODHES) - sd(data$displaced_CODHES)
         & displaced_CODHES < mean(data$displaced_CODHES) + sd(data$displaced_CODHES)) %>% 
  filter(displaced_RUV > mean(data$displaced_RUV) - sd(data$displaced_RUV)
         & displaced_RUV < mean(data$displaced_RUV) + sd(data$displaced_RUV)) %>%  
  filter(displaced_CEDE > mean(data$displaced_CEDE) - sd(data$displaced_CEDE)
         & displaced_CEDE < mean(data$displaced_CEDE) + sd(data$displaced_CEDE)) %>%   
  filter(displaced_JYP > mean(data$displaced_JYP) - sd(data$displaced_JYP)
         & displaced_JYP < mean(data$displaced_JYP) + sd(data$displaced_JYP)) 

#Pairwise scatterplots of dispacement flows (one period)
for (period in c(1,2,4,8)) {
for (n in seq(1,period)) {
if (n==1) {
lower_year = 1996
}

if (period!=1 & n!=1) {
lower_year = higher_year + 1 
}

if (period!=1 & n!=period) {
higher_year = lower_year + (16/period) - 1
}

if (n==period) {
higher_year = 2012
}

  #aggregate displacement numbers during that period by municiplality
  data_filtered = data_out %>% filter(year>=lower_year & year<=higher_year)
  data_filtered = aggregate(cbind(data_filtered$displaced_AS,
                                  data_filtered$displaced_CODHES,
                                  data_filtered$displaced_RUV,
                                  data_filtered$displaced_CEDE,
                                  data_filtered$displaced_JYP),
                                  by=list(municipality=data_filtered$municipality),FUN=sum)
  data_filtered = data_filtered %>% 
                      rename(displaced_AS=V1,
                             displaced_CODHES=V2,
                             displaced_RUV=V3,
                             displaced_CEDE=V4,
                             displaced_JYP=V5) %>% 
    select(c("displaced_AS","displaced_CODHES","displaced_RUV","displaced_CEDE","displaced_JYP"))

  year_range = paste(lower_year,higher_year,sep="-")
  
  graph_title = paste("Pairwise Displacement Flows for ",year_range,sep="")

  file_name = paste(paste(paste("figures/pairwise_plots_period/",period,sep=""),
                                year_range,sep="_"),
                                  ".png",sep="")

  #Source: https://www.r-bloggers.com/scatterplot-matrices-pair-plots-with-cdata-and-ggplot2/
  PairPlot(data_filtered,colnames(data_filtered)[1:5],graph_title,alpha=.3,point_color="black")

  ggsave(file_name,width=11, height =8)

}
}

