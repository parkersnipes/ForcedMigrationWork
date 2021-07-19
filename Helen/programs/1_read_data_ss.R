#Author: Helen Burkhardt
#Date: 5/12/2020
#Purpose: read in data and create summary statistics/figures

#load packages
library(tidyverse)
library(ggplot2)
library(GGally)
library(gridExtra)
library(WVPlots)
library(knitr)
library(gdata)

#import data
muni = read.csv(file= "../raw_data/DANE municipality codes and regions .csv") %>% 
  rename(municipality = Muni_code)

#import data
data = read.csv(file = "../raw_data/Book1.csv")

#append province and region to data
data = left_join(data,muni,by=c("municipality"))

#Make province id
data$province_code = data %>% group_indices(number_dept,province)

#save dataset
write.csv(data,file="../output/COL_muni_flows.csv",row.names=FALSE)

#check cumulative sums
data_check = data %>% 
  group_by(municipality) %>%
  mutate(cum_victims_UR_2 = cumsum(victims__UR))

data_check$match = data_check$cum_victims_UR_2 == data_check$cum_victims_UR

sum(data_check$match) == 17867

for (var in c("displaced_AS","displaced_CODHES","displaced_RUV","displaced_CEDE","displaced_JYP")) {
  
data_check = data %>% 
    group_by(municipality) %>%
   mutate(var_2 = do.call(cumsum,list(as.name(var))))

data_check = data_check %>% 
  mutate(match = do.call("==",list(as.name(paste("cum_",var,sep="")),var_2)))

test = sum(data_check$match) == 17867

print(paste(var,test,sep = " Correct cumulative sums? "))

}

#Correlation between 5 cumulative displacement measures
data_lastyear = data %>% filter(year == 2012) %>% 
  select(c("cum_displaced_AS","cum_displaced_CODHES","cum_displaced_RUV","cum_displaced_CEDE","cum_displaced_JYP"))

corr_matrix = cor(data_lastyear,method=c("pearson")) %>% 
  round(3)

corr_matrix

#Correlation between 5 displacement flows by year
for (y in seq(1996,2012,1)) {
  
  corr_displ_flow = data %>% filter(year == y) %>% 
    select(c("displaced_AS","displaced_CODHES","displaced_RUV","displaced_CEDE","displaced_JYP"))
  
  corr_displ_flow = cor(corr_displ_flow,method=c("pearson"),use="na.or.complete") %>% 
    round(3)
  
  mv(from="corr_displ_flow",to=paste("corr_displ_flow",y,sep=""))
  
  
}

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
outliers = anti_join(data,data_out,by=c("year","municipality"))
write.csv(outliers,"../figures/outliers.csv",row.names=F)

#Pairwise scatterplots of dispacement flows
for (y in seq(1996,2012,1)) {
  
data_displ = data_out %>% filter(year==y) %>% 
  select(c("displaced_AS","displaced_CODHES","displaced_RUV","displaced_CEDE","displaced_JYP"))

graph_title = paste("Pairwise Displacement Flows for ",y,sep="")

file_name = paste(paste("../figures/pairwise_plots_year/",y,sep=""),".png",sep="")

#Source: https://www.r-bloggers.com/scatterplot-matrices-pair-plots-with-cdata-and-ggplot2/
PairPlot(data_displ,colnames(data_displ)[1:5],graph_title,alpha=.3,point_color="black")

ggsave(file_name,width=11, height =8)

}

#Identify instances where there is an increase in displacement when there is an increase in violence (lag = 2)
for (var in c("displaced_AS","displaced_CODHES","displaced_RUV","displaced_CEDE","displaced_JYP")) {
  
  if (var != "displaced_AS") {
    data_instance = data %>% select(-c("displaced_AS")) %>% 
      rename(displaced_AS = var)  
  }
  else {
    data_instance = data
  }
data_instance = data_instance %>% group_by(municipality) %>%
                          arrange(municipality,year) %>% 
                          mutate(n_disp_0lead = displaced_AS,
                                 n_disp_1lead = lead(displaced_AS,1),
                                 n_disp_2lead = lead(displaced_AS,2),
                                 n_viol_0lead = victims__UR,
                                 n_viol_1lead = lead(victims__UR,1),
                                 n_viol_2lead = lead(victims__UR,2)) %>% 
                          mutate(instance = ifelse(n_viol_0lead > 0 & n_viol_1lead == 0 & n_viol_2lead == 0 &
                                                    n_disp_0lead == 0 & n_disp_1lead == 0 & n_disp_2lead > 0,
                                                   1,0))

data_instance = data_instance %>% filter(instance == 1)

file_name = paste("../figures/",paste(var,"_instances.csv",sep=""),sep="")

write.csv(data_instance,file_name,row.names=FALSE)

}

#Distribution between number of years between an increase in violence and an increase in displacement
for (var in c("displaced_AS","displaced_CODHES","displaced_RUV","displaced_CEDE","displaced_JYP")) {

if (var != "displaced_AS") {
data_dist = data %>% select(-c("displaced_AS")) %>% 
  rename(displaced_AS = var)  
}
else {
  data_dist = data
}

data_dist = data_dist %>% mutate(v_incr = if_else(victims__UR>0,1,0)) %>%   #flag for if there was increase in violence 
                     group_by(municipality) %>% 
                     mutate(n_disp_0lead = displaced_AS,
                            n_disp_1lead = lead(displaced_AS,1),         #number of people displaced in future period
                            n_disp_2lead = lead(displaced_AS,2),
                            n_disp_3lead = lead(displaced_AS,3),
                            n_disp_4lead = lead(displaced_AS,4),
                            n_disp_5lead = lead(displaced_AS,5)) %>% 
                     mutate(disp_0lead = ifelse(n_disp_0lead != 0,1,0),  #indicator for if first positive displacement occured in that period
                            disp_1lead = ifelse(disp_0lead == 0 & n_disp_1lead > 0, 1,0),
                            disp_2lead = ifelse(disp_0lead == 0 & disp_1lead == 0 & n_disp_2lead > 0, 1,0),
                            disp_3lead = ifelse(disp_0lead == 0 & disp_1lead == 0 & disp_2lead == 0 & 
                                                  n_disp_2lead > 0, 1, 0),
                            disp_4lead = ifelse(disp_0lead == 0 & disp_1lead == 0 & disp_2lead == 0 & disp_3lead == 0 &
                                                  n_disp_4lead > 0, 1, 0),
                            disp_5lead = ifelse(disp_0lead == 0 & disp_1lead == 0 & disp_2lead == 0 & disp_3lead == 0 &
                                                  disp_4lead == 0 & n_disp_5lead > 0, 1, 0)) %>% 
                    filter(v_incr == 1) %>% # only look at observations for which there was an increase in violence
                    ungroup() %>%                 
                    summarise(period0 = sum(disp_0lead,na.rm=TRUE),
                              period1= sum(disp_1lead,na.rm=TRUE),
                              period2 = sum(disp_2lead,na.rm=TRUE),
                              period3 = sum(disp_3lead,na.rm=TRUE),
                              period4 = sum(disp_4lead,na.rm=TRUE),
                              period5 = sum(disp_5lead,na.rm=TRUE),
                              N=n())

data_dist = data_dist %>% pivot_longer(c(period0,period1,period2,period3,period4,period5),
                                       names_to="period",values_to="number")

data_dist$period = str_replace(data_dist$period,"period","")
data_dist$period = as.numeric(data_dist$period)

graph_title = paste(paste(paste("Distribution of displacement flow (",var,sep=""),
                          ") ",sep=""),
                    "increases after violence occured",sep="")
file_name = paste("../figures/dist_displace/",paste(var,".png",sep=""),sep="")

ggplot(data=data_dist,aes(x=period,y=number)) +
  geom_bar(stat="identity", width = .75) +
  geom_text(aes(label=number), vjust=-0.3, size=3.5) +
  labs(title=graph_title,
       x="Number of years after violence first occured",
       y="Number with displacement increases") +
  scale_x_discrete(limits=c(0,1,2,3,4,5))

ggsave(file_name,width=11, height=8)

}

#cumulative violence versus cumalitve displacement per capita graph, by municipality
for (muni in unique(data$municipality)) {
print(muni)
data_cum = data %>% filter(municipality == muni) %>% 
                    group_by(municipality,cum_victims_UR) %>% 
                    mutate(max_year = max(year)) %>% 
                    mutate(event = ifelse(year == max_year,1,0),
                            cum_AS_percap = cum_displaced_AS / Population_1993,
                            cum_CODHES_percap = cum_displaced_CODHES / Population_1993,
                            cum_CEDE_percap = cum_displaced_CEDE / Population_1993,
                            cum_RUV_percap = cum_displaced_RUV / Population_1993,
                            cum_JYP_percap = cum_displaced_JYP / Population_1993) %>% 
                    filter(event == 1) %>% 
                    ungroup() %>% 
                    select(c(cum_AS_percap,cum_CODHES_percap,cum_CEDE_percap,cum_RUV_percap,cum_JYP_percap,cum_victims_UR)) %>% 
                    pivot_longer(c(cum_AS_percap,cum_CODHES_percap,cum_CEDE_percap,cum_RUV_percap,cum_JYP_percap),
                            names_to="type_disp",values_to="displacement_percap")                          
if (length(data_cum$displacement_percap)==5) {
  ggplot(data = data_cum, aes(x=cum_victims_UR,y=displacement_percap,col=type_disp)) +
    geom_point(size=1.5) +
    labs(title=paste("Municipality: ", muni,sep=""),
         x="Cumulative Violence (UR)",
         y="Cumulative Displacement per capita") +
    scale_colour_discrete(name="Agency",
                          labels=c("AS","CEDE","CODHES","JYP","RUV")) +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 12),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          plot.title = element_text(size = 14, hjust = .5))
  }
else {
ggplot(data = data_cum, aes(x=cum_victims_UR,y=displacement_percap,col=type_disp)) +
  geom_line(size=1) +
  labs(title=paste("Municipality: ", muni,sep=""),
       x="Cumulative Violence (UR)",
       y="Cumulative Displacement per capita") +
  scale_colour_discrete(name="Agency",
                    labels=c("AS","CEDE","CODHES","JYP","RUV")) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        plot.title = element_text(size = 14, hjust = .5))
}

ggsave(paste(paste("../figures/cum_viol_disp_bymuni/",muni,sep=""),".png",sep=""),width=6,height=4)

}

#how many instances there are in which one of the displacement measures records a zero,
#   while the other four record positive numbers.
data_how = data %>% mutate(not_insync_AS = ifelse(displaced_AS == 0 & displaced_CODHES > 0 &
                                                    displaced_CEDE > 0 & displaced_RUV > 0 &
                                                    displaced_JYP > 0, 1, 0),
                           not_insync_CODHES = ifelse(displaced_CODHES==0 & displaced_AS > 0 &
                                                       displaced_CEDE > 0 & displaced_RUV > 0 &
                                                        displaced_JYP > 0, 1, 0),
                           not_insync_CEDE = ifelse(displaced_CEDE == 0 & displaced_AS > 0 &
                                                      displaced_RUV > 0 & displaced_CODHES >0 &
                                                      displaced_JYP > 0, 1, 0),
                           not_insync_RUV = ifelse(displaced_RUV == 0 & displaced_AS > 0 & 
                                                     displaced_CEDE > 0 & displaced_CODHES > 0 &
                                                     displaced_JYP > 0, 1, 0),
                           not_insync_JYP = ifelse(displaced_JYP == 0  & displaced_AS> 0 &
                                                     displaced_CEDE > 0 & displaced_CODHES > 0 &
                                                     displaced_RUV > 0, 1, 0))
data_how = data_how %>% filter(not_insync_AS == 1 | not_insync_CODHES == 1 | not_insync_CEDE == 1
                               | not_insync_RUV == 1 | not_insync_JYP == 1)
print(length(data_how$Population_1993))
#   how many of these are found in the years where one of the measures has zeroes for all municipalities?
#check that these years are 1996, 2010, 2011, and 2012
data_how2 = data %>% mutate(CEDE_zero = ifelse(displaced_CEDE == 0, 1, 0)) %>%
                     group_by(year) %>% 
                     summarise(sum_CEDE_zero = sum(CEDE_zero)) %>% 
                     filter(sum_CEDE_zero == 1051)
data_how2 = data_how %>% filter(year == 1996 | (year>2009 & year<2013))
print(length(data_how2$Population_1993))

#how many instances there are in which two of the displacement measures records a zero, 
#   while the other three record positive numbers.
data_how = data %>% mutate(equal_zero_AS = ifelse(displaced_AS == 0,1,0),
                           equal_zero_CODHES = ifelse(displaced_CODHES == 0,1,0),
                           equal_zero_CEDE = ifelse(displaced_CEDE==0,1,0),
                           equal_zero_RUV = ifelse(displaced_RUV==0,1,0),
                           equal_zero_JYP = ifelse(displaced_AS==0,1,0),
                           pos_AS = ifelse(displaced_AS > 0,1,0),
                           pos_CODHES = ifelse(displaced_CODHES > 0,1,0),
                           pos_CEDE = ifelse(displaced_CEDE > 0,1,0),
                           pos_RUV = ifelse(displaced_RUV > 0,1,0),
                           pos_JYP = ifelse(displaced_AS > 0,1,0))

data_how = data_how %>% 
  mutate(two_zeros = ifelse(equal_zero_AS + equal_zero_CEDE + equal_zero_CODHES +
                              equal_zero_RUV + equal_zero_JYP == 2,1,0),
         three_pos = ifelse(pos_AS + pos_CODHES + pos_CEDE + pos_RUV + pos_JYP == 3,1,0)) %>% 
  mutate(condition_met = ifelse(two_zeros == 1 & three_pos == 1, 1,0)) %>% 
  filter(condition_met == 1)

print(length(data_how$Population_1993))

#Flows of displacement by municipality
for (muni in unique(data$municipality)) {
  print(muni)
data_flow = data %>% filter(municipality==muni) %>% 
  select(c(year,displaced_AS,displaced_CODHES,displaced_RUV,displaced_CEDE,displaced_JYP,victims__UR)) %>% 
  pivot_longer(c(displaced_AS,displaced_CODHES,displaced_RUV,displaced_CEDE,displaced_JYP),
               names_to="type_disp",values_to="displacement_flow") 

#identify if had a non-negative violence flow at some point
if (sum(data_flow$victims__UR)==0) {
  folder = "v_t all zero/"
} else {
  folder = "v_t pos/"
}

ggplot(data = data_flow, aes(x=year,y=displacement_flow,col=type_disp)) +
  geom_line(size=1) +
  labs(title=paste("Municipality: ", muni,sep=""),
       x="Year",
       y="Displacement Flow") +
  scale_colour_discrete(name="Agency",
                        labels=c("AS","CEDE","CODHES","JYP","RUV")) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        plot.title = element_text(size = 14, hjust = .5))

ggsave(paste(paste(paste("../figures/disp_flow_bymuni/",folder,sep=""),muni,sep=""),".png",sep=""),
       width=6,height=4)
}

#how many instances there are where v_1>0  and d_t =0 for some of the measures (one or several)?
data_how = data %>% filter(victims__UR > 0 & (displaced_AS == 0 |
                                              displaced_CODHES == 0 |
                                              displaced_CEDE == 0 |
                                              displaced_RUV == 0 |
                                              displaced_JYP == 0))
print(length(data_how$Population_1993))

#RANDOM CODE FRAGMENTS
# ggpairs(data=data_displ,columns=1:3,
#         lower=list(continuous="points"),diag="blankDiag",upper = "blank",
#         axisLabels = "show")
