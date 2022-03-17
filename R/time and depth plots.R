library(here)
library(tidyverse)
library(RColorBrewer)


## file names
shrimp<-"shrimp_data_for_analysis_time_depth.csv"

## raw file locations
shrimp <- here("data", "clean", shrimp)

## pull in files from sub directory
shrimp <- read_csv(shrimp,
                  na = c("", "NA"))



shrimp$shift <- factor(shrimp$shift, 
                         levels=c("afternoon","evening",
                                  "night", "morning",
                                  "dawn"))



ggplot(data = shrimp,aes(x=depth_m,y=cpue,fill=shift))+
  geom_col(position = "stack")+
  theme_classic()+
  scale_x_continuous(expand = c(0,0),name = "depth(m)",breaks = c(10,25,50,70))+
  scale_y_continuous(expand = c(0,0),name = "CPUE")+
  scale_fill_brewer(palette = "Set1")
  
