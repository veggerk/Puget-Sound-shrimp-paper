#packages needed
library(here)
library(tidyverse)
library(RColorBrewer)
library(patchwork)


## file names
shrimp<-"shrimp_data_for_analysis_time_depth.csv"

## raw file locations
shrimp <- here("data", "clean", shrimp)

## pull in files from sub directory
shrimp <- read_csv(shrimp,
                  na = c("", "NA"))


# set the order of the timing of trawls
shrimp$shift <- factor(shrimp$shift, 
                         levels=c("afternoon","evening",
                                  "night", "dawn",
                                  "morning"))

# subset by species
crangon<-subset(shrimp,subset = latin_name=="Crangon alaskensis")
pink_shrimp<-subset(shrimp,subset = latin_name=="Pandalus eous / jordani")
spot_shrimp<-subset(shrimp,subset = latin_name=="Pandalus platyceros")

# make plot to show diel patterns (diel vertical migration)
p1<-ggplot(data = crangon,aes(x=depth_m,y=cpue,fill=shift, color=shift))+
  geom_col(position = "stack")+
  theme_classic()+
  labs(title = "Northern Crangon shrimp")+
  scale_x_continuous(expand = c(0,0),name = "",breaks = c(10,25,50,70))+
  scale_y_continuous(expand = c(0,0),name = "")+
  scale_fill_brewer(palette = "Set1")+
  scale_color_brewer(palette = "Set1")+
  theme(legend.position="none")

p2<-ggplot(data = spot_shrimp,aes(x=depth_m,y=cpue,fill=shift, color=shift))+
  geom_col(position = "stack")+
  theme_classic()+
  labs(title = " spot shrimp")+
  scale_x_continuous(expand = c(0,0),name = "",
                     breaks = c(10,25,50,70))+
  scale_y_continuous(expand = c(0,0),name = "shrimp CPUE",
                     limits = c(0,400))+
  scale_fill_brewer(palette = "Set1")+
  scale_color_brewer(palette = "Set1")+
  guides(fill=guide_legend(title="sampling time"))+
  guides(color = FALSE)

p3<-ggplot(data = pink_shrimp,aes(x=depth_m,y=cpue,fill=shift, color=shift))+
  geom_col(position = "stack")+
  theme_classic()+
  labs(title = " pink shrimp")+
  scale_x_continuous(expand = c(0,0),name = "depth (m)",
                     breaks = c(10,25,50,70))+
  scale_y_continuous(expand = c(0,0),
                     name = "",
                     limits = c(0,1000))+
  scale_fill_brewer(palette = "Set1")+
  scale_color_brewer(palette = "Set1")+
  theme(legend.position="none")


diel_patterns<-p1/p2/p3

ggsave(plot = diel_patterns,filename = "figures/diel patterns.pdf")


# make plot to show abundance over time (temporal trends)
p4<-ggplot(data = crangon,aes(x=year,y=cpue))+
  geom_col(color="#969696",fill="#969696")+
  theme_classic()+
  labs(title = "Northern Crangon shrimp")+
  scale_x_continuous(expand = c(0,0),
                     name = "")+
  scale_y_continuous(expand = c(0,0),
                     name = "",
                     limits = c(0,250),
                     breaks = c(0,100,200))

p5<-ggplot(data = spot_shrimp,aes(x=year,y=cpue))+
  geom_col(color="#969696",fill="#969696")+
  theme_classic()+
  labs(title = " spot shrimp")+
  scale_x_continuous(expand = c(0,0),
                     name = "")+
  scale_y_continuous(expand = c(0,0),
                     name = "CPUE",
                     limits = c(0,250),
                     breaks = c(0,100,200))


p6<-ggplot(data = pink_shrimp,aes(x=year,y=cpue))+
  geom_col(color="#969696",fill="#969696")+
  theme_classic()+
  labs(title = " pink shrimp")+
  scale_x_continuous(expand = c(0,0),
                     name = "year")+
  scale_y_continuous(expand = c(0,0),
                     name = "",
                     limits = c(0,250),
                     breaks = c(0,100,200))

temporal_patterns<-p4/p5/p6

ggsave(plot = temporal_patterns,filename = "figures/temporal patterns.pdf")


