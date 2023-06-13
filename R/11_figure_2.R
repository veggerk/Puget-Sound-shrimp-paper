#packages needed
library(here)
library(tidyverse)
library(patchwork)


## file names
shrimp<-"shrimp_data_for_analysis.csv"

## raw file locations
shrimp <- here("data", "clean", shrimp)

## pull in files from sub directory
shrimp <- read_csv(shrimp,
                   na = c("", "NA"))


# subset by species
crangon<-subset(shrimp,subset = latin_name=="Crangon alaskensis")
pink_shrimp<-subset(shrimp,subset = latin_name=="Pandalus eous / jordani")
spot_shrimp<-subset(shrimp,subset = latin_name=="Pandalus platyceros")

# add in a 0 for 1999, since there were no spot shrimp caught that year. 
# This is necessary for ggplot to plot correctly, otherwise 1999 would be cut 
row1<-c("Pandalus platyceros",1999,0)
spot_shrimp<-rbind(row1,spot_shrimp)
spot_shrimp$year<-as.numeric(spot_shrimp$year)
spot_shrimp$cpue<-as.numeric(spot_shrimp$cpue)

# make plot to show abundance over time (temporal trends)
p4<-ggplot(data = crangon,aes(x=year,y=cpue))+
  geom_col(color="#969696",fill="#969696")+
  theme_classic()+
  labs(title = "Northern Crangon shrimp")+
  scale_x_continuous(expand = c(0,0),
                     name = "")+
  scale_y_continuous(expand = c(0,0),
                     name = "",
                     limits = c(0,110),
                     breaks = c(0,50,100))

p5<-ggplot(data = spot_shrimp,aes(x=year,y=cpue))+
  geom_col(color="#969696",fill="#969696")+
  theme_classic()+
  labs(title = " Spot shrimp")+
  scale_x_continuous(expand = c(0,0),
                     name = "")+
  scale_y_continuous(expand = c(0,0),
                     name = "CPUE (# shrimp/trawl distance)",
                     limits = c(0,260),
                     breaks = c(0,100,200))


p6<-ggplot(data = pink_shrimp,aes(x=year,y=cpue))+
  geom_col(color="#969696",fill="#969696")+
  theme_classic()+
  labs(title = " Pink shrimp")+
  scale_x_continuous(expand = c(0,0),
                     name = "year")+
  scale_y_continuous(expand = c(0,0),
                     name = "",
                     limits = c(0,220),
                     breaks = c(0,100,200))

temporal_patterns<-p4/p5/p6

ggsave(plot = temporal_patterns,filename = "figures/fig_02.pdf")






# used this code to make a diel vertical migration plot.
# Ultimately decided not to include in paper.

#library(RColorBrewer)

# # set the order of the timing of trawls
# shrimp$shift <- factor(shrimp$shift, 
#                        levels=c("afternoon","evening",
#                                 "night", "dawn",
#                                 "morning"))


# make plot to show diel patterns (diel vertical migration)
# p1<-ggplot(data = crangon,aes(x=depth_m,y=cpue,fill=shift, color=shift))+
#   geom_col(position = "stack")+
#   theme_classic()+
#   labs(title = "Northern Crangon shrimp")+
#   scale_x_continuous(expand = c(0,0),name = "",breaks = c(10,25,50,70))+
#   scale_y_continuous(expand = c(0,0),name = "")+
#   scale_fill_brewer(palette = "Set1")+
#   scale_color_brewer(palette = "Set1")+
#   theme(legend.position="none")
# 
# p2<-ggplot(data = spot_shrimp,aes(x=depth_m,y=cpue,fill=shift, color=shift))+
#   geom_col(position = "stack")+
#   theme_classic()+
#   labs(title = " spot shrimp")+
#   scale_x_continuous(expand = c(0,0),name = "",
#                      breaks = c(10,25,50,70))+
#   scale_y_continuous(expand = c(0,0),name = "shrimp CPUE",
#                      limits = c(0,400))+
#   scale_fill_brewer(palette = "Set1")+
#   scale_color_brewer(palette = "Set1")+
#   guides(fill=guide_legend(title="sampling time"))+
#   guides(color = FALSE)
# 
# p3<-ggplot(data = pink_shrimp,aes(x=depth_m,y=cpue,fill=shift, color=shift))+
#   geom_col(position = "stack")+
#   theme_classic()+
#   labs(title = " pink shrimp")+
#   scale_x_continuous(expand = c(0,0),name = "depth (m)",
#                      breaks = c(10,25,50,70))+
#   scale_y_continuous(expand = c(0,0),
#                      name = "",
#                      limits = c(0,1000))+
#   scale_fill_brewer(palette = "Set1")+
#   scale_color_brewer(palette = "Set1")+
#   theme(legend.position="none")
# 
# 
# diel_patterns<-p1/p2/p3
# 
# ggsave(plot = diel_patterns,filename = "figures/diel patterns.pdf")


