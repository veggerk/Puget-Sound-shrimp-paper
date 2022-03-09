library(rgdal)  #may need to reinstall
library(raster) #may need to reinstall
library(marmap)
library(tidyverse)
library(patchwork)
library(png)



#get the bathymetry from NOAA. Resolution in minutes, default = 4, 
#minimum = 1. Smaller = higher resolution and longer times
#keep=TRUE writes downloaded data into a file

bathy <- getNOAA.bathy(lon1=-126, lon2=-122, lat1=46, lat2=48.5,
                        resolution=1, keep=FALSE) 


blues <- c("#08519c",
            "#2171b5",
            "#6baed6",
            "#f7fbff")


p1<-autoplot(bathy,geom=c('raster'),
                 show.legend=FALSE) + #turn off legend
geom_point(aes(x=-122.3, y=47.62), colour="black", size=1)+
  annotate("segment", x = -122.2, xend = -122.3, y = 47.07, yend = 47.62,
           colour = "black")+
  annotate("text", x = -122.35, y = 47, label = "Seattle", color="black", size=5)+
  annotate("segment", x = -125.45, xend = -124.97, y = 47.9, yend = 47.97,
           colour = "red")+
  annotate("segment", x = -124.43, xend = -124.91, y = 46.75, yend = 46.68,
           colour = "red")+
  annotate("segment", x = -124.43, xend = -124.97, y = 46.75, yend = 47.97,
           colour = "red")+
  annotate("segment", x = -124.91, xend = -125.45, y = 46.68, yend = 47.9,
           colour = "red")+
  geom_point(aes(x=-122.5, y=47.74), colour="red", size=2)+
  scale_fill_gradient2(midpoint = -1,
                     high = "#d9d9d9",
                     low = blues,
                     limits=c(-9000,-1),
                     na.value = "#d9d9d9")+ #make the land area a custom color
  theme(axis.title = element_blank()) +  #remove the axis titles
  scale_x_continuous(breaks=seq(-126,-122, 2),
                     limits = c(-126,-122),#where to place the values
                     labels=paste0(seq(126,122, -2),'W'),
                     expand = c(0, 0)) +
  scale_y_continuous(breaks=seq(46,48.5,1),
                     limits = c(46,48.5),#where to place the values
                     labels=paste0(seq(46,48.5,1),'N'),
                     expand = c(0, 0))


ggsave(plot = p1, filename = "Puget_sound_map.pdf")




