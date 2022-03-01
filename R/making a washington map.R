library(rgdal)  #may need to reinstall
library(raster) #may need to reinstall
library(marmap)
library(tidyverse)

#get the bathymetry from NOAA. Resolution in minutes, default = 4, 
#minimum = 1. Smaller = higher resolution and longer times
#keep=TRUE writes downloaded data into a file

bathy2 <- getNOAA.bathy(lon1=-126, lon2=-122, lat1=46, lat2=48.5,
                        resolution=1, keep=FALSE) 

blues <- c("lightsteelblue4",
           "lightsteelblue3",
           "lightsteelblue2",
           "lightsteelblue1")

blues2 <- c("#08519c",
            "#2171b5",
            "#6baed6",
            "#f7fbff")

greys <- c(grey(0.6),
           grey(0.93),
           grey(0.99))

autoplot(bathy2,geom=c('raster'),
                 show.legend=FALSE) +     #turn off legend
scale_fill_gradient2(midpoint = -1,high = "#bdbdbd",low = blues2,limits=c(-9000,-1))+
  theme(axis.title = element_blank()) +  #remove the axis titles
  scale_x_continuous(breaks=seq(-126,-122, 2),
                     limits = c(-126,-122),#where to place the values
                     labels=paste0(seq(126,122, -2),'W'),
                     expand = c(0, 0)) +
  scale_y_continuous(breaks=seq(46,48.5,1),
                     limits = c(46,48.5),#where to place the values
                     labels=paste0(seq(46,48.5,1),'N'),
                     expand = c(0, 0))+
  geom_point(aes(x=-122.5, y=47.74), colour="red", size=2)



ggsave(filename = "newplot.pdf")


