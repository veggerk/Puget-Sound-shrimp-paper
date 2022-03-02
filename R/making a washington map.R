library(rgdal)  #may need to reinstall
library(raster) #may need to reinstall
library(marmap)
library(tidyverse)
library(patchwork)
library(png)



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
#other pretty r color brewer grey options to use for land.

# lightest
#ffffff

#f0f0f0
#d9d9d9

# darkest
#bdbdbd


p1<-autoplot(bathy2,geom=c('raster'),
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
                     low = blues2,
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


  
### Figure 1E (top right) from PLOS1 example, trying to get rid of on land details,
# and change contour lines

# use base R graphing for this
# colorRampPalette(c("#08519c","#3182bd","#6baed6","#bdd7e7","#eff3ff","white")) -> blues 
# 
# colorRampPalette(c("#08519c","#3182bd","#6baed6","#bdd7e7","white")) -> blues2 
# 
# 
# plot(bathy2, image=T, land=FALSE, bpal=blues2(200), 
#      deep=c(-9000,-3000,0), shallow=c(-3000,-100, 0), step=c(1000, 1000, 0),
#      col=c("lightgrey","darkgrey","black"), lwd=c(0.3,0.3,0.6), lty=c(1,1,1),
#      drawlabel=c(F,F,F))



# example from online for reference

#  Fetch data on NOAA servers and write on disk
bat <- getNOAA.bathy(-125.5, -122, 46, 48.5, res = 1, keep=TRUE)
# Create nice looking color palettes
blues <- c("lightsteelblue4",
           "lightsteelblue3",
           "lightsteelblue2",
           "lightsteelblue1")


blues2 <- c("#2171b5",
            "#4292c6",
            "#6baed6",
            "#9ecae1")


greys <- c(grey(0.6),
           grey(0.93),
           grey(0.99))

# version 1
# plot(bat, 
#      image = TRUE, 
#      land = TRUE,
#      lwd = 0.1, 
#      bpal = list(c(0, max(bat), greys), c(min(bat), 0, blues2)))
# plot(bat, 
#      lwd = 0.8,
#      deep = 0, 
#      shallow = 0, 
#      step = 0, 
#      col="darkgrey",
#      add = TRUE) # highlight coastline






