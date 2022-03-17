#### draw_PS_map.R ####

## credits

## much of the code here was graciously provided by
## Markus Min (mmin@uw.edu, https://github.com/markusmin)

## housekeeping

library(here)
library(rgdal)
library(broom)
library(ggplot2)
library(cowplot)

## figure save dir
fig_dir <- here("figures")


#### base map of Puget Sound ####

## load USA shape file
usa_spdf <- readOGR(dsn = here("data", "map_files", "USA_adm0.shp"))
## convert to df(ish)
usa_spdf_fort <- tidy(usa_spdf)


## draw puget sound
puget_sound <- ggplot(usa_spdf_fort, aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "gray70", fill = rgb(251, 234, 194, max = 255)) +
  ylab("Latitude") +
  xlab("Longitude") +
  coord_fixed(xlim = c(-123.3, -122), ylim = c(46.95, 48.8), ratio = 1.3) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill="white", color = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  scale_x_continuous(breaks = c(-123, -122.5),
                     expand = c(0, 0),
                     labels=c(expression(paste(123*degree,"W")),
                              expression(paste(122.5*degree,"W")))) +
  scale_y_continuous(breaks = seq(47.5, 48.5, 0.5),
                     expand = c(0, 0),
                     labels=c(expression(paste(47.5*degree,"N")),
                              expression(paste(48*degree,"N")),
                              expression(paste(48.5*degree,"N")))) + 
  annotate("segment", x = -122.60, xend = -122.60, y = 47.67,
           yend = 47.79, lwd = 0.5, color = "black") + 
  annotate("segment", x = -122.43, xend = -122.43, y = 47.67,
           yend = 47.79, lwd = 0.5, color = "black") + 
  annotate("segment", x = -122.60, xend = -122.43, y = 47.67,
           yend = 47.67, lwd = 0.5, color = "black") + 
  annotate("segment", x = -122.60, xend = -122.43, y = 47.79,
           yend = 47.79, lwd = 0.5, color = "black")+ 
annotate("text", 
         label = "Seattle",
         x = -122.22, 
         y = 47.6,
         size = 3,
         color = "black")+ 
  annotate("point", 
           x = -122.32, 
           y = 47.6,
           size = 1,
           color = "black")



## draw port_madison inset
port_madison <- ggplot(usa_spdf_fort, aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "gray70", fill = rgb(251, 234, 194, max = 255)) +
  coord_fixed(xlim = c(-122.60, -122.43), ylim = c(47.67, 47.79),  ratio = 1.3) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill="white", color = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(rep(0.1, 4), "cm"))+
  annotate("text", label = "Port Madison", x = -122.50, y = 47.73, size = 4, color = "black") #+ 



## combine maps
combined_maps <- ggdraw() +
  draw_plot(puget_sound, x = -0.15) +
  draw_plot(port_madison, x = 0.58, y = 0.5, width = 0.4, height = 0.4)

 combined_maps

## write map to file
ggsave(filename = file.path(fig_dir, "fig_01_PS_map.png"), 
       plot = combined_maps,
       width = 6, 
       height = 6,
       dpi = 300)


