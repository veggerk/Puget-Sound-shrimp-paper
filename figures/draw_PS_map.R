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
usa_spdf <- readOGR(dsn = here("analysis", "map_files", "USA_adm0.shp"))
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
  annotate("segment", x = -122.47, xend = -122.47, y = 47.55,
           yend = 47.67, lwd = 0.5, color = "black") + 
  annotate("segment", x = -122.3, xend = -122.3, y = 47.55,
           yend = 47.67, lwd = 0.5, color = "black") + 
  annotate("segment", x = -122.3, xend = -122.47, y = 47.55,
           yend = 47.55, lwd = 0.5, color = "black") + 
  annotate("segment", x = -122.3, xend = -122.47, y = 47.67,
           yend = 47.67, lwd = 0.5, color = "black") 


## draw elliott bay inset
elliott_bay <- ggplot(usa_spdf_fort, aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "gray70", fill = rgb(251, 234, 194, max = 255)) +
  coord_fixed(xlim = c(-122.47, -122.3), ylim = c(47.55, 47.67),  ratio = 1.3) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill="white", color = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(rep(0.1, 4), "cm")) +
  # annotate("segment",
  #          x = -122.41347424370157, xend = -122.4197651914652,
  #          y = 47.63917076878349 - 1/60, yend = 47.576654192683336 + 1/60,
  #          lwd = 0.7, lty = "solid", color = "black") +
  annotate("text", label = "Elliott\nBay", x = -122.385, y = 47.61, size = 4, color = "black") + 
  annotate("text", label = "Fourmile Rock", x = -122.377, y = 47.642,
           vjust = 0, size = 3, color = "black") + 
  annotate("text", label = "Alki Point", x = -122.387, y = 47.575,
           size = 3, color = "black")


## combine maps
combined_maps <- ggdraw() +
  draw_plot(puget_sound, x = -0.15) +
  draw_plot(elliott_bay, x = 0.58, y = 0.5, width = 0.4, height = 0.4)

# combined_maps

## write map to file
ggsave(filename = file.path(fig_dir, "fig_01_PS_map.png"), 
       plot = combined_maps,
       width = 6, 
       height = 6,
       dpi = 300)


#### map of Salish Sea ####

# ## load BC shape file
# BC_shp <- readOGR(dsn = here("analysis", "map_files", "canada", "lpr_000b16a_e.shp"))
# ## set coord ref system
# BC_shp_transform <- spTransform(BC_shp, "+init=epsg:4326")
# ## convert to df(ish)
# BC_spdf_fort <- tidy(BC_shp_transform)


