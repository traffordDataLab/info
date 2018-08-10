## IMD 2015 map ##

# load packages
library(tidyverse) ; library(sf) ; library(ggsn)

# load lab theme
source("https://trafforddatalab.github.io/assets/theme/ggplot2/theme_lab.R")

# load IMD data
imd <- read_csv("https://www.traffordDataLab.io/open_data/imd_2015/IMD_2015_long.csv") %>% 
  filter(measure == "Decile" & index_domain == "Index of Multiple Deprivation")

# load LSOA boundaries and join attributes for Trafford
lsoa <- st_read("https://www.traffordDataLab.io/spatial_data/lookups/lsoa_to_ward_best-fit_lookup.geojson") %>% 
  filter(lad17nm == "Trafford") %>% 
  left_join(., imd, by = "lsoa11cd")

# load user chosen ward boundaries
ward <- st_read("https://www.traffordDataLab.io/spatial_data/ward/2017/trafford_ward_generalised.geojson") %>% 
  filter(area_name == "Davyhulme East")

# load buildings and clip by ward boundary 
bldgs <- st_read("https://www.traffordDataLab.io/open_data/buildings/trafford_buildings.geojson") %>% 
  select(-area_code, -area_name) %>% 
  st_intersection(ward) %>% 
  st_join(lsoa, join = st_within, left = FALSE) %>% 
  mutate(value = factor(value))

# load roads
roads <- st_read("https://www.traffordDataLab.io/open_data/open_roads/trafford_roadLink.geojson") %>% 
  select(identifier, class, roadNumber, name1) %>% 
  st_intersection(ward)

# load built-up areas
urban <- st_read("https://www.traffordDataLab.io/open_data/built_up_areas/gm_built_up_areas.geojson") %>% 
  select(-area_code, -area_name) %>%
  st_intersection(ward)

# plot data  ---------------------------
palette <- c("1" = "#A31A31", "2" = "#D23B33", "3" = "#EB6F4A", "4" = "#FCB562", 
             "5" = "#F4D78D", "6" = "#D8E9EC", "7" = "#AAD1DE", "8" = "#75A8C8", 
             "9" = "#4D77AE", "10" = "#353B91")

p <- ggplot() +
  geom_sf(data = ward, lwd = 0, fill = "#8AA37B", alpha = 0.7) +
  geom_sf(data = urban, lwd = 0, fill = "#d9d9d9") +
  geom_sf(data = bldgs, aes(colour = factor(value)), fill = NA, alpha = 1, size = 0.3, show.legend = "line") +
  geom_sf(data = subset(roads, class == "A Road"), size = 1.2, colour = "#ffffff", alpha = 1) +
  geom_sf(data = subset(roads, class != "A Road"), size = 0.5, colour = "#ffffff", alpha = 1) +
  geom_sf(data = ward, colour = "#212121", fill = "transparent", size = 0.5) +
  scale_colour_manual(values = palette,
                      labels = c("Most deprived", 2:9, "Least deprived"),
                      limits = c("1","2","3","4","5","6","7","8","9","10"),
                      drop = TRUE) +
  labs(x = NULL, y = NULL, title = NULL, subtitle = NULL,
       caption = "Source: MHCLG  |  @traffordDataLab\n Contains National Statistics data © and Ordnance Survey data © Crown copyright and database right 2018",
       colour = "IMD decile") +
  theme_lab() +
  theme(plot.title = element_text(size = 14, colour = "#757575", hjust = 0.5),
        plot.subtitle = element_text(size = 8, colour = "#757575", hjust = 0.5),
        plot.caption = element_text(size = 6, colour = "#757575", hjust = 1, margin = margin(b = 2)),
        legend.position = c(0.90, 0.15),
        legend.title = element_text(size = 9, colour = "#757575", face = "bold"),
        legend.text = element_text(size = 7, colour = "#757575")) +
  guides(colour = guide_legend(override.aes = list(size = 1),
                               title.position = "top", 
                               label.position = "right", 
                               title.hjust = 0.06,
                               label.hjust = 0,
                               direction = "vertical",
                               ncol = 1,
                               keyheight = unit(3, units = "mm"), 
                               keywidth = unit(10, units = "mm"))) +
  coord_sf(datum = NA)

# save plot
ggsave(file = "output/figures/imd_plot.svg", scale = 1.5, width = 6, height = 6)
p + labs(title = "Davyhulme East", subtitle = "Index of Multiple Deprivation, 2015") +
  ggsave("output/figures/imd_plot.png", dpi = 300, scale = 1.5, width = 6, height = 6)
