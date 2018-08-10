## Ward map ##

# load packages
library(tidyverse) ; library(sf) ; library(ggsn)

# load lab theme
source("https://trafforddatalab.github.io/assets/theme/ggplot2/theme_lab.R")

# load user chosen ward boundary
ward <- st_read("https://www.traffordDataLab.io/spatial_data/ward/2017/trafford_ward_generalised.geojson") %>% 
  filter(area_name == "Gorse Hill")

# load buildings and clip to ward boundary 
bldgs <- st_read("https://www.traffordDataLab.io/open_data/buildings/trafford_buildings.geojson") %>% 
  select(-area_code, -area_name) %>% 
  st_intersection(ward)

# load roads and clip to ward boundary 
roads <- st_read("https://www.traffordDataLab.io/open_data/open_roads/trafford_roadLink.geojson") %>% 
  select(identifier, class, roadNumber, name1) %>% 
  st_intersection(ward)

# load built-up areas
urban <- st_read("https://www.traffordDataLab.io/open_data/built_up_areas/gm_built_up_areas.geojson") %>% 
  select(-area_code, -area_name) %>%
  st_intersection(ward)

# plot data
p <- ggplot() +
  geom_sf(data = ward, lwd = 0, fill = "#8AA37B", alpha = 0.7) +
  geom_sf(data = urban, lwd = 0, fill = "#d9d9d9") +
  geom_sf(data = bldgs, colour = "#757575", fill = NA, alpha = 1, size = 0.3) +
  geom_sf(data = subset(roads, class == "A Road"), size = 1.2, colour = "#ffffff", alpha = 1) +
  geom_sf(data = subset(roads, class != "A Road"), size = 0.5, colour = "#ffffff", alpha = 1) +
  geom_sf(data = ward, colour = "#212121", fill = "transparent", size = 0.5) +
  labs(x = NULL, y = NULL, title = NULL, subtitle = NULL,
       caption = "Contains National Statistics data © and Ordnance Survey data © Crown copyright and database right 2018  |  @traffordDataLab") +
  theme_lab() +
  theme(plot.title = element_text(size = 14, colour = "#757575", hjust = 0.5),
        plot.subtitle = element_text(size = 8, colour = "#757575", hjust = 0.5),
        plot.caption = element_text(size = 6, colour = "#757575", hjust = 1, margin = margin(b = 2))) +
  coord_sf(datum = NA) +
  scalebar(data = ward, location = "bottomright", 
           dist = 0.5, height = 0.01, st.dist = 0.02,
           st.size = 4, dd2km = TRUE, model = "WGS84",
           st.color = "#212121", box.color = "#212121", box.fill = c("#757575", "#ffffff"))

# save plot
ggsave(file = "output/figures/ward_plot.svg", scale = 1.5, width = 6, height = 6)
p + labs(title = "Gorse Hill") +
  north(data = ward, symbol = 16) + 
  ggsave("output/figures/ward_plot.png", dpi = 300, scale = 1.5, width = 6, height = 6)
