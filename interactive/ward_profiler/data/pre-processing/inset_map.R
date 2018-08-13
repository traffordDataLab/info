library(tidyverse) ; library(sf)

wards <- st_read("https://www.traffordDataLab.io/spatial_data/ward/2017/trafford_ward_generalised.geojson", quiet = TRUE)

codes <- paste0("E0", 5000819:5000839)

for(item in codes){
  
  p <- ggplot() +
    geom_sf(data = wards, fill = "transparent", colour = "#757575", size = 0.5) +
    geom_sf(data = filter(wards, area_code == item), fill = "#fc6721") +
    coord_sf(crs = 4326, datum = NA) +
    theme_void()
  
  plot <- paste0(item, "_thumbnail.png")
  ggsave(plot, scale = 1, dpi = 300, plot = p)
  
}