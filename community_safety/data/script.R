## Load crime data

# load necessary packages
library(sf) ; library(tidyverse) ; library(zoo)
# load necessary functions
source("https://github.com/traffordDataLab/assets/raw/master/rfunctions/crime_data.R")
# load Trafford boundary with 1000m buffer and simplify
trafford <- st_read("https://github.com/traffordDataLab/spatial_data/raw/master/local_authority/2016/trafford_local_authority_full_resolution.geojson") %>% 
  st_transform(27700) %>%
  st_buffer(dist = 500) %>%
  st_simplify(preserveTopology = TRUE, dTolerance = 100) %>% 
  st_transform(4326)
# extract polygon coordinates
coords <- get_coords(x = trafford)
# select a range of months
periods <- format(seq(as.Date("2014-12-01"), length = 36, by = "months"), "%Y-%m")
# run the get_crimes() function
results <- get_crimes(coords, periods)
# convert to a simple features object
results_sf <- results %>%
  st_as_sf(crs = 4326, coords = c("long", "lat"))
# load the ward layer
wards <- st_read("https://github.com/traffordDataLab/spatial_data/raw/master/ward/2017/trafford_ward_full_resolution.geojson") %>% 
  st_transform(crs = 4326) %>% 
  select(area_code, area_name)
# run point in polygon to obtain ward names
crimes_sf <- st_join(results_sf, wards, join = st_within, left = FALSE) %>% 
  mutate(category = factor(category),
         month = as.Date(as.yearmon(month)))
# convert to data frame
crimes_df <- crimes_sf %>% 
  st_set_geometry(value = NULL) %>% 
  mutate(category = factor(category),
         month = as.Date(as.yearmon(month))) %>% 
  select(month, category, location, area_code, area_name)
# write data
write_csv(crimes_df, "data/crime_data.csv")

