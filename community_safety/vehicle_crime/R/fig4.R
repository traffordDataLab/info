## Vehicle crime: location quotients ##

# load R packages  ---------------------------
library(tidyverse); library(ggplot2); library(sf); library(viridis); library(svglite)

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data  ---------------------------
df <- read_csv("https://github.com/traffordDataLab/open_data/raw/master/police_recorded_crime/data/trafford.csv") %>% 
  filter(category == "Vehicle crime")

lookup <- read_csv("https://github.com/traffordDataLab/spatial_data/raw/master/lookups/ward_to_local_authority.csv") %>% 
  filter(la_name == "Trafford") %>% 
  pull(ward_code)

population <- read_csv("https://github.com/traffordDataLab/open_data/raw/master/mid-year_pop_estimates_2016/ONS_mid-year_population_estimates_2016.csv") %>% 
  filter(area_code %in% lookup) %>% 
  select(area_code, area_name, population = all_ages)

# load geospatial data  ---------------------------
sf <- st_read("https://github.com/traffordDataLab/spatial_data/raw/master/ward/2017/trafford_ward_generalised.geojson") %>% 
  st_as_sf(crs = 4326, coords = c("long", "lat")) %>% 
  select(-lat, -lon)

lookup <- read_csv("https://github.com/traffordDataLab/spatial_data/raw/master/lookups/ward_abbreviations.csv") %>% 
  select(-area_name)

sf <- left_join(sf, lookup, by = "area_code")

# manipulate data ---------------------------
results <- df %>% 
  filter(month == max(df$month)) %>% 
  group_by(area_name) %>% 
  count() %>% 
  ungroup() %>% 
  left_join(., population, by = "area_name") %>% 
  mutate(rate = round((n/population)*1000,1)) %>% 
  arrange(desc(rate)) %>% 
  mutate(lq = round((n/population)/(sum(n)/sum(population)),2)) %>% 
  select(area_code, area_name, everything())

sf_df <- left_join(sf, results, by = "area_code") # merge with ward attribute table

# plot data ---------------------------
ggplot(sf_df) +
  geom_sf(aes(fill = lq), colour = "white") +
  geom_text(aes(lon, lat, label = area_abbr), size = 2.5, color = "#212121") +
  scale_fill_gradientn(colours = c("#feedde","#fdbe85","#fd8d3c","#e6550d","#a63603"), 
                       na.value = "#f0f0f0",
                       guide = guide_legend(keyheight = unit(3, units = "mm"), 
                                            keywidth=unit(12, units = "mm"), 
                                            label.position = "bottom", 
                                            title.position = 'top', 
                                            nrow = 1)) +
  labs(x = NULL, y = NULL, title = NULL, fill = 'Vehicle crime\nlocation quotient',
       caption = "Source: data.police.uk  |  @traffordDataLab") +
  theme_lab() +
  theme(plot.margin = unit(c(0.5,0,1,0), "cm"),
        axis.text = element_blank(),
        plot.caption = element_text(margin = margin(t = 55)),
        legend.position = c(0.1,-0.05),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9))

# save plot / data  ---------------------------
ggsave(file = "output/figures/fig4.svg", width = 8, height = 6)
ggsave(file = "output/figures/fig4.png", width = 8, height = 6)

results %>% 
  mutate(month = max(df$month), category = "Vehicle crime") %>% 
  select(month, category, area_code, area_name, n, population, rate, lq) %>% 
  write_csv("output/data/fig4.csv")
