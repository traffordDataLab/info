## Population: median age by ward ##

# load R packages  ---------------------------
library(tidyverse) ; library(readxl) ; library(sf) ; library(ggplot2) 

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data ---------------------------
# ward level mid-year population estimates (2016)
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/wardlevelmidyearpopulationestimatesexperimental/mid2016sape19dt8/sape19dt8mid2016ward2016syoaestimatesunformatted.zip"
download.file(url, dest = "output/data/sape19dt8mid2016ward2016syoaestimatesunformatted.zip")
unzip("output/data/sape19dt8mid2016ward2016syoaestimatesunformatted.zip", exdir = "output/data")
file.remove("output/data/sape19dt8mid2016ward2016syoaestimatesunformatted.zip")

males <- read_xls("output/data/SAPE19DT8-mid-2016-ward-2016-syoa-estimates-unformatted.xls", sheet = 3, skip = 3) %>% 
  filter(`Local Authority` == "Trafford") %>% 
  rename(`90` = `90+`) %>% 
  mutate(gender = "Male") %>% 
  select(area_code = `Ward Code 1`, area_name = `Ward Name 1`, gender, everything(), -`Local Authority`)
females <- read_xls("output/data/SAPE19DT8-mid-2016-ward-2016-syoa-estimates-unformatted.xls", sheet = 4, skip = 3) %>% 
  filter(`Local Authority` == "Trafford") %>% 
  rename(`90` = `90+`) %>% 
  mutate(gender = "Female") %>% 
  select(area_code = `Ward Code 1`, area_name = `Ward Name 1`, gender, everything(), -`Local Authority`)

# load geospatial data  ---------------------------
sf <- st_read("https://github.com/traffordDataLab/spatial_data/raw/master/ward/2017/trafford_ward_generalised.geojson") %>% 
  st_as_sf(crs = 4326, coords = c("long", "lat")) %>% 
  select(-lon, -lat)

lookup <- read_csv("https://github.com/traffordDataLab/spatial_data/raw/master/lookups/ward_abbreviations.csv") %>% 
  select(-area_name)

sf <- left_join(sf, lookup, by = "area_code")

# manipulate data  ---------------------------
population <- bind_rows(males, females) %>% 
  select(-`All Ages`) %>% 
  gather(age, n, -area_code, -area_name, -gender) %>% 
  mutate(age = as.integer(age))
rm(url, males, females)

results <- population %>% 
  group_by(area_code, age) %>%
  summarise(n = sum(n)) %>% 
  summarise(median_age = age[max(which(cumsum(n)/sum(n) <= 0.5))]) %>%
  arrange(median_age) 

sf_df <- left_join(sf, results, by = "area_code")

# plot data ---------------------------
ggplot(sf_df) +
  geom_sf(aes(fill = median_age), colour = "white") +
  geom_text(aes(lon, lat, label = area_abbr), size = 2.5, color = "#212121") +
  scale_fill_gradientn(colours = c("#feedde","#fdbe85","#fd8d3c","#e6550d","#a63603"), na.value = "#f0f0f0") +
  labs(x = NULL, y = NULL, title = NULL, fill = 'Median\nage',
       caption = "Source: ONS  |  @traffordDataLab") +
  theme_lab() +
  theme(axis.text = element_blank())

# save plot / data  ---------------------------
ggsave(file = "output/figures/fig2.svg", width = 6, height = 6)
ggsave(file = "output/figures/fig2.png", width = 6, height = 6)

sf_df %>% 
  select(-lat, -lon) %>% 
  st_set_geometry(value = NULL) %>% 
  write_csv("output/data/fig2.csv")
