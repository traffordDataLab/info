## Burglary

library(tidyverse) ; library(ggplot2) ; library(tibble) ; library(svglite)
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load crime data
df <- read_csv("data/crime_data.csv") %>% 
  filter(category == "Burglary")
# load ONS mid-year population estimates (2016)
pop <- read_csv("https://github.com/traffordDataLab/open_data/raw/master/mid-year_pop_estimates_2016/ONS_mid-year_population_estimates_2016.csv") %>% 
  filter(lad16nm == "Trafford") %>% 
  select(area_name = wd16nm, total_pop) %>% 
  add_row(area_name = "Trafford", total_pop = sum(.$total_pop)) # add Trafford total

# ------------------------------------------

## Rate of burglary by ward (November 2017)
df %>% 
  filter(month == "2017-11-01") %>% 
  group_by(area_name) %>% 
  count() %>% 
  ungroup() %>% 
  add_row(area_name = "Trafford", n = sum(.$n)) %>% # add Trafford total
  left_join(., pop, by = "area_name") %>% 
  mutate(rate = round((n/total_pop)*1000,1)) %>% 
  arrange(desc(rate)) %>%
  mutate(area_name = factor(area_name, levels = area_name)) %>% 
  ggplot(aes(rate, area_name)) +
  geom_segment(aes(x = 0, y = area_name, xend = rate, yend = area_name), color = "#f0f0f0") +
  geom_point(colour = "#fc6721", size = 6) +
  geom_text(aes(label = rate, fontface = "bold"), color = "white", size = 2.5) + 
  labs(x = "Crimes per 1,000 residents", y = NULL,
       title = NULL,
       caption = "Source: data.police.uk  |  @traffordDataLab") +
  theme_lab() + 
  theme(panel.grid.major = element_blank(),
        axis.text.y = element_text(hjust = 0))
ggsave(file = "burglary/outputs/fig1.svg", width = 6, height = 8)
