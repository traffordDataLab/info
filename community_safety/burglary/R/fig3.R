## Burglary: rate of burglary by ward ##

# load R packages  ---------------------------
library(tidyverse); library(ggplot2); library(svglite)

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data  ---------------------------
df <- read_csv("https://github.com/traffordDataLab/open_data/raw/master/police_recorded_crime/data/trafford.csv") %>% 
  filter(category == "Burglary")

gm <- read_csv("https://github.com/traffordDataLab/open_data/raw/master/police_recorded_crime/data/gm.csv.gz") %>% 
  filter(category == "Burglary" & month >= "2017-11-01") %>% 
  count() %>% 
  pull()

households <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_619_1.data.csv?date=latest&geography=1237320482...1237320496,1237320498,1237320497,1237320499...1237320502,1946157089,1937768449&rural_urban=0&cell=0&measures=20100&select=date_name,geography_name,geography_code,rural_urban_name,cell_name,measures_name,obs_value,obs_status_name") %>% 
  select(area_code = GEOGRAPHY_CODE, area_name = GEOGRAPHY_NAME, households = OBS_VALUE)

# manipulate data ---------------------------
results <- df %>% 
  filter(month == "2017-11-01") %>% 
  group_by(area_name) %>% 
  count() %>% 
  ungroup() %>% 
  left_join(., households, by = "area_name") %>% 
  mutate(rate = round((n/households)*1000,1)) %>% 
  arrange(desc(rate)) %>%
  mutate(area_name = factor(area_name, levels = area_name)) %>% 
  add_row(area_name = "Trafford", n = sum(.$n), rate = round((n/94484)*1000,1)) %>% 
  add_row(area_name = "Greater Manchester", n = gm, rate = round((n/1128066)*1000,1))

# plot data ---------------------------
ggplot(results, aes(rate, area_name)) +
  geom_segment(aes(x = 0, y = area_name, xend = rate, yend = area_name), color = "#f0f0f0") +
  geom_point(colour = "#fc6721", size = 4) +
  geom_text(aes(label = rate, fontface = "bold"), color = "white", size = 2) + 
  labs(x = "Crimes per 1,000 households", y = NULL,
       title = NULL,
       caption = "Source: data.police.uk  |  @traffordDataLab") +
  theme_lab() + 
  theme(panel.grid.major = element_blank(),
        axis.text.y = element_text(hjust = 0))

# save plot / data  ---------------------------
ggsave(file = "output/figures/fig3.svg", width = 6, height = 6)
ggsave(file = "output/figures/fig3.png", width = 6, height = 6)
write_csv(select(results, -area_code), "output/data/fig3.csv")
