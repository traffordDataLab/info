## Violence and sexual offences: rate by ward ##

# load R packages  ---------------------------
library(tidyverse); library(ggplot2); library(svglite)

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data  ---------------------------
df <- read_csv("https://github.com/traffordDataLab/open_data/raw/master/police_recorded_crime/data/trafford.csv") %>% 
  filter(category == "Violence and sexual offences")

lookup <- read_csv("https://github.com/traffordDataLab/spatial_data/raw/master/lookups/ward_to_local_authority.csv") %>% 
  filter(la_name == "Trafford") %>% 
  pull(ward_code)
lookup <- c(lookup, "E08000009","E47000001") # add Trafford and Greater Manchester

population <- read_csv("https://github.com/traffordDataLab/open_data/raw/master/mid-year_pop_estimates_2016/ONS_mid-year_population_estimates_2016.csv") %>% 
  filter(area_code %in% lookup) %>% 
  select(area_code, area_name, population = all_ages)

gm <- read_csv("https://github.com/traffordDataLab/open_data/raw/master/police_recorded_crime/data/gm.csv.gz") %>% 
  filter(category == "Violence and sexual offences" & month == max(df$month)) %>% 
  count() %>% 
  pull()

# manipulate data ---------------------------
results <- df %>% 
  filter(month == max(df$month)) %>% 
  group_by(area_name) %>% 
  count() %>% 
  ungroup() %>% 
  left_join(., population, by = "area_name") %>% 
  mutate(rate = round((n/population)*1000,1)) %>% 
  arrange(desc(rate)) %>%
  mutate(area_name = factor(area_name, levels = area_name)) %>% 
  add_row(area_name = "Trafford", n = sum(.$n), area_code = "E08000009", population = population[population$area_name == "Trafford", ]$population, rate = round((n/population)*1000,1)) %>% 
  add_row(area_name = "Greater Manchester", n = gm, area_code = "E47000001", population = population[population$area_name == "Greater Manchester", ]$population, rate = round((n/population)*1000,1)) %>% 
  select(area_code, area_name, everything())
  
# plot data ---------------------------
ggplot(results, aes(rate, area_name)) +
  geom_segment(aes(x = 0, y = area_name, xend = rate, yend = area_name), color = "#f0f0f0") +
  geom_point(colour = "#fc6721", size = 4) +
  geom_text(aes(label = rate, fontface = "bold"), color = "white", size = 2) + 
  labs(x = "crimes per 1,000 population", y = NULL,
       title = NULL,
       caption = "Source: data.police.uk  |  @traffordDataLab") +
  theme_lab() + 
  theme(panel.grid.major = element_blank(),
        axis.text.y = element_text(hjust = 0))

# save plot / data  ---------------------------
ggsave(file = "output/figures/fig3.svg", width = 6, height = 6)
ggsave(file = "output/figures/fig3.png", width = 6, height = 6)

results %>% 
  mutate(month = max(df$month), category = "Violence and sexual offences") %>% 
  select(month, category, area_code, area_name, n, population, rate) %>% 
  write_csv("output/data/fig3.csv")
