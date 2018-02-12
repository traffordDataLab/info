## Vehicle crime: sparklines ##
# credits: ggplot2 code adapted from http://www.motioninsocial.com/tufte/#sparklines

# load R packages  ---------------------------
library(tidyverse); library(ggplot2); library(svglite)

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data
df <- read_csv("https://github.com/traffordDataLab/open_data/raw/master/police_recorded_crime/data/trafford.csv") %>% 
  filter(category == "Vehicle crime")

lookup <- read_csv("https://github.com/traffordDataLab/spatial_data/raw/master/lookups/ward_to_local_authority.csv") %>% 
  filter(la_name == "Trafford") %>% 
  pull(ward_code)

population <- read_csv("https://github.com/traffordDataLab/open_data/raw/master/mid-year_pop_estimates_2016/ONS_mid-year_population_estimates_2016.csv") %>% 
  filter(area_code %in% lookup) %>% 
  select(area_code, area_name, population = all_ages)

# manipulate data
results <- df %>% 
  group_by(month, area_code, area_name) %>% 
  count() %>% 
  ungroup() %>% 
  left_join(., population, by = "area_name") %>% 
  mutate(rate = round((n/population)*1000,1)) %>% 
  select(month, area_name, rate)

mins <- group_by(results, area_name) %>% slice(which.min(rate))
maxs <- group_by(results, area_name) %>% slice(which.max(rate))
ends <- group_by(results) %>% filter(month == max(month))

# plot data ---------------------------
ggplot(results, aes(x = month, y = rate)) + 
  geom_line(colour = "#757575", size = 0.3) +
  geom_point(data = mins, col = '#31a354', size = 1.5) +
  geom_text(data = mins, aes(label = rate), size = 3, vjust = 2) +
  geom_point(data = maxs, col = '#dd1c77', size = 1.5) +
  geom_text(data = maxs, aes(label = rate), size = 3, hjust = -0.5, vjust = 0.5) +
  geom_text(data = ends, aes(label = rate), size = 3, fontface = "bold", alpha = 0.8, hjust = -0.2) +
  expand_limits(x = max(df$month) + (0.25 * (max(df$month) - min(df$month)))) +
  scale_x_date(date_labels = "'%y") +
  scale_y_continuous(limits = c(-2, NA), expand = c(0.1, 0)) +
  facet_wrap(~area_name, ncol = 3, strip.position = "top") + 
  labs(title = "Vehicle crime rates",
       subtitle = "Crimes per 1,000 population",
       caption = "Source: data.police.uk; ONS  |  @traffordDataLab") +
  theme_lab() +
  theme(panel.spacing = unit(0, "lines"),
        axis.title=element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8, hjust = 1),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_text(hjust = 0.02),
        plot.subtitle = element_text(size = 10, hjust = 0.018),
        strip.text = element_text(size = 8, face = "bold", angle = 0, hjust = 0.05, vjust = 1))

ggsave(file = "output/figures/sparkline_plot.svg", dpi = 300, scale = 1)
