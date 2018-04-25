## Road Safety: Collisions in Trafford by mode and month, 2016 ##

# Source: Greater Manchester Police
# Publisher URL: https://data.gov.uk/dataset/road-accidents-safety-data
# Licence: Open Government Licence

# load R packages  ---------------------------
library(tidyverse) ; library(zoo) ; library(ggplot2) ; library(viridis)

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data  ---------------------------
df <- read_csv("https://www.trafforddatalab.io/open_data/road_casualties/2016/STATS19_casualty_data_2016.csv") %>% 
  filter(area_name == "Trafford") %>% 
  distinct(AREFNO, .keep_all = TRUE)

# manipulate data ---------------------------
collisions <- df %>% 
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>% 
  group_by(date, mode) %>%
  summarise(n = n()) %>% 
  mutate(month_year = as.Date(as.yearmon(date, "%m/%Y"))) %>% 
  group_by(month_year, mode) %>% 
  summarise(n = n()) %>% 
  spread(mode, n)

dates <- data_frame(month_year = seq(as.Date('2016-01-01'), as.Date('2016-12-01'), by = "1 month"))

results <- left_join(dates, collisions, by = "month_year") %>% 
  gather(mode, n, -month_year) %>% 
  mutate(n = replace(n, is.na(n), 0))

# plot data  ---------------------------
p <- ggplot(results, aes(x = month_year, y = mode, fill = n)) + 
  geom_tile(color = "white", size = 0.4) + 
  scale_fill_viridis(name = "Number of collisions", 
                     direction = -1, 
                     na.value = "grey93", 
                     breaks = c(1, 3, 5, 7, 9, 11),
                     limits = c(1, max(results$n)),
                     guide = guide_legend( keyheight = unit(3, units = "mm"),
                                           keywidth=unit(12, units = "mm"), 
                                           label.position = "bottom", 
                                           title.position = 'top', 
                                           nrow = 1)) +
  labs(x = NULL, y = NULL, fill = NULL, title = NULL, subtitle = NULL) +
  facet_grid(mode ~ ., scales = "free") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0,0)) +
  theme_lab() +
  theme(panel.grid = element_blank(), 
        axis.text.x = element_text(hjust = 0),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        legend.position = "bottom",
        strip.text.y = element_blank(),
        plot.margin = unit(c(1,1,3,1), "cm"))

# save plot / data  ---------------------------
ggsave(file = "output/figures/fig4.svg", scale = 1.8, width = 10, height = 4)

p + labs(title = "Collisions in Trafford by mode and month, 2016") +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm"),
        plot.title = element_text(face = "bold", vjust = 10, hjust = 0.5))
ggsave(file = "output/figures/fig4.png", scale = 1.5, width = 10, height = 4)

write_csv(results, "output/data/fig4.csv")
