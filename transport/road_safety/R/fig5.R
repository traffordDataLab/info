## Road Safety: Collisions in Trafford by mode, day and hour, 2016 ##

# Source: Greater Manchester Police
# Publisher URL: https://data.gov.uk/dataset/road-accidents-safety-data
# Licence: Open Government Licence

# Credits: https://rud.is/projects/facetedheatmaps.html

# load R packages  ---------------------------
library(tidyverse) ; library(ggplot2) ; library(viridis); library(cowplot)

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data  ---------------------------
df <- read_csv("https://www.trafforddatalab.io/open_data/road_casualties/2016/STATS19_casualty_data_2016.csv") %>% 
  filter(area_name == "Trafford") %>% 
  distinct(AREFNO, .keep_all = TRUE)

# manipulate data ---------------------------
collisions <- df %>% 
  mutate(mode = fct_recode(mode, 
                           "Other" = "Bus or Coach", "Other" = "Goods Vehicle", 
                           "Other" = "Other Vehicle", "Other" = "Taxi")) %>% 
  group_by(mode, day, hour) %>%
  summarise(n = n()) %>% 
  ungroup() %>% 
  complete(mode, day, hour, fill = list(n = NA)) %>% 
  spread(mode, n) %>% 
  mutate(Total = rowSums(.[3:7], na.rm = TRUE)) %>% 
  gather(mode, n, -day, -hour) %>% 
  mutate(day_hour = paste(day, hour, sep = '_'),
         mode = factor(mode, 
                levels = c("Car","Pedal Cycle", "Pedestrian",
                           "Powered 2 Wheeler", "Other", "Total"))) %>% 
  select(-day, -hour)

periods <- data_frame(day = c(rep("Mon", 25), rep("Tue", 25), rep("Wed", 25),
                   rep("Thu", 25), rep("Fri", 25), rep("Sat", 25),
                   rep("Sun", 25)),
           hour = rep(seq(0, 24, by = 1), 7)) %>% 
  mutate(day_hour = paste(day, hour, sep = '_'))

results <- left_join(periods, collisions, by = "day_hour") %>% 
  complete(day, hour, mode, fill = list(n = NA)) %>% 
  mutate(day = factor(day, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
         n = replace(n, n == 0, NA)) %>% 
  filter(!is.na(mode))

# plot data  ---------------------------
lapply(unique(results$mode), function(mode_list) {
ggplot(filter(results, mode == mode_list), aes(x = factor(hour), y = fct_rev(day), fill = factor(n), frame = mode)) +
  geom_tile(colour = "white", size = 0.1) + 
  scale_fill_viridis(name = "", discrete = T, direction = -1, na.value = "grey93",
                     breaks = c("1", "2", "3", "4", "5"),
                     labels = c("1", "2", "3", "4", "5"),
                     guide = guide_legend(override.aes = list(size = 1),
                                          title.position = "top",
                                          label.position = "bottom",
                                          title.hjust = 0.5,
                                          label.hjust = 0.5,
                                          nrow = 1)) +
  scale_x_discrete(breaks = c(0,6,12,18,24), expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  coord_equal() +
  labs(x = NULL, y = NULL, fill = NULL, title = sprintf("%s", mode_list), subtitle = NULL) +
  theme_lab() +
  theme(panel.border = element_blank(),
        plot.title = element_text(hjust = 0, size = 10),
        axis.text.x = element_text(hjust = 0, size = 6),
        axis.text.y = element_text(hjust = 0, size = 6),
        legend.title = element_text(size = 6),
        legend.text = element_text(size = 6),
        legend.position = "bottom",
        legend.box.margin = margin(-15,-15,-15,-15),
        legend.key.size = unit(0.2, "cm"),
        legend.key.width = unit(1, "cm"))
}) -> mode_list

plot <- plot_grid(plotlist = mode_list, ncol = 2)

# save plot / data  ---------------------------
ggsave(file = "output/figures/fig5.svg", plot = plot, scale = 0.6, width = 10, height = 8)
ggsave(file = "output/figures/fig5.png", plot = plot, scale = 0.6, width = 10, height = 8)

write_csv(results, "output/data/fig5.csv")
