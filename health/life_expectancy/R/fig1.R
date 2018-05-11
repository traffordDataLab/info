## Life expectancy: Life expectancy at birth by local authority, 2014-2016 ##

# Dataset: Life expectancy at birth, 2014-2016
# Source: ONS
# Publisher: ONS
# Publisher URL: https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/healthandlifeexpectancies/datasets/lifeexpectancyatbirthandatage65bylocalareasuk
# Licence: Open Government Licence

# load R packages  ---------------------------
library(tidyverse) ; library(fingertipsR) ; library(ggplot2) ; library(viridis) ; library(cowplot)

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data ---------------------------
query <- fingertips_data(IndicatorID = 90366, AreaTypeID = 101) %>% 
  filter(AreaType == "District & UA" & Timeperiod == "2014 - 16") %>% 
  select(area_code = AreaCode, area_name = AreaName, sex = Sex, value = Value) %>% 
  filter(!is.na(value)) %>% 
  mutate(area_name = factor(area_name),
         value = as.numeric(value))

# plot data ---------------------------
f <- ggplot(filter(query, area_name %in% c("Manchester", "Trafford", "Camden") & sex == "Female"), aes(x = value, y = "")) +
  geom_point(data = filter(query, sex == "Female"), aes(x = value, y = "", fill = value), colour = "transparent", pch = 21, size = 4, alpha = 0.5) +
  geom_point(shape = 1, size = 4, alpha = 1) +
  geom_label(data = filter(query, sex == "Female" & area_name == "Manchester"), label = "Manchester", fill = "#f0f0f0", size = 2.3, nudge_y = -0.25) +
  geom_label(data = filter(query, sex == "Female" & area_name == "Trafford"), label = "Trafford", fill = "#f0f0f0", size = 2.3, nudge_y = -0.25) +
  geom_label(data = filter(query, sex == "Female" & area_name == "Camden"), label = "Camden", fill = "#f0f0f0", size = 2.3, nudge_y = -0.25) +
  geom_vline(aes(xintercept = 83.1), colour = "#757575", linetype = "dashed") +
  geom_text(aes(x = 83.1, label = "\nEngland", y = ""), colour = "#757575", angle = 90, size = 3, hjust = 0.6) +
  annotate("text", label = "Female", x = 73, y = "", color = "#757575", fontface = "bold", hjust = 0) +
  scale_fill_gradient(low = "#d8b365", high = "#eee4d1") +
  labs(x = NULL, y = NULL,  
       title = NULL,
       subtitle = NULL, 
       caption = NULL) +
  scale_x_continuous(limits = c(73,87), breaks = seq(74,87, by = 1)) +
  theme_lab() +
  theme(plot.margin = unit(c(0, 0, -2.8, 0), "cm"),
        panel.grid.major.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(hjust = 0),
        legend.position = "none") +
  coord_fixed(ratio = 1)

m <- ggplot(filter(query, area_name %in% c("Blackpool", "Trafford", "Kensington and Chelsea") & sex == "Male"), aes(x = value, y = "")) +
  geom_point(data = filter(query, sex == "Male"), aes(x = value, y = "", fill = value), colour = "transparent", pch = 21, size = 4, alpha = 0.5) +
  geom_point(shape = 1, size = 4, alpha = 1) +
  geom_label(data = filter(query, sex == "Male" & area_name == "Blackpool"), label = "Blackpool", fill = "#f0f0f0", size = 2.3, nudge_y = -0.25) +
  geom_label(data = filter(query, sex == "Male" & area_name == "Trafford"), label = "Trafford", fill = "#f0f0f0", size = 2.3, nudge_y = -0.25) +
  geom_label(data = filter(query, sex == "Male" & area_name == "Kensington and Chelsea"), label = "Kensington and Chelsea", fill = "#f0f0f0", size = 2.3, nudge_y = -0.25) +
  geom_vline(aes(xintercept = 79.5), colour = "#757575", linetype = "dashed") +
  geom_text(aes(x = 79.5, label = "\nEngland", y = ""), colour = "#757575", angle = 90, size = 3, hjust = 0.6) +
  annotate("text", label = "Male", x = 73, y = "", color = "#757575", fontface = "bold", hjust = 0) +
  scale_fill_gradient(low = "#5ab4ac", high = "#cee5e3") +
  labs(x = NULL, y = NULL,  
       title = NULL,
       subtitle = NULL, 
       caption = "Source: ONS  |  @traffordDataLab") +
  scale_x_continuous(limits = c(73, 87), breaks = seq(74,87, by = 1)) +
  theme_lab() +
  theme(plot.margin = unit(c(-9, 0, 0, 0), "cm"),
        panel.grid.major.y = element_blank(),
        plot.caption = element_text(vjust = -1, hjust = 0.9),
        axis.text.y = element_text(hjust = 0),
        legend.position = "none") +
  coord_fixed(ratio = 1)

# save plot / data  ---------------------------
p1 <- plot_grid(f, m, nrow = 2)
save_plot("output/figures/fig1.svg", p1, nrow = 2, base_aspect_ratio = 3)

f <- f + labs(title = "Life expectancy at birth by local authority, 2014-16") +
  theme(plot.title = element_text(vjust = 13, hjust = 0.5, size = 14, face = "bold"))
p2 <- plot_grid(f, m, nrow = 2)
save_plot("output/figures/fig1.png", p2, nrow = 2, base_aspect_ratio = 3)

write_csv(query, "output/data/fig1.csv")

