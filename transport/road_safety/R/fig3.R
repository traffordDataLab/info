## Road Safety: Casualties in Trafford by sex, age_band and mode of travel, 2016 ##

# Source: Greater Manchester Police
# Publisher URL: https://data.gov.uk/dataset/road-accidents-safety-data
# Licence: Open Government Licence

# load R packages  ---------------------------
library(tidyverse) ; library(ggplot2) ; library(scales)

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data  ---------------------------
df <- read_csv("https://www.trafforddatalab.io/open_data/road_casualties/2016/STATS19_casualty_data_2016.csv") %>% 
  filter(area_name == "Trafford")

# manipulate data --------------------------- 
df %>%
  mutate(ageband = fct_recode(ageband,
                              "less than 15" = "0-4", "less than 15" = "5-9", "less than 15" = "10-14",
                              "15-39" = "15-19", "15-39" = "20-24", "15-39" = "25-29", "15-39" = "30-34", "15-39" = "35-39", 
                              "40-64" = "40-44", "40-64" = "45-49", "40-64" = "50-54", "40-64" = "55-59", "40-64" = "60-64", 
                              "65 or more" = "65-69", "65 or more" = "70-74", "65 or more" = "75-79", "65 or more" = "80-84", "65 or more" = "85-89")) %>%
  group_by(ageband, sex, mode) %>% 
  summarise(n = n()) %>% 
  ungroup() %>%
  complete(ageband, sex, mode, fill = list(n = 0)) %>% 
  group_by(ageband, sex) %>% 
  mutate(percent = round(n/sum(n) * 100, 1))

results <- df %>%
  mutate(mode = fct_recode(mode, 
                           "Other" = "Bus or Coach",
                           "Other" = "Goods Vehicle",
                           "Other" = "Other Vehicle",
                           "Other" = "Taxi")) %>% 
  group_by(ageband, sex, mode) %>% 
  summarise(n = n()) %>% 
  ungroup() %>%
  complete(ageband, sex, mode, fill = list(n = 0)) %>% 
  spread(mode, n) %>% 
  mutate(Total = rowSums(.[3:6])) %>%
  gather(mode, n, -ageband, -sex) %>% 
  ungroup() %>% 
  mutate(percent = n/sum(n),
         ageband = fct_relevel(ageband, "0-4", "5-9"),
         n_adjusted = ifelse(sex == "Male", n*-1, n),
         mode = factor(mode, 
                       levels = c("Car","Pedal Cycle", "Pedestrian",
                                  "Powered 2 Wheeler", "Other", "Total")))

# plot data  ---------------------------
p <- ggplot(results, aes(ageband, colour = sex)) +
  geom_linerange(data = filter(results, sex == "Male"), 
                 aes(ymin = -2, ymax = -2+n_adjusted), size = 3, alpha = 0.8) +
  geom_linerange(data = filter(results, sex == "Female"), 
                 aes(ymin = 2, ymax = 2+n_adjusted), size = 3, alpha = 0.8)+
  geom_text(aes(x = ageband, y = 0, label = ageband), size = 3, color = "#757575") +
  scale_colour_manual(values = c("#d8b365", "#5ab4ac"), labels = c("Female", "Male"),
                      guide = guide_legend(keyheight = unit(1, units = "mm"), 
                                           label.position = "left", 
                                           nrow = 1,
                                           reverse = TRUE)) +
  scale_y_continuous(breaks = c(c(-10, -5, 0) + -2, c(0, 5, 10) + 2),
                     labels = c("10%", "5%", "0%", "0%", "5%", "10%")) +
  labs(x = NULL, y = NULL, colour = NULL,
       caption = "Source: Greater Manchester Police  |  @traffordDataLab") +
  facet_wrap(~mode, nrow = 3) +
  coord_flip() +
  theme_lab() +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm"),
        panel.spacing = unit(0.5, "lines"),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.2, color = "#757575"),
        panel.grid.major.y = element_blank(),
        panel.ontop = TRUE,
        strip.text = element_text(size = 11, face = "bold", hjust = 0.030),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_blank(),
        legend.position = "bottom")

# save plot / data  ---------------------------
ggsave(file = "output/figures/fig3.svg", scale = 1.6, width = 8, height = 6)

p + labs(title = "Casualties in Trafford by sex, age band and mode of travel, 2016") +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm"),
        plot.title = element_text(face = "bold", vjust = 10, hjust = 0.5))

ggsave(file = "output/figures/fig3.png", scale = 1.6, width = 8, height = 6)

results %>% 
  select(-n_adjusted) %>% 
  write_csv("output/data/fig3.csv")
  




