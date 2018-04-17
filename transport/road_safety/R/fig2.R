## Road Safety: Casualties in Trafford by sex and mode of travel, 2016 ##

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
df %>% # total casualties by sex
  group_by(sex) %>% 
  summarise(n = n()) %>% 
  mutate(percent = n/sum(n)*100)

results <- df %>%
  group_by(sex, mode) %>% 
  summarise(n = n()) %>% 
  ungroup() %>%
  complete(sex, mode, fill = list(n = 0)) %>% 
  spread(mode, n) %>% 
  mutate(Total = rowSums(.[2:8])) %>%
  gather(mode, n, -sex) %>% 
  group_by(mode) %>% 
  mutate(percent = n/sum(n),
         sex = factor(sex, 
                      levels = c(sex, levels = c("Female, Male"))))

# plot data  ---------------------------
p <- ggplot(results, aes(x = fct_rev(mode), y = percent, fill = sex)) + 
  geom_col(position = "stack",  alpha = 0.8) +
  scale_y_continuous(trans = "reverse",  expand = c(0, 0), breaks = c(0.5, NA), labels = scales::percent) +
  scale_fill_manual(values = c("#d8b365", "#5ab4ac"), labels = c("Female", "Male"),
                      guide = guide_legend(keyheight = unit(1, units = "mm"), 
                                           label.position = "left", 
                                           nrow = 1,
                                           reverse = TRUE)) +
  coord_flip() +
  labs(x = NULL, y = NULL, fill = NULL, title = NULL,
       caption = "Source: Greater Manchester Police  |  @traffordDataLab") +
  theme_lab() +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm"),
        panel.grid.major.y = element_blank(),
        panel.ontop = TRUE,
        axis.text.y = element_text(hjust = 0),
        legend.position = c(0.9, 1.025))

# save plot / data  ---------------------------
ggsave(file = "output/figures/fig2.svg", plot = p, scale = 1.2, width = 8, height = 5)

p + labs(title = "Casualties in Trafford by sex and mode of travel, 2016") +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm"),
        plot.title = element_text(face = "bold", vjust = 10, hjust = 0.5))

ggsave(file = "output/figures/fig2.png", scale = 1.2, width = 8, height = 5)

write_csv(results, "output/data/fig2.csv")
  




