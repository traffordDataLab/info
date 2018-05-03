## Life expectancy: Life expectancy at birth in Trafford's wards ##

# Dataset: Life expectancy at birth 2011-2015
# Source: ONS
# Publisher: PHE Local Health
# Publisher URL: http://www.localhealth.org.uk/
# Licence: Open Government Licence

# load R packages  ---------------------------
library(tidyverse) ; library(readxl) ; library(stringr) ; library(ggplot2)

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load and manipulate data  ---------------------------
results <- read_excel("data/ward_life_expectancy_2011-2015.xls", skip = 1) %>% 
  separate(`Ward 2016` , c('area_code', 'area_name'), sep=" - ") %>% 
  mutate(area_name = str_sub(area_name, 1, str_length(area_name)-10)) %>% 
  select(area_code, area_name,
         Female = `Life expectancy at birth for females, 2011- 2015 (years)`,
         Male = `Life expectancy at birth for males, 2011- 2015 (years)`) %>% 
  gather(sex, value, Male, Female) %>% 
  mutate(sex = factor(sex, levels = c("Male", "Female"), ordered = T))

# plot data ---------------------------
p <- ggplot(results, aes(value, fct_reorder(area_name, value))) +
  geom_line(color = "#f0f0f0") +
  geom_point(aes(color = sex), size = 6) +
  geom_vline(aes(xintercept = 79.8), colour = "#5ab4ac", linetype = "dashed") +
  geom_text(aes(x = 79.8, label = "\nAverage life expectancy", y = 19), colour = "#5ab4ac", angle = 90, size = 3) +
  geom_vline(aes(xintercept = 83.5), colour = "#d8b365", linetype = "dashed") +
  geom_text(aes(x = 83.5, label = "\nAverage life expectancy", y = 3), colour = "#d8b365", angle = 90, size = 3) +
  geom_text(aes(label = value), size = 2.5, colour = "#FFFFFF", fontface = "bold") +
  scale_x_continuous(limits = c(75, 90)) +
  scale_colour_manual(values = c("Female" = "#d8b365", "Male" = "#5ab4ac")) +
  labs(x = NULL, y = NULL,
       title = NULL,
       caption = "Source: ONS |  @traffordDataLab")+
  theme_lab() +
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(hjust = 0),
        legend.position = "bottom", 
        legend.title = element_blank()) +
  guides(colour = guide_legend(override.aes = list(size = 4)))

# save plot / data  ---------------------------
ggsave(file = "output/figures/fig2.svg", scale = 1, width = 10, height = 8)

p + labs(title = "Average life expectancy at birth in Trafford's wards, 2011-2015") +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm"),
        plot.title = element_text(face = "bold", vjust = 8, hjust = 0.5))
ggsave(file = "output/figures/fig2.png", scale = 1.3, width = 10, height = 8)

write_csv(results, "output/data/fig2.csv")
