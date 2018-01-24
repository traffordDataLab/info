## Violent crime: Count of violence and sexual offences in Trafford by month ##

# load R packages  ---------------------------
library(tidyverse); library(ggplot2); library(svglite)

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data  ---------------------------
df <- read_csv("https://github.com/traffordDataLab/open_data/raw/master/police_recorded_crime/data/trafford.csv") %>% 
  filter(category == "Violence and sexual offences")

# manipulate data ---------------------------
results <- df %>% 
  filter(month >= "2016-11-01") %>% 
  group_by(month) %>% 
  count() 

# plot data ---------------------------
ggplot(results, aes(month, n)) +
  geom_col(fill = "#fc6721", alpha = 0.8) +
  scale_x_date(date_labels = "%b-%y", date_breaks = "1 month", expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = NULL, y = NULL,
       caption = "Source: data.police.uk  |  @traffordDataLab") +
  theme_lab() +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x  = element_text(angle = 90))

# save plot / data  ---------------------------
ggsave(file = "output/figures/fig1.svg", width = 6, height = 3)
ggsave(file = "output/figures/fig1.png", width = 6, height = 3)
write_csv(results, "output/data/fig1.csv")
