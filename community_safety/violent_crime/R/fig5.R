## Violence and sexual offences: proportion of total crime (excluding ASB) ##

# load R packages  ---------------------------
library(tidyverse); library(ggplot2); library(svglite)

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data  ---------------------------
df <- read_csv("https://github.com/traffordDataLab/open_data/raw/master/police_recorded_crime/data/trafford.csv")

# manipulate data ---------------------------
gm <- read_csv("https://github.com/traffordDataLab/open_data/raw/master/police_recorded_crime/data/gm.csv.gz") %>% 
  filter(month >= "2017-11-01" & category != "Anti-social behaviour") %>% 
  group_by(category) %>%
  summarise(n = n()) %>% 
  arrange(desc(n)) %>%
  mutate(area_name = "Greater Manchester", 
         percent = round(n/sum(n)*100, 0)) %>% 
  filter(category == "Violence and sexual offences") %>% 
  arrange(desc(percent)) %>% 
  select(area_name, category, n, percent)

trafford <- df %>% 
  filter(month >= "2017-11-01" & category != "Anti-social behaviour") %>% 
  group_by(category) %>%
  summarise(n = n()) %>% 
  arrange(desc(n)) %>%
  mutate(area_name = "Trafford", 
         percent = round(n/sum(n)*100, 0)) %>% 
  filter(category == "Violence and sexual offences") %>% 
  arrange(desc(percent)) %>% 
  select(area_name, category, n, percent)

results <- df %>% 
  filter(month == "2017-11-01" & category != "Anti-social behaviour") %>% 
  group_by(area_name, category) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(area_name) %>%
  arrange(desc(n)) %>%
  mutate(percent = round(n/sum(n)*100, 0)) %>% 
  filter(category == "Violence and sexual offences") %>% 
  arrange(desc(percent)) %>% 
  ungroup() %>% 
  mutate(area_name = factor(area_name, levels = area_name)) %>% 
  add_row(area_name = "Trafford", category = "Violence and sexual offences", n = trafford$n, percent = trafford$percent) %>% 
  add_row(area_name = "Greater Manchester", category = "Violence and sexual offences", n = gm$n, percent = gm$percent)

# plot data ---------------------------
ggplot(results, aes(percent, area_name)) +
  geom_segment(aes(x = 0, y = area_name, xend = percent, yend = area_name), color = "#f0f0f0") +
  geom_point(colour = "#fc6721", size = 4) +
  geom_text(aes(label = paste0(percent, "%"), fontface = "bold"), color = "white", size = 2) + 
  scale_x_continuous(labels = function(x){ paste0(x, "%") }, limits=c(0, 50), expand = c(0,0)) + # adjust limits
  labs(x = "percentage of all crime", y = NULL,
       title = NULL,
       caption = "Source: data.police.uk  |  @traffordDataLab") +
  theme_lab() + 
  theme(panel.grid.major = element_blank(),
        axis.text.y = element_text(hjust = 0))

# save plot / data  ---------------------------
ggsave(file = "output/figures/fig5.svg", width = 6, height = 6)
ggsave(file = "output/figures/fig5.png", width = 6, height = 6)
write_csv(results, "output/data/fig5.csv")