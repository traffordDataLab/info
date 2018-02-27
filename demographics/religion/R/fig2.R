## Religion: Religious affiliation in Trafford, 2011 ##

# Source: Table KS209EW, 2011 Census
# Publisher: nomis
# Publisher URL: https://www.nomisweb.co.uk/census/2011/ks209ew
# Licence: Open Government Licence

# load R packages  ---------------------------
library(tidyverse)

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data  ---------------------------
df <- read_csv("https://www.nomisweb.co.uk/api/v01/dataset/nm_616_1.bulk.csv?time=latest&measures=20100&rural_urban=total&geography=TYPE464")

# manipulate data ---------------------------
df_tidy <- df %>% 
  select(-c(date, `Rural Urban`), area_code = `geography code`, area_name = geography) %>% 
  filter(area_code == "E08000009") %>% 
  gather(religion, n, -area_code, -area_name) %>% 
  mutate(religion = str_replace(religion, "Religion: ", "")) %>% 
  mutate(religion = str_replace(religion, "; measures: Value", ""))

# plot data ---------------------------
results <- df_tidy %>% 
  filter(religion %in% c("Buddhist", "Christian", "Hindu", "Jewish", "Muslim", "Other religion", "Sikh", "Has religion")) %>% 
  group_by(religion) %>% 
  spread(religion, n) %>% 
  select(total = "Has religion", everything()) %>%
  summarise_at(vars(Buddhist:Sikh), funs(round(100 * (. / total),1))) %>% 
  gather(religion, percent) %>% 
  arrange(desc(percent)) %>% 
  mutate(religion = factor(religion, levels = religion))

ggplot(results, aes(percent, religion)) +
  geom_segment(aes(x = 0, y = religion, xend = percent, yend = religion), color = "#f0f0f0") +
  geom_point(colour = "#fc6721", size = 7) +
  geom_text(aes(label = paste0(percent, "%"), fontface = "bold"), color = "white", size = 2) + 
  scale_x_continuous(labels = function(x){ paste0(x, "%") }, limits=c(-5, 100), expand = c(0,0)) +
  labs(x = NULL, y = NULL,
       title = NULL,
       caption = "Source: Census 2011, Office for National Statistics  |  @traffordDataLab") +
  theme_lab() + 
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(hjust = 0)) 

# save plot / data  ---------------------------
ggsave(file = "output/figures/fig2.svg", width = 8, height = 8)
ggsave(file = "output/figures/fig2.png", width = 8, height = 8)

write_csv(results, "output/data/fig2.csv")
