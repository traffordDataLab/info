## Religion: Religious affiliation by ward, 2011 ##

# Source: Table KS209EW, 2011 Census
# Publisher: nomis
# Publisher URL: https://www.nomisweb.co.uk/census/2011/ks209ew
# Licence: Open Government Licence

# load R packages  ---------------------------
library(tidyverse)

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data  ---------------------------
df <- read_csv("https://www.nomisweb.co.uk/api/v01/dataset/nm_616_1.bulk.csv?time=latest&measures=20100&rural_urban=total&geography=TYPE295")

# manipulate data ---------------------------
df_tidy <- df %>% 
  select(-c(date, `Rural Urban`), area_code = `geography code`, area_name = geography) %>% 
  filter(area_code %in% c("E05000819", "E05000820", "E05000821", "E05000822", "E05000823", "E05000824", 
                          "E05000825", "E05000826", "E05000827", "E05000828", "E05000829", "E05000830", 
                          "E05000831", "E05000832", "E05000833", "E05000834", "E05000835", "E05000836",
                          "E05000837", "E05000838", "E05000839")) %>% 
  gather(religion, n, -area_code, -area_name) %>% 
  mutate(religion = str_replace(religion, "Religion: ", "")) %>% 
  mutate(religion = str_replace(religion, "; measures: Value", ""))

# plot data ---------------------------
results <- df_tidy %>% 
  filter(religion %in% c("Buddhist", "Christian", "Hindu", "Jewish", "Muslim", "Other religion", "Sikh", "Has religion")) %>% 
  group_by(area_code, area_name, religion) %>% 
  spread(religion, n) %>% 
  select(area_code, area_name, total = "Has religion", everything()) %>% View()


## to complete !!##

# save plot / data  ---------------------------
ggsave(file = "output/figures/fig2.svg", width = 6, height = 6)
ggsave(file = "output/figures/fig2.png", width = 6, height = 6)

write_csv(results, "output/data/fig2.csv")