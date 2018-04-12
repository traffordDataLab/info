## Road Safety: Casualty severity by mode of travel, 2016 ##

# Source: Greater Manchester Police
# Publisher URL: https://data.gov.uk/dataset/road-accidents-safety-data
# Licence: Open Government Licence

# load R packages  ---------------------------
library(tidyverse) ; library(ggplot2)

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data  ---------------------------
df <- read_csv("https://www.trafforddatalab.io/open_data/road_casualties/2016/STATS19_casualty_data_2016.csv") %>% 
  filter(area_name == "Trafford")

# manipulate data ---------------------------
results <- df %>% 
  group_by(mode, severity) %>% 
  summarise(n = n()) %>%
  ungroup() %>%
  complete(mode, severity, fill = list(n = 0)) %>% 
  spread(severity, n) %>% 
  mutate(total = rowSums(.[2:4])) %>% 
  add_row(mode = "Total", 
          Fatal = sum(.$Fatal),
          Serious = sum(.$Serious),
          Slight = sum(.$Slight),
          total = sum(.$total)) %>% 
  mutate(total_percent = (total/.[.$mode == "Total", ]$total)*100) %>% 
  setNames(tolower(names(.))) 

# save plot / data  ---------------------------
write_csv(results, "output/data/tab1.csv")
