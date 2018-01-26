## Population: population pyramid for Trafford ##

# load R packages  ---------------------------
library(tidyverse) ; library(readxl) ; library(ggplot2)

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data ---------------------------
# ward level mid-year population estimates (2016)
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/wardlevelmidyearpopulationestimatesexperimental/mid2016sape19dt8/sape19dt8mid2016ward2016syoaestimatesunformatted.zip"
download.file(url, dest = "output/data/sape19dt8mid2016ward2016syoaestimatesunformatted.zip")
unzip("output/data/sape19dt8mid2016ward2016syoaestimatesunformatted.zip", exdir = "output/data")
file.remove("output/data/sape19dt8mid2016ward2016syoaestimatesunformatted.zip")

males <- read_xls("output/data/SAPE19DT8-mid-2016-ward-2016-syoa-estimates-unformatted.xls", sheet = 3, skip = 3) %>% 
  filter(`Local Authority` == "Trafford") %>% 
  rename(`90` = `90+`) %>% 
  mutate(gender = "Male") %>% 
  select(area_code = `Ward Code 1`, area_name = `Ward Name 1`, gender, everything(), -`Local Authority`)
females <- read_xls("output/data/SAPE19DT8-mid-2016-ward-2016-syoa-estimates-unformatted.xls", sheet = 4, skip = 3) %>% 
  filter(`Local Authority` == "Trafford") %>% 
  rename(`90` = `90+`) %>% 
  mutate(gender = "Female") %>% 
  select(area_code = `Ward Code 1`, area_name = `Ward Name 1`, gender, everything(), -`Local Authority`)

# manipulate data  ---------------------------
population <- do.call("rbind", list(males, females)) %>% 
  select(-`All Ages`) %>% 
  gather(age, n, -area_code, -area_name, -gender) %>% 
  mutate(age = as.integer(age)) %>% 
  group_by(gender, age) %>% 
  summarise(n = sum(n))
rm(url, males, females)

# plot data  ---------------------------
ggplot() +
  geom_bar(aes(age, n, group = gender, fill = gender), stat = "identity", 
           filter(population, age != 90 & gender == "Female"), alpha = 0.8) +
  geom_bar(aes(age, -n, group = gender, fill = gender), stat = "identity", 
           filter(population, age != 90 & gender == "Male"), alpha = 0.8) +
  scale_y_continuous(labels = abs, limits = max(population$n) * c(-1,1)) +
  scale_fill_manual(values = c("#d8b365", "#5ab4ac")) +
  guides(fill = guide_legend(keywidth = 4, keyheight = 0.1, reverse = TRUE)) +
  coord_flip() +
  labs(x = "age", y = "population", 
       caption = "Source: ONS  |  @traffordDataLab", fill = NULL) +
  theme_lab() +
  theme(axis.text.x = element_text(size = 8, hjust = 1),
        axis.text.y = element_text(size = 8),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.02),
        legend.position = "bottom")

ggsave(file = "output/figures/fig1.svg", width = 6, height = 6)
ggsave(file = "output/figures/fig1.png", width = 6, height = 6)
