## Population: population pyramid ##

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

# merge data  ---------------------------
population <- do.call("rbind", list(males, females)) %>% 
  select(-`All Ages`) %>% 
  gather(age, n, -area_code, -area_name, -gender) %>% 
  mutate(age = as.integer(age))
rm(url, males, females)

# create age bands  ---------------------------
age_bands <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
               "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
               "70-74","75-79","80-84","85-89","90+")
population$age_band <- cut(population$age,
                           breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,150),
                           labels = age_bands,
                           right = FALSE)
population_banded <- population %>% 
  group_by(area_code, area_name, gender, age_band) %>% 
  summarise(n = sum(n))

# plot data: individual ward  ---------------------------
filter(population_banded, area_name == "Clifford" & age_band != "90+") %>% 
  ggplot(mapping = aes(x = age_band, fill = gender, y = ifelse(test = gender == "Male", yes = -n, no = n))) +
  geom_bar(stat = "identity", alpha = 0.7) +
  scale_y_continuous(labels = abs, limits = max(filter(population_banded, area_name == "Clifford")$n) * c(-1,1)) +
  scale_fill_manual(values = c("#d8b365", "#5ab4ac")) +
  guides(fill = guide_legend(keywidth = 3, keyheight = 1, reverse = TRUE)) +
  coord_flip() +
  labs(x = NULL, y = NULL, 
       caption = "Source: ONS  |  @traffordDataLab", fill = NULL) +
  theme_lab() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "bottom")

# plot data: small multiples ---------------------------
filter(population_banded, age_band != "90+") %>% 
  ggplot(mapping = aes(x = age_band, fill = gender, y = ifelse(test = gender == "Male", yes = -n, no = n))) +
  geom_bar(stat = "identity", alpha = 0.7) +
  scale_y_continuous(labels = abs, limits = max(population_banded$n) * c(-1,1)) +
  scale_fill_manual(values = c("#d8b365", "#5ab4ac")) +
  coord_flip() +
  guides(fill = guide_legend(keywidth = 3, keyheight = 0.4, reverse = TRUE)) +
  facet_wrap(~area_name, nrow = 7, strip.position = "top") + 
  labs(x = NULL, y = NULL, 
       caption = "Source: ONS  |  @traffordDataLab", fill = NULL) +
  theme_lab() +
  theme(panel.spacing = unit(0.2, "lines"),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 8, hjust = 1),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(hjust = 0.02),
        plot.subtitle = element_text(hjust = 0.02, vjust = 70),
        strip.text = element_text(size = 8, face = "bold", angle = 0, hjust = 0.5, vjust = 1),
        legend.position = "bottom")

# plot data: small multiples (all ages) ---------------------------

ggplot() +
  geom_bar(aes(age, n, group = gender, fill = gender), stat = "identity", 
           filter(population, age != 90 & gender == "Female")) +
  geom_bar(aes(age, -n, group = gender, fill = gender), stat = "identity", 
           filter(population, age != 90 & gender == "Male")) +
  scale_y_continuous(labels = abs, limits = max(population$n) * c(-1,1)) +
  scale_fill_manual(values = c("#d8b365", "#5ab4ac")) +
  guides(fill = guide_legend(keywidth = 4, keyheight = 0.1, reverse = TRUE)) +
  coord_flip() +
  facet_wrap(~area_name, ncol = 7, strip.position = "top")  + 
  labs(x = "age", y = "population", 
       title = "Mid-2016 ward level population estimates for Trafford",
       caption = "Source: ONS  |  @traffordDataLab", fill = NULL) +
  theme_lab() +
  theme(panel.spacing = unit(0.2, "lines"),
        axis.text.x = element_text(size = 8, hjust = 1),
        axis.text.y = element_text(size = 8),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.02),
        strip.text = element_text(size = 8, face = "bold", angle = 0, hjust = 0.5, vjust = 1),
        legend.position = "bottom")

ggsave(file = "output/figures/fig1.svg", width = 10, height = 6)
ggsave(file = "output/figures/fig1.png", width = 10, height = 6)
