## Population: population pyramid for wards by 5-year interval ##

# load R packages  ---------------------------
library(tidyverse) ; library(readxl) ; library(ggplot2)

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data ---------------------------
# ward level mid-year population estimates (2016)
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/wardlevelmidyearpopulationestimategenderperimental/mid2016sape19dt8/sape19dt8mid2016ward2016syoaestimatesunformatted.zip"
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
  mutate(age = as.integer(age))
rm(url, males, females)

population$ageband <- cut(population$age,
                          breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,120),
                          labels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
                                     "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
                                     "70-74","75-79","80-84","85-89","90+"),
                          right = FALSE)

results <- population %>% 
  group_by(area_code, area_name, gender, ageband) %>% 
  summarise(n = sum(n)) %>% 
  ungroup() %>%
  group_by(area_code) %>% 
  mutate(percent = round(n/sum(n)*100, 1))

# plot data ---------------------------
ggplot() +
  geom_bar(aes(ageband, n, group = gender, fill = gender), stat = "identity", 
           filter(results, gender == "Female"), alpha = 0.6) +
  geom_bar(aes(ageband, -n, group = gender, fill = gender), stat = "identity", 
           filter(results, gender == "Male"), alpha = 0.6) +
  scale_y_continuous(labels = abs, limits = max(results$n) * c(-1,1)) +
  scale_fill_manual(values = c("#d8b365", "#5ab4ac"), labels = c("Female", "Male")) +
  guides(fill = guide_legend(keywidth = 4, keyheight = 0.1, reverse = TRUE)) +
  coord_flip() +
  facet_wrap(~area_name, ncol = 7, strip.position = "top")  + 
  labs(x = NULL, y = NULL, 
       title = "Population pyramids, mid-2016",
       caption = "Source: ONS  |  @traffordDataLab", fill = NULL) +
  theme_lab() +
  theme(panel.spacing = unit(0.2, "lines"),
        panel.grid.major.y = element_blank(),
        axis.text.x = element_text(size = 8, hjust = 1),
        axis.text.y = element_text(size = 8),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.02),
        strip.text = element_text(size = 8, face = "bold", angle = 0, hjust = 0.5, vjust = 1),
        legend.position = "bottom")

# save plot ---------------------------
ggsave(file = "output/figures/pyramids.svg", width = 10, height = 9)

