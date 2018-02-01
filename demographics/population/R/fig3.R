## Population: old-age dependency ratio ##

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
  mutate(age = as.integer(age))
rm(url, males, females)

population$ageband <- cut(population$age,
                          breaks = c(15,65,120),
                          labels = c("15-64","65+"),
                          right = FALSE)

results <- population %>% 
  filter(!is.na(ageband)) %>% 
  group_by(area_code, area_name, ageband) %>% 
  summarise(n = sum(n)) %>% 
  ungroup() %>%
  group_by(area_code) %>% 
  select(area_code, area_name, ageband, n) %>% 
  spread(ageband, n) %>% 
  mutate(ratio = round((`65+`/`15-64`)*100,0)) %>% 
  arrange(desc(ratio)) %>%  ungroup() %>%
  mutate(area_name = factor(area_name, levels = area_name))

# plot data ---------------------------
ggplot(results, aes(ratio, area_name)) +
  geom_segment(aes(x = 0, y = area_name, xend = ratio, yend = area_name), color = "#f0f0f0") +
  geom_point(colour = "#fc6721", size = 4) +
  geom_text(aes(label = paste0(ratio, "%"), fontface = "bold"), color = "white", size = 2) + 
  scale_x_continuous(labels = function(x){ paste0(x, "%") }, limits=c(0, 50), expand = c(0,0)) + # adjust limits
  labs(x = NULL, y = NULL,
       title = NULL,
       caption = "Source: ONS  |  @traffordDataLab") +
  theme_lab() + 
  theme(panel.grid.major = element_blank(),
        axis.text.y = element_text(hjust = 0))

# save plot / data  ---------------------------
ggsave(file = "output/figures/fig3.svg", width = 6, height = 6)
ggsave(file = "output/figures/fig3.png", width = 6, height = 6)

write_csv(results, "output/data/fig3.csv")
