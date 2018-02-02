## Population: population pyramid for Trafford by single year ##

# load R packages  ---------------------------
library(tidyverse) ; library(stringr) ; library(forcats) ; library(ggplot2)

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data ---------------------------
df <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=1879048225&date=latestMINUS10,latest&gender=1,2&age=101...191&measures=20100&select=date_name,geography_name,geography_code,gender_name,age_name,measures_name,obs_value,obs_status_name")

# manipulate data  ---------------------------
results <- df %>% 
  select(year = DATE_NAME, area_code = GEOGRAPHY_CODE, area_name = GEOGRAPHY_NAME, 
         gender = GENDER_NAME, age = AGE_NAME, n = OBS_VALUE) %>% 
  mutate(age = str_replace_all(age, "Age.", ""),
         age = str_trim(age),
         age = fct_recode(age, "90" = "90+"),
         age = factor(age, levels = c(0:90))) %>% 
  group_by(year, gender, age) %>% 
  summarise(n = sum(n)) %>% 
  group_by(year) %>% 
  mutate(percent = round(n/sum(n)*100, 1))
  
# plot data  ---------------------------
ggplot() +
  geom_bar(aes(age, n, group = gender, fill = gender), stat = "identity", 
           filter(results, year == "2016" & gender == "Female"), alpha = 0.6) +
  geom_bar(aes(age, -n, group = gender, fill = gender), stat = "identity", 
           filter(results, year == "2016" & gender == "Male"), alpha = 0.6) +
  geom_line(aes(age, n, group = gender), stat = "identity", 
            filter(results, year == "2006" & gender == "Female"), size = 1, colour = "#d8b365", alpha = 1) +
  geom_line(aes(age, -n, group = gender), stat = "identity", 
            filter(results, year == "2006" & gender == "Male"), size = 1, colour = "#5ab4ac", alpha = 1) +
  scale_x_discrete(breaks = seq(0, 90, by = 5), labels = c(seq(0, 85, by = 5), "90+")) +
  scale_y_continuous(labels = abs, limits = max(results$n) * c(-1,1), breaks = seq(-1500, 1500, by = 500)) +
  scale_fill_manual(values = c("#d8b365", "#5ab4ac"), labels = c("Female", "Male")) +
  coord_flip() +
  labs(x = NULL, y = NULL, 
       subtitle = "male  female",
       caption = "Source: ONS  |  @traffordDataLab") +
  theme_lab() +
  theme(panel.grid.major.y = element_blank(),
        axis.text.x = element_text(size = 9, hjust = 1),
        axis.text.y = element_text(size = 9),
        axis.ticks = element_blank(),
        plot.subtitle = element_text(hjust = 0.5, size = 9),
        legend.position = "none")

# save plot / data  ---------------------------
ggsave(file = "output/figures/fig1.svg", width = 5, height = 5)
ggsave(file = "output/figures/fig1.png", width = 5, height = 5)

write_csv(results, "output/data/fig1.csv")

