#Economic inactivity and unemployment on Trafford wards
#Source: Census 2011, QS601EW - Economic activity, ONS
#Publisher: https://www.nomisweb.co.uk/census/2011/qs601ew
# All usual residents 16 to 74

# load R packages  ---------------------------
library(tidyverse) ; library(forcats) ; library(ggplot2); library(svglite)

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data  ---------------------------
# 2011 Wards within Trafford, 5 categories under Economically inactive, Total
df <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_556_1.data.csv?date=latest&geography=1237320482...1237320496,1237320498,1237320497,1237320499...1237320502&rural_urban=0&cell=11...15&measures=20100&select=date_name,geography_name,geography_code,rural_urban_name,cell_name,measures_name,obs_value,obs_status_name")
         
results <- df %>% 
  select(date = DATE_NAME, area_code = GEOGRAPHY_CODE, area_name = GEOGRAPHY_NAME, measure = CELL_NAME, count = OBS_VALUE) %>%
  group_by(area_name) %>%
  mutate(total=sum(count), percent=round(count*100/total,1)) %>%
  select(-c(total)) %>%
  ungroup() %>%
  mutate(measure = fct_recode(measure, "retired" = "Economically inactive: Retired",
                              "student" = "Economically inactive: Student (including full-time students)",
                              "looking after\nhome" = "Economically inactive: Looking after home or family",
                              "long-term\nsick/disable" = "Economically inactive: Long-term sick or disabled",
                              "other" = "Economically inactive: Other")) %>%
  mutate(measure = factor(measure, levels = c("retired", "student", "looking after\nhome","long-term\nsick/disable","other")))

results$area_name = reorder(results$area_name, ifelse(results$measure == "retired", -results$percent, NA), na.rm = TRUE)

            
p <- ggplot(results, aes(percent, area_name)) +
  geom_point(aes(colour = measure), size = 2) +
  facet_grid(. ~ measure, scales = "free_x") +
  scale_x_continuous(breaks = function(x){floor(unlist(lapply(x,max))/10)*10}, labels = function(x){ paste0(x, "%") }, limits = c(0, NA)) +
  labs(x = "percentage of economically inactive", y = NULL,
       title = NULL,
       caption = "Source: Census 2011  |  @traffordDataLab") +
  theme_lab() + 
  theme(panel.grid.major.x = element_line(linetype = "dotted", colour = "#757575"),
        panel.grid.major.y = element_line(linetype = "solid", colour = "#cccccc"),
        legend.position = "none",
        strip.text.x = element_text(size = 9, face = "bold", hjust = 0),
        axis.text.y = element_text(hjust = 0),
        panel.spacing.x = unit(0.02, "npc"))


ggsave(file = "fig5.svg", width = 8, height = 7)

p + labs(title = "Reasons for economic inactivity in Trafford wards") +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm"),
        plot.title = element_text(face = "bold", vjust = 15, hjust = 1))

ggsave(file = "fig5.png", width = 8, height = 9)

# save plot / data  ---------------------------
results <- results %>% mutate(measure = gsub("\n", " ", measure))
write_csv(results, "fig5.csv")

         