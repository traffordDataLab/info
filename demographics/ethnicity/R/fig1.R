## Ethnicity: proportion of usual residents by ethnic group and ward

# load R packages  ---------------------------
library(tidyverse) ; library(sf) ; library(svglite)

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data  ---------------------------
# source: https://www.nomisweb.co.uk/census/2011/ks201ew
df <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_608_1.data.csv?date=latest&geography=1237320482...1237320496,1237320498,1237320497,1237320499...1237320502&rural_urban=0&cell=100,200,300,400,500&measures=20100&select=date_name,geography_name,geography_code,rural_urban_name,cell_name,measures_name,obs_value,obs_status_name")

# manipulate data ---------------------------
results <- df %>%
  select(area_code = GEOGRAPHY_CODE, area_name = GEOGRAPHY_NAME, group = CELL_NAME, n = OBS_VALUE) %>% 
  spread(group, n) %>% 
  rename(Asian = `Asian/Asian British`,
         Black = `Black/African/Caribbean/Black British`, 
         Mixed = `Mixed/multiple ethnic groups`, 
         Other = `Other ethnic group`) %>% 
  gather(ethnic_group, n, -area_code, -area_name) %>% 
  group_by(area_code, area_name) %>% 
  mutate(percent = (n/sum(n)))

# plot data ---------------------------
pal <- c("Asian" = "#FF0000", "Black" = "#55FF00", "Mixed" = "#FFAA01", "Other" = "#8A5B47", "White" = "#82B3FF")

ggplot(data = results, mapping = aes(x = percent, fill = ethnic_group)) + 
  geom_bar(position = "fill") +
  scale_y_continuous(expand = c(0, 0), labels = scales::percent) +
  labs(title = "Better Cut Diamonds have Better Clarity",
       subtitle = "Share of Diamonds with Different Qualities by Clarity of Cut",
       caption = "The Source of Diamond Data",
       x = "Clarity",
       y = "Count")

ggplot(results, aes(x = fct_rev(area_name), y = percent, fill = ethnic_group)) + 
  geom_col(position = "stack", width = 0.3, alpha = 0.8) +
  scale_y_continuous(expand = c(0, 0), labels = scales::percent) +
  scale_fill_manual(values = pal,
                    guide = guide_legend(keyheight = unit(2, units = "mm"), 
                                         keywidth = unit(12, units = "mm"), 
                                         label.position = "top", 
                                         label.vjust = 10,
                                         nrow = 1)) +
  coord_flip() +
  labs(x = NULL, y = NULL, fill = NULL, title = NULL,
       caption = "Source: 2011 Census  |  @traffordDataLab") +
  theme_lab() +
  theme(panel.grid.major.y = element_blank(),
        plot.margin=unit(c(1,1,2,1), "cm"),
        legend.position = "bottom")

# save plot / data  ---------------------------
ggsave(file = "output/figures/fig1.svg", width = 10, height = 8)
ggsave(file = "output/figures/fig1.png", width = 10, height = 8)

results %>% mutate(percent = round(percent*100,1)) %>% 
  write_csv("output/data/fig1.csv")
