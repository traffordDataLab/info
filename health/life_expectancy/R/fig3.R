## Life expectancy: Life expectancy and healthy life expectancy by ward, 2009-13 ##

# Dataset: Health state life expectancy by 2011 Census wards, England and Wales: 2009 to 2013
# Source: ONS
# Publisher: ONS
# Publisher URL: https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/healthandlifeexpectancies/articles/healthstatelifeexpectancyby2011censuswardsenglandandwales/2009to2013
# Licence: Open Government Licence

# load R packages  ---------------------------
library(tidyverse) ; library(ggplot2) ; library(ggrepel)

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

df <- read_csv("https://www.ons.gov.uk/visualisations/dvc479/scatter-wards/data.csv", skip = 10) %>% 
  filter(`Local authority name` == "Trafford") %>% 
  select(area_code = `2011 Census Ward code`,
         area_name = `2011 Census Ward name`,
         LE = `LE (years)`,
         HLE = `HLE (years)`,
         sex = Sex)

# inequality in health life expectancy
select(df, sex, HLE) %>% 
  group_by(sex) %>% 
  summarise(min = min(HLE),
            max = max(HLE),
            inequality = max-min)

# plot data ---------------------------
p <- ggplot(df, aes(x = LE, y = HLE, colour = factor(sex))) + 
  geom_point() + 
  scale_colour_manual(values = c("Female" = "#d8b365", "Male" = "#5ab4ac")) +
  scale_x_continuous(limits = c(75,90), breaks = seq(75,90,5)) +
  scale_y_continuous(limits = c(55,75), breaks = seq(55,75,5)) +
  geom_text_repel(aes(LE, HLE, label = area_name, color = factor(sex)), size = 3) +
  facet_grid(sex ~ .) +  
  labs(title = NULL, 
       subtitle = NULL, 
       caption = "Source: ONS  |  @traffordDataLab", 
       x = "Life expectancy (years)",
       y = "Healthy life expectancy (years)") + 
  theme_lab() + 
  theme(panel.spacing = unit(3, "lines"),
        panel.grid.major.x = element_blank(),
        legend.position = "none")  

# save plot / data  ---------------------------
ggsave(file = "output/figures/fig3.svg", scale = 1, width = 10, height = 8)

p + labs(title = "Life expectancy and healthy life expectancy at birth by ward, 2009-2013") +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm"),
        plot.title = element_text(face = "bold", vjust = 8, hjust = 0.5))
ggsave(file = "output/figures/fig3.png", scale = 1.3, width = 10, height = 8)

write_csv(df, "output/data/fig3.csv")
