## Life expectancy: Trend in the inequality in life expectancy at birth ##

# Dataset: Slope Index of Inequality, 2014-16
# Source: Public Health Outcomes Framework
# Publisher: Public Health Outcomes Framework
# Publisher URL: https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/healthandlifeexpectancies/datasets/lifeexpectancyatbirthandatage65bylocalareasuk
# Licence: Open Government Licence

# load R packages  ---------------------------
library(tidyverse) ; library(fingertipsR) ; library(ggplot2)

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data ---------------------------
query <- fingertips_data(IndicatorID = 92901, AreaTypeID = 101) %>% 
  filter(AreaType == "District & UA") %>% 
  select(area_code = AreaCode, area_name = AreaName, period = Timeperiod, sex = Sex, 
         value = Value, LCL = LowerCI95.0limit, UCL = UpperCI95.0limit) %>% 
  filter(!is.na(value)) %>% 
  mutate(area_name = factor(area_name),
         value = as.numeric(value))

# manipulate data ---------------------------
results <- filter(query, area_name == "Trafford")

# plot data ---------------------------
p <- ggplot(results, aes(x = period, y = value, group = sex, colour = sex)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymax = UCL, ymin = LCL), width = 0.3) +
  scale_y_continuous(limits = c(0,12), breaks = scales::pretty_breaks()) +
  scale_colour_manual(values = c("Female" = "#d8b365", "Male" = "#5ab4ac")) +
  labs(x = NULL, y = "Inequality in life expectancy at birth (years)",
       title = NULL,
       caption = "Source: Public Health Outcomes Framework |  @traffordDataLab") +
  facet_wrap(~sex) +
  theme_lab() +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90),
        legend.position = "none")

# save plot / data  ---------------------------
ggsave(file = "output/figures/fig5.svg", scale = 1.2, width = 8, height = 6)

p + labs(title = "Trend in the inequality in life expectancy at birth, 2010-12 to 2014-16") +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm"),
        plot.title = element_text(face = "bold", vjust = 8, hjust = 0.5))
ggsave(file = "output/figures/fig5.png", scale = 1.3, width = 10, height = 8)

write_csv(results, "output/data/fig5.csv")
