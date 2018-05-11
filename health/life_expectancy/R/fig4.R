## Life expectancy: Inequality in life expectancy at birth, 2013-15 ##

# Dataset: Slope index of inequality in life expectancy at birth, 2013-2015
# Source: Public Health Outcomes Framework
# Publisher: Public Health Outcomes Framework
# Publisher URL: 
# Licence: Open Government Licence

# load R packages  ---------------------------
library(tidyverse) ; library(ggplot2)

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data ---------------------------
df <- read_csv("data/SII_LE.csv")

# plot data ---------------------------
p <- ggplot(df, aes(x = factor(decile), y = LE, colour = sex, group = sex)) +
  geom_point() +
  geom_errorbar(aes(ymax = UCL, ymin = LCL), width = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(limits = c(65,95), breaks = seq(65,95,5), expand = c(0,0)) +
  scale_colour_manual(values = c("Female" = "#d8b365", "Male" = "#5ab4ac")) +
  labs(x = "IMD decile", y = "Life expectancy at birth (years)",
       title = NULL,
       caption = "Source: Public Health Outcomes Framework |  @traffordDataLab") +
  facet_grid(sex ~ .) +  
  theme_lab() +
  theme(panel.spacing = unit(2, "lines"), 
        panel.grid.major.x = element_blank(),
        legend.position = "none")

# save plot / data  ---------------------------
ggsave(file = "output/figures/fig4.svg", scale = 1.2, width = 8, height = 6)

p + labs(title = "Life expectancy at birth by deprivation decile, 2013-15") +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm"),
        plot.title = element_text(face = "bold", vjust = 8, hjust = 0.5))
ggsave(file = "output/figures/fig4.png", scale = 1.3, width = 10, height = 8)

write_csv(df, "output/data/fig4.csv")

