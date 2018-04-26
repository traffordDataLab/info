#Economic inactivity and unemployment on Trafford wards
#Source: Census 2011, Economic Activity by sex by age LC6107EW
#Publisher: https://www.nomisweb.co.uk/census/2011/lc6107ew
# All usual resident 16 and over

# load R packages  --------------------------
library(ggplot2) ; library(tidyverse) ; library(scales) ; library(svglite) ; library(forcats) ; library(stringr) ;

# load Lab's ggplot2 theme  ---------------------------
source("https://trafforddatalab.github.io/assets/theme/ggplot2/theme_lab.R")

# load data  ---------------------------
df <- read_csv("https://www.nomisweb.co.uk/api/v01/dataset/nm_1046_1.bulk.csv?time=latest&measures=20100&geography=TYPE295")

# manipulate data ---------------------------
df_tidy <- select(df, "date", area_name = "geography", area_code = "geography code", `total_16+` = contains("Sex: All persons; Age: All categories: Age 16 and over; Economic Activity: All categories: Economic activity; measures: Value"), 
                  unemployed = "Sex: All persons; Age: All categories: Age 16 and over; Economic Activity: Economically active: Unemployed (including full-time students); measures: Value", 
                  inactive = "Sex: All persons; Age: All categories: Age 16 and over; Economic Activity: Economically inactive; measures: Value") %>%
  filter(area_code %in% c("E05000819", "E05000820", "E05000821", "E05000822", "E05000823", "E05000824", 
                          "E05000825", "E05000826", "E05000827", "E05000828", "E05000829", "E05000830", 
                          "E05000831", "E05000832", "E05000833", "E05000834", "E05000835", "E05000836",
                          "E05000837", "E05000838", "E05000839")) %>%
  mutate("unemployed%" = round(unemployed*100/`total_16+`,1), "inactive%" = round(inactive*100/`total_16+`, 1)) %>%
  mutate(area_name = fct_reorder(area_name, `unemployed%`))

results <- df_tidy%>%select(-c(`total_16+`,unemployed, inactive)) %>%
  gather(measure,percent,`unemployed%`:`inactive%`) %>%
  mutate(measure= str_replace(measure,"%", "")) %>%
  mutate(measure = factor(measure, levels = c("unemployed","inactive"))) 

# plot data ---------------------------
p <- ggplot(results, aes(percent,area_name)) +
  geom_line(color = "#f0f0f0") +
  geom_point(aes(shape = measure, color = measure), size = 3) +
  scale_x_continuous(labels = function(x){ paste0(x, "%") }, limits = c(0, 50)) +
  scale_colour_manual(values = c("inactive" = "#fd8d3c", "unemployed" = "#e6550d")) +
  scale_shape_manual(values = c("inactive" = 15, "unemployed" = 16)) +
  labs(x = "percentage of residents aged 16 or over", y = NULL,
       title = NULL,
       caption = "Source: 2011 Census  |  @traffordDataLab")+
  theme_lab() +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_text(hjust = 0),
        legend.position = "bottom", 
        legend.title = element_blank())

# save plot / data  ---------------------------
ggsave(file = "fig4.svg", width = 8, height = 8)


p + labs(title = "Economic inactivity and unemployment, Trafford 2011") +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm"),
        plot.title = element_text(face = "bold", vjust = 15, hjust = 1))


ggsave(file = "fig4.png", width = 8, height = 9)

write_csv(df_tidy, "fig4.csv")
 