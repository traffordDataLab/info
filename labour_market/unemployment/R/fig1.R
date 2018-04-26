#Rate of unemployment of residents aged 16+ in Trafford. 
#Source: Labour Force Survey, Office for National Statistics
#Publisher: https://www.nomisweb.co.uk/datasets/apsnew

# load R packages  ---------------------------
library(tidyverse) ; library(forcats) ; library(ggplot2) ; library(zoo)

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data  ---------------------------
df <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_17_5.data.csv?geography=1946157089&date=latestMINUS10-latest&variable=83&measures=20599,21001,21002,21003&select=date_name,geography_name,geography_code,variable_name,measures_name,obs_value,obs_status_name") 

# manipulate data ---------------------------
results <- df %>%
  select(date = DATE_NAME, area_code = GEOGRAPHY_CODE, area_name = GEOGRAPHY_NAME, 
                measure = VARIABLE_NAME, type = MEASURES_NAME, value = OBS_VALUE) %>% 
  mutate(dateS = as.Date(as.yearmon(gsub(".*-", "", date), "%B %Y")))%>%
  mutate(date = gsub("20", "", date)) %>%
  spread(type,value) %>%
  arrange(dateS) %>%
  mutate(date = factor(date, levels = date)) %>% 
  select(-c(Denominator, Numerator, dateS), rate = "Variable", confidence = "Confidence")

# plot data ---------------------------
p <- ggplot(results, aes(x = date, y = rate)) +
  geom_line(aes(group = 1), size = 1, colour = "#fc6721")+
  geom_point(size = 1, colour = "#fc6721")+
  scale_y_continuous(limits = c(0, NA), labels = function(x){ paste0(x, "%") })+
  labs(x = NULL, y = NULL,
       caption = "Source: Labour Force Survey, ONS  |  @traffordDataLab") +
  theme_lab() +
  theme(axis.text.x = element_text(size = 9, angle = 90, hjust = 1))

# save plot / data  ---------------------------
ggsave(file = "fig1.svg", width = 6, height = 5)

p + labs(title = "Unemployment rate of population aged 16+ in Trafford") +
    theme(plot.margin = unit(c(2, 2, 2, 2), "cm"),
          plot.title = element_text(face = "bold", vjust = 15, hjust = 0.5))

ggsave(file = "fig1.png", width = 8, height = 7)

write_csv(results, "fig1.csv")
