#Claimant count Trafford Time series
#Source: Claimant count by sex and age
#Publisher: https://www.nomisweb.co.uk/datasets/ucjsa
#rate, Claimants as a proportion of economically active residents aged 16+

# load R packages  ---------------------------
library(tidyverse) ; library(stringr) ; library(forcats) ; library(ggplot2) ; library(zoo)

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data  ---------------------------
# Local authority Trafford,Jan 2015-Mar 2018,Claimant Count, and Claimants as a proportion of economically active residents aged 16+,sex Total
df <-read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_162_1.data.csv?geography=1946157089&date=latestMINUS38-latest&gender=0&age=0&measure=1,3&measures=20100&select=date_name,geography_name,geography_code,gender_name,age_name,measure_name,measures_name,obs_value,obs_status_name")

# manipulate data ---------------------------
results<- df %>%
  select(date = DATE_NAME, area_code = GEOGRAPHY_CODE, area_name = GEOGRAPHY_NAME, 
                     measure = MEASURE_NAME, value = OBS_VALUE) %>% 
  mutate(date = as.Date(as.yearmon(date, "%B %Y"))) %>%
  mutate(measure = fct_recode(measure, "count" = "Claimant count",
                              "rate" = "Claimants as a proportion of economically active residents aged 16+")) %>%
  spread(measure,value)

# plot data ---------------------------
p <- ggplot(results, aes(date, rate)) +
  geom_line(size = 1, colour = "#fc6721") +
  geom_point(size = 1, colour = "#fc6721") +
  scale_x_date(date_labels = "%b-%y", date_breaks = "3 month") +
  scale_y_continuous(limits = c(0, 3), labels = function(x){ paste0(x, "%") }) +
  labs(x = NULL, y = NULL,
       caption = "Source: Claimant count, DWP  |  @traffordDataLab") +
  theme_lab() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# save plot / data  ---------------------------
ggsave(file = "fig2.svg", width = 6, height = 4)

p + labs(title = "Claimant count rates of population aged 16+ in Trafford") +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm"),
        plot.title = element_text(face = "bold", vjust = 15, hjust = 0.5))

ggsave(file = "fig2.png", width = 8, height = 6)

write_csv(results, "fig2.csv")
