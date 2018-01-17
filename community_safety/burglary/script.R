## Burglary ##

library(tidyverse); library(ggplot2); library(forcats); library(tibble); library(svglite)
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load crime data
df <- read_csv("data/burglary.csv")

# load ONS mid-year population estimates (2016)
pop <- read_csv("https://github.com/traffordDataLab/open_data/raw/master/mid-year_pop_estimates_2016/ONS_mid-year_population_estimates_2016.csv") %>% 
  filter(lad16nm == "Trafford") %>% 
  select(area_name = wd16nm, total_pop) %>% 
  add_row(area_name = "Trafford", total_pop = sum(.$total_pop)) # add Trafford total

# ------------------------------------------

## Count of burglary in Trafford by month
fig1 <- df %>% 
  filter(month >= "2016-11-01") %>% 
  group_by(month) %>% 
  count() 

ggplot(fig1, aes(month, n)) +
  geom_col(fill = "#fc6721", alpha = 0.8) +
  scale_x_date(date_labels = "%b-%y", date_breaks = "1 month", expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = NULL, y = NULL,
       caption = "Source: data.police.uk  |  @traffordDataLab") +
  theme_lab() +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x  = element_text(angle = 90))

ggsave(file = "outputs/fig1.svg", width = 6, height = 3)
ggsave(file = "outputs/fig1.png", width = 6, height = 3)
write_csv(fig1, "outputs/fig1.csv")

# ------------------------------------------

## Trend and seasonality
count <- df %>% group_by(month) %>% count()
ts_crime <- ts(count$n, start = c(2014, 12), end = c(2017, 11), frequency = 12)
ts_decomp = stl(ts_crime, "periodic")
time_series <- data.frame(
  month = seq(as.Date("2014-12-01"), by = "month", length.out = 36),
  Observed = as.vector(ts_crime),
  Trend = ts_decomp$time.series[,2],
  Seasonal = ts_decomp$time.series[,1]
) %>% gather(type, value, -month) %>% 
  mutate(type = factor(type, levels = c("Observed", "Trend", "Seasonal")))
fig2 <- time_series

ggplot(data = time_series, aes(month, value)) +
  geom_line(colour = "#fc6721", size = 1) + 
  scale_x_date(date_labels = "%b-%y", date_breaks = "3 month", expand = c(0,0)) +
  scale_y_continuous(position = "right") +
  labs(x = NULL, y = NULL,
       caption = "Source: data.police.uk  |  @traffordDataLab") +
  facet_grid(type ~ ., scales = "free", switch = "y") +
  theme_lab() +
  theme(axis.text.x  = element_text(angle = 90),
        panel.spacing = unit(2, "lines"),
        strip.text.y = element_text(angle = 0, vjust = 1, hjust = 0))

ggsave(file = "outputs/fig2.svg", width = 6, height = 5)
ggsave(file = "outputs/fig2.png", width = 6, height = 5)
write_csv(fig2, "outputs/fig2.csv")

# ------------------------------------------

## Rate of burglary by ward (November 2017)
fig3 <- df %>% 
  filter(month == "2017-11-01") %>% 
  group_by(area_name) %>% 
  count() %>% 
  ungroup() %>% 
  add_row(area_name = "Trafford", n = sum(.$n)) %>% # add Trafford total
  left_join(., pop, by = "area_name") %>% 
  mutate(rate = round((n/total_pop)*1000,1)) %>% 
  arrange(desc(rate)) %>%
  mutate(area_name = factor(area_name, levels = area_name))

ggplot(fig3, aes(rate, area_name)) +
  geom_segment(aes(x = 0, y = area_name, xend = rate, yend = area_name), color = "#f0f0f0") +
  geom_point(colour = "#fc6721", size = 4) +
  geom_text(aes(label = rate, fontface = "bold"), color = "white", size = 1.5) + 
  labs(x = "Crimes per 1,000 residents", y = NULL,
       title = NULL,
       caption = "Source: data.police.uk  |  @traffordDataLab") +
  theme_lab() + 
  theme(panel.grid.major = element_blank(),
        axis.text.y = element_text(hjust = 0))

ggsave(file = "outputs/fig3.svg", width = 6, height = 6)
ggsave(file = "outputs/fig3.png", width = 6, height = 6)
write_csv(fig3, "outputs/fig3.csv")
