## Burglary ##

library(tidyverse); library(ggplot2); library(forcats); library(tibble); library(sf); library(viridis); library(svglite)
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load crime data
df <- read_csv("https://github.com/traffordDataLab/open_data/raw/master/police_recorded_crime/data/trafford.csv") %>% 
  filter(category == "Burglary")

# load number of households with at least one resident (Census 2011)
denominator <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_619_1.data.csv?date=latest&geography=1237320482...1237320496,1237320498,1237320497,1237320499...1237320502,1946157089,1937768449&rural_urban=0&cell=0&measures=20100&select=date_name,geography_name,geography_code,rural_urban_name,cell_name,measures_name,obs_value,obs_status_name") %>% 
  select(area_code = GEOGRAPHY_CODE, area_name = GEOGRAPHY_NAME, denominator = OBS_VALUE)

# load geospatial data (Trafford wards)
sf <- st_read("https://github.com/traffordDataLab/spatial_data/raw/master/ward/2017/trafford_ward_generalised.geojson") %>% 
  st_as_sf(crs = 4326, coords = c("long", "lat"))

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

ggsave(file = "outputs/plots/fig1.svg", width = 6, height = 3)
ggsave(file = "outputs/plots/fig1.png", width = 6, height = 3)
write_csv(fig1, "outputs/data/fig1.csv")

# ------------------------------------------

## Observed and trend
count <- df %>% group_by(month) %>% count()
min(count$n) # minimum number of offences
max(count$n) # maximum number of offences
ts_crime <- ts(count$n, start = c(2014, 12), end = c(2017, 11), frequency = 12)
ts_decomp = stl(ts_crime, "periodic")
plot(ts_decomp)

time_series <- data.frame(
  month = seq(as.Date("2014-12-01"), by = "month", length.out = 36),
  Observed = as.vector(ts_crime),
  Trend = ts_decomp$time.series[,2]
) %>% gather(type, value, -month) %>% 
  mutate(type = factor(type, levels = c("Observed", "Trend")))
fig2 <- time_series

ggplot(data = time_series, aes(month, value)) +
  geom_line(colour = "#fc6721", size = 1) + 
  scale_x_date(date_labels = "%b-%y", date_breaks = "3 month", expand = c(0,0)) +
  scale_y_continuous(position = "right", limits = c(0, 200)) +
  labs(x = NULL, y = NULL,
       caption = "Source: data.police.uk  |  @traffordDataLab") +
  facet_grid(type ~ ., scales = "free", switch = "y") +
  theme_lab() +
  theme(axis.text.x  = element_text(angle = 90),
        panel.spacing = unit(2, "lines"),
        strip.text.y = element_text(angle = 0, vjust = 1, hjust = 0))

ggsave(file = "outputs/plots/fig2.svg", width = 6, height = 5)
ggsave(file = "outputs/plots/fig2.png", width = 6, height = 5)
write_csv(fig2, "outputs/data/fig2.csv")

# ------------------------------------------

## Rate of burglary by ward (November 2017)
fig3 <- df %>% 
  filter(month == "2017-11-01") %>% 
  group_by(area_name) %>% 
  count() %>% 
  ungroup() %>% 
  left_join(., denominator, by = "area_name") %>% 
  mutate(rate = round((n/denominator)*1000,1)) %>% 
  arrange(desc(rate)) %>%
  mutate(area_name = factor(area_name, levels = area_name)) %>% 
  add_row(area_name = "Trafford", n = sum(.$n), rate = round((n/94484)*1000,1)) %>% 
  add_row(area_name = "Greater Manchester", n = NA, rate = round((NA/1128066)*1000,1))

ggplot(fig3, aes(rate, area_name)) +
  geom_segment(aes(x = 0, y = area_name, xend = rate, yend = area_name), color = "#f0f0f0") +
  geom_point(colour = "#fc6721", size = 4) +
  geom_text(aes(label = rate, fontface = "bold"), color = "white", size = 1.5) + 
  labs(x = "Crimes per 1,000 households", y = NULL,
       title = NULL,
       caption = "Source: data.police.uk  |  @traffordDataLab") +
  theme_lab() + 
  theme(panel.grid.major = element_blank(),
        axis.text.y = element_text(hjust = 0))

ggsave(file = "outputs/plots/fig3.svg", width = 6, height = 6)
ggsave(file = "outputs/plots/fig3.png", width = 6, height = 6)
write_csv(fig3, "outputs/data/fig3.csv")

# ------------------------------------------

# Location quotients
fig4 <- df %>% 
  filter(month == "2017-11-01") %>% 
  group_by(area_name) %>% 
  count() %>% 
  ungroup() %>% 
  left_join(., denominator, by = "area_name") %>% 
  mutate(rate = round((n/denominator)*1000,1)) %>% 
  arrange(desc(rate)) %>% 
  mutate(lq = round((n/denominator)/(sum(n)/sum(denominator)),2)) %>% 
  select(area_code, area_name, everything())
  
sf_df <- left_join(sf, fig4, by = "area_code") # merge with ward attribute table

ggplot(sf_df) +
  geom_sf(aes(fill = lq), colour = "white") +
  scale_fill_viridis(option = "viridis", name = 'Burglay\nlocation\nquotient') +
  labs(x = NULL, y = NULL, title = NULL,
       caption = "Source: data.police.uk  |  @traffordDataLab") +
  theme_lab() +
  theme(axis.text = element_blank())

ggsave(file = "outputs/plots/fig4.svg", width = 6, height = 6)
ggsave(file = "outputs/plots/fig4.png", width = 6, height = 6)
write_csv(fig4, "outputs/data/fig4.csv")
