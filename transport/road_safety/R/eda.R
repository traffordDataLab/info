## Exploratory Data Analysis ##

# Source: Department for Transport
# Publisher URL: https://data.gov.uk/dataset/road-accidents-safety-data
# Licence: Open Government Licence 3.0

# load libraries ---------------------------
library(tidyverse) ; library(ggplot2)

# load data ---------------------------
df <- read_csv("https://www.trafforddatalab.io/open_data/road_casualties/2016/STATS19_casualty_data_2016.csv") %>% 
  filter(area_name == "Trafford") 

# Total collisions
n_distinct(df$AREFNO)

# Total casualties
df %>% tally()

# Total casualties by severity
df %>% 
  group_by(severity) %>%
  summarize(frequency = n())

# Total KSIs
df %>% 
  filter(severity != "Slight") %>% 
  summarize(frequency = n())

# Percentage collisions by month                                                      
df %>% 
  distinct(AREFNO, .keep_all = TRUE) %>% 
  group_by(month) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  mutate(percent = round(n/sum(n)*100, 1)) %>% 
  arrange(desc(percent))

# Percentage collisions by day
df %>% 
  #  filter(temp, hour > "0600" & hour < "1900") 
  distinct(AREFNO, .keep_all = TRUE) %>% 
  group_by(day) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  mutate(percent = round(n/sum(n)*100, 1)) %>% 
  arrange(desc(percent))

# Percentage collisions by hour
df %>%
  # filter(hour >= 13 & hour <= 19) %>% 
  filter(mode == "Pedestrian") %>% 
  distinct(AREFNO, .keep_all = TRUE) %>% 
  group_by(hour) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  mutate(percent = round(n/sum(n)*100, 1)) %>% 
  arrange(desc(percent))
# %>% summarise(total = sum(n))
# %>% ggplot() + geom_col(aes(x = hour, y = n), fill = "steelblue")

