## Summary statistics ##

# Source: Department for Transport
# Publisher URL: https://data.gov.uk/dataset/road-accidents-safety-data
# Licence: Open Government Licence 3.0

# load libraries ---------------------------
library(tidyverse) ; library(ggplot2)

# load data ---------------------------
df <- read_csv("https://www.trafforddatalab.io/open_data/road_casualties/2016/STATS19_casualty_data_2016.csv") %>% 
  filter(area_name == "Trafford") 

# summary stats ---------------------------

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

