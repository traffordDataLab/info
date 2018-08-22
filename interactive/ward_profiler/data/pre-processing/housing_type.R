## Housing type, 2011 ##
# Source: 2011 Census, ONS
# Publisher URL: https://www.nomisweb.co.uk/census/2011/ks401ew
# Licence: Open Government Licence

# load libraries ---------------------------
library(tidyverse)

# load data ---------------------------
df <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_618_1.data.csv?date=latest&geography=E05000819...E05000839&rural_urban=0&cell=7...13&measures=20100&select=date_name,geography_name,geography_code,cell_name,obs_value")

# tidy data ---------------------------
housing_type <- df %>% 
  select(period = DATE_NAME, area_code = GEOGRAPHY_CODE, area_name = GEOGRAPHY_NAME, type = CELL_NAME, n = OBS_VALUE) %>% 
  mutate(type = 
           factor(case_when(
             type == "Whole house or bungalow: Detached" ~ "Detached",
             type == "Whole house or bungalow: Semi-detached" ~ "Semi-detached",
             type == "Whole house or bungalow: Terraced (including end-terrace)" ~ "Terraced",
             type %in% c("Flat, maisonette or apartment: Purpose-built block of flats or tenement",
                         "Flat, maisonette or apartment: Part of a converted or shared house (including bed-sits)",
                         "Flat, maisonette or apartment: In a commercial building") ~ "Flat",
             type == "Caravan or other mobile or temporary structure" ~ "Caravan"))) %>% 
  group_by(period, area_code, area_name, type) %>% 
  summarise(n = sum(n))

# write data ---------------------------
write_csv(housing_type, "housing_type.csv")
