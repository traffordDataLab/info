# Exploratory Data Analysis

library(tidyverse) ; library(fingertipsR)

# life expectancy at birth ranked by local authority with quintiles
query <- fingertips_data(IndicatorID = 90366, AreaTypeID = 101) %>% 
  filter(AreaType == "District & UA" & Timeperiod == "2014 - 16") %>% 
  select(area_code = AreaCode, area_name = AreaName, sex = Sex, value = Value) %>% 
  filter(!is.na(value)) %>% 
  mutate(area_name = factor(area_name),
         value = as.numeric(value))

filter(query, sex == "Male") %>%
  mutate(rank = dense_rank(value),
         quintile = ntile(value, 5)) %>% 
  arrange(value)
