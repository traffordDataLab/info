## Exploratory Data Analysis ##

library(tidyverse) ; library(fingertipsR)

# search for relevant indicators
fingertipsR::indicators_unique() %>%
  filter(str_detect(IndicatorName, regex('life expectancy', ignore_case = T))) %>%
  select(IndicatorID,IndicatorName) %>% View()

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

# inequality in life expectancy at birth
fingertips_data(IndicatorID = 92901) %>%
  filter(AreaName == "Trafford" & Timeperiod == "2014 - 16") %>%
  select(IndicatorName,AreaName,Sex,Value,Timeperiod) %>%
  View()

# inequality in healthy life expectancy at birth
fingertips_data(IndicatorID = 92031) %>%
  filter(AreaName == "Trafford") %>%
  select(IndicatorName,AreaName,Sex,Value,Timeperiod) %>%
  View()
