# QS204EW - Main language ##
# Source: 2011 Census, ONS
# Publisher URL: https://www.nomisweb.co.uk/census/2011/qs204ew
# Licence: Open Government Licence 3.0

# load libraries ---------------------------
library(tidyverse) ; library(stringr)

# load data ---------------------------
df <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_525_1.data.csv?date=latest&geography=E05000819...E05000839&rural_urban=0&cell=1...103&measures=20100&select=geography_name,geography_code,cell_name,obs_value")

# tidy data ---------------------------
languages <- df %>% 
  select(area_code = GEOGRAPHY_CODE, area_name = GEOGRAPHY_NAME, language = CELL_NAME, n = OBS_VALUE) %>% 
  mutate(language = str_replace(language, "Main Language: ", "")) %>% 
  mutate(language = str_replace(language, "African Language: |Caribbean Creole: |East Asian Language: |Other European Language \\(EU\\): |Other European Language \\(non EU\\): |Other European Language \\(non-national\\): |Other Languages: |Other UK language: |Sign Language: |South Asian Language: |West/Central Asian Language: ", "")) %>% 
  mutate(language = str_replace(language, "; measures: Value", "")) %>% 
  filter(language != "Total") %>% 
  mutate(geography = "Ward",
         period = 2011) %>% 
  select(geography, period, everything())

# write results ---------------------------
write_csv(languages, "languages.csv")
