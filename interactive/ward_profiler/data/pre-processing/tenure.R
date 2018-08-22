# KS402EW - Tenure ##
# Source: 2011 Census, ONS
# Publisher URL: https://www.nomisweb.co.uk/census/2011/ks402ew
# Licence: Open Government Licence 3.0

# load libraries ---------------------------
library(tidyverse) 

# load data ---------------------------
df <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_619_1.data.csv?date=latest&geography=E05000819...E05000839&rural_urban=0&cell=100,3,200,300,8&measures=20100&select=date_name,geography_name,geography_code,cell_name,measures_name,obs_value")

# tidy data ---------------------------
tenure <- df %>% 
  select(period = DATE_NAME, area_code = GEOGRAPHY_CODE, area_name = GEOGRAPHY_NAME, type = CELL_NAME, n = OBS_VALUE) %>% 
  mutate(geography = "Ward") %>%
  select(geography, period, everything())

# write results ---------------------------  
write_csv(tenure, "tenure.csv")
