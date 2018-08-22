## Housing size, 2011 ##
# Source: 2011 Census, ONS
# Publisher URL: https://www.nomisweb.co.uk/census/2011/qs411ew
# Licence: Open Government Licence

# load libraries ---------------------------
library(tidyverse)

# load data ---------------------------
df <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_543_1.data.csv?date=latest&geography=E05000819...E05000839&rural_urban=0&cell=1...6&measures=20100&select=date_name,geography_name,geography_code,cell_name,obs_value")

# tidy data ---------------------------
housing_size <- df %>% 
  select(period = DATE_NAME, area_code = GEOGRAPHY_CODE, area_name = GEOGRAPHY_NAME, bedrooms = CELL_NAME, n = OBS_VALUE)

# write data ---------------------------
write_csv(housing_size, "housing_size.csv")
