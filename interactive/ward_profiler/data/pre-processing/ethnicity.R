# KS201EW - Ethnic group ##
# Source: 2011 Census, ONS
# Publisher URL: https://www.nomisweb.co.uk/census/2011/ks201ew
# Licence: Open Government Licence 3.0

# load libraries ---------------------------
library(tidyverse) ; library(stringr)

# load data ---------------------------
df <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_608_1.data.csv?date=latest&geography=E05000819...E05000839&rural_urban=0&cell=100,200,300,400,500&measures=20100&select=date_name,geography_name,geography_code,rural_urban_name,cell_name,measures_name,obs_value,obs_status_name")

# tidy data ---------------------------
ethnicity <- df %>% 
  select(period = DATE_NAME, area_code = GEOGRAPHY_CODE, area_name = GEOGRAPHY_NAME, group = CELL_NAME, n = OBS_VALUE) %>% 
  mutate(group = case_when(
    group == "Asian/Asian British" ~ "Asian",
    group == "Black/African/Caribbean/Black British" ~ "Black",
    group == "Mixed/multiple ethnic groups" ~ "Mixed",
    group == "Other ethnic group" ~ "Other",
    TRUE ~ group),
    geography = "Ward") %>%
  select(geography, period, everything())

# write results ---------------------------  
write_csv(ethnicity, "ethnicity.csv")
