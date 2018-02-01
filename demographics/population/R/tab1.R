## Population: estimates and projections for broad age groups ##

# load R packages  ---------------------------
library(tidyverse) ; library(ggplot2)

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data ---------------------------
# population estimates - local authority based by single year of age
estimates <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=1879048225&date=latestMINUS21,latestMINUS16,latestMINUS11,latestMINUS6,latestMINUS1,latest&gender=0&age=201,203,209&measures=20100&select=date_name,geography_name,geography_code,gender_name,age_name,measures_name,obs_value,obs_status_name") %>% 
  select(area_code = GEOGRAPHY_CODE, area_name = GEOGRAPHY_NAME, year = DATE_NAME, ageband = AGE_NAME, n = OBS_VALUE) %>% 
  group_by(year) %>% 
  mutate(percent = round(n/sum(n)*100, 1)) %>% 
  select(-n) %>% 
  spread(ageband, percent)

# population projections - local authority based by single year of age
projections <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_2006_1.data.csv?geography=1879048225&projected_year=2020,2025,2030,2035&gender=0&age=201,203,209&measures=20100&select=geography_name,geography_code,projected_year_name,gender_name,age_name,measures_name,obs_value,obs_status_name") %>% 
  select(area_code = GEOGRAPHY_CODE, area_name = GEOGRAPHY_NAME, year = PROJECTED_YEAR_NAME, ageband = AGE_NAME, n = OBS_VALUE) %>% 
  group_by(year) %>% 
  mutate(percent = round(n/sum(n)*100, 1)) %>% 
  select(-n) %>% 
  spread(ageband, percent)

# manipulate data  ---------------------------
results <- bind_rows(estimates, projections)

# save data  ---------------------------
write_csv(results, "output/data/tab1.csv")
